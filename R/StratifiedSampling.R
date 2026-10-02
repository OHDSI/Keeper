# Copyright 2026 Observational Health Data Sciences and Informatics
#
# This file is part of Keeper
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Review Keeper profiles using an LLM and stratified sampling
#'
#' @param keeper      Output from the [generateKeeper()] function.
#' @param settings    Prompt creating settings as created using the [createPromptSettings] function.
#' @param phenotypeName The name of the disease to use in the prompt. If not provided, the name in the Keeper input will
#'                      be used.
#' @param clinicalDefinition Optionally prove a text blob with the definition and any other information about the
#'                            phenotype.
#' @param client      An LLM client created using the `ellmer` package.
#' @param cacheFolder A folder where the LLM responses are cached. If the process terminates for some
#'                    reason, it can pick up where it left off using the cache.
#'
#' @returns
#' A tibble with these columns:
#'
#' - `generatedId`
#' - `isCase`, with possible values "yes" or "no",
#' - `certainty`, certainty of the LLM in its decision, can be "high" or "low".
#' - `justification`, written by the LLM.
#' - `cohortPrevalence`, prevalence of the cohort in the entire population.
#' - `model`, the LMM used to review.
#' - `keeperVersion`, the version of the Keeper package.
#' 
#' When the Keeper profiles were generated with `removePii = FALSE`, the following columns are also included:
#' 
#' - `personId`
#' - `cohortStartDate`
#'
#' @export
reviewCasesUsingStratifiedSampling <- function(keeper,
                                               settings = createPromptSettings(),
                                               phenotypeName = NULL,
                                               clinicalDefinition = NULL,
                                               client,
                                               cacheFolder) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(keeper, add = errorMessages)
  checkmate::assertNames(colnames(keeper), must.include = c(
    "generatedId",
    "startDay",
    "endDay",
    "conceptId",
    "conceptName",
    "category",
    "target",
    "extraData"
  ), add = errorMessages)
  checkmate::assertClass(settings, "PromptSettings", add = errorMessages)
  checkmate::assertCharacter(phenotypeName, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(clinicalDefinition, null.ok = TRUE, add = errorMessages)
  checkmate::assertR6(client, "Chat", add = errorMessages)
  checkmate::assertCharacter(cacheFolder, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  if (!"doiBin" %in% keeper$category) {
    stop("Keeper does not contain stratification information. Run `createSensitiveCohort()` with `addStratificationInfo = TRUE")
  }
  
  # Create strata based on stratification information
  stratificationInfo <- inner_join(
    keeper |>
      filter(.data$category == "doiBin") |>
      transmute(.data$generatedId, doiBin = as.numeric(.data$conceptName)),
    keeper |>
      filter(.data$category == "categoryBin") |>
      transmute(.data$generatedId, categoryBin = as.numeric(.data$conceptName)),
    by = join_by("generatedId")
  )
  minStratumSize <- max(200, 0.05 * nrow(stratificationInfo))
  stratification <- mergeStrata(stratificationInfo, minStratumSize)
  strataSizes <- stratification |>
    group_by(.data$stratumId) |>
    summarise(personCount = n())
  message(sprintf("Defined %s strata with sizes: %s ",
                  nrow(strataSizes), 
                  paste(sort(strataSizes$personCount), collapse = ", ")))
  
  if (length(strataSizes) == 1) {
    # TODO: don't use stratification
  } else {
    phase1SampleSize <- 100
    message(sprintf("Phase 1: Review %d per stratum to estimate prevalence", probeSampleSize))
    
    strata <- stratification |>
      group_by(.data$stratumId) |>
      group_split()
    sampledGeneratedIdsPhase1 <- unlist(lapply(strata, function(x) uniformSelect(x$generatedId, phase1SampleSize)))
    keeperSample <- keeper |>
      filter(.data$generatedId %in% sampledGeneratedIdsPhase1)
    llmReviewsPhase1 <- reviewCases(keeper = keeperSample,
                                   settings = settings,
                                   phenotypeName = phenotypeName,
                                   clinicalDefinition = clinicalDefinition,
                                   client = client,
                                   cacheFolder = cacheFolder)
    # llmReviewsPhase1 <- llmReviews |>
    #   filter(.data$generatedId %in% sampledGeneratedIdsPhase1)
    
    prevalencePerStratum <- llmReviewsPhase1 |>
      inner_join(stratification, by = join_by("generatedId")) |>
      group_by(.data$stratumId) |>
      summarise(p = mean(.data$isCase == "yes")) |>
      inner_join(strataSizes, join_by("stratumId"))
    overallPrevalence <- prevalencePerStratum  |>
      summarise(overallP = sum(personCount * p) / sum(personCount)) |>
      pull()
    message(sprintf("Overal prevalence in sensitive cohort estimated to be %0.1f%%, with per-stratum prevalences ranging from %0.1f%% to %0.1f%%",
                    100 * overallPrevalence,
                    100 * min(prevalencePerStratum$p),
                    100 * max(prevalencePerStratum$p)))
    
    sUnstratified <- sqrt(overallPrevalence * (1-overallPrevalence))
    sStratified <- prevalencePerStratum |>
      summarise(s = sum(personCount * sqrt(p * (1-p))) / sum(personCount)) |>
      pull()
    requiredSampleSize <- round(sum(strataSizes$personCount) * sStratified / sUnstratified)
    message(sprintf("Stratified sampling can achieve roughly the same power using %d instead of %d samples",
                    requiredSampleSize,
                    sum(strataSizes$personCount)))
    
    message(sprintf("Phase 2: Review %d additional profiles using Neyman allocation",
                    requiredSampleSize - length(sampledGeneratedIdsPhase1)))
    phase2Allocations <- calculatePhase2Allocation(stratumData = prevalencePerStratum,
                                                   nOpt = requiredSampleSize,
                                                   phase1Count = phase1SampleSize)
    samplePhase2 <- function(stratum) {
      remainingGeneratedIds <- stratum |>
        filter(!.data$generatedId %in% sampledGeneratedIdsPhase1) |>
        pull(.data$generatedId)
      phase2SampleSize <- phase2Allocations |>
        filter(.data$stratumId == stratum$stratumId[1]) |>
        pull(.data$phase2Count)
      return(uniformSelect(remainingGeneratedIds, phase2SampleSize))
    }
    sampledGeneratedIdsPhase2 <- unlist(lapply(strata, samplePhase2))
    keeperSample <- keeper |>
      filter(.data$generatedId %in% sampledGeneratedIdsPhase2)
    llmReviewsPhase1 <- reviewCases(keeper = keeperSample,
                                   settings = settings,
                                   phenotypeName = phenotypeName,
                                   clinicalDefinition = clinicalDefinition,
                                   client = client,
                                   cacheFolder = cacheFolder)
    # llmReviewsPhase2 <- llmReviews |>
    #   filter(.data$generatedId %in% sampledGeneratedIdsPhase2)
    
    llmReviews <- bind_rows(
      llmReviewsPhase1,
      llmReviewsPhase2
    ) |>
      inner_join(stratification |>
                   select("generatedId", "stratumId"),
                 by = join_by("generatedId"))
    
  }
  return(llmReviews)
}

uniformSelect <- function(items, size) {
  totalSize <- length(items)
  if (size == 1) {
    idx <- floor((totalSize + 1) / 2)
  } else {
    idx <- floor(1 + (0:(size - 1)) * (totalSize - 1) / (size - 1))
  }
  return(items[idx])
}

mergeStrata <- function(stratificationInfo, minStratumSize) {
  # Step 1: Initialize Grid
  stratificationWork <- stratificationInfo |>
    group_by(.data$doiBin, .data$categoryBin) |>
    summarise(personCount = n(), .groups = "drop") |>
    mutate(stratumId = row_number())
  if (nrow(stratificationWork) <= 1) return(stratificationInfo)
  
  # Step 2: Main Loop
  while(TRUE) {
    strataCounts <- stratificationWork |>
      group_by(.data$stratumId) |>
      summarise(totalCount = sum(.data$personCount), .groups = "drop")
    
    # Terminate if all strata meet the threshold or only 1 remains
    if (min(strataCounts$totalCount) >= minStratumSize || nrow(strataCounts) <= 1) {
      break
    }
    
    # Identify the target stratum with the absolute lowest count
    targetId <- strataCounts |> arrange(.data$totalCount) |> slice(1) |> pull(.data$stratumId)
    
    # Separate points of the target stratum from all other strata
    targetPoints <- stratificationWork |> filter(.data$stratumId == targetId) |> select(t1 = "doiBin", t2 = "categoryBin")
    otherPoints <- stratificationWork |> filter(.data$stratumId != targetId) |> select(o1 = "doiBin", o2 = "categoryBin", otherId = "stratumId")
    
    # Step 3: Calculate coordinate distances (cross join)
    distances <- merge(targetPoints, otherPoints, by = NULL) |>
      mutate(
        dx1 = abs(.data$t1 - .data$o1),
        dx2 = abs(.data$t2 - .data$o2),
        manhattan = .data$dx1 + .data$dx2
      )
    
    # Identify adjacent candidates
    intraX1Ids <- distances |> filter(.data$dx1 == 0, .data$dx2 == 1) |> pull(.data$otherId) |> unique()
    interX1Ids <- distances |> filter(.data$dx1 == 1, .data$dx2 == 0) |> pull(.data$otherId) |> unique()
    
    # Step 4: Prioritized Selection
    if (length(intraX1Ids) > 0) {
      # Priority 1: Merge adjacent categoryBin within the same doiBin
      candidateId <- strataCounts |>
        filter(.data$stratumId %in% intraX1Ids) |>
        arrange(.data$totalCount) |>
        slice(1) |>
        pull(.data$stratumId)
      
    } else if (length(interX1Ids) > 0) {
      # Priority 2: Merge adjacent doiBin across same categoryBin
      candidateId <- strataCounts |>
        filter(.data$stratumId %in% interX1Ids) |>
        arrange(.data$totalCount) |>
        slice(1) |>
        pull(.data$stratumId)
      
    } else {
      # Priority 3: Emergency fallback to closest overall Manhattan distance
      closestIds <- distances |>
        arrange(.data$manhattan) |>
        pull(.data$otherId) |> unique()
      
      candidateId <- strataCounts |>
        filter(.data$stratumId %in% closestIds) |>
        arrange(.data$totalCount) |>
        slice(1) |>
        pull(.data$stratumId)
    }
    
    # Step 5: Merge target into candidate
    stratificationWork <- stratificationWork |>
      mutate(stratumId = ifelse(.data$stratumId == targetId, candidateId, .data$stratumId))
  }
  stratification <- stratificationInfo |>
    inner_join(stratificationWork |>
                 select("doiBin", "categoryBin", "stratumId"),
               by = join_by("doiBin", "categoryBin"))
  return(stratification)
}

calculatePhase2Allocation <- function(stratumData, nOpt, phase1Count) {
  # Ensure nOpt is an integer
  nOpt <- round(nOpt)
  stratumCount <- nrow(stratumData)
  targetPhase2Total <- nOpt - (stratumCount * phase1Count)
  
  if (targetPhase2Total <= 0) {
    message("Total optimal size is less than or equal to Phase 1 samples. No Phase 2 needed.")
    stratumData$phase2Count <- 0
    stratumData$totalCount <- phase1Count
    return(stratumData)
  }
  
  # 1. Variance Safeguarding: 
  # Prevent p=0 or p=1 from creating absolute 0 variance, which forces weights to 0.
  pSafe <- pmax(0.0001, pmin(0.9999, stratumData$p))
  stratumSd <- sqrt(pSafe * (1 - pSafe))
  
  # 2. Compute Base Neyman Weights
  baseWeights <- (stratumData$personCount * stratumSd)
  baseWeights <- baseWeights / sum(baseWeights)
  
  # 3. Iterative Constrained Allocation
  # Ensures no stratum is allocated fewer samples than it already received in Phase 1
  eligibleStrata <- rep(TRUE, stratumCount)
  finalTotalAllocation <- rep(0, stratumCount)
  remainingBudget <- nOpt
  
  repeat {
    currentWeightSum <- sum(baseWeights[eligibleStrata])
    
    # Pro-rata distribute the budget only among eligible strata
    tempAllocation <- rep(0, stratumCount)
    tempAllocation[eligibleStrata] <- remainingBudget * (baseWeights[eligibleStrata] / currentWeightSum)
    
    # Identify strata that mathematically "should" get less than phase1Count
    violators <- eligibleStrata & (tempAllocation <= phase1Count)
    
    if (any(violators)) {
      # Cap these strata exactly at phase1Count and remove them from eligibility
      finalTotalAllocation[violators] <- phase1Count
      eligibleStrata[violators] <- FALSE
      # Update the remaining budget to redistribute in the next loop
      remainingBudget <- nOpt - sum(finalTotalAllocation[!eligibleStrata])
    } else {
      # All remaining eligible strata are above phase1Count
      finalTotalAllocation[eligibleStrata] <- tempAllocation[eligibleStrata]
      break
    }
  }
  
  # 4. Integer Rounding (Largest Remainder Method)
  # Extracts the continuous Phase 2 allocation and forces strict integer sums
  phase2Continuous <- finalTotalAllocation - phase1Count
  phase2Count <- floor(phase2Continuous)
  
  remainders <- phase2Continuous - phase2Count
  shortfall <- targetPhase2Total - sum(phase2Count)
  
  if (shortfall > 0) {
    # Distribute +1 to the strata with the highest decimal remainders
    orderIndices <- order(remainders, decreasing = TRUE)
    phase2Count[orderIndices[1:shortfall]] <- phase2Count[orderIndices[1:shortfall]] + 1
  }
  
  # 5. Append to dataframe
  stratumData$phase2Count <- phase2Count
  stratumData$totalCount <- phase2Count + phase1Count
  
  return(stratumData)
}

#' Compute Bayesian Sensitivity and PPV for Stratified Samples
#'
#' @param sampleData Data frame with: personId, stratumId, isCase (logical), inCohort (logical)
#' @param stratumData Data frame with: stratumId, personCount (Total N_h in the HSC for that stratum)
#' @param priorWeight The pseudocount added to each cell (0.5 for Jeffreys prior, 1.0 for uniform)
#' @param nDraws Number of posterior draws for the simulation
#' @param ciLevel The confidence/credible interval level (default 0.95)
#' @return A data frame containing the median estimate and credible intervals for Sens and PPV
computeMetricsBayesian <- function(sampleData, stratumData, priorWeight = 0.5, nDraws = 10000, ciLevel = 0.95) {
  
  # 1. Aggregate empirical counts per stratum
  counts <- sampleData %>%
    group_by(stratumId) %>%
    summarise(
      tpCount = sum(isCase & inCohort),
      fnCount = sum(isCase & !inCohort),
      fpCount = sum(!isCase & inCohort),
      tnCount = sum(!isCase & !inCohort),
      .groups = "drop"
    )
  
  # 2. Merge with population counts to get N_h weights
  strataMerged <- inner_join(stratumData, counts, by = "stratumId")
  nStrata <- nrow(strataMerged)
  
  # Vectors to hold the simulated global totals across all draws
  globalTotalTp <- numeric(nDraws)
  globalTotalCases <- numeric(nDraws)
  globalTotalInCohort <- numeric(nDraws)
  
  # 3. Simulate from the posterior distribution for each stratum
  for (i in 1:nStrata) {
    # Add prior weights to observed counts for the Dirichlet parameters
    aTp <- strataMerged$tpCount[i] + priorWeight
    aFn <- strataMerged$fnCount[i] + priorWeight
    aFp <- strataMerged$fpCount[i] + priorWeight
    aTn <- strataMerged$tnCount[i] + priorWeight
    
    # Draw from Dirichlet using independent Gamma distributions
    gTp <- rgamma(nDraws, shape = aTp, rate = 1)
    gFn <- rgamma(nDraws, shape = aFn, rate = 1)
    gFp <- rgamma(nDraws, shape = aFp, rate = 1)
    gTn <- rgamma(nDraws, shape = aTn, rate = 1)
    
    gSum <- gTp + gFn + gFp + gTn
    
    # Calculate the posterior proportion of each category in this stratum
    propTp <- gTp / gSum
    propFn <- gFn / gSum
    propFp <- gFp / gSum
    
    # Scale up to the total stratum population
    stratumNh <- strataMerged$personCount[i]
    
    stratumTotalTp <- propTp * stratumNh
    stratumTotalCases <- (propTp + propFn) * stratumNh
    stratumTotalInCohort <- (propTp + propFp) * stratumNh
    
    # Add to the global totals
    globalTotalTp <- globalTotalTp + stratumTotalTp
    globalTotalCases <- globalTotalCases + stratumTotalCases
    globalTotalInCohort <- globalTotalInCohort + stratumTotalInCohort
  }
  
  # 4. Compute metrics for each draw at the population level
  sensDraws <- globalTotalTp / globalTotalCases
  ppvDraws <- globalTotalTp / globalTotalInCohort
  
  # 5. Extract summary statistics (Median and Credible Intervals)
  alpha <- 1 - ciLevel
  lowerProb <- alpha / 2
  upperProb <- 1 - (alpha / 2)
  
  results <- data.frame(
    metric = c("Sensitivity", "PPV"),
    estimate = c(median(sensDraws), median(ppvDraws)),
    lowerCi = c(quantile(sensDraws, lowerProb), quantile(ppvDraws, lowerProb)),
    upperCi = c(quantile(sensDraws, upperProb), quantile(ppvDraws, upperProb))
  )
  
  # Remove rownames added by quantile
  rownames(results) <- NULL 
  
  return(results)
}


