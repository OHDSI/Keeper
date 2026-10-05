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
    message(sprintf("Phase 1: Review %d per stratum to estimate prevalence", phase1SampleSize))
    
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
    
    sUnstratified <- sqrt(overallPrevalence * (1 - overallPrevalence))
    sStratified <- prevalencePerStratum |>
      summarise(s = sum(personCount * sqrt(p * (1 - p))) / sum(personCount)) |>
      pull()
    requiredSampleSize <- round(sum(strataSizes$personCount) * (sStratified / sUnstratified)^2)
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
    llmReviewsPhase2 <- reviewCases(keeper = keeperSample,
                                    settings = settings,
                                    phenotypeName = phenotypeName,
                                    clinicalDefinition = clinicalDefinition,
                                    client = client,
                                    cacheFolder = cacheFolder)
    # llmReviewsPhase2 <- llmReviews |>
    #   filter(.data$generatedId %in% sampledGeneratedIdsPhase2)
    
    stratification <- stratification |>
      distinct(.data$doiBin, .data$categoryBin, .data$stratumId)
    result <- list(
      llmReviews = bind_rows(
        llmReviewsPhase1,
        llmReviewsPhase2
      ) ,
      stratification = stratification
    )
  }
  return(result)
}

#' Create reference cohort table names
#' 
#' @description
#' Derives a metadata table name from the reference cohort table name in a systematic way.
#'
#' @param referenceCohortTable The name of the cohort table itself. The metadata table name will be derived from this
#'                             by appending '_metadata'.
#'
#' @returns
#' A list with `referenceCohortTable` and `referenceCohortMetadataTable`.
#'
#' @export
createReferenceCohortTableNamesUsingStratifiedSample <- function(referenceCohortTable) {
  tableNames <- list(
    referenceCohortTable = referenceCohortTable,
    referenceCohortMetadataTable = paste0(referenceCohortTable, "_metadata"),
    referenceSensitiveCohortTable = paste0(referenceCohortTable, "_sensitive")
  )
  return(tableNames)
}

#' Upload a reference cohort
#'
#' @description
#' A reference cohort is typically a large sample (e.g. 10,000 persons) of a highly-sensitive cohort (as created using 
#' [createSensitiveCohort()]), reviewed by an LLM (using [reviewCases()]).
#' 
#' The reference cohort can be used to compute operating characteristics of a cohort definition for the same phenotype
#' using [computeCohortOperatingCharacteristics()].
#'
#' @template Connection
#'
#' @template TempEmulationSchema
#'
#' @param referenceCohortDatabaseSchema  The name of the database schema where the reference
#'                                       cohort will be stored.
#' @param referenceCohortTableNames      The table names where the reference cohort and metadata will be stored. Should
#'                                       be created using [createReferenceCohortTableNamesUsingStratifiedSample()].
#' @param referenceCohortDefinitionId    The cohort definition ID that will be used for the
#'                                       reference cohort.
#' @param createReferenceCohortTables    Create the reference cohort and metadata tables? If `TRUE` and the tables
#'                                       already exists they will first be deleted.
#' @param stratifiedReviews              An object as generated by [reviewCasesUsingStratifiedSampling()].
#'
#' @returns
#' This function does not return a value. It is called for the side effect of uploading
#' the reference cohort to the database.
#'
#' @export
uploadReferenceCohortUsingStratifiedSample <- function(connectionDetails = NULL,
                                                       connection = NULL,
                                                       tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                                                       sensitiveCohortDatabaseSchema,
                                                       sensitiveCohortTable,
                                                       sensitiveCohortDefinitionId,
                                                       referenceCohortDatabaseSchema,
                                                       referenceCohortTableNames,
                                                       referenceCohortDefinitionId,
                                                       createReferenceCohortTables = FALSE,
                                                       stratifiedReviews) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(connectionDetails, "ConnectionDetails", null.ok = TRUE, add = errorMessages)
  checkmate::assertClass(connection, "DatabaseConnectorConnection", null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(sensitiveCohortDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertCharacter(sensitiveCohortTable, len = 1, add = errorMessages)
  checkmate::assertIntegerish(sensitiveCohortDefinitionId, len = 1, add = errorMessages)
  checkmate::assertCharacter(referenceCohortDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertList(referenceCohortTableNames, len = 3, add = errorMessages)
  checkmate::assertNames(names(referenceCohortTableNames), must.include = c(
    "referenceCohortTable",
    "referenceSensitiveCohortTable",
    "referenceCohortMetadataTable"
  ), add = errorMessages)
  checkmate::assertIntegerish(referenceCohortDefinitionId, len = 1, add = errorMessages)
  checkmate::assertLogical(createReferenceCohortTables, len = 1, add = errorMessages)
  checkmate::assertList(stratifiedReviews, len = 2, add = errorMessages)
  checkmate::assertNames(names(stratifiedReviews), must.include = c(
    "llmReviews",
    "stratification"
  ), add = errorMessages)
  checkmate::reportAssertions(errorMessages)
  if (is.null(connectionDetails) && is.null(connection)) {
    stop("Must provide either connectionDetails or a connection.")
  }
  
  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }
  DatabaseConnector::assertTempEmulationSchemaSet(
    dbms = DatabaseConnector::dbms(connection),
    tempEmulationSchema = tempEmulationSchema
  )
  tableNamesMinusSensitive <- referenceCohortTableNames
  tableNamesMinusSensitive$referenceSensitiveCohortTable <- NULL
  uploadReferenceCohort(
    connection = connection,
    tempEmulationSchema = tempEmulationSchema,
    referenceCohortDatabaseSchema = referenceCohortDatabaseSchema,
    referenceCohortTableNames = tableNamesMinusSensitive,
    referenceCohortDefinitionId = referenceCohortDefinitionId,
    createReferenceCohortTables = createReferenceCohortTables,
    reviews = stratifiedReviews$llmReviews
  )
  
  if (createReferenceCohortTables) {
    message("Creating reference sensitive cohort table")
    sql <- "
      DROP TABLE IF EXISTS @reference_cohort_database_schema.@reference_sensitive_cohort_table;

      CREATE TABLE @reference_cohort_database_schema.@reference_sensitive_cohort_table (
        cohort_definition_id INT,
        subject_id BIGINT,
        cohort_start_date DATE,
        stratum_id INT
      );
    "
    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = sql,
      reference_cohort_database_schema = referenceCohortDatabaseSchema,
      reference_sensitive_cohort_table = referenceCohortTableNames$referenceSensitiveCohortTable
    )
  }
  
  message("Uploading stratification mapping")
  DatabaseConnector::insertTable(
    connection = connection,
    data = stratifiedReviews$stratification,
    tableName = "#stratification",
    dropTableIfExists = TRUE,
    createTable = TRUE,
    tempTable = TRUE,
    tempEmulationSchema = tempEmulationSchema,
    progressBar = FALSE,
    camelCaseToSnakeCase = TRUE
  )
  
  message("Genrating stratified sensitive table")
  sql <- "
    DELETE FROM @reference_cohort_database_schema.@reference_sensitive_cohort_table
    WHERE cohort_definition_id = @reference_cohort_definition_id;
    
    INSERT INTO @reference_cohort_database_schema.@reference_sensitive_cohort_table (
        cohort_definition_id,
        subject_id,
        cohort_start_date,
        stratum_id
    ) 
    SELECT @reference_cohort_definition_id AS cohort_definition_id,
      subject_id,
      cohort_start_date,
      stratum_id
    FROM @sensitive_cohort_database_schema.@sensitive_cohort_table cohort
    INNER JOIN #stratification stratification
      ON cohort.doi_bin = stratification.doi_bin
        AND cohort.category_bin = stratification.category_bin
    WHERE cohort_definition_id = @sensitive__cohort_definition_id;
    
    TRUNCATE TABLE #stratification;
    DROP TABLE #stratification;
  "
  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = sql,
    sensitive_cohort_database_schema = sensitiveCohortDatabaseSchema,
    sensitive_cohort_table = sensitiveCohortTable,
    sensitive__cohort_definition_id = sensitiveCohortDefinitionId,
    reference_cohort_database_schema = referenceCohortDatabaseSchema,
    reference_sensitive_cohort_table = referenceCohortTableNames$referenceSensitiveCohortTable,
    reference_cohort_definition_id = referenceCohortDefinitionId,
    reportOverallTime = TRUE,
    progressBar = TRUE
  )
}


#' Evaluate a cohort when using stratified sampling
#'
#' @description
#' Computes operating characteristics (sensitivity and positive predictive value) of a cohort definition by comparing it
#' against a reference cohort created from LLM review of KEEPER profiles. Assumes stratified sampling was used, and uses
#' the Begg and Greenes framework for optimal power.
#'
#' @template Connection
#'
#' @param cohortDatabaseSchema           The name of the database schema containing the cohort
#'                                       to evaluate.
#' @param cohortTable                    The table name containing the cohort to evaluate.
#' @param cohortDefinitionId             The cohort definition ID of the cohort to evaluate.
#' @param referenceCohortDatabaseSchema  The name of the database schema containing the reference
#'                                       cohort (as uploaded by [uploadReferenceCohortUsingStratifiedSample()]).
#' @param referenceCohortTableNames      The table names where the reference cohort and metadata are stored. Should
#'                                       be created using [createReferenceCohortTableNamesUsingStratifiedSample())].
#' @param referenceCohortDefinitionId    The cohort definition ID of the reference cohort.
#'
#' @returns
#' A tibble with one row, with columns for PPV, sensitivity, and their confidence intervals.
#'
#' @export
evaluateCohortUsingStratifiedSample <- function(
    connectionDetails = NULL,
    connection = NULL,
    cohortDatabaseSchema,
    cohortTable,
    cohortDefinitionId,
    referenceCohortDatabaseSchema,
    referenceCohortTableNames,
    referenceCohortDefinitionId
) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(connectionDetails, "ConnectionDetails", null.ok = TRUE, add = errorMessages)
  checkmate::assertClass(connection, "DatabaseConnectorConnection", null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(cohortDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertCharacter(cohortTable, len = 1, add = errorMessages)
  checkmate::assertIntegerish(cohortDefinitionId, len = 1, add = errorMessages)
  checkmate::assertCharacter(referenceCohortDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertList(referenceCohortTableNames, len = 3, add = errorMessages)
  checkmate::assertNames(names(referenceCohortTableNames), must.include = c(
    "referenceCohortTable",
    "referenceSensitiveCohortTable",
    "referenceCohortMetadataTable"
  ), add = errorMessages)
  checkmate::assertIntegerish(referenceCohortDefinitionId, len = 1, add = errorMessages)
  checkmate::reportAssertions(errorMessages)
  if (is.null(connectionDetails) && is.null(connection)) {
    stop("Must provide either connectionDetails or a connection.")
  }
  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }
  message("Computing confusion with annotated sample")
  sql <- SqlRender::loadRenderTranslateSql(
    sqlFilename = "ComputeCohortConfusion.sql",
    packageName = "Keeper",
    reference_cohort_database_schema = referenceCohortDatabaseSchema,
    reference_cohort_table = referenceCohortTableNames$referenceCohortTable,
    reference_cohort_definition_id = referenceCohortDefinitionId,
    cohort_database_schema = cohortDatabaseSchema,
    cohort_table = cohortTable,
    cohort_definition_id = cohortDefinitionId,
    type = "incident",
    washout_period = 0,
    stratified = TRUE,
    reference_sensitive_cohort_table = referenceCohortTableNames$referenceSensitiveCohortTable
  )
  confusionCounts <- DatabaseConnector::querySql(
    connection = connection,
    sql = sql,
    snakeCaseToCamelCase = TRUE
  )
  confusionCounts <- confusionCounts |>
    group_by(.data$stratumId) |>
    summarise(truePositives = sum(.data$truePositives),
              trueNegatives = sum(.data$trueNegatives),
              falsePositives = sum(.data$falsePositives),
              falseNegatives = sum(.data$falseNegatives))
  sql <- "
    SELECT stratum_id,
      SUM(in_cohort) AS n_in_cohort,
      COUNT(*) - SUM(in_cohort) AS n_not_in_cohort
    FROM (
      SELECT stratum_id,
        CASE WHEN cohort.subject_id IS NULL THEN 0 ELSE 1 END AS in_cohort
      FROM @reference_cohort_database_schema.@reference_sensitive_cohort_table sensitive_cohort
      LEFT JOIN @cohort_database_schema.@cohort_table cohort
        ON sensitive_cohort.subject_id = cohort.subject_id
            AND DATEDIFF(DAY, sensitive_cohort.cohort_start_date, cohort.cohort_start_date) <= 30
            AND DATEDIFF(DAY, sensitive_cohort.cohort_start_date, cohort.cohort_start_date) >= -30
            AND cohort.cohort_definition_id = @cohort_definition_id
      WHERE sensitive_cohort.cohort_definition_id = @reference_cohort_definition_id
    ) per_person
    GROUP BY stratum_id;
  "
  strataCounts <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    reference_cohort_database_schema = referenceCohortDatabaseSchema,
    reference_sensitive_cohort_table = referenceCohortTableNames$referenceSensitiveCohortTable,
    reference_cohort_definition_id = referenceCohortDefinitionId,
    cohort_database_schema = cohortDatabaseSchema,
    cohort_table = cohortTable,
    cohort_definition_id = cohortDefinitionId,    
    snakeCaseToCamelCase = TRUE
  )  
  
  mergedData <- strataCounts |>
    inner_join(confusionCounts, by = join_by("stratumId")) |>
    mutate(
      sampledInCohort = .data$truePositives + .data$falsePositives,
      sampledNotInCohort = .data$falseNegatives + .data$trueNegatives,
      
      # Compute empirical probabilities (rho). 
      rhoIn = if_else(.data$sampledInCohort > 0, .data$truePositives / .data$sampledInCohort, 0),
      rhoOut = if_else(.data$sampledNotInCohort > 0, .data$falseNegatives / .data$sampledNotInCohort, 0),
      
      # Extrapolate to the full HSC population
      estTpStratum = .data$nInCohort * .data$rhoIn,
      estFnStratum = .data$nNotInCohort * .data$rhoOut
    )
  totalPopulationInCohort <- sum(mergedData$nInCohort)
  estTpTotal <- sum(mergedData$estTpStratum)
  estFnTotal <- sum(mergedData$estFnStratum)
  pointPpv <- estTpTotal / totalPopulationInCohort
  pointSens <- estTpTotal / (estTpTotal + estFnTotal)
  
  # Parametric Bootstrap for non-normal Confidence Intervals
  nBootstrap <- 10000

  bootstrapResults <- replicate(nBootstrap, {
    # Draw simulated observed cases based on the empirical rates and actual sample sizes
    simTp <- rbinom(nrow(mergedData), mergedData$sampledInCohort, mergedData$rhoIn)
    simFn <- rbinom(nrow(mergedData), mergedData$sampledNotInCohort, mergedData$rhoOut)
    
    # Calculate simulated rates
    simRhoIn <- if_else(mergedData$sampledInCohort > 0, simTp / mergedData$sampledInCohort, 0)
    simRhoOut <- if_else(mergedData$sampledNotInCohort > 0, simFn / mergedData$sampledNotInCohort, 0)
    
    # Calculate simulated population totals
    simEstTpTotal <- sum(mergedData$nInCohort * simRhoIn)
    simEstFnTotal <- sum(mergedData$nNotInCohort * simRhoOut)
    
    # Calculate simulated metrics
    simPpv <- simEstTpTotal / totalPopulationInCohort
    simSens <- simEstTpTotal / (simEstTpTotal + simEstFnTotal)
    
    c(simPpv, simSens)
  })
  
  ciPpv <- quantile(bootstrapResults[1, ], probs = c(0.025, 0.975), na.rm = TRUE)
  ciSens <- quantile(bootstrapResults[2, ], probs = c(0.025, 0.975), na.rm = TRUE)
  
  metricsSummary <- tibble(
    ppv = pointPpv,
    ppvLb = ciPpv[1],
    ppvUb = ciPpv[2],
    sensitivity = pointSens,
    sensitivityLb = ciSens[1],
    sensitivityUb = ciSens[2]
  )
  return(metricsSummary)
}

uniformSelect <- function(items, size) {
  totalSize <- length(items)
  
  if (size >= totalSize) {
    return(items)
  }
  
  idx <- unique(round(seq(1, totalSize, length.out = size)))
  
  # Fill any missing indices caused by rounding
  while (length(idx) < size) {
    candidates <- setdiff(seq_len(totalSize), idx)
    idx <- sort(c(idx, candidates[1]))
  }
  
  items[idx]
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
  
  # Assuming phase1Count can be a vector (if varying per stratum) or a scalar.
  # If it's a scalar, rep it to match stratumCount for safer vector math.
  if(length(phase1Count) == 1) phase1Count <- rep(phase1Count, stratumCount)
  
  targetPhase2Total <- nOpt - sum(phase1Count)
  
  if (targetPhase2Total <= 0) {
    message("Total optimal size is less than or equal to Phase 1 samples. No Phase 2 needed.")
    stratumData$phase2Count <- 0
    stratumData$totalCount <- phase1Count
    return(stratumData)
  }
  
  # Ensure we don't ask for more total samples than exist in the entire cohort
  if (nOpt > sum(stratumData$personCount)) {
    warning("nOpt exceeds total cohort size. Capping nOpt to total cohort.")
    nOpt <- sum(stratumData$personCount)
  }
  
  # 1. Variance Safeguarding: 
  pSafe <- pmax(0.0001, pmin(0.9999, stratumData$p))
  stratumSd <- sqrt(pSafe * (1 - pSafe))
  
  # 2. Compute Base Neyman Weights
  baseWeights <- (stratumData$personCount * stratumSd)
  baseWeights <- baseWeights / sum(baseWeights)
  
  # 3. Iterative Constrained Allocation (Floor AND Ceiling)
  eligibleStrata <- rep(TRUE, stratumCount)
  finalTotalAllocation <- rep(0, stratumCount)
  remainingBudget <- nOpt
  
  repeat {
    currentWeightSum <- sum(baseWeights[eligibleStrata])
    
    # Pro-rata distribute the budget only among eligible strata
    tempAllocation <- rep(0, stratumCount)
    if (currentWeightSum > 0) {
      tempAllocation[eligibleStrata] <- remainingBudget * (baseWeights[eligibleStrata] / currentWeightSum)
    }
    
    # Identify strata that hit the floor (<= phase1) or the ceiling (>= total persons)
    # 1e-9 handles floating-point math rounding issues
    floorViolators <- eligibleStrata & (tempAllocation <= phase1Count + 1e-9)
    ceilingViolators <- eligibleStrata & (tempAllocation >= stratumData$personCount - 1e-9)
    
    if (any(floorViolators) || any(ceilingViolators)) {
      # Apply constraints and remove from eligibility
      if (any(floorViolators)) {
        finalTotalAllocation[floorViolators] <- phase1Count[floorViolators]
        eligibleStrata[floorViolators] <- FALSE
      }
      if (any(ceilingViolators)) {
        finalTotalAllocation[ceilingViolators] <- stratumData$personCount[ceilingViolators]
        eligibleStrata[ceilingViolators] <- FALSE
      }
      
      # Update the remaining budget to redistribute in the next loop
      remainingBudget <- nOpt - sum(finalTotalAllocation[!eligibleStrata])
    } else {
      # All remaining eligible strata fit safely within the bounds
      finalTotalAllocation[eligibleStrata] <- tempAllocation[eligibleStrata]
      break
    }
  }
  
  # 4. Integer Rounding (Largest Remainder Method)
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
  counts <- sampleData |>
    group_by(stratumId) |>
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
