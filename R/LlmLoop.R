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

#' Review Keeper profiles using an LLM
#'
#' @param keeper      Output from the [generateKeeper()] function.
#' @param settings    Prompt creating settings as created using the [createPromptSettings] function.
#' @param phenotypeName The name of the disease to use in the prompt. If not provided, the name in the Keeper input will
#'                      be used.
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
reviewCases <- function(keeper,
                        settings = createPromptSettings(),
                        phenotypeName = NULL,
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
  checkmate::assertCharacter(phenotypeName, null.ok = TRUE, add = errorMessages)
  checkmate::assertR6(client, "Chat", add = errorMessages)
  checkmate::assertCharacter(cacheFolder, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  
  startTime <- Sys.time()
  
  structured <- supportsStructuredOutput(client)
  
  maxRetries <- 5
  
  keeperSplit <- split(keeper, keeper$generatedId)
  
  if (!dir.exists(cacheFolder)) {
    dir.create(cacheFolder)
  }
  
  cost <- 0
  nPersons <- length(keeperSplit)
  results <- list()
  for (i in seq_along(keeperSplit)) {
    message(sprintf("Reviewing person %d of %d", i, nPersons))
    if (i %% 100 == 0) {
      message(sprintf("- Cost so far: $%0.2f", cost))
    }
    keeperSubset <- keeperSplit[[i]]
    if (is.null(phenotypeName)) {
      phenotype <- keeperSubset |>
        filter(.data$category == "phenotype") |>
        pull(.data$conceptName)
    } else {
      phenotype <- phenotypeName
    }
    
    responseFileName <- generateCacheFileName(phenotype, keeperSubset$generatedId[1], cacheFolder)
    if (file.exists(responseFileName)) {
      if (settings$legacy) {
        response <- paste(readLines(responseFileName), collapse = "\n")
        parsedResponse <- parseLegacyLlmResponse(response, noMatchIsInsufficientInformation = FALSE)
      } else {
        response <- jsonlite::read_json(responseFileName)
        parsedResponse <- parseLlmResponse(response, noMatchIsInsufficientInformation = FALSE)
      }
    } else {
      systemPrompt <- createSystemPrompt(settings = settings, phenotypeName = phenotype)
      if (settings$legacy) {
        prompt <- createLegacyPrompt(
          settings = settings,
          phenotypeName = phenotype,
          subset = keeperSubset
        )
      } else {
        prompt <- createPrompt(
          settings = settings,
          subset = keeperSubset
        )
      }
      
      # Store full prompt for easy review:
      fullPrompt <- sprintf("[System Prompt]\n%s\n[Prompt]\n%s", systemPrompt, prompt)
      promptFileName <- generateCacheFileName(phenotype, keeperSubset$generatedId[1], cacheFolder, type = "prompt")
      writeLines(fullPrompt, promptFileName)
      
      # Ellmer is supposed to retry automatically, but I haven't seen it work when using LM Studio, so using own retry loop:
      for (j in seq_len(maxRetries)) {
        parsedResponse <- tryCatch(
          {
            client$set_turns(list())
            client$set_system_prompt(systemPrompt)
            if (settings$legacy) {
              response <- client$chat(prompt,
                                      echo = "none"
              )
              writeLines(response, responseFileName)
              parsedResponse <- parseLegacyLlmResponse(response, noMatchIsInsufficientInformation = FALSE)
            } else {
              if (structured) {
                response <- client$chat_structured(prompt,
                                                   echo = "none",
                                                   type = ellmer::type_object(
                                                     justification = ellmer::type_string(),
                                                     verdict = ellmer::type_string(),
                                                     certainty = ellmer::type_string(),
                                                     day_of_onset = ellmer::type_integer()
                                                   )
                )
              } else {
                response <- client$chat(prompt, echo = "none")
                response <- jsonlite::fromJSON(response)
              }
              jsonlite::write_json(response, responseFileName)
              parsedResponse <- parseLlmResponse(response, noMatchIsInsufficientInformation = FALSE)
            }
            cost <- cost + client$get_cost()
            parsedResponse
          },
          error = function(e) {
            message(paste("Attempt", j, "failed:", e$message))
            if (j < maxRetries) {
              Sys.sleep(30)
              return(NULL)
            } else {
              stop("Exceeding maximum number of retries when calling LLM")
            }
          }
                )
                if (!is.null(parsedResponse)) {
                  break
                }
      }
    }
    
    cohortPrevalence <-  keeperSubset |>
      filter(.data$category == "cohortPrevalence") |>
      pull(.data$conceptName) |>
      as.numeric()
    
    resultsRow <- tibble(
      generatedId = keeperSubset$generatedId[1],
      phenotype = phenotype
    ) |> 
      bind_cols(parsedResponse) |>
      mutate(
        cohortPrevalence = cohortPrevalence,
        model = client$get_model(),
        keeperVersion = as.character(packageVersion("Keeper"))
      )
    if ("personId" %in% keeperSubset$category) {
      personId <-  keeperSubset |>
        filter(.data$category == "personId") |>
        pull(.data$conceptName) 
      cohortStartDate <-  keeperSubset |>
        filter(.data$category == "cohortStartDate") |>
        pull(.data$conceptName) 
      resultsRow <- resultsRow |>
        mutate(
          personId = personId,
          cohortStartDate = cohortStartDate
        )
    }
    results[[i]] <- resultsRow
  }
  delta <- Sys.time() - startTime
  message(paste0(
    "Reviewing cases took ",
    round(delta, 1),
    " ",
    attr(delta, "units"),
    " and cost $",
    round(cost, 2)
  ))
  return(bind_rows(results))
}

generateCacheFileName <- function(phenotypeName, generatedId, cacheFolder, type = "response") {
  fileName <- sprintf(
    "%s_p%s",
    gsub("[^[:alnum:]]", "", phenotypeName),
    generatedId
  )
  if (type != "response") {
    fileName <- paste(fileName, type, sep = "_")
  }
  fileName <- paste(fileName, "txt", sep = ".")
  
  return(file.path(cacheFolder, fileName))
}


supportsStructuredOutput <- function(client) {
  test_type <- type_object(
    supported = type_boolean("Always return true")
  )
  success <- tryCatch({
    res <- client$chat_structured("Respond using the schema.", 
                                  type = test_type, 
                                  echo = "none")
    TRUE
  }, error = function(e) {
    FALSE
  })
  return(success)
}
