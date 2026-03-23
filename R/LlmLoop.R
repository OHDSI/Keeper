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

#' Review cases using an LLM
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
#' A data frame with two columns:
#'
#' 1. `personId`
#' 2. `isCase`, with possible values "yes", "no", or "I don't know".
#'
#' @export
reviewCases <- function(keeper,
                        settings = createPromptSettings(),
                        phenotypeName = NULL,
                        client,
                        cacheFolder) {
  if ("age" %in% colnames(keeper)) {
    format <- "keeperTable"
  } else {
    format <- "keeper"
  }

  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(keeper, add = errorMessages)
  if (format == "keeper") {
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
  } else {
    checkmate::assertNames(colnames(keeper), must.include = c(
      "age",
      "gender",
      "observationPeriod",
      "visitContext",
      "presentation",
      "comorbidities",
      "symptoms",
      "priorDisease",
      "priorDrugs",
      "priorTreatmentProcedures",
      "diagnosticProcedures",
      "measurements",
      "alternativeDiagnosis",
      "afterDisease",
      "afterTreatmentProcedures",
      "afterDrugs",
      "death"
    ), add = errorMessages)
  }
  checkmate::assertClass(settings, "PromptSettings", add = errorMessages)
  checkmate::assertCharacter(phenotypeName, null.ok = TRUE, add = errorMessages)
  checkmate::assertR6(client, "Chat", add = errorMessages)
  checkmate::assertCharacter(cacheFolder, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  startTime <- Sys.time()

  maxRetries <- 5

  if (format == "keeper") {
    keeperTable <- convertKeeperToTable(keeper)
  } else {
    keeperTable <- keeper
    if (!"generatedId" %in% colnames(keeperTable)) {
      keeperTable$generatedId <- keeperTable$personId
    }
  }
  if (!is.null(phenotypeName)) {
    keeperTable$phenotype <- phenotypeName
  }

  result <- tibble(
    generatedId = keeperTable$generatedId,
    isCase = as.character(NA),
    certainty = as.character(NA),
    indexDay = as.numeric(NA),
    justification = as.character(NA)
  )
  if ("personId" %in% colnames(keeperTable)) {
    result$personId <- keeperTable$personId
    result$indexDate <- keeperTable$indexDate
    result$cdmDatabaseSchema <- keeperTable$cdmDatabaseSchema
  }
  if (!dir.exists(cacheFolder)) {
    dir.create(cacheFolder)
  }

  cost <- 0
  nPersons <- nrow(keeperTable)
  for (i in seq_len(nPersons)) {
    message(sprintf("Reviewing person %d of %d", i, nPersons))
    if (i %% 100 == 0) {
      message(sprintf("- Cost so far: $%0.2f", cost))
    }
    row <- keeperTable[i, ]

    responseFileName <- generateCacheFileName(row$phenotype, row$generatedId, cacheFolder)
    if (file.exists(responseFileName)) {
      if (settings$legacy) {
        response <- paste(readLines(responseFileName), collapse = "\n")
        parsedResponse <- parseLegacyLlmResponse(response, noMatchIsInsufficientInformation = FALSE)
      } else {
        response <- jsonlite::read_json(responseFileName)
        parsedResponse <- parseLlmResponse(response, noMatchIsInsufficientInformation = FALSE)
      }
    } else {
      systemPrompt <- createSystemPrompt(settings = settings, phenotypeName = row$phenotype)
      prompt <- createPrompt(
        settings = settings,
        phenotypeName = row$phenotype,
        keeperTableRow = row
      )

      # Store full prompt for easy review:
      fullPrompt <- sprintf("[System Prompt]\n%s\n[Prompt]\n%s", systemPrompt, prompt)
      promptFileName <- generateCacheFileName(row$phenotype, row$generatedId, cacheFolder, type = "prompt")
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
              response <- client$chat_structured(prompt,
                echo = "none",
                type = ellmer::type_object(
                  justification = ellmer::type_string(),
                  verdict = ellmer::type_string(),
                  certainty = ellmer::type_string(),
                  day_of_onset = ellmer::type_integer()
                )
              )
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
    result$isCase[i] <- parsedResponse$isCase
    result$certainty[i] <- parsedResponse$certainty
    result$indexDay[i] <- parsedResponse$indexDay
    result$justification[i] <- parsedResponse$justification
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
  return(result)
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
