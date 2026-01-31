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
#' @param keeper      Output from the [createKeeper] function.
#' @param settings    Prompt creating settings as created using the [createPromptSettings] function.
#' @param diseaseName The name of the disease to use in the prompt.
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
                        diseaseName,
                        client,
                        cacheFolder) {
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(keeper, add = errorMessage)
  checkmate::assertNames(
    colnames(keeper),
    must.include = c(
      "personId",
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
    ),
    add = errorMessage
  )
  checkmate::assert_class(settings, "PromptSettings", add = errorMessage)
  checkmate::assert_character(diseaseName, add = errorMessage)
  checkmate::assertR6(client, "Chat", add = errorMessage)
  checkmate::assert_character(cacheFolder, add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)
  
  startTime <- Sys.time()
  
  systemPrompt <- createSystemPrompt(settings = settings, diseaseName = diseaseName)  
  client$set_system_prompt(systemPrompt)
  if (!dir.exists(cacheFolder)) {
    dir.create(cacheFolder)
  }
  result <- tibble(
    personId = keeper$personId,
    isCase = as.character(NA),
    indexDay = as.numeric(NA)
  )
  
  cost <- 0
  nPersons <- nrow(keeper)
  for (i in seq_len(nPersons)) {
    message(sprintf("Reviewing person %d of %d", i, nPersons))
    row <- keeper[i, ]
    
    responseFileName <- generateCacheFileName(diseaseName, row$personId, cacheFolder)
    if (file.exists(responseFileName)) {
      response <- readLines(responseFileName)
      response <- paste(response, collapse = "\n")
    } else {
      prompt <- createPrompt(settings = settings, 
                             diseaseName = diseaseName,
                             keeperRow = row)  
      
      # Store full prompt for easy review:
      fullPrompt <- sprintf("[System Prompt]\n%s\n[Prompt]\n%s", systemPrompt, prompt)
      promptFileName <- generateCacheFileName(diseaseName, row$personId, cacheFolder, type = "prompt")
      writeLines(fullPrompt, promptFileName)
      
      client$set_turns(list())
      response <- client$chat(prompt, echo = "none")  
      cost <- cost + client$get_cost()
      writeLines(response, responseFileName)
    }
    parsedResponse <- parseLlmResponse(response, noMatchIsInsufficientInformation = FALSE)
    result$isCase[i] <- parsedResponse$isCase  
    result$indexDay[i] <- parsedResponse$indexDay
  }
  delta <- Sys.time() - startTime
  message(paste0("Reviewing cases took ",
                 round(delta,1), 
                 " ",
                 attr(delta, "units"),
                 " and cost $",
                 round(cost, 2)))
  return(result)
}

generateCacheFileName <- function(diseaseName, personId, cacheFolder, type = "response") {
  fileName <- sprintf("%s_p%s", 
                      gsub("[^[:alnum:]]", "", diseaseName),
                      personId)
  if (type != "response") {
    fileName <- paste(fileName, type, sep = "_")
  }
  fileName <- paste(fileName, "txt", sep = ".")
  
  return(file.path(cacheFolder, fileName))
}
