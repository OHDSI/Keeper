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


queryLlm <- function(prompt,
                     systemPrompt = "",
                     llmClient,
                     costTracker = NULL,
                     outputType = NULL) {
  
  llmClient$set_system_prompt(systemPrompt)
  
  maxRetries <- 3
  attempt <- 0
  while (attempt <= maxRetries) {
    attempt <- attempt + 1
    llmClient$set_turns(list())
    tryCatch({
      if (is.null(outputType)) {
        response <- llmClient$chat(prompt, echo = "none")
      } else {
        llmClient$set_turns(list())
        if (getOption("forceUnstructured", FALSE)) {
          # Currently, LM Studio doesn't play nice with ellmer as it relates to structured output.
          response <- llmClient$chat(prompt, echo = "none")
          response <- gsub("^\\s*```json|```\\s*$", "", response)
          response <- jsonlite::fromJSON(response)
        } else {
          response <- llmClient$chat_structured(prompt, echo = "none", type = outputType)
        }
      }
      if (!is.null(costTracker)) {
        costTracker$amount <- costTracker$amount + llmClient$get_cost()
      }
      return(response)
    },
    error = function(e) {
      message("LLM attempt ", attempt, " failed: ", e$message)
      
      if (grepl("abort", e$message, ignore.case = TRUE)) {
        cat("Stopping the run as requested.\n")
        stop("Execution stopped by user.")
      }
      if (attempt >= maxRetries) {
        message("Reached attempt limit.")
        if (("body" %in% names(e) && grepl("content", e$body) && grepl("filter", e$body)) ||
            (grepl("content", e$message) && grepl("filter", e$message))) {
          error <- errorCondition(
            message = e$message,
            class = "ContentFilterError"
          )
          stop(error)
        } else {
          stop(e)
        }
      }
    })
  }
}