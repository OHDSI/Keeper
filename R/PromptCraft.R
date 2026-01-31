# Copyright 2025 Observational Health Data Sciences and Informatics
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

#' Create settings for generating prompts

#' @param maxParts                How many parts can a category have? For example,
#'                                if `maxParts = 100` and there are more than 100
#'                                measurements, a random sample of 100 will be
#'                                taken. Set to `0` if there is no maximum.
#' @param maxDays                 How many days can a single code have? For example,
#'                                if `maxDays = 5` and there is a measurement code
#'                                that appears on more than 5 days, a random sample
#'                                of 5 days will be taken. Set to `0` if there is no maximum.
#'
#' @return A settings object, to be used in `createSystemPrompt()` and `createPrompt()`.
#'
#' @export
createPromptSettings <- function(maxParts = 100,
                                 maxDays = 5) {
  settings <- list(
    maxParts = maxParts,
    maxDays = maxDays
  )
  class(settings) <- "PromptSettings"
  return(settings)
}

#' Create a system prompt for a LLM
#'
#' @param settings     A settings object as created using [createPromptSettings()].
#' @param diseaseName  The name of the disease to use in the prompt.
#'
#' @return
#' A character string with the system prompt.
#'
#' @export
createSystemPrompt <- function(settings, diseaseName) {
  promptFile <- system.file("KeeperPrompt.txt", package = "Keeper")
  prompt <- readLines(promptFile)
  prompt <- paste(prompt, collapse = "\n")
  prompt <- gsub("<disease>", diseaseName, prompt)
  return(prompt)
}


#' Create the main prompt based on a Keeper output row.
#'
#' @param settings     A settings object as created using `createPromptSettings()`.
#' @param diseaseName  The name of the disease to use in the prompt.
#' @param keeperRow    A single row from the output of `createKeeper()`.
#'
#' @return
#' A character string containing the main prompt.
#'
#' @export
createPrompt <- function(settings,
                         diseaseName,
                         keeperRow) {
  prompt <- c(
    "Healthcare data:",
    ""
  )
  
  prompt <- c(prompt, sprintf(
    "Demographics and details about the visit: %s, %s yo; Visit: %s",
    keeperRow$gender,
    convertAgeToText(keeperRow$age),
    formatVisitContext(keeperRow$visitContext)
  ))
  prompt <- c(prompt, sprintf(
    "Diagnoses recorded on the day of the visit: %s",
    formatPresentation(keeperRow$presentation,
      maxParts = settings$maxParts
    )
  ))
  prompt <- c(prompt, sprintf(
    "Diagnoses recorded prior to the visit: %s",
    formatList(keeperRow$priorDisease, keeperRow$symptoms, keeperRow$comorbidities,
      maxParts = settings$maxParts,
      maxDays = settings$maxDays
    )
  ))
  prompt <- c(prompt, sprintf(
    "Treatments recorded prior to the visit: %s",
    formatList(keeperRow$priorDrugs, keeperRow$priorTreatmentProcedures,
      maxParts = settings$maxParts,
      maxDays = settings$maxDays
    )
  ))
  prompt <- c(prompt, sprintf(
    "Diagnostic procedures recorded proximal to the visit: %s",
    formatList(keeperRow$diagnosticProcedures,
      maxParts = settings$maxParts,
      maxDays = settings$maxDays
    )
  ))
  prompt <- c(prompt, sprintf(
    "Laboratory tests recorded proximal to the visit: %s",
    formatList(keeperRow$measurements,
      maxParts = settings$maxParts,
      maxDays = settings$maxDays
    )
  ))
  prompt <- c(prompt, sprintf(
    "Alternative diagnoses recorded proximal to the visit: %s",
    formatList(keeperRow$alternativeDiagnosis,
      maxParts = settings$maxParts,
      maxDays = settings$maxDays
    )
  ))
  prompt <- c(prompt, sprintf(
    "Diagnoses recorded after the visit: %s",
    formatList(keeperRow$afterDisease,
      maxParts = settings$maxParts,
      maxDays = settings$maxDays
    )
  ))
  prompt <- c(prompt, sprintf(
    "Treatments recorded during or after the visit: %s",
    formatList(keeperRow$afterDrugs, keeperRow$afterTreatmentProcedures,
      maxParts = settings$maxParts,
      maxDays = settings$maxDays
    )
  ))
  prompt <- paste(prompt, collapse = "\n\n")
  return(prompt)
}

convertAgeToText <- function(age) {
  return(english::as.english(age))
}

formatVisitContext <- function(visitContext) {
  visitContext <- gsub("->", " followed by ", visitContext)
  return(visitContext)
}

formatPresentation <- function(presentation, maxParts = 0) {
  presentation <- gsub("\\(Claim, ", "(", formatList(presentation,
    maxParts = maxParts
  ))
  return(presentation)
}

formatList <- function(..., maxParts = 0, maxDays = 0) {
  items <- list(...)
  strings <- c()
  for (item in items) {
    if (!is.na(item) && trimws(item) != "") {
      strings <- c(strings, item)
    }
  }
  if (length(strings) == 0) {
    return("None")
  } else {
    result <- paste(strings, collapse = "; ")
    if (maxParts > 0) {
      nParts <- lengths(regmatches(result, gregexpr(";", result)))
      if (nParts > maxParts) {
        parts <- strsplit(result, ";")[[1]]
        parts <- sample(parts, maxParts, replace = FALSE)
        result <- trimws(paste(parts, collapse = ";"))
      }
    }
    if (maxDays > 0) {
      dayStrings <- stringr::str_extract_all(result, "\\(day[0-9-, ]+\\)")[[1]]
      replacements <- sapply(dayStrings, removeExcessDays, maxDays)
      for (i in seq_along(dayStrings)) {
        if (!is.na(replacements[i])) {
          result <- gsub(dayStrings[i], replacements[i], result)
        }
      }
    }
    return(result)
  }
}

removeExcessDays <- function(dayString, maxDays) {
  days <- strsplit(gsub("^\\(day|\\)$", "", dayString), ",")[[1]]
  if (length(days) > maxDays) {
    days <- sample(days, maxDays, replace = FALSE)
    dayString <- sprintf("(day%s)", paste(days, collapse = ","))
    return(dayString)
  } else {
    return(NA)
  }
}
