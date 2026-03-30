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

#' Create settings for generating prompts

#' @param maxDays                 How many days can a single code have? For example,
#'                                if `maxDays = 5` and there is a measurement code
#'                                that appears on more than 5 days, a random sample
#'                                of 5 days will be taken. Set to `0` if there is no maximum.
#' @param legacy                  IF TRUE, will use the prompt from the Nature Digital Medicine paper.
#'
#' @return A settings object, to be used in [reviewCases()].
#'
#' @export
createPromptSettings <- function(maxDays = 20,
                                 legacy = FALSE) {
  settings <- list(
    maxDays = maxDays,
    legacy = legacy
  )
  class(settings) <- "PromptSettings"
  return(settings)
}

createSystemPrompt <- function(settings, phenotypeName) {
  if (settings$legacy) {
    promptFile <- system.file("KeeperLegacyPrompt.txt", package = "Keeper")
  } else {
    promptFile <- system.file("KeeperPrompt.txt", package = "Keeper")
  }
  prompt <- readLines(promptFile)
  prompt <- paste(prompt, collapse = "\n")
  prompt <- gsub("<disease>", phenotypeName, prompt)
  return(prompt)
}

# Modern prompt --------------------------------------------------------------------------------------------------------
createPrompt <- function(settings, subset) {
  if (nrow(subset) == 0) {
    return("No data")
  }
  prompt <- c(
    "# Healthcare data"
  )

  # Demographics
  age <- subset |>
    filter(.data$category == "age") |>
    pull(.data$conceptName)
  sex <- subset |>
    filter(.data$category == "sex") |>
    pull(.data$conceptName)
  observationPeriod <- subset |>
    filter(.data$category == "observationPeriod") |>
    select("startDay", "endDay")
  race <- subset |>
    filter(.data$category == "race") |>
    pull(.data$conceptName)
  ethnicity <- subset |>
    filter(.data$category == "ethnicity") |>
    pull(.data$conceptName)
  demographics <- c(
    sprintf("Age: %s", age),
    sprintf("Sex: %s", sex),
    sprintf("Observation period: day %d to day %d", observationPeriod$startDay, observationPeriod$endDay)
  )
  if (race != "") {
    demographics <- c(demographics, sprintf("Race: %s", race))
  }
  if (ethnicity != "") {
    demographics <- c(demographics, sprintf("Ethnicity: %s", ethnicity))
  }
  prompt <- c(prompt,
              "## Demographics", 
              paste(demographics, collapse = "\n"))
  
  
  keeperTables <- c("presentation",
                    "visits",
                    "symptoms",
                    "priorDisease",
                    "priorDrugs",
                    "priorTreatmentProcedures",
                    "measurements",
                    "alternativeDiagnoses",
                    "diagnosticProcedures",
                    "postDisease",
                    "postDrugs",
                    "postTreatmentProcedures",
                    "death")
  
  for (keeperTable in keeperTables) {
    labels <- subset |>
      filter(.data$category == keeperTable) |>
      mutate(extraGroup = if (keeperTable %in% c("presentation", "visits")) .data$extraData else "") |>
      group_by(.data$conceptName, .data$target, .data$extraGroup) |>
      slice_sample(n = settings$maxDays) |>
      arrange(.data$startDay) |>
      summarise(label = generateLabel(.data$conceptName, .data$startDay, .data$endDay, .data$extraData, keeperTable), .groups = "drop") |>
      mutate(          
        sortOrder = case_when(
          .data$target == "Disease of interest" ~ 2,
          .data$target == "Both" ~ 1,
          .data$target == "Alternative diagnoses" ~ 0,
          TRUE ~ -1)) |>
      arrange(desc(.data$sortOrder), .data$label) |>
      pull(.data$label)
    
    prompt <- c(prompt,
                sprintf("## %s", case_when(
                  keeperTable == "presentation" ~ "Conditions recorded on day 0",
                  keeperTable == "visits" ~ "Visits recorded proximal to day 0",
                  keeperTable == "symptoms" ~ "Symptoms recorded prior to day 0",
                  keeperTable == "priorDisease" ~ "Diagnoses recorded prior to day 0",
                  keeperTable == "priorDrugs" ~ "Drug treatments recorded prior to day 0",
                  keeperTable == "priorTreatmentProcedures" ~ "Treatment procedures recorded prior to day 0",
                  keeperTable == "diagnosticProcedures" ~ "Diagnostic procedures recorded proximal to day 0",
                  keeperTable == "measurements" ~ "Laboratory Tests recorded proximal to day 0",
                  keeperTable == "alternativeDiagnoses" ~ "Alternative Diagnoses recorded proximal to day 0",
                  keeperTable == "postDisease" ~ "Diagnoses recorded after day 0",
                  keeperTable == "postDrugs" ~ "Drug treatments recorded on or after day 0",
                  keeperTable == "postTreatmentProcedures" ~ "Treatment procedures recorded on or after day 0",
                  TRUE ~ prettifyName(keeperTable))), 
                paste(labels, collapse = "\n"))
  }
  prompt <- paste(prompt, collapse = "\n\n")
  # writeLines(prompt)
  return(prompt)
}

generateLabel <- function(conceptName, startDay, endDay, extraData, keeperTable) {
  if (keeperTable == "presentation") {
    return(paste0(conceptName, if_else(extraData == "", "", sprintf(" (%s)", extraData))))
  } else if (keeperTable == "visits") {
    return(sprintf("%s%s (%s)",
                   conceptName[1],
                   if_else(extraData[1] == "", "", sprintf(" - %s", extraData[1])),
                   paste(if_else(startDay == endDay,
                                 sprintf("day %s", startDay),
                                 sprintf("days %s to %s", startDay, endDay)),
                         collapse = ", ")))
  } else if (keeperTable %in% c("priorDrugs", "postDrugs")) {
    return(sprintf("%s (%s)", 
                   conceptName[1],
                   paste(sprintf("day %d for %d day%s", 
                                 startDay, 
                                 endDay - startDay + 1,
                                 if_else(endDay == startDay, "", "s")),
                         collapse = ", ")))
  } else if (keeperTable == "measurements") {
    return(sprintf("%s (day %s)", 
                   conceptName[1],
                   paste(if_else(extraData == "",
                                 as.character(startDay),
                                 sprintf("%d with value %s", startDay, extraData)),
                         collapse = ", ")))     
  } else {
    return(sprintf("%s (day %s)", 
                   conceptName[1],
                   paste(startDay, collapse = ", ")))     
  }
}

prettifyName <- function(name){
  name <- gsub("([A-Z])", " \\1", name)
  name <- tolower(name)
  name <- gsub("([a-z])([0-9])", "\\1_\\2", name)
  name <- tolower(name)
  name <- gsub("\\b([a-z])", "\\U\\1", name, perl = TRUE)
  return(name)
}

# Legacy prompt --------------------------------------------------------------------------------------------------------
createLegacyPrompt <- function(settings,
                         phenotypeName,
                         subset) {
  keeperTableRow <- convertKeeperToTable(subset)
  prompt <- c(
    "Healthcare data:",
    ""
  )
  if ("sex" %in% colnames(keeperTableRow)) {
    keeperTableRow$gender <- keeperTableRow$sex
  }
  prompt <- c(prompt, sprintf(
    "Demographics and details about the visit: %s, %s yo; Visit: %s",
    keeperTableRow$gender,
    convertAgeToText(keeperTableRow$age),
    formatVisitContext(keeperTableRow$visitContext)
  ))
  prompt <- c(prompt, sprintf(
    "Diagnoses recorded on the day of the visit: %s",
    formatPresentation(keeperTableRow$presentation)
  ))
  prompt <- c(prompt, sprintf(
    "Diagnoses recorded prior to the visit: %s",
    formatList(keeperTableRow$priorDisease, keeperTableRow$symptoms,
               maxDays = settings$maxDays
    )
  ))
  prompt <- c(prompt, sprintf(
    "Treatments recorded prior to the visit: %s",
    formatList(keeperTableRow$priorDrugs, keeperTableRow$priorTreatmentProcedures,
               maxDays = settings$maxDays
    )
  ))
  prompt <- c(prompt, sprintf(
    "Diagnostic procedures recorded proximal to the visit: %s",
    formatList(keeperTableRow$diagnosticProcedures,
               maxDays = settings$maxDays
    )
  ))
  prompt <- c(prompt, sprintf(
    "Laboratory tests recorded proximal to the visit: %s",
    formatList(keeperTableRow$measurements,
               maxDays = settings$maxDays
    )
  ))
  if ("alternativeDiagnosis" %in% colnames(keeperTableRow)) {
    keeperTableRow$alternativeDiagnoses <- keeperTableRow$alternativeDiagnosis
  }
  prompt <- c(prompt, sprintf(
    "Alternative diagnoses recorded proximal to the visit: %s",
    formatList(keeperTableRow$alternativeDiagnoses,
               maxDays = settings$maxDays
    )
  ))
  if ("afterDisease" %in% colnames(keeperTableRow)) {
    keeperTableRow$postDisease <- keeperTableRow$afterDisease
  }
  prompt <- c(prompt, sprintf(
    "Diagnoses recorded after the visit: %s",
    formatList(keeperTableRow$postDisease,
               maxDays = settings$maxDays
    )
  ))
  if ("afterDrugs" %in% colnames(keeperTableRow)) {
    keeperTableRow$postDrugs <- keeperTableRow$afterDrugs
  }
  if ("afterTreatmentProcedures" %in% colnames(keeperTableRow)) {
    keeperTableRow$postTreatmentProcedures <- keeperTableRow$afterTreatmentProcedures
  }
  prompt <- c(prompt, sprintf(
    "Treatments recorded during or after the visit: %s",
    formatList(keeperTableRow$postDrugs, keeperTableRow$postTreatmentProcedures,
               maxDays = settings$maxDays
    )
  ))
  prompt <- paste(prompt, collapse = "\n\n")
  return(prompt)
}

convertAgeToText <- function(age) {
  return(english::as.english(as.integer(age)))
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


