# Copyright 2024 Observational Health Data Sciences and Informatics
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
#'
#' @param writeNarrative          Ask the LLM to write a clinical narrative matching
#'                                the provided data?
#' @param testingReminder         Remind the LLM that a diagnosis can be recorded
#'                                just to justify a test, and therefore by itself
#'                                is not sufficient evidence?
#' @param uncertaintyInstructions Provide instructions to the LLM on how to deal
#'                                with uncertainty?
#' @param discussEvidence         Prompt the LLM to first discuss evidence in favor
#'                                and against the disease of interest?
#' @param provideExamples         Provide examples? (few-shot prompting)
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
createPromptSettings <- function(writeNarrative = TRUE,
                                 testingReminder = TRUE,
                                 uncertaintyInstructions = TRUE,
                                 discussEvidence = TRUE,
                                 provideExamples = FALSE,
                                 maxParts = 100,
                                 maxDays = 5) {
  settings <- list(
    writeNarrative = writeNarrative,
    testingReminder = testingReminder,
    uncertaintyInstructions = uncertaintyInstructions,
    discussEvidence = discussEvidence,
    provideExamples = provideExamples,
    maxParts = maxParts,
    maxDays = maxDays
  )
  return(settings)
}

systemPromptBasis <- "Act as a medical doctor reviewing a patient's healthcare data captured during routine clinical care, such as electronic health records and insurance claims."

writeNarrativeTemplate <- "Write a medical narrative that fits the recorded health data followed by a determination of whether the patient had <disease>."
noNarrativeTemplate <- "Determine whether the patient had <disease>."


writeNarrativeFormat <- "Clinical narrative:"

testingReminderTemplate <- "Remember that recording a diagnosis for a disease could occur either because the patient had the disease or as justification for performing a diagnostic procedure to determine whether the patient has the disease. A diagnosis by itself or accompanied with only diagnostic procedures may therefore be insufficient evidence, even if recorded more than once. Lack of additional evidence of <disease> other than the diagnosis and diagnostic procedures probably means that the patient was only being tested, and does not actually have <disease>. However, it unlikely that a patient will be tested many times over, so an abundance of diagnoses will mean the patient has the disease."

uncertaintyInstructionsTemplate <- c(
  "In your final summary, indicate \"yes\" if the most probable scenario is that the patient had <disease>.",
  "Indicate \"no\" if it is not the most probable scenario, for example when it is more likely that the patient was tested for the disease but the diagnosis was not confirmed. Also indicate \"no\" when there is insufficient information to say anything about the relative probability of scenarios."
)

discussEvidenceFormatTemplate <- c(
  "Evidence in favor of <disease>:",
  "",
  "Evidence against <disease>:"
)

#' Create a system prompt for a LLM
#'
#' @param settings     A settings object as created using `createPromptSettings()`.
#' @param diseaseName  The name of the disease to use in the prompt.
#'
#' @return
#' A character string with the system prompt.
#'
#' @export
createSystemPrompt <- function(settings, diseaseName) {
  prompt <- systemPromptBasis
  if (settings$provideExamples) {
    prompt <- c(
      prompt,
      "Examples: \n\nPrompt: \""
    )
    if (settings$writeNarrative) {
      prompt <- c(
        prompt,
        "Write a medical narrative that fits the recorded health data followed by a determination of whether the patient had Rheumatoid arthritis.",
        ""
      )
    } else {
      prompt <- c(
        prompt,
        "Determine whether the patient had Rheumatoid arthritis.",
        ""
      )
    }
    if (settings$testingReminder) {
      prompt <- c(
        prompt,
        "Remember that recording a diagnosis for a disease could occur either because the patient had the disease or as justification for performing a diagnostic procedure to determine whether the patient has the disease. A diagnosis by itself or accompanied with only diagnostic procedures may therefore be insufficient evidence, even if recorded more than once. Lack of additional evidence of Rheumatoid arthritis other than the diagnosis and diagnostic procedures probably means that the patient was only being tested, and does not actually have Rheumatoid arthritis. However, it unlikely that a patient will be tested many times over, so an abundance of diagnoses will mean the patient has the disease.",
        ""
      )
    }
    if (settings$uncertaintyInstructions) {
      prompt <- c(
        prompt,
        "In your final summary, indicate 'yes' if the most probable scenario is that the patient had Rheumatoid arthritis.",
        "Indicate 'no' if it is not the most probable scenario, for example when it is more likely that the patient was tested for the disease but the diagnosis was not confirmed. Also indicate 'no' when there is insufficient information to say anything about the relative probability of scenarios.",
        ""
      )
    }
    prompt <- c(
      prompt, "Healthcare data:",
      "",
      "Demographics and details about the visit: Female, 70 yo; Visit: Laboratory Visit",
      "",
      "Diagnoses recorded on the day of the visit: Rheumatoid arthritis (Primary diagnosis);",
      "",
      "Diagnoses recorded prior to the visit: None",
      "",
      "Treatments recorded prior to the visit: None",
      "",
      "Diagnostic procedures recorded proximal to the visit: Collection of venous blood by venipuncture (day -30, 0, 30)",
      "",
      "Laboratory tests recorded proximal to the visit: None",
      "",
      "Alternative diagnoses recorded proximal to the visit: None",
      "",
      "Diagnoses recorded after the visit: Seropositive rheumatoid arthritis (day 90)",
      "",
      "Treatments recorded during or after the visit: None",
      ""
    )
    prompt <- c(prompt, "\"", "\n", "Response: \"")
    if (settings$writeNarrative) {
      prompt <- c(
        prompt,
        "Clinical narrative: A 70-year-old female patient visited the laboratory for the collection of venous blood by venipuncture. The primary diagnosis recorded on the day of the visit was rheumatoid arthritis. There were no alternative diagnoses recorded proximal to the visit. The patient did not receive any treatments prior to, during, or after the visit.",
        "\n"
      )
    }
    if (settings$discussEvidence) {
      prompt <- c(
        prompt,
        "Evidence in favor of Rheumatoid arthritis: The recorded primary diagnosis recorded was rheumatoid arthritis, and this diagnosis was recorded again after the visit. The collection of venous blood by venipuncture was performed multiple times proximal to the visit, which suggests that the patient was being monitored for rheumatoid arthritis.",
        "",
        "Evidence against Rheumatoid arthritis: No treatments for rheumatoid arthritis were recorded. For a chronic disease such as rheumatoid arthritis it is unlikely the diagnosis would have been recorded only twice.",
        ""
      )
    }
    prompt <- c(
      prompt,
      "Summary: No",
      "\"",
      "Prompt: \""
    )
    if (settings$writeNarrative) {
      prompt <- c(
        prompt,
        "Write a medical narrative that fits the recorded health data followed by a determination of whether the patient had Acute bronchitis.",
        ""
      )
    } else {
      prompt <- c(
        prompt,
        "Determine whether the patient had Acute bronchitis.",
        ""
      )
    }
    if (settings$testingReminder) {
      prompt <- c(
        prompt,
        "Remember that recording a diagnosis for a disease could occur either because the patient had the disease or as justification for performing a diagnostic procedure to determine whether the patient has the disease. A diagnosis by itself or accompanied with only diagnostic procedures may therefore be insufficient evidence, even if recorded more than once. Lack of additional evidence of Acute bronchitis other than the diagnosis and diagnostic procedures probably means that the patient was only being tested, and does not actually have Acute bronchitis. However, it unlikely that a patient will be tested many times over, so an abundance of diagnoses will mean the patient has the disease.",
        ""
      )
    }
    if (settings$uncertaintyInstructions) {
      prompt <- c(
        prompt,
        "In your final summary, indicate 'yes' if the most probable scenario is that the patient had Acute bronchitis.",
        "Indicate 'no' if it is not the most probable scenario, for example when it is more likely that the patient was tested for the disease but the diagnosis was not confirmed. Also indicate 'no' when there is insufficient information to say anything about the relative probability of scenarios.",
        ""
      )
    }
    prompt <- c(
      prompt, "Healthcare data:",
      "",
      "Demographics and details about the visit: Male, 18 yo; Visit: Pharmacy visit followed by Outpatient Visit",
      "",
      "Diagnoses recorded on the day of the visit: Acute bronchitis (Primary diagnosis);",
      "",
      "Diagnoses recorded prior to the visit: None",
      "",
      "Treatments recorded prior to the visit: None",
      "",
      "Diagnostic procedures recorded proximal to the visit: None",
      "",
      "Laboratory tests recorded proximal to the visit: None",
      "",
      "Alternative diagnoses recorded proximal to the visit: None",
      "",
      "Diagnoses recorded after the visit: None",
      "",
      "Treatments recorded during or after the visit: azithromycin (day 0, for 4 days);",
      ""
    )
    prompt <- c(prompt, "\"", "\n", "Response: \"")
    if (settings$writeNarrative) {
      prompt <- c(
        prompt,
        "Clinical narrative: A 18-year-old male visited the pharmacy, had an outpatient visit, and was prescribed a short course of azithromycin.The primary diagnosis recorded on the day of the visit was Acute bronchitis. There were no alternative diagnoses recorded proximal to the visit.",
        "\n"
      )
    }
    if (settings$discussEvidence) {
      prompt <- c(
        prompt,
        "Evidence in favor of Acute bronchitis: The primary diagnosis recorded on the day of the visit was acute bronchitis. The patient was prescribed azithromycin, which is commonly used to treat respiratory infections such as bronchitis.",
        "",
        "Evidence against Acute bronchitis: No diagnostic procedures or laboratory tests were performed to confirm the diagnosis of acute bronchitis.",
        ""
      )
    }
    prompt <- c(
      prompt,
      "Summary: Yes",
      "\""
    )
  } else { # settings$provideExamples == FALSE
    if (settings$writeNarrative) {
      prompt <- c(
        prompt,
        writeNarrativeTemplate,
        ""
      )
    } else {
      prompt <- c(
        prompt,
        noNarrativeTemplate,
        ""
      )
    }
    if (settings$testingReminder) {
      prompt <- c(
        prompt,
        testingReminderTemplate,
        ""
      )
    }
    if (settings$uncertaintyInstructions) {
      prompt <- c(
        prompt,
        uncertaintyInstructionsTemplate,
        ""
      )
    }
    prompt <- c(
      prompt,
      "Use the following format:",
      ""
    )
    if (settings$writeNarrative) {
      prompt <- c(
        prompt,
        writeNarrativeFormat,
        ""
      )
    }
    if (settings$discussEvidence) {
      prompt <- c(
        prompt,
        discussEvidenceFormatTemplate,
        ""
      )
    }
    prompt <- c(
      prompt,
      "Summary: (Only \"yes\" or \"no\")",
      ""
    )
  }
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
  prompt <- c()
  if (settings$provideExamples) {
    if (settings$writeNarrative) {
      prompt <- c(
        prompt,
        writeNarrativeTemplate,
        ""
      )
    }
    if (settings$testingReminder) {
      prompt <- c(
        prompt,
        testingReminderTemplate,
        ""
      )
    }
    if (settings$uncertaintyInstructions) {
      prompt <- c(
        prompt,
        uncertaintyInstructionsTemplate,
        ""
      )
    }
    prompt <- c(
      prompt,
      "Healthcare data:",
      ""
    )
  }
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
