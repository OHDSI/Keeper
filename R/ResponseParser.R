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

parseLlmResponse <- function(response, noMatchIsInsufficientInformation = TRUE) {
  if (tolower(response$verdict) == "no") {
    isCase <- "no"
  } else if (tolower(response$verdict) == "yes") {
    isCase <- "yes"
  } else if (tolower(response$verdict) == "insufficient information") {
    isCase <- "insufficient information"
  } else if (noMatchIsInsufficientInformation) {
    isCase <- "insufficient information"
  } else {
    isCase <- NA
    warning("Unable to parse response: ", response)
  }
  if (!is.na(isCase) && isCase == "yes") {
    indexDay <- response$`day of onset`
  } else {
    indexDay <- NA
  }
  return(tibble(isCase = isCase, indexDay = indexDay, narrative = response$narrative))
}

parseLegacyLlmResponse <- function(response, noMatchIsInsufficientInformation = TRUE) {
  response <- paste(response, collapse = "\n")
  response <- trimws(gsub("^.*\nSummary:", "", response))
  response <- gsub("\\(Only \"yes\" or \"no\"\\)", "", response)
  if (grepl("500 Internal Server Error", response, ignore.case = TRUE)) {
    result <- NA
  } else if (grepl("\"yes\" or \"no\"", response, ignore.case = TRUE)) {
    result <- "insufficient information"
  } else if (grepl("\"yes\\.?\"", response, ignore.case = TRUE)) {
    result <- "yes"
  } else if (grepl("\"no\\.?\"", response, ignore.case = TRUE)) {
    result <- "no"
  } else if (grepl("It is not the most probable scenario", response, ignore.case = TRUE)) {
    result <- "no"
  } else if (grepl("(^|2[\\. ]+|\\()no([^y-z]|$)", response, ignore.case = TRUE)) {
    result <- "no"
  } else if (grepl("\"don't know\\.?\"", response, ignore.case = TRUE)) {
    result <- "insufficient information"
  } else if (grepl("\"unclear\\.?\"", response, ignore.case = TRUE)) {
    result <- "insufficient information"
  } else if (grepl("\"uncertain\\.?\"", response, ignore.case = TRUE)) {
    result <- "insufficient information"
  } else if (grepl("don't know", response, ignore.case = TRUE)) {
    result <- "insufficient information"
  } else if (grepl("\"maybe\"", response, ignore.case = TRUE)) {
    result <- "insufficient information"
  } else if (grepl("([^a-z]|^)yes([^a-z]|$)", response, ignore.case = TRUE)) {
    result <- "yes"
  } else if (grepl("([^a-z]|^)no([^a-z]|$)", response, ignore.case = TRUE)) {
    result <- "no"
  } else if (grepl("the evidence supports the diagnosis of", response, ignore.case = TRUE)) {
    result <- "yes"
  } else if (grepl("finding is inconclusive", response, ignore.case = TRUE)) {
    result <- "insufficient information"
  } else if (grepl("further information is needed", response, ignore.case = TRUE)) {
    result <- "insufficient information"
  } else if (grepl("I cannot[a-z ]+determine ", response, ignore.case = TRUE)) {
    result <- "insufficient information"
  } else if (grepl("insufficient information", response, ignore.case = TRUE)) {
    result <- "insufficient information"
  } else if (grepl("insufficient evidence", response, ignore.case = TRUE)) {
    result <- "insufficient information"
  } else if (grepl("unclear", response, ignore.case = TRUE)) {
    result <- "insufficient information"
  } else if (grepl("inconclusive", response, ignore.case = TRUE)) {
    result <- "insufficient information"
  } else if (grepl("uncertain", response, ignore.case = TRUE)) {
    result <- "insufficient information"
  } else if (grepl("definitive", response, ignore.case = TRUE)) {
    result <- "insufficient information"
  } else if (grepl("possible", response, ignore.case = TRUE)) {
    result <- "insufficient information"
  } else if (grepl("[^a-z]likely ", response, ignore.case = TRUE)) {
    result <- "yes"
  } else if (grepl("[^a-z]probable ", response, ignore.case = TRUE)) {
    result <- "yes"
  } else if (noMatchIsInsufficientInformation) {
    result <- "insufficient information"
  } else {
    result <- NA
    warning("Unable to parse response")
  }
  return(tibble(isCase = result, indexDay = NA, narrative = NA))
}
