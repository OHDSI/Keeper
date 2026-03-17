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

#' Launch the reviewer Shiny app
#'
#' @param keeper            The output of the [generateKeeper()] function.
#' @param keeperConceptSets The output of the [generateKeeperConceptSets()] function. 
#' @param decisionsFileName The location of the CSV file where the decisions made by the reviewer will be written.
#'
#' @returns
#' Returns nothing. Called for launching the Shiny app.
#' 
#' @export
launchReviewerApp <- function(keeper, keeperConceptSets, decisionsFileName) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(keeper, add = errorMessages)
  checkmate::assertNames(colnames(keeper), must.include = c("generatedId",
                                                            "startDay", 
                                                            "endDay",
                                                            "conceptId",
                                                            "conceptName",
                                                            "category",
                                                            "target",
                                                            "extraData"), add = errorMessages)
  checkmate::assertDataFrame(keeperConceptSets, min.rows = 1, add = errorMessages)
  checkmate::assertNames(colnames(keeperConceptSets), must.include = c("conceptId",
                                                                       "conceptName", 
                                                                       "vocabularyId",
                                                                       "conceptSetName",
                                                                       "target"), add = errorMessages) 
  checkmate::assertCharacter(decisionsFileName, min.chars = 1, add = errorMessages)
  checkmate::reportAssertions(errorMessages)
  appDir <- system.file("shiny", package = "Keeper")
  if (appDir == "") {
    stop("Could not find shiny directory. Try re-installing `Keeper`.", call. = FALSE)
  }
  ensureInstalled(c("shiny", "bslib", "shinyjs", "pool", "readr", "plotly"))
  
  .GlobalEnv$.shinyArgs <- list(keeper = keeper,
                                decisionsFileName = decisionsFileName,
                                conceptSets = keeperConceptSets)
  on.exit(rm(".shinyArgs", envir = .GlobalEnv))
  
  shiny::runApp(appDir, display.mode = "normal")
}

isInstalled <- function(package) {
  return(length(find.package(package, quiet = TRUE)) != 0)
}

ensureInstalled <- function(packages) {
  notInstalled <- packages[!sapply(packages, isInstalled)]
  
  if (interactive() & length(notInstalled) > 0) {
    message("Package(s): ", paste(notInstalled, collapse = ", "), " not installed")
    if (!isTRUE(utils::askYesNo("Would you like to install them?"))) {
      return(invisible(NULL))
    }
  }
  for (package in notInstalled) {
    install.packages(package)
  }
}