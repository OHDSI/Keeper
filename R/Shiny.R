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
#' @param keeper            The output of the [createKeeper()] function.
#' @param decisionsFileName The location where the decisions made by the reviewer wil be written.
#'
#' @returns
#' Returns nothing. Called for launching the Shiny app.
#' 
#' @export
launchReviewerApp <- function(keeper, decisionsFileName) {
  appDir <- system.file("shiny", package = "Keeper")
  
  if (appDir == "") {
    stop("Could not find shiny directory. Try re-installing `Keeper`.", call. = FALSE)
  }
  
  .GlobalEnv$.shinyArgs <- list(keeper = keeper, decisionsFileName = decisionsFileName)
  on.exit(rm(.shinyArgs, envir = .GlobalEnv))
  
  shiny::runApp(appDir, display.mode = "normal")
}