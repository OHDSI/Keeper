library(dplyr)
library(readr)
library(shiny)

if (exists(".shinyArgs", envir = .GlobalEnv)) {
  args <- get(".shinyArgs", envir = .GlobalEnv)
  keeper <- args$keeper
  decisionsFileName <- args$decisionsFileName
} else {
  # keeper <- readRDS("/Users/schuemie/Library/CloudStorage/OneDrive-JNJ/QuickShare/KeeperMm.rds")
  # decisionsFileName <- "/Users/schuemie/Library/CloudStorage/OneDrive-JNJ/QuickShare/Decisions.csv"
  keeper <- readRDS("data/KeeperMm.rds")
  decisionsFileName <- "data/Decisions.csv"
}

generatedIds <- keeper$demographics |>
  pull(generatedId) 

database <- keeper$metaData$cdmSourceAbbreviation
phenotype <- keeper$metaData$phenotypeName
keeper$metaData <- NULL

nPersons <- length(generatedIds)

hasPersonIds <- "personId" %in% colnames(keeper$demographics)

if (file.exists(decisionsFileName)) {
  message("Loading existing decisions file")
  decisionsDataFrame <- read_csv(decisionsFileName, show_col_types = FALSE)
  decisionsDataFrame <- as.data.frame(decisionsDataFrame)
} else {
  decisionsDataFrame <- data.frame(generatedId = generatedIds,
                                   decision = NA,
                                   indexDay = 0)
}