library(dplyr)
library(readr)
library(shiny)

if (exists(".shinyArgs", envir = .GlobalEnv)) {
  args <- get(".shinyArgs", envir = .GlobalEnv)
  keeper <- args$keeper
  decisionsFileName <- args$decisionsFileName
} else {
  keeper <- readRDS("/Users/schuemie/Library/CloudStorage/OneDrive-JNJ/QuickShare/KeeperMm - Copy.rds")
  decisionsFileName <- "/Users/schuemie/Library/CloudStorage/OneDrive-JNJ/QuickShare/Decisions.csv"
  # keeper <- readRDS("data/KeeperMm.rds")
  # decisionsFileName <- "data/Decisions.csv"
}

generatedIds <- unique(keeper$generatedId)

database <- keeper |>
  filter(category == "cdmSourceAbbreviation") |>
  head(1) |>
  pull(conceptName)
phenotype <- keeper |>
  filter(category == "phenotype") |>
  head(1) |>
  pull(conceptName)

nPersons <- length(generatedIds)

hasPersonIds <- "personId" %in% keeper$category

if (file.exists(decisionsFileName)) {
  message("Loading existing decisions file")
  decisionsDataFrame <- read_csv(decisionsFileName, show_col_types = FALSE)
  decisionsDataFrame <- as.data.frame(decisionsDataFrame)
} else {
  decisionsDataFrame <- data.frame(generatedId = generatedIds,
                                   decision = NA,
                                   indexDay = 0)
}