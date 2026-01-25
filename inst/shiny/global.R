library(dplyr)
library(readr)
library(shiny)

if (exists(".shinyArgs", envir = .GlobalEnv)) {
  args <- get(".shinyArgs", envir = .GlobalEnv)
  keeper <- args$keeper
  decisionsFileName <- args$decisionsFileName
} else {
  keeper <- read_csv("/Users/schuemie/Library/CloudStorage/OneDrive-JNJ/QuickShare/KeeperCd.csv",
                     show_col_types = FALSE)
  decisionsFileName <- "/Users/schuemie/Library/CloudStorage/OneDrive-JNJ/QuickShare/Decisions.csv"
}

personIds <- keeper |>
  pull(personId) |>
  unique()

if (file.exists(decisionsFileName)) {
  message("Loading existing decisions file")
  decisionsDataFrame <- read_csv(decisionsFileName, show_col_types = FALSE)
} else {
  decisionsDataFrame <- data.frame(personId = personIds,
                                   decision = NA,
                                   indexDay = 0)
}