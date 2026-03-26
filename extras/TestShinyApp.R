library(testthat)
keeper <- readRDS("inst/shuffledKeeper.rds")
conceptSets <- readr::read_csv("inst/t1dmConceptSets.csv", show_col_types = FALSE)


# Run Shiny app via function in package --------------------------------------------------------------------------------
library(Keeper)

# With new decisions file
decisionsFile <- tempfile(pattern = "decisions", fileext = ".csv")
launchReviewerApp(keeper = keeper,
                  keeperConceptSets = conceptSets,
                  decisionsFileName = decisionsFile)
expect_true(file.exists(decisionsFile))

# With existing decisions file
launchReviewerApp(keeper = keeper,
                  keeperConceptSets = conceptSets,
                  decisionsFileName = decisionsFile)

unlink(decisionsFile)

# Copy app to folder (e.g. to run on Shiny server) ---------------------------------------------------------------------
# appFolder <- tempfile(pattern = "keeperShiny")
# dir.create(appFolder)
# dir.create(file.path(appFolder, "www"))
# dir.create(file.path(appFolder, "data"))
# toCopy <- list.files("inst/shiny", recursive = TRUE)
# file.copy(from = file.path("inst/shiny", toCopy),
#           to = file.path(appFolder, toCopy))
# 
# unlink(appFolder)
