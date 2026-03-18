library(Keeper)
library(testthat)
library(Eunomia)

connectionDetails <- getEunomiaConnectionDetails()
createCohorts(connectionDetails)
gibConceptSets <- read.csv(system.file("gibConceptSets.csv", package = "Keeper"))

test_that("Run Keeper on Eunomia", {
  keeper <- generateKeeper(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = "main",
    cohortDatabaseSchema = "main",
    cohortTable = "cohort",
    cohortDefinitionId = 3,
    phenotypeName = "GI Bleed",
    sampleSize = 10,
    keeperConceptSets = gibConceptSets
  )

  expect_s3_class(keeper, "data.frame")
  expect_true("personId" %in% keeper$category)

  keeperTable <- convertKeeperToTable(keeper)
  expect_s3_class(keeperTable, "data.frame")
  expect_true("personId" %in% colnames(keeperTable))

  settings <- createPromptSettings()
  systempPrompt <- createSystemPrompt(settings, "GI bleed")
  prompt <- createPrompt(settings, "GI bleed", keeperTable[1, ])
  expect_type(systempPrompt, "character")
  expect_type(prompt, "character")
})

test_that("Run Keeper supressing person IDs", {
  keeper <- generateKeeper(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = "main",
    cohortDatabaseSchema = "main",
    cohortTable = "cohort",
    cohortDefinitionId = 3,
    phenotypeName = "GI Bleed",
    sampleSize = 10,
    removePersonId = TRUE,
    keeperConceptSets = gibConceptSets
  )

  expect_s3_class(keeper, "data.frame")
  expect_false("personId" %in% keeper$category)

  keeperTable <- convertKeeperToTable(keeper)
  expect_s3_class(keeperTable, "data.frame")
  expect_false("personId" %in% colnames(keeperTable))

  settings <- createPromptSettings()
  prompt <- createPrompt(settings, "GI bleed", keeperTable[1, ])
  expect_type(prompt, "character")
})

test_that("Create sensitive cohort in existing cohort table on Eunomia", {
  specConcepts <- createSensitiveCohort(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = "main",
    cohortDatabaseSchema = "main",
    cohortTable = "cohort",
    cohortDefinitionId = 999,
    createCohortTable = FALSE,
    keeperConceptSets = gibConceptSets
  )

  expect_s3_class(specConcepts, "data.frame")
  expect_true("Endoscopy" %in% specConcepts$conceptName)

  connection <- DatabaseConnector::connect(connectionDetails)
  cohortCount <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT COUNT(*) FROM main.cohort WHERE cohort_definition_id = 999;"
  )
  DatabaseConnector::disconnect(connection)
  expect_gt(cohortCount[1, 1], 0)
})

test_that("Create sensitive cohort in new cohort table on Eunomia", {
  specConcepts <- createSensitiveCohort(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = "main",
    cohortDatabaseSchema = "main",
    cohortTable = "sens_cohort",
    cohortDefinitionId = 1,
    createCohortTable = TRUE,
    keeperConceptSets = gibConceptSets
  )

  connection <- DatabaseConnector::connect(connectionDetails)
  cohortCount <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT COUNT(*) FROM main.sens_cohort WHERE cohort_definition_id = 1;"
  )
  DatabaseConnector::disconnect(connection)
  expect_gt(cohortCount[1, 1], 0)
})
