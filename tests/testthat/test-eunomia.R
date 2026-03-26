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
    keeperConceptSets = gibConceptSets,
    removePii = FALSE
  )

  expect_s3_class(keeper, "data.frame")
  expect_true("personId" %in% keeper$category)

  keeperTable <- convertKeeperToTable(keeper)
  expect_s3_class(keeperTable, "data.frame")
  expect_true("personId" %in% colnames(keeperTable))
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
    removePii = TRUE,
    keeperConceptSets = gibConceptSets
  )

  expect_s3_class(keeper, "data.frame")
  expect_false("personId" %in% keeper$category)

  keeperTable <- convertKeeperToTable(keeper)
  expect_s3_class(keeperTable, "data.frame")
  expect_false("personId" %in% colnames(keeperTable))
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

test_that("Upload LLM reviews to Eunomia", {
  llmReviews <- readRDS(system.file("llmReviews.rds", package = "Keeper"))
  # Make up Pii:
  llmReviews$personId <- round(runif(nrow(llmReviews), 1, 10000000))
  llmReviews$cohortStartDate <- as.Date("2000-01-01")
  
  uploadReferenceCohort(
    connectionDetails = connectionDetails,
    referenceCohortDatabaseSchema = "main",
    referenceCohortTableNames = createReferenceCohortTableNames("ref_cohort"),
    referenceCohortDefinitionId = 1,
    createReferenceCohortTables = TRUE,
    reviews = llmReviews
  )
  connection <- DatabaseConnector::connect(connectionDetails)
  cohortCount <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT COUNT(*) FROM main.ref_cohort WHERE cohort_definition_id = 1;"
  )
  metadataCount <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT COUNT(*) FROM main.ref_cohort_metadata WHERE cohort_definition_id = 1;"
  )
  DatabaseConnector::disconnect(connection)
  expect_equal(cohortCount[1, 1], nrow(llmReviews))
  expect_equal(metadataCount[1, 1], 1)

})
