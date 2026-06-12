library(Keeper)
library(testthat)
library(dplyr)
gibConceptSets <- read.csv(system.file("gibConceptSets.csv", package = "Keeper"))

# One concept per group to speed up testing (some databases require long time for uploading):
gibConceptSets <- gibConceptSets |>
  group_by(conceptSetName, target) |>
  slice_head(n = 1) |>
  ungroup()

# testServer = testServers[[1]]
for (testServer in testServers) {
  test_that(addDbmsToLabel("Run Keeper on database", testServer), {
    # Create tiny cohort
    connection <- DatabaseConnector::connect(testServer$connectionDetails)
    sql <- "
      SELECT TOP 10 CAST(1 AS INT) cohort_definition_id,
        person_id AS subject_id,
        DATEADD(DAY, 90, observation_period_start_date) AS cohort_start_date,
        DATEADD(DAY, 90, observation_period_start_date) AS cohort_end_date
      INTO @cohort_database_schema.@cohort_table
      FROM @cdm_database_schema.observation_period;
    "
    cohortTable <- paste("cohort", paste(sample(c(letters, 0:9), 8), collapse = ""), sep = "_")
    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = sql,
      cdm_database_schema = testServer$cdmDatabaseSchema,
      cohort_database_schema = testServer$cohortDatabaseSchema,
      cohort_table = cohortTable
    )
    on.exit(DatabaseConnector::renderTranslateExecuteSql(connection, 
                                                         "DROP TABLE @cohort_database_schema.@cohort_table;",
                                                         cohort_database_schema = testServer$cohortDatabaseSchema,
                                                         cohort_table = cohortTable))
    on.exit(DatabaseConnector::disconnect(connection), add = TRUE)
    
    # Run KEEPER
    keeper <- generateKeeper(
      connection = connection,
      cdmDatabaseSchema = testServer$cdmDatabaseSchema,
      cohortDatabaseSchema = testServer$cohortDatabaseSchema,
      cohortTable = cohortTable,
      cohortDefinitionId = 1,
      tempEmulationSchema = testServer$cohortDatabaseSchema,
      phenotypeName = "Dummy",
      sampleSize = 10,
      removePii = FALSE,
      keeperConceptSets = gibConceptSets
    )
    expect_s3_class(keeper, "data.frame")
    expect_true("personId" %in% keeper$category)
    
    # Create highly-sensitive cohort
    specConcepts <- createSensitiveCohort(
      connection = connection,
      cdmDatabaseSchema = testServer$cdmDatabaseSchema,
      cohortDatabaseSchema = testServer$cohortDatabaseSchema,
      cohortTable = cohortTable,
      cohortDefinitionId = 999,
      createCohortTable = FALSE,
      tempEmulationSchema = testServer$cohortDatabaseSchema,
      keeperConceptSets = gibConceptSets
    )
    sql <- "
      SELECT COUNT(*) 
      FROM @cohort_database_schema.@cohort_table 
      WHERE cohort_definition_id = 999;
    "
    cohortCount <- DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = sql,
      cohort_database_schema = testServer$cohortDatabaseSchema,
      cohort_table = cohortTable
    )
    expect_gt(cohortCount[1, 1], 0)
  })
}
