library(Keeper)
library(dplyr)


connectionDetails <- createConnectionDetails(
  dbms = "spark",
  connectionString = keyring::key_get("databricksConnectionString"),
  user = "token",
  password = keyring::key_get("databricksToken")
)
cdmDatabaseSchema <- "merative_ccae.cdm_merative_ccae_v3789"
cohortDatabaseSchema <- "scratch.scratch_mschuemi"
cohortTable <- "test_keeper_sens_cohort"

options(sqlRenderTempEmulationSchema = "scratch.scratch_mschuemi")

conceptSetsFileName <- "e:/temp/mmConceptSets.csv"
outputFileName <- "e:/temp/mmSpecConcepts.csv"
keeperFileName <- "e:/temp/mmKeeper10K.csv"

conceptSetsFileName <- "e:/temp/cdConceptSets.csv"
outputFileName <- "e:/temp/cdRatios.csv"

conceptSetsFileName <- "extras/t1dmConceptSets.csv"
outputFileName <- "e:/temp/t1dmRatios.csv"


# Create sensitive cohort  -----------------------------------------
conceptSets <- readr::read_csv(conceptSetsFileName, show_col_types = FALSE)

concepts <- createSensitiveCohort(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortTable = cohortTable,
  cohortDefinitionId = 1,
  createCohortTable = TRUE,
  keeperConceptSets = conceptSets
)
readr::write_csv(concepts, outputFileName)

# 
# 
# 
# keeperConceptSets <- readr::read_csv(conceptSetsFileName)
# cohortDefinitionId = 1
# 
# tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")
# connection <- DatabaseConnector::connect(connectionDetails)
# conceptSets <- keeperConceptSets |>
#   filter(.data$target == "Disease of interest")
# DatabaseConnector::insertTable(
#   connection = connection,
#   tableName = "#concept_sets",
#   data = conceptSets,
#   dropTableIfExists = TRUE,
#   createTable = TRUE,
#   tempTable = TRUE,
#   camelCaseToSnakeCase = TRUE,
#   tempEmulationSchema = tempEmulationSchema
# )
# message("Computing concept ratios")
# sql <- SqlRender::loadRenderTranslateSql(
#   sqlFilename = "CreateSensCohortRatios.sql", 
#   packageName = "Keeper", 
#   dbms = DatabaseConnector::dbms(connection),
#   cdm_database_schema = cdmDatabaseSchema,
#   tempEmulationSchema = tempEmulationSchema
# )
# DatabaseConnector::executeSql(connection = connection, sql = sql)
# 
# conceptRatios <- DatabaseConnector::renderTranslateQuerySql(
#   connection = connection,
#   sql = "SELECT * FROM #concept_ratios",
#   snakeCaseToCamelCase = TRUE,
#   tempEmulationSchema = tempEmulationSchema
# )
# conceptRatios <- conceptRatios |>
#   filter(ppv > 0.1)
# 
# sql <- "
#       DROP TABLE IF EXISTS @cohort_database_schema.@cohort_table;
#       
#       CREATE TABLE @cohort_database_schema.@cohort_table (
#         cohort_definition_id BIGINT,
#         subject_id BIGINT,
#         cohort_start_date DATE,
#         cohort_end_date DATE
#       );
#     "
# DatabaseConnector::renderTranslateExecuteSql(
#   connection = connection,
#   sql = sql,
#   cohort_database_schema = cohortDatabaseSchema,
#   cohort_table = cohortTable
# )
# 
# # sql <- "SELECT COuNT(DISTINCT person_id) 
# # FROM @cdm_database_schema.condition_occurrence
# # INNER JOIN @cdm_database_schema.concept_ancestor
# #   ON condition_concept_id = descendant_concept_id
# # WHERE ancestor_concept_id = 378253;"
# # DatabaseConnector::renderTranslateQuerySql(connection, sql, cdm_database_schema = cdmDatabaseSchema)
# # conceptRatios |>
# #   filter(conceptId == 378253)
# 
# 
# message("Constructing sensitive cohort")
# sql <- SqlRender::loadRenderTranslateSql(
#   sqlFilename = "CreateSensitiveCohort.sql", 
#   packageName = "Keeper", 
#   dbms = DatabaseConnector::dbms(connection),
#   cdm_database_schema = cdmDatabaseSchema,
#   cohort_database_schema = cohortDatabaseSchema,
#   cohort_table = cohortTable,
#   cohort_definition_id = cohortDefinitionId,
#   tempEmulationSchema = tempEmulationSchema
# )
# 
# DatabaseConnector::executeSql(connection = connection, sql = sql)
# 
# sql <- "SELECT COUNT(*) FROM #doi_cohort;"
# DatabaseConnector::renderTranslateQuerySql(connection, sql)
# sql <- "SELECT COUNT(*) FROM #combi_cohort;"
# DatabaseConnector::renderTranslateQuerySql(connection, sql)
# 
# 
# DatabaseConnector::dropEmulatedTempTables(connection)
# 

# Run Keeper on sensitive cohort ----------------------------------------
getConceptIds <- function(name) {
  conceptIds <- conceptSets |>
    filter(conceptSetName == name) |>
    pull(conceptId) |>
    unique()
  return(conceptIds)
}
keeper <- createKeeper(
  connectionDetails = connectionDetails,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortTable = cohortTable,
  cohortDefinitionId = 1,
  cohortName = "",
  sampleSize = 10000,
  databaseId = "",
  doi = getConceptIds("doi"), 
  comorbidities = getConceptIds("comorbidities"),
  symptoms = getConceptIds("symptoms"),
  alternativeDiagnosis = getConceptIds("alternativeDiagnosis"),
  drugs = getConceptIds("drugs"),
  diagnosticProcedures = getConceptIds("diagnosticProcedures"),
  measurements = getConceptIds("measurements"),
  treatmentProcedures = getConceptIds("treatmentProcedures"),
  complications = getConceptIds("complications")
)
readr::write_csv(keeper, keeperFileName)


