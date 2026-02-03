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
outputFileName <- "e:/temp/mmRatios.csv"

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

# Run Keepper on sensitive cohort ----------------------------------------
getConceptIds <- function(name) {
  conceptIds <- conceptSets |>
    filter(conceptSetName == name) |>
    pull(conceptId) |>
    unique()
  return(conceptIds)
}
keeper <- createKeeper(
  connection = connection,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortTable = cohortTable,
  cohortDefinitionId = cohortDefinitionId,
  cohortName = "",
  sampleSize = 20,
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
readr::write_csv(keeper, "e:/temp/KeeperMm.csv")


