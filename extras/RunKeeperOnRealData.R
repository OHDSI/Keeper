library(Keeper)
library(ROhdsiWebApi)
library(CohortGenerator)
library(readr)
library(dplyr)

connectionDetails <- createConnectionDetails(
  dbms = "spark",
  connectionString = keyring::key_get("databricksConnectionString"),
  user = "token",
  password = keyring::key_get("databricksToken")
)
cdmDatabaseSchema <- "merative_ccae.cdm_merative_ccae_v3789"
cohortDatabaseSchema <- "scratch.scratch_mschuemi"
cohortTable  <- "keeper_test"
options(sqlRenderTempEmulationSchema = "scratch.scratch_mschuemi")

cohortDefinitionId <- 20892

connection <- connect(connectionDetails)

# Create cohort -------------------------------------------------------------------------------------
authorizeWebApi(
  baseUrl = Sys.getenv("baseUrl"),
  authMethod = "windows"
)
cohortDefinitionSet <- exportCohortDefinitionSet(cohortIds = cohortDefinitionId, 
                                                 baseUrl = Sys.getenv("baseUrl"))
cohortTableNames <- getCohortTableNames(cohortTable = cohortTable)
createCohortTables(connection = connection,
                   cohortDatabaseSchema = cohortDatabaseSchema,
                   cohortTableNames = cohortTableNames)
generateCohortSet(connection = connection,
                  cdmDatabaseSchema = cdmDatabaseSchema,
                  cohortDatabaseSchema = cohortDatabaseSchema,
                  cohortTableNames = cohortTableNames,
                  cohortDefinitionSet = cohortDefinitionSet)
getCohortCounts(connection = connection,
                cohortDatabaseSchema = cohortDatabaseSchema,
                cohortTable = cohortTable)

# Run KEEPER -------------------------------------------------------------------------------------
conceptSets <- read_csv("extras/t1dmConceptSets.csv", show_col_types = FALSE)
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
  cohortName = "T1DM",
  sampleSize = 20,
  databaseId = "CCAE",
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
readr::write_csv(keeper, "e:/temp/Keeper.csv")


# Run Shiny app ------------------------------------------------------------------------
keeper <- readr::read_csv("/Users/schuemie/Library/CloudStorage/OneDrive-JNJ/QuickShare/KeeperCd.csv",
                   show_col_types = FALSE)
decisionsFileName <- "/Users/schuemie/Library/CloudStorage/OneDrive-JNJ/QuickShare/Decisions.csv"

launchReviewerApp(keeper, decisionsFileName)


