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

cohortDefinitionId <- 20765 
phenotypeName <- "Multple myeloma"

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
conceptSets <- read_csv("e:/temp/mmConceptSets.csv", show_col_types = FALSE)

keeper <- generateKeeper(
  connection = connection,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortTable = cohortTable,
  cohortDefinitionId = cohortDefinitionId,
  sampleSize = 20,
  keeperConceptSets = conceptSets,
  phenotypeName = phenotypeName
)
saveRDS(keeper, "e:/temp/KeeperMm.rds")
keeperTable <- convertKeeperToTable(keeper)
readr::write_csv(keeperTable, "e:/temp/KeeperMm.csv")


# Run Shiny app ------------------------------------------------------------------------
keeper <- readRDS("/Users/schuemie/Library/CloudStorage/OneDrive-JNJ/QuickShare/KeeperMm.rds")
decisionsFileName <- "/Users/schuemie/Library/CloudStorage/OneDrive-JNJ/QuickShare/Decisions.csv"

launchReviewerApp(keeper, decisionsFileName)


