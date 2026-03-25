library(Keeper)
library(dplyr)
library(ellmer)
library(DatabaseConnector)

client <- chat_azure_openai(
  endpoint = gsub("/openai/deployments.*", "", keyring::key_get("genai_o3_endpoint")),
  api_version = "2024-12-01-preview",
  model = "o3",
  credentials = function() keyring::key_get("genai_api_gpt4_key")
)

connectionDetails <- createConnectionDetails(
  dbms = "spark",
  connectionString = keyring::key_get("databricksConnectionString"),
  user = "token",
  password = keyring::key_get("databricksToken")
)
cdmDatabaseSchema <- "optum_extended_dod.cdm_optum_extended_dod_v3787"
cohortDatabaseSchema <- "scratch.scratch_mschuemi"
cohortTable <- "keeper_cohort"
referenceCohortDatabaseSchema <- "scratch.scratch_all"
referenceCohortTable <- "reference_cohort_optum_extended_dod_v3787"
options(sqlRenderTempEmulationSchema = "scratch.scratch_mschuemi")

folder <- "e:/KeeperReferenceCohorts"

# Create concept sets --------------------------------------------------------------------------------------------------
conceptSets <- generateKeeperConceptSets(
  phenotype = "Type I Diabetes Mellitus (afib)",
  client = client,
  vocabConnectionDetails = connectionDetails,
  vocabDatabaseSchema = cdmDatabaseSchema
)
# readr::write_csv(conceptSets, "extras/atrialFibrillationConceptSets.csv")

# Create simple cohort -------------------------------------------------------------------------------------------------
library(Capr)

afibConceptIds <- c(313217, 4068155)
afibCs <- cs(
  descendants(afibConceptIds),
  name = "Atrial fibrillation"
)

afibCohort <- cohort(
  entry = entry(
    conditionOccurrence(afibCs, firstOccurrence())
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)
# Note: this will automatically assign cohort ID 1:
cohortSet <- makeCohortSet(afibCohort)

library(CohortGenerator)
connection <- connect(connectionDetails)
createCohortTables(
  connection = connection,
  cohortTableNames = getCohortTableNames(cohortTable),
  cohortDatabaseSchema = cohortDatabaseSchema
)
CohortGenerator::generateCohortSet(
  connection = connection,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortTableNames = getCohortTableNames(cohortTable),
  cohortDefinitionSet = cohortSet
)
disconnect(connection)


# Run Keeper on the cohort ---------------------------------------------------------------------------------------------
conceptSets <- readr::read_csv( "extras/atrialFibrillationConceptSets.csv")

keeper <- generateKeeper(
  connectionDetails = connectionDetails,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortTable = cohortTable,
  cohortDefinitionId = 1,
  sampleSize = 100,
  phenotypeName = "Atrial fibrillation",
  keeperConceptSets = conceptSets,
  removePii = TRUE
)

saveRDS(keeper, file.path(folder, "afibKeeper.rds"))


# Run LLM adjudication -------------------------------------------------------------------------------------------------
keeper <- readRDS(file.path(folder, "afibKeeper.rds"))

library(ellmer)
client <- chat_azure_openai(
  endpoint = gsub("/openai/deployments.*", "", keyring::key_get("genai_o3_endpoint")),
  api_version = "2024-12-01-preview",
  model = "o3",
  credentials = function() keyring::key_get("genai_api_gpt4_key")
)
promptSettings <- createPromptSettings()
llmResponses <- reviewCases(keeper = keeper,
                            settings = promptSettings,
                            phenotypeName = "Atrial Fibrillation",
                            client = client,
                            cacheFolder = file.path(folder, "cacheAfib"))
llmResponses |>
  group_by(isCase) |>
  summarise(n())
saveRDS(llmResponses, file.path(folder, "llmReviewsAfib.rds"))

# Create highly sensitive cohort ---------------------------------------------------------------------------------------
conceptSets <- readr::read_csv( "extras/atrialFibrillationConceptSets.csv")

createSensitiveCohort(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortTable = cohortTable,
  cohortDefinitionId = 2,
  createCohortTable = FALSE,
  keeperConceptSets = conceptSets
)

# Run Keeper on the sensitive cohort -----------------------------------------------------------------------------------
keeperHsc <- generateKeeper(
  connectionDetails = connectionDetails,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortTable = cohortTable,
  cohortDefinitionId = 2,
  sampleSize = 10000,
  phenotypeName = "Atrial Fibrillation",
  keeperConceptSets = conceptSets,
  removePii = FALSE
)
saveRDS(keeperHsc, file.path(folder, "keeperAfibHsc.rds"))


# Run LLM adjudication on highly-sensitive cohort ----------------------------------------------------------------------
keeperHsc <- readRDS(file.path(folder, "keeperAfibHsc.rds"))

library(ellmer)
client <- chat_azure_openai(
  endpoint = gsub("/openai/deployments.*", "", keyring::key_get("genai_o3_endpoint")),
  api_version = "2024-12-01-preview",
  model = "o3",
  credentials = function() keyring::key_get("genai_api_gpt4_key")
)
promptSettings <- createPromptSettings()
llmResponsesHsc <- reviewCases(keeper = keeperHsc,
                               settings = promptSettings,
                               client = client,
                               cacheFolder =  file.path(folder, "cacheAfibHsc"))
saveRDS(llmResponsesHsc, file.path(folder, "llmReviewsAfibHsc.rds"))


# Upload reference cohort to server ------------------------------------------------------------------------------------
llmResponsesHsc <- readRDS(file.path(folder, "llmReviewsAfibHsc.rds"))

uploadReferenceCohort(
  connectionDetails = connectionDetails,
  referenceCohortDatabaseSchema = referenceCohortDatabaseSchema,
  referenceCohortTableNames = createReferenceCohortTableNames(referenceCohortTable),
  referenceCohortDefinitionId = 1,
  createReferenceCohortTables = TRUE,
  reviews = llmResponsesHsc
)

# Compute cohort operating characteristics -----------------------------------------------------------------------------
computeCohortOperatingCharacteristics(
  connectionDetails = connectionDetails,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortTable = cohortTable,
  cohortDefinitionId = 1,
  referenceCohortDatabaseSchema = referenceCohortDatabaseSchema,
  referenceCohortTableNames = createReferenceCohortTableNames(referenceCohortTable),
  referenceCohortDefinitionId = 1
)
