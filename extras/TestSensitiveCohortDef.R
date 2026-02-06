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
specConceptsFileName <- "e:/temp/mmSpecConcepts.csv"
keeperFileName <- "e:/temp/mmKeeper10K.rds"

conceptSetsFileName <- "e:/temp/cdConceptSets.csv"
specConceptsFileName <- "e:/temp/cdSpecConcepts.csv"
keeperFileName <- "e:/temp/cdKeeper10K.rds"

conceptSetsFileName <- "extras/t1dmConceptSets.csv"
specConceptsFileName <- "e:/temp/t1dmSpecConcepts.csv"
keeperFileName <- "e:/temp/t1dmKeeper10K.rds"

conceptSetsFileName <- "e:/temp/afConceptSets.csv"
specConceptsFileName <- "e:/temp/afSpecConcepts.csv"
keeperFileName <- "e:/temp/afKeeper10K.rds"

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
readr::write_csv(concepts, specConceptsFileName)

# Run Keeper on sensitive cohort ----------------------------------------
# keeper <- readRDS(keeperFileName)
# personIds <- keeper$demographics$personId

keeper <- generateKeeper(
  connectionDetails = connectionDetails,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortTable = cohortTable,
  cohortDefinitionId = 1,
  sampleSize = 10000,
  personIds = personIds,
  phenotypeName = "Atrial Fibrillation",
  keeperConceptSets = conceptSets
)
# newIds <- keeper2 |>
#   filter(category == "personId") |>
#   select(personId = "conceptName", "generatedId")
# newToOldIds <- newIds |>
#   inner_join(keeper$demographics |>
#                select("personId", oldId = "generatedId"), by = join_by("personId"))
# 
# newKeeper <- keeper2 |>
#   inner_join(newToOldIds, by = join_by("generatedId")) |>
#   mutate(generatedId = oldId) |>
#   select(-"oldId")

saveRDS(keeper, keeperFileName)
# keeperTable <- convertKeeperToTable(keeper)
# readr::write_csv(keeperTable, "e:/temp/KeeperMm.csv")

# Use LLM to adjudicate cases -------------------------------------------
library(ellmer)
client <- chat_azure_openai(
  endpoint = gsub("/openai/deployments.*", "", keyring::key_get("genai_o3_endpoint")),
  api_version = "2024-12-01-preview",
  model = "o3",
  credentials = function() keyring::key_get("genai_api_gpt4_key")
)
cacheFolder <- "cacheAf"
keeper <- readRDS(keeperFileName)
reviewCases(keeper = keeper,
            client = client,
            cacheFolder = cacheFolder)

