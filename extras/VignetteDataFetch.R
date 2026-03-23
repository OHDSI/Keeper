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
cdmDatabaseSchema <- "merative_mdcr.cdm_merative_mdcr_v3788"
cohortDatabaseSchema <- "scratch.scratch_mschuemi"
cohortTable <- "test_keeper_sens_cohort"
options(sqlRenderTempEmulationSchema = "scratch.scratch_mschuemi")

# Create concept sets --------------------------------------------------------------------------------------------------
conceptSets <- generateKeeperConceptSets(
  phenotype = "Type I Diabetes Mellitus (T1DM)",
  client = client,
  vocabConnectionDetails = connectionDetails,
  vocabDatabaseSchema = cdmDatabaseSchema
)
readr::write_csv(conceptSets, "inst/t1dmConceptSets.csv")

# Create sensitive cohort ----------------------------------------------------------------------------------------------
conceptSets <- readr::read_csv( "inst/t1dmConceptSets.csv")

createSensitiveCohort(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortTable = cohortTable,
  cohortDefinitionId = 1,
  createCohortTable = TRUE,
  keeperConceptSets = conceptSets
)

# Run Keeper on sensitive cohort ---------------------------------------------------------------------------------------
keeper <- generateKeeper(
  connectionDetails = connectionDetails,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortTable = cohortTable,
  cohortDefinitionId = 1,
  sampleSize = 20,
  phenotypeName = "T1DM",
  keeperConceptSets = conceptSets,
  removePersonId = TRUE
)

# Shuffle data
idx <- which(keeper$category == "age")
keeper$conceptName[idx] <- round(pmax(0, rnorm(length(idx), as.numeric(keeper$conceptName[idx]), 2)))

idx <- which(keeper$category == "sex")
keeper$conceptName[idx] <- if_else(runif(length(idx)) < 0.5, "MALE", "FEMALE")
keeper$conceptId[idx] <- if_else(keeper$conceptName[idx] == "MALE", 8507, 8532)

idx <- which(keeper$category == "observationPeriod")
keeper$startDay[idx] <- round(keeper$startDay[idx] - rnorm(length(idx), 2, 6))
keeper$endDay[idx] <- round(keeper$endDay[idx] + rnorm(length(idx), 2, 6))

saveRDS(keeper, "inst/shuffledKeeper.rds")

# Run LLM adjudication -------------------------------------------------------------------------------------------------
keeper <- readRDS("inst/shuffledKeeper.rds")

library(ellmer)
client <- chat_openai_compatible(
  base_url = "http://localhost:1234/v1",
  credentials = function() "lm-studio",
  model = "qwen/qwen3-coder-next"
)
promptSettings <- createPromptSettings()
llmResponses <- reviewCases(keeper = keeper,
                      settings = promptSettings,
                      phenotypeName = "Type I Diabetes Mellitus (T1DM)",
                      client = client,
                      cacheFolder = "cacheVignette")

saveRDS(llmResponses, "inst/llmResponses.rds")
