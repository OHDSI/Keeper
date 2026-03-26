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
cohortTable <- "keeper_vignette_cohort"
referenceCohortDatabaseSchema <- "scratch.scratch_mschuemi"
referenceCohortTable <- "keeper_vignette_reference_cohort"


options(sqlRenderTempEmulationSchema = "scratch.scratch_mschuemi")

# Create concept sets --------------------------------------------------------------------------------------------------
conceptSets <- generateKeeperConceptSets(
  phenotype = "Type I Diabetes Mellitus (T1DM)",
  client = client,
  vocabConnectionDetails = connectionDetails,
  vocabDatabaseSchema = cdmDatabaseSchema
)
readr::write_csv(conceptSets, "inst/t1dmConceptSets.csv")

# Create simple cohort -------------------------------------------------------------------------------------------------
library(Capr)

t1dmConceptIds <- c(201254, 435216)
t1dmCs <- cs(
  descendants(t1dmConceptIds),
  name = "Type 1 Diabetes Mellitus"
)

t1dmCohort <- cohort(
  entry = entry(
    conditionOccurrence(t1dmCs, firstOccurrence())
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)
# Note: this will automatically assign cohort ID 1:
cohortSet <- makeCohortSet(t1dmCohort)

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
keeper <- generateKeeper(
  connectionDetails = connectionDetails,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortTable = cohortTable,
  cohortDefinitionId = 1,
  sampleSize = 20,
  phenotypeName = "T1DM",
  keeperConceptSets = conceptSets,
  removePii = TRUE
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
# client <- chat_openai_compatible(
#   base_url = "http://localhost:1234/v1",
#   credentials = function() "lm-studio",
#   model = "qwen/qwen3-coder-next"
# )
client <- chat_azure_openai(
  endpoint = gsub("/openai/deployments.*", "", keyring::key_get("genai_o3_endpoint")),
  api_version = "2024-12-01-preview",
  model = "o3",
  credentials = function() keyring::key_get("genai_api_gpt4_key")
)
promptSettings <- createPromptSettings()
llmReviews <- reviewCases(keeper = keeper,
                            settings = promptSettings,
                            phenotypeName = "Type I Diabetes Mellitus (T1DM)",
                            client = client,
                            cacheFolder = "cacheVignette")

saveRDS(llmReviews, "inst/llmReviews.rds")


# Create sensitive cohort ----------------------------------------------------------------------------------------------
conceptSets <- readr::read_csv( "inst/t1dmConceptSets.csv")

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
  sampleSize = 200,
  phenotypeName = "T1DM",
  keeperConceptSets = conceptSets,
  removePii = FALSE
)
saveRDS(keeperHsc, "e:/temp/keeperVignette/keeperHsc.rds")


# Run LLM adjudication on highly-sensitive cohort ----------------------------------------------------------------------
keeperHsc <- readRDS("e:/temp/keeperVignette/keeperHsc.rds")

library(ellmer)
client <- chat_azure_openai(
  endpoint = gsub("/openai/deployments.*", "", keyring::key_get("genai_o3_endpoint")),
  api_version = "2024-12-01-preview",
  model = "o3",
  credentials = function() keyring::key_get("genai_api_gpt4_key")
)
promptSettings <- createPromptSettings()
llmReviewsHsc <- reviewCases(keeper = keeperHsc,
                               settings = promptSettings,
                               phenotypeName = "Type I Diabetes Mellitus (T1DM)",
                               client = client,
                               cacheFolder = "cacheVignetteHsc")

saveRDS(llmReviewsHsc, "e:/temp/keeperVignette/llmReviewsHscHsc.rds")


# Upload reference cohort to server ------------------------------------------------------------------------------------
llmReviewsHsc <- readRDS("e:/temp/keeperVignette/llmReviewsHsc.rds")

uploadReferenceCohort(
  connectionDetails = connectionDetails,
  referenceCohortDatabaseSchema = referenceCohortDatabaseSchema,
  referenceCohortTable = referenceCohortTable,
  referenceCohortDefinitionId = 1,
  createReferenceCohortTable = TRUE,
  reviews = llmReviewsHsc
)

# Compute cohort operating characteristics -----------------------------------------------------------------------------
computeCohortOperatingCharacteristics(
  connectionDetails = connectionDetails,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortTable = cohortTable,
  cohortDefinitionId = 1,
  referenceCohortDatabaseSchema = referenceCohortDatabaseSchema,
  referenceCohortTable = referenceCohortTable,
  referenceCohortDefinitionId = 1
)









