library(Keeper)
library(dplyr)
library(ellmer)
library(DatabaseConnector)

# Local services -------------------------------------------------
client <- chat_openai_compatible(
  base_url = "http://localhost:1234/v1",
  credentials = function() "lm-studio",
  model = "nvidia/nemotron-3-nano"
)

vocabConnectionDetails <- createConnectionDetails(
  dbms = "postgresql",
  server = Sys.getenv("LOCAL_POSTGRES_SERVER"),
  user = Sys.getenv("LOCAL_POSTGRES_USER"),
  password = Sys.getenv("LOCAL_POSTGRES_PASSWORD")
)
vocabDatabaseSchema <- Sys.getenv("LOCAL_POSTGRES_VOCAB_SCHEMA")

# Shared services --------------------------------------------------
client <- chat_azure_openai(
  endpoint = gsub("/openai/deployments.*", "", keyring::key_get("genai_o3_endpoint")),
  api_version = "2024-12-01-preview",
  model = "o3",
  credentials = function() keyring::key_get("genai_api_gpt4_key")
)

vocabConnectionDetails <- createConnectionDetails(
  dbms = "spark",
  connectionString = keyring::key_get("databricksConnectionString"),
  user = "token",
  password = keyring::key_get("databricksToken")
)
vocabDatabaseSchema <- "merative_mdcr.cdm_merative_mdcr_v3788"


conceptSets <- generateKeeperConceptSets(
  condition = "Atrial fibrillation",
  client = client,
  vocabConnectionDetails = vocabConnectionDetails,
  vocabDatabaseSchema = vocabDatabaseSchema
)
readr::write_csv(conceptSets, "e:/temp/afConceptSets.csv")

# conceptSetsOld <- readr::read_csv("e:/temp/afConceptSetsOld.csv")
# 
# joined <- conceptSets |>
#   mutate(new = TRUE) |>
#   full_join(conceptSetsOld |>
#               mutate(old = TRUE),
#             by = join_by(conceptId, conceptName, vocabularyId, conceptSetName, target))|>
#   mutate(new = if_else(is.na(new), FALSE, TRUE),
#          old = if_else(is.na(old), FALSE, TRUE)) |>
#   mutate(both = new & old)
# readr::write_csv(joined, "e:/temp/afConceptSetsCompared.csv")

# Create many concept sets -----------------------------------------------------
phenotypes <- c("Thrombocytopenia",
                "Heart failure",
                "Arrhythmia",
                "Fever",
                "Neutropenia",
                "Seizure",
                "Anaphylaxis",
                "Myocardial infarction",
                "Angioedema",
                "Atrial Fibrillation",
                "Bradycardia",
                "Diarrhea",
                "Hyperglycemia",
                "Interstitial lung disease",
                "Pancreatitis",
                "Stevens-Johnson syndrome",
                "Syncope",
                "Tachycardia",
                "Thrombosis")
folder <- "e:/KeeperConceptSets"
ParallelLogger::addDefaultFileLogger(file.path(folder, "log.txt"))
ParallelLogger::addDefaultErrorReportLogger(file.path(folder, "errorLog.txt"))

phenotypes <- phenotypes[phenotypes != "Seizure"]

for (i in seq_along(phenotypes)) {
  phenotype <- phenotypes[i]
  message(sprintf("*** %s ***", phenotype))
  fileName <- file.path(folder, paste0(SqlRender::snakeCaseToCamelCase(gsub("[ -]", "_", phenotype)), ".csv"))
  if (!file.exists(fileName)) {
    conceptSets <- generateKeeperConceptSets(
      condition = phenotype,
      client = client,
      vocabConnectionDetails = vocabConnectionDetails,
      vocabDatabaseSchema = vocabDatabaseSchema
    )
    readr::write_csv(conceptSets, fileName)
  }
}

# Create sensitive cohorts
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

counts <- list()
specConcepts <- list()
for (i in seq_along(phenotypes)) {
  phenotype <- phenotypes[i]
  message(sprintf("*** %s ***", phenotype))
  fileName <- file.path(folder, paste0(SqlRender::snakeCaseToCamelCase(gsub("[ -]", "_", phenotype)), ".csv"))
  conceptSets <- readr::read_csv(fileName, show_col_types = FALSE)
  
  fileName <- file.path(folder, paste0("spec_", SqlRender::snakeCaseToCamelCase(gsub("[ -]", "_", phenotype)), ".rds"))
  if (!file.exists(fileName)) {
    concepts <- createSensitiveCohort(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = cdmDatabaseSchema,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTable,
      cohortDefinitionId = i,
      createCohortTable = (i == 1),
      keeperConceptSets = conceptSets
    )
    saveRDS(concepts, fileName)
  } else {
    concepts <- readRDS(fileName)
  }
  counts[[i]] <- tibble(
    phenotype = phenotype,
    count = attr(concepts, "count"),
    countDoi = attr(concepts, "countDoi"),
    countCombi = attr(concepts, "countCombi")
  )
  specConcepts[[i]] <- concepts |>
    mutate(phenotype = phenotype)
}
counts <- bind_rows(counts)
specConcepts <- bind_rows(specConcepts)
readr::write_csv(counts, file.path(folder, "CohortCounts.csv"))
readr::write_csv(specConcepts, file.path(folder, "SpecConcepts.csv"))
