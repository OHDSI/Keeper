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




generateKeeperConceptSets(
  condition = "Type I Diabetes Mellitus (T1DM)",
  client = client,
  vocabConnectionDetails = vocabConnectionDetails,
  vocabDatabaseSchema = vocabDatabaseSchema
)
