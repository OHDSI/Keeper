library(Keeper)
library(dplyr)
library(ellmer)
library(DatabaseConnector)

client <- chat_openai_compatible(
  base_url = "http://localhost:1234/v1",
  credentials = function() "lm-studio",
  model = "nvidia/nemotron-3-nano"
)
conceptBatchSize <- 20

vocabConnectionDetails <- createConnectionDetails(
  dbms = "postgresql",
  server = Sys.getenv("LOCAL_POSTGRES_SERVER"),
  user = Sys.getenv("LOCAL_POSTGRES_USER"),
  password = Sys.getenv("LOCAL_POSTGRES_PASSWORD")
)
vocabDatabaseSchema <- Sys.getenv("LOCAL_POSTGRES_VOCAB_SCHEMA")


generateKeeperConceptSets(
  condition = "Type I Diabetes Mellitus (T1DM)",
  client = client,
  vocabConnectionDetails = vocabConnectionDetails,
  vocabDatabaseSchema = vocabDatabaseSchema
)
