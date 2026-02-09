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

conceptSetsFileName <- "e:/KeeperSensitiveCohort/afConceptSets.csv"
specConceptsFileName <- "e:/KeeperSensitiveCohort/afSpecConcepts.csv"
keeperFileName <- "e:/KeeperSensitiveCohort/afKeeper10K.rds"

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
adjudications <- reviewCases(keeper = keeper,
                             client = client,
                             cacheFolder = cacheFolder)
saveRDS(adjudications, "e:/KeeperSensitiveCohort/adjudications.rds")

# Analyse LLM adjudication -----------------------------------------------
adjudications <- readRDS("e:/KeeperSensitiveCohort/adjudications.rds")
keeper <- readRDS(keeperFileName)
conceptSets <- readr::read_csv(conceptSetsFileName, show_col_types = FALSE)

adjudications |>
  group_by(isCase) |>
  count()


doiConceptIds <- conceptSets |>
  filter(conceptSetName == "doi") |>
  pull(conceptId)
hasDoiGeneratedId <- keeper |>
  filter(conceptId %in% doiConceptIds) |>
  distinct(generatedId) |>
  pull()

adjudications |>
  mutate(hasDoi = generatedId %in% hasDoiGeneratedId) |>
  group_by(isCase, hasDoi) |>
  count()

# Sample 
sample <- bind_rows(
  adjudications |>
    filter(isCase == "yes") |>
    slice_sample(n = 50),
  adjudications |>
    filter(isCase == "no") |>
    slice_sample(n = 150),
  adjudications |>
    filter(isCase == "insufficient information") |>
    slice_sample(n = 20),
) |>
  sample_frac()

keeperSample <- keeper |>
  filter(generatedId %in% sample$generatedId)
  
saveRDS(keeperSample, "e:/KeeperSensitiveCohort/KeeperSample.rds")
  

# Push to database server for adjudication by humans -----------------------
library(DatabaseConnector)
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "postgresql",
  server = Sys.getenv("KEEPER_SERVER"),
  user = Sys.getenv("KEEPER_USER"),
  password = Sys.getenv("KEEPER_PASSWORD")
)
keeperDatabaseSchema <- Sys.getenv("KEEPER_DATABASE_SCHEMA")

connection <- connect(connectionDetails)
# renderTranslateExecuteSql(connection, "CREATE SCHEMA @keeper_database_schema;", keeper_database_schema = keeperDatabaseSchema)

keeperSample <- readRDS("e:/KeeperSensitiveCohort/KeeperSample.rds")
keeperSample <- keeperSample |>
  inner_join(keeperSample |>
               filter(category == "cdmSourceAbbreviation") |>
               select(generatedId, databaseId = conceptName),
             by = join_by(generatedId)) |>
  inner_join(keeperSample |>
               filter(category == "phenotype") |>
               select(generatedId, phenotype = conceptName),
             by = join_by(generatedId))

insertTable(
  connection = connection,
  data = keeperSample,
  databaseSchema = keeperDatabaseSchema,
  tableName = "keeper",
  camelCaseToSnakeCase = TRUE
)

adjudications <- keeperSample |>
  distinct(databaseId, phenotype, generatedId) |>
  mutate(decision = as.character(NA),
         indexDay = 0,
         adjudicator = "MSCHUEMI")

insertTable(
  connection = connection,
  data = adjudications,
  databaseSchema = keeperDatabaseSchema,
  tableName = "adjudications",
  camelCaseToSnakeCase = TRUE
)

disconnect(connection)
