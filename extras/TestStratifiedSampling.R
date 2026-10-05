library(Keeper)
library(dplyr)
library(ellmer)

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "spark",
  connectionString = keyring::key_get("databricksConnectionString"),
  user = "token",
  password = keyring::key_get("databricksToken")
)
cdmDatabaseSchema <- "optum_extended_dod.cdm_optum_extended_dod_v4020"
cohortDatabaseSchema <- "scratch.scratch_mschuemi"
cohortTable <- "test_keeper_sens_cohort"
referenceCohortDatabaseSchema <- "scratch.scratch_mschuemi"
referenceCohortTable <- "test_keeper_reference_cohort"

testCohortDatabaseSchema <- "scratch.scratch_mschuemi"
testCohortTable <- "large_scale_phenotyping_cohorts"
testCohortId <- 26127

options(sqlRenderTempEmulationSchema = "scratch.scratch_mschuemi")
options(andromedaTempFolder = "e:/andromedaTemp")

client <- chat_azure_openai(
  endpoint = gsub("/openai/deployments.*", "", keyring::key_get("genai_o3_endpoint")),
  api_version = "2024-12-01-preview",
  model = "o3",
  credentials = function() keyring::key_get("genai_api_gpt4_key")
)

folder <- "../largescalephentest/Keeper/Bipolar_disorder_I"
# folder <- "../largescalephentest/Keeper/acute_pancreatitis"
# folder <- "../largescalephentest/Keeper/Stevens_Johnson_syndrome"
# folder <- "../largescalephentest/Keeper/venous_thromboembolism"
phenotypeName <- "Bipolar disorder I"
clinicalDefinition <- "Bipolar disorder I is a primary, chronic, episodic psychiatric mood disorder defined by the lifetime occurrence of at least one manic episode â€” a distinct period of abnormally and persistently elevated, expansive, or irritable mood with increased goal-directed activity or energy, lasting at least seven days (or any duration if hospitalization is required), and representing a marked departure from baseline functioning. The manic episode must cause significant impairment in social or occupational functioning or necessitate hospitalization, and may include psychotic features (e.g., grandiose delusions, hallucinations) that are temporally confined to the mood disturbance. Major depressive or hypomanic episodes may occur across the illness course but are not required for diagnosis. The disorder is idiopathic; manic episodes directly attributable to the physiological effects of a substance, medication, or general medical condition are excluded. Schizoaffective disorder, bipolar type â€” in which psychotic symptoms persist independently outside of mood episodes â€” is also excluded from this phenotype."

keeperConceptSets <- readr::read_csv(file.path(folder, "KeeperConceptSets.csv"), show_col_types = FALSE)
llmReviews <- readRDS(file.path(folder, "llmReviewsHsc.rds"))
mean(llmReviews$isCase == "yes")

# Reconstruct HSC to get stratification information --------------------------------------------------------------------
specificConcepts <- createSensitiveCohort(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortTable = cohortTable,
  cohortDefinitionId = 1,
  createCohortTable = TRUE,
  keeperConceptSets = keeperConceptSets
)

# Rerun Keeper on sensitive cohort to extract stratification information -----------------------------------------------
keeper <- generateKeeper(
  connectionDetails = connectionDetails,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortTable = cohortTable,
  cohortDefinitionId = 1,
  sampleSize = 10000,
  personIds = llmReviews$personId,
  phenotypeName = phenotypeName,
  keeperConceptSets = conceptSets,
  removePii = FALSE
)
# Re-use the same generated ID as before (so we can re-use the prompt cache):
newIds <- keeper |>
  filter(category == "personId") |>
  select(personId = "conceptName", "generatedId")
newToOldIds <- newIds |>
  inner_join(llmReviews  |>
               select("personId", oldId = "generatedId"),
             by = join_by("personId"))
keeper <- keeper |>
  inner_join(newToOldIds, by = join_by("generatedId")) |>
  mutate(generatedId = oldId) |>
  select(-"oldId")

keeper |>
  distinct(generatedId) |>
  count()
saveRDS(keeper, file.path(folder, "KeeperWithStratInfo.rds"))

# Run stratified sampling --------------------------------------------
keeper <- readRDS(file.path(folder, "KeeperWithStratInfo.rds"))
stratifiedReviews <- reviewCasesUsingStratifiedSampling(keeper = keeper,
                                                        settings = createPromptSettings(),
                                                        phenotypeName = phenotypeName,
                                                        clinicalDefinition = clinicalDefinition,
                                                        client = client,
                                                        cacheFolder = file.path(folder, "cache"))
saveRDS(stratifiedReviews, file.path(folder, "llmReviewsStratifiedSample.rds"))

# Upload stratified sample ----------------------------------------------------
referenceCohortTableNames <- createReferenceCohortTableNamesUsingStratifiedSample(referenceCohortTable)

uploadReferenceCohortUsingStratifiedSample(connectionDetails = connectionDetails,
                                           tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                                           sensitiveCohortDatabaseSchema = cohortDatabaseSchema,
                                           sensitiveCohortTable = cohortTable,
                                           sensitiveCohortDefinitionId = 1,
                                           referenceCohortDatabaseSchema = referenceCohortDatabaseSchema,
                                           referenceCohortTableNames = referenceCohortTableNames,
                                           referenceCohortDefinitionId = 1,
                                           createReferenceCohortTables = TRUE,
                                           stratifiedReviews = stratifiedReviews)
  
  

  
# Evaluate cohort ---------------------------------------------
evaluateCohortUsingStratifiedSample(
  connectionDetails = connectionDetails,
  cohortDatabaseSchema = testCohortDatabaseSchema,
  cohortTable = testCohortTable,
  cohortDefinitionId = 26210,
  referenceCohortDatabaseSchema = referenceCohortDatabaseSchema,
  referenceCohortTableNames = referenceCohortTableNames,
  referenceCohortDefinitionId = 1
)
  

# Compute prevalence in all HSCs --------------------------------------------
keeperFolder <- "../largescalephentest/Keeper"
phenotypeFolders <- list.dirs(keeperFolder, full.names = TRUE, recursive = FALSE)
phenotypeFolders <- phenotypeFolders[!grepl("Type_B_lactic_acidosis", phenotypeFolders)]
for (phenotypeFolder in phenotypeFolders) {
  llmReviews <- readRDS(file.path(phenotypeFolder, "llmReviewsHsc.rds"))
  message(phenotypeFolder, ": ", mean(llmReviews$isCase == "yes"))
}
  
  