library(Keeper)
library(dplyr)

connectionDetails <- createConnectionDetails(
  dbms = "spark",
  connectionString = keyring::key_get("databricksConnectionString"),
  user = "token",
  password = keyring::key_get("databricksToken")
)
cdmDatabaseSchema <- "optum_extended_dod.cdm_optum_extended_dod_v4020"
cohortDatabaseSchema <- "scratch.scratch_mschuemi"
cohortTable <- "test_keeper_sens_cohort"

options(sqlRenderTempEmulationSchema = "scratch.scratch_mschuemi")
options(andromedaTempFolder = "e:/andromedaTemp")

folder <- "../largescalephentest/Keeper/Bipolar_disorder_I"
# folder <- "../largescalephentest/Keeper/acute_pancreatitis"
# folder <- "../largescalephentest/Keeper/Stevens_Johnson_syndrome"
# folder <- "../largescalephentest/Keeper/venous_thromboembolism"


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
  phenotypeName = "Multiple Myeloma",
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



# Compute prevalence in all HSCs --------------------------------------------
keeperFolder <- "../largescalephentest/Keeper"
phenotypeFolders <- list.dirs(keeperFolder, full.names = TRUE, recursive = FALSE)
phenotypeFolders <- phenotypeFolders[!grepl("Type_B_lactic_acidosis", phenotypeFolders)]
for (phenotypeFolder in phenotypeFolders) {
  llmReviews <- readRDS(file.path(phenotypeFolder, "llmReviewsHsc.rds"))
  message(phenotypeFolder, ": ", mean(llmReviews$isCase == "yes"))
}
