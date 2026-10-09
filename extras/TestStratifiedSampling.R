# Rerun for all phenotypes ---------------------------------------------------------------------------------------------
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
sensitiveCohortDatabaseSchema <- "scratch.scratch_mschuemi"
sensitiveCohortTable <- "keeper_hsc_acute_liver_failure"
referenceCohortDatabaseSchema <- "scratch.scratch_mschuemi"
referenceCohortTable <- "test_keeper_reference_cohort"
testCohortDatabaseSchema <- "scratch.scratch_mschuemi"
testCohortTable <- "large_scale_phenotyping_cohorts"

options(sqlRenderTempEmulationSchema = "scratch.scratch_mschuemi")
options(andromedaTempFolder = "e:/andromedaTemp")

client <- chat_azure_openai(
  endpoint = gsub("/openai/deployments.*", "", keyring::key_get("genai_o3_endpoint")),
  api_version = "2024-12-01-preview",
  model = "o3",
  credentials = function() keyring::key_get("genai_api_gpt4_key")
)

keeperFolder <- "../largescalephentest/Keeper"
phenotypes <- readLines("../largescalephentest/SelectedPhenotypes.txt")[1:15]
sensCohortRef <- readr::read_csv(file.path(keeperFolder, "HscCohortRef.csv"), show_col_types = FALSE)

sampleSizes <- list()
# i = 1
for (i in 1:length(phenotypes)) {
  phenotype <- phenotypes[i]
  message("Processing ", phenotype)
  phenotypeFolder <- file.path(keeperFolder, gsub("[^[:alnum:]]", "_", phenotype))
  keeperConceptSets <- readr::read_csv(file.path(phenotypeFolder, "KeeperConceptSets.csv"), show_col_types = FALSE)
  llmReviews <- readRDS(file.path(phenotypeFolder, "llmReviews_OptumClinformatics.rds"))
  
  sensitiveCohortDefinitionId <- sensCohortRef |>
    filter(phenotype == !!phenotype) |>
    pull(cohortId)
  
  options(cheatReviews = llmReviews)
  stratifiedReviews <- reviewCasesUsingStratifiedSampling(connectionDetails = connectionDetails,
                                                          tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                                                          cdmDatabaseSchema = cdmDatabaseSchema,
                                                          sensitiveCohortDatabaseSchema = sensitiveCohortDatabaseSchema,
                                                          sensitiveCohortTable = sensitiveCohortTable,
                                                          sensitiveCohortDefinitionId = sensitiveCohortDefinitionId,
                                                          keeperConceptSets = keeperConceptSets,
                                                          settings = createPromptSettings(),
                                                          phenotypeName = phenotype,
                                                          clinicalDefinition = "",
                                                          client = client,
                                                          cacheFolder = file.path(phenotypeFolder, "cache_OptumClinformatics"))
  saveRDS(stratifiedReviews, file.path(phenotypeFolder, "llmReviewsStratifiedSample.rds"))
  sampleSizes[[i]] <- tibble(
    phenotype = phenotype,
    sampleSize = nrow(stratifiedReviews$llmReviews)
  )
  
  # Upload stratified sample
  referenceCohortTableNames <- createReferenceCohortTableNamesUsingStratifiedSample(referenceCohortTable)
  uploadReferenceCohortUsingStratifiedSample(connectionDetails = connectionDetails,
                                             tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                                             sensitiveCohortDatabaseSchema = sensitiveCohortDatabaseSchema,
                                             sensitiveCohortTable = sensitiveCohortTable,
                                             sensitiveCohortDefinitionId = sensitiveCohortDefinitionId,
                                             referenceCohortDatabaseSchema = referenceCohortDatabaseSchema,
                                             referenceCohortTableNames = referenceCohortTableNames,
                                             referenceCohortDefinitionId = i,
                                             createReferenceCohortTables = i == 1,
                                             stratifiedReviews = stratifiedReviews)
}
sampleSizes <- bind_rows(sampleSizes)
readr::write_csv(sampleSizes, file.path(keeperFolder, "EquivalentSampleSizesWhenStratifyingNewHeuristic.csv"))

# Re-evaluate cohorts using stratified sampling:
evaluationFolder <- "../largescalephentest/PhenotypeEvaluation"
oldEvaluation <- readr::read_csv(file.path(evaluationFolder, "PhenotypeEvaluations.csv"), show_col_types = FALSE)
connection <- DatabaseConnector::connect(connectionDetails)
referenceCohortTableNames <- createReferenceCohortTableNamesUsingStratifiedSample(referenceCohortTable)
refCohortRef <- DatabaseConnector::renderTranslateQuerySql(
  connection = connection, 
  sql = "SELECT * FROM @schema.@table",
  schema = referenceCohortDatabaseSchema,
  table = referenceCohortTableNames$referenceCohortMetadataTable,
  snakeCaseToCamelCase = TRUE
)
results <- list()
i = 1
for (i in seq_along(phenotypes)) {
  phenotype <- phenotypes[i]
  message("Processing ", phenotype)
  phenotypeFolder <- file.path(keeperFolder, gsub("[^[:alnum:]]", "_", phenotype))
  cohortRef <- oldEvaluation |>
    filter(phenotype == !!phenotype)
  referenceCohortDefinitionId <- refCohortRef |>
    filter(phenotype == !!phenotype) |>
    pull(cohortDefinitionId)
  for (j in seq_len(nrow(cohortRef))) {
    metrics <- evaluateCohortUsingStratifiedSample(
      connection = connection,
      cohortDatabaseSchema = testCohortDatabaseSchema,
      cohortTable = testCohortTable,
      cohortDefinitionId = cohortRef$cohortId[j],
      referenceCohortDatabaseSchema = referenceCohortDatabaseSchema,
      referenceCohortTableNames = referenceCohortTableNames,
      referenceCohortDefinitionId = referenceCohortDefinitionId
    )
    row <- cohortRef[j, ] |>
      select("phenotype",
             "approach",
             "cohortId",
             "cohortName") |>
      bind_cols(metrics)
    results[[length(results) + 1]] <- row
  }
}
results <- bind_rows(results)
readr::write_csv(results, file.path(evaluationFolder, "PhenotypeEvaluationsUsingStratifiedSampleNewHeuristic.csv"))
disconnect(connection)

# Visualize new metrics --------------------------------------------------------
results <- readr::read_csv(file.path(evaluationFolder, "PhenotypeEvaluationsUsingStratifiedSampleNewHeuristic.csv"))
library(ggplot2)
library(dplyr)

vizData <- results |>
  mutate(phenotype = case_when(phenotype == "Stevens-Johnson syndrome" ~ "Stevens-Johnson\nsyndrome",
                               phenotype == "Major Depressive Disorder" ~ "Major Depressive\nDisorder",
                               phenotype == "Pulmonary arterial hypertension" ~ "Pulmonary arterial\nhypertension",
                               phenotype == "venous thromboembolism" ~ "venous\nthromboembolism",
                               phenotype == "ST-elevation myocardial infarction" ~ "ST-elevation\nmyocardial infarction",
                               TRUE ~ phenotype))

ggplot(vizData, aes(x = sensitivity, y = ppv, color = approach, shape = approach)) +
  geom_point(alpha = 0.7) +
  geom_errorbar(aes(ymax = ppvUb, ymin = ppvLb), alpha = 0.5) +
  geom_errorbarh(aes(xmax = sensitivityUb, xmin = sensitivityLb), alpha = 0.5) +
  scale_x_continuous("Sensitivity", limits = c(0,1)) +
  scale_y_continuous("Positive Predictive Value (PPV)", limits = c(0,1)) +
  facet_wrap(~phenotype, ncol = 5) +
  theme(
    panel.spacing.x = unit(0.3, "cm"),
    legend.title = element_blank()) 
ggsave(file.path(evaluationFolder, "PhenotypeEvaluationsStratifiedSamplingNewHeuristic.png"), width = 9, height = 5)

# Compare to old metrics -----------------------------------------------
library(ggplot2)
library(dplyr)

evaluationFolder <- "../largescalephentest/PhenotypeEvaluation"
results <- readr::read_csv(file.path(evaluationFolder, "PhenotypeEvaluationsUsingStratifiedSampleNewHeuristic.csv"))
oldEvaluation <- readRDS(file.path(evaluationFolder, "Performance.rds"))
vizData <- bind_rows(
  results |>
    select(phenotype, cohortId, cohortName, approach, value = ppv, lb = ppvLb, ub = ppvUb) |>
    mutate(type = "Stratified",
           metric = "PPV"),
  results |>
    select(phenotype, cohortId, cohortName, approach, value = sensitivity, lb = sensitivityLb, ub = sensitivityUb) |>
    mutate(type = "Stratified",
           metric = "Sensitivity"),
  oldEvaluation |>
    select(phenotype, cohortId, cohortName, approach, value = ppv, lb = ppvLb, ub = ppvUb) |>
    mutate(type = "Unstratified",
           metric = "PPV"),
  oldEvaluation |>
    select(phenotype, cohortId, cohortName, approach, value = sensitivity, lb = sensitivityLb, ub = sensitivityUb) |>
    mutate(type = "Unstratified",
           metric = "Sensitivity")
) |>
  arrange(approach, cohortId) |>
  group_by(phenotype, approach, metric, type) |>
  mutate(label = paste(approach, row_number())) |>
  ungroup() |>
  mutate(dy = if_else(type == "Stratified", +0.2, -0.2))
labels <- vizData |>
  distinct(label)
ggplot(vizData, aes(x = value, y = label, color = type)) +
  geom_point(position = position_nudge(y = vizData$dy), shape = 16, alpha = 0.6) +
  geom_errorbarh(aes(xmin = lb, xmax = ub), position = position_nudge(y = vizData$dy), alpha = 0.6) +
  facet_grid(phenotype ~ metric, scales = "free_y", space = "free_y")
ggsave(file.path(evaluationFolder, "EvalStratifiedVsUnstratifiedNewHeuristic.png"), width = 8, height = 49)

