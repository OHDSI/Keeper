library(Keeper)
library(dplyr)


connectionDetails <- createConnectionDetails(
  dbms = "spark",
  connectionString = keyring::key_get("databricksConnectionString"),
  user = "token",
  password = keyring::key_get("databricksToken")
)
cdmDatabaseSchema <- "merative_ccae.cdm_merative_ccae_v3789"
options(sqlRenderTempEmulationSchema = "scratch.scratch_mschuemi")

conceptSetsFileName <- "e:/temp/mmConceptSets.csv"
outputFileName <- "e:/temp/mmRatios.csv"

conceptSetsFileName <- "e:/temp/cdConceptSets.csv"
outputFileName <- "e:/temp/cdRatios.csv"

conceptSetsFileName <- "extras/t1dmConceptSets.csv"
outputFileName <- "e:/temp/t1dmRatios.csv"


# Execute heuristic to find non-specific concepts -----------------------------------------
connection <- DatabaseConnector::connect(connectionDetails)

conceptSets <- readr::read_csv(conceptSetsFileName, show_col_types = FALSE) |>
  filter(target == "Disease of interest")

DatabaseConnector::insertTable(
  connection = connection,
  tableName = "#concept_sets",
  data = conceptSets,
  dropTableIfExists = TRUE,
  createTable = TRUE,
  tempTable = TRUE,
  camelCaseToSnakeCase = TRUE
)

sql <- "
SELECT COUNT(DISTINCT(person_id)) AS person_count
FROM @cdm_database_schema.condition_occurrence
INNER JOIN @cdm_database_schema.concept_ancestor
  ON condition_concept_id = descendant_concept_id
WHERE ancestor_concept_id IN (
  SELECT concept_id
  FROM #concept_sets
  WHERE concept_set_name = 'doi'
);
"
dxCount <- DatabaseConnector::renderTranslateQuerySql(connection, sql, cdm_database_schema = cdmDatabaseSchema)[1,1]

sql <- "
SELECT concept_id,
  concept_name,
  vocabulary_id,
  concept_set_name,
  COUNT(DISTINCT(person_id)) AS person_count
FROM @cdm_database_schema.drug_exposure
INNER JOIN @cdm_database_schema.concept_ancestor
  ON drug_concept_id = descendant_concept_id
INNER JOIN #concept_sets
  ON ancestor_concept_id = concept_id
WHERE concept_set_name IN ('drugs')
GROUP BY concept_id,
  concept_name,
  vocabulary_id,
  concept_set_name

UNION ALL

SELECT concept_id,
  concept_name,
  vocabulary_id,
  concept_set_name,
  COUNT(DISTINCT(person_id)) AS person_count
FROM @cdm_database_schema.condition_occurrence
INNER JOIN @cdm_database_schema.concept_ancestor
  ON condition_concept_id = descendant_concept_id
INNER JOIN #concept_sets
  ON ancestor_concept_id = concept_id
WHERE concept_set_name IN ('symptoms', 'comorbidities', 'complications')
GROUP BY concept_id,
  concept_name,
  vocabulary_id,
  concept_set_name
  
UNION ALL

SELECT concept_id,
  concept_name,
  vocabulary_id,
  concept_set_name,
  COUNT(DISTINCT(person_id)) AS person_count
FROM @cdm_database_schema.observation
INNER JOIN @cdm_database_schema.concept_ancestor
  ON observation_concept_id = descendant_concept_id
INNER JOIN #concept_sets
  ON ancestor_concept_id = concept_id
WHERE concept_set_name IN ('symptoms', 'comorbidities')
GROUP BY concept_id,
  concept_name,
  vocabulary_id,
  concept_set_name
  
UNION ALL

SELECT concept_id,
  concept_name,
  vocabulary_id,
  concept_set_name,
  COUNT(DISTINCT(person_id)) AS person_count
FROM @cdm_database_schema.procedure_occurrence
INNER JOIN @cdm_database_schema.concept_ancestor
  ON procedure_concept_id = descendant_concept_id
INNER JOIN #concept_sets
  ON ancestor_concept_id = concept_id
WHERE concept_set_name IN ('diagnosticProcedures', 'treatmentProcedures')
GROUP BY concept_id,
  concept_name,
  vocabulary_id,
  concept_set_name
  
UNION ALL

SELECT concept_id,
  concept_name,
  vocabulary_id,
  concept_set_name,
  COUNT(DISTINCT(person_id)) AS person_count
FROM @cdm_database_schema.measurement
INNER JOIN @cdm_database_schema.concept_ancestor
  ON measurement_concept_id = descendant_concept_id
INNER JOIN #concept_sets
  ON ancestor_concept_id = concept_id
WHERE concept_set_name IN ('measurements')
GROUP BY concept_id,
  concept_name,
  vocabulary_id,
  concept_set_name;
"
concepts <- DatabaseConnector::renderTranslateQuerySql(connection, sql, cdm_database_schema = cdmDatabaseSchema, snakeCaseToCamelCase = TRUE)

sql <- "DROP TABLE #concept_sets;"
DatabaseConnector::renderTranslateExecuteSql(connection, sql)

concepts <- concepts |>
  group_by(.data$conceptId, .data$conceptName, .data$vocabularyId, .data$conceptSetName) |>
  summarise(personCount = sum(.data$personCount)) |>
  mutate(ratio = personCount / dxCount) |>
  arrange(desc(ratio))
readr::write_csv(concepts, outputFileName)

DatabaseConnector::disconnect(connection)

