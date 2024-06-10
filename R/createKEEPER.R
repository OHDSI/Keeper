# Copyright 2024 Observational Health Data Sciences and Informatics
#
# This file is part of Keeper
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Export person level data from OMOP CDM tables for eligible persons in the cohort.
#' 
#' @description
#' Use `useAncestor = TRUE` to switch from verbatim string of concept_ids vs ancestors. In latter 
#' case, the app will take you concept_ids and include them along with their descendants.
#'
#' Use `sampleSize` to specify desired number of patients to be selected.
#'
#' Use `assignNewId = TRUE` to replace person_id with a new sequence.
#' 
#' Explanation of categories:
#' - instantiated cohort with patients of interest in COHORT table or in another table that has the same fields as COHORT;
#' - doi: string for disease of interest (ex.: diabetes type I). Hereon, assume a string of concept_ids;
#' - symptoms: symptoms of disease of interest or alternative/competing diagnoses (those that you want to see to be able to distinguish your doi from another close disease, ex.: polyuria, weight gain or loss, vision disturbances);
#' - comorbidities: relevant diseases that co-occur with doi or alternative/competing diagnoses (ex.: obesity, metabolic syndrome, pancreatic disorders, pregnancy);
#' - drugs: drugs, relevant to the disease of interest or those that can be used to treat alternative/competing diagnoses (ex.: insulin, oral glucose lowering drugs);
#' - diagnosticProcedures: relevant diagnostic procedures (ex.: ultrasound of pancreas);
#' - measurements: relevant lab tests (ex.: islet cell ab, HbA1C, glucose measurement in blood, insulin ab);
#' - alternativeDiagnosis: alternative/competing diagnoses (ex.: diabetes type 2, cystic fibrosis, gestational diabetes, renal failure, pancreonecrosis)
#' - treatmentProcedures: relevant treatment procedures (ex.: operative procedures on pancreas);
#' - complications: relevant complications (ex.: retinopathy, CKD).
#' 
#' *note: if no suitable concept_ids exists for an input string, input c(0)
#'
#'
#' @template Connection
#'
#' @template CohortTable
#' 
#' @template CdmDatabaseSchema
#'
#' @template TempEmulationSchema
#'
#' @param cohortDefinitionId          The cohort id to extract records.
#'
#' @param cohortName                  (optional) Cohort Name
#'
#' @param sampleSize                  (Optional, default = 20) The number of persons to randomly sample. Ignored, if personId is given.
#'
#' @param personIds                   (Optional) A vector of personId's to look for in Cohort table and CDM.
#'
#' @param databaseId                  A short string for identifying the database (e.g. 'Synpuf'). This will be displayed
#'                                    in shiny app to toggle between databases. Should not have space or underscore (_).
#'
#' @param assignNewId                 (Default = FALSE) Do you want to assign a newId for persons. This will replace the personId in the source with a randomly assigned newId.
#'
#' @param drugs                       keeperOutput: input vector of concept_ids for drug exposures relevant to the disease of interest, to be used for prior exposures and treatment after the index date. 
#'                                    You may input drugs that are used to treat disease of interest and drugs used to treat alternative diagnosis
#'
#' @param doi                         keeperOutput: input vector of concept_ids for disease of interest
#'
#' @param comorbidities               keeperOutput: input vector of concept_ids for comorbidities associated with the disease of interest (such as smoking or hyperlipidemia for diabetes)
#'
#' @param symptoms                    keeperOutput: input vector of concept_ids for symptoms associated with the disease of interest (such as weight gain or loss for diabetes)
#'
#' @param diagnosticProcedures        keeperOutput: input vector of concept_ids for diagnostic procedures relevant to the condition of interest within a month prior and after the index date
#'
#' @param measurements	              keeperOutput: input vector of concept_ids for lab tests relevant to the disease of interest within a month prior and after the index date
#'
#' @param alternativeDiagnosis        keeperOutput: input vector of concept_ids for competing diagnosis within a month after the index date
#'
#' @param treatmentProcedures	        keeperOutput: input vector of concept_ids for treatment procedures relevant to the disease of interest within a month after the index date
#'
#' @param complications               keeperOutput: input vector of concept_ids for complications of the disease of interest within a year after the index date
#'
#' @param useAncestor                 keeperOutput: a switch for using concept_ancestor to retrieve relevant terms vs using verbatim strings of codes
#'
#' @examples
#' \dontrun{
#' connectionDetails <- createConnectionDetails(
#'   dbms = 'postgresql',
#'   server = 'ohdsi.com',
#'   port = 5432,
#'   user = 'me',
#'   password = 'secure'
#' )
#'
#'keeper <- createKeeper(
#'  connectionDetails = connectionDetails,
#'  databaseId = "Synpuf",
#'  cdmDatabaseSchema = "dbo",
#'  cohortDatabaseSchema = "results",
#'  cohortTable = "cohort",
#'  cohortDefinitionId = 1234,
#'  cohortName = "DM type I",
#'  sampleSize = 100,
#'  assignNewId = TRUE,
#'  useAncestor = TRUE,
#'  doi = c(435216, 201254),
#'  symptoms = c(79936, 432454, 4232487, 4229881, 254761),
#'  comorbidities = c(141253, 432867, 436670, 433736, 255848),
#'  drugs = c(21600712, 21602728, 21603531),
#'  diagnosticProcedures = c(0),
#'  measurements	= c(3004410,3005131,3005673,3010084,3033819,4149519,4229110),
#'  alternativeDiagnosis = c(192963,201826,441267,40443308),
#'  treatmentProcedures = c(4242748),
#'  complications =  c(201820,375545,380834,433968,442793,4016045,4209145,4299544)                             
#' )
#' }
#' @return 
#' Output is a data frame with one row per patient, with the following information per patient:
#' 
#' - demographics (age, gender);
#' - visit_context: information about visits overlapping with the index date (day 0) formatted as the type of visit and its duration;
#' - observation_period: information about overlapping OBSERVATION_PERIOD formatted as days prior - days after the index date;
#' - presentation: all records in CONDITION_OCCURRENCE on day 0 with corresponding type and status;
#' - comorbidities: records in CONDITION_ERA and OBSERVATION that were selected as comorbidities and risk factors within all time prior excluding day 0. The list does not inlcude symptoms, disease of interest and complications;
#' - symptoms: records in CONDITION_ERA that were selected as symptoms 30 days prior excluding day 0. The list does not include disease of interest and complications. If you want to see symptoms outside of this window, please place them in complications;
#' - prior_disease: records in CONDITION_ERA that were selected as disease of interest or complications all time prior excluding day 0;
#' - prior_drugs: records in DRUG_ERA that were selected as drugs of interest all time prior excluding day 0 formatted as day of era start and length of drug era;
#' - prior_treatment_procedures: records in PROCEDURE_OCCURRENCE that were selected as treatments of interest within all time prior excluding day 0;
#' - diagnostic_procedures: records in PROCEDURE_OCCURRENCE that were selected as diagnostic procedures within all time prior excluding day 0;
#' - measurements: records in MEASUREMENT that were selected as measurements (lab tests) of interest within 30 days before and 30 days after day 0 formatted as value and unit (if exists) and assessment compared to the reference range provided in MEASUREMENT table (normal, abnormal high and abnormal low);
#' - alternative_diagnosis: records in CONDITION_ERA that were selected as alternative (competing) diagnosis within 90 days before and 90 days after day 0. The list does not include disease of interest;
#' - after_disease: same as prior_disease but after day 0;
#' - after_drugs: same as prior_drugs but after day 0;
#' - after_treatment_procedures: same as prior_treatment_procedures but after day 0;
#' - death: death record any time after day 0.
#' 
#' @export
createKeeper <- function(connectionDetails = NULL,
                         connection = NULL,
                         cohortDatabaseSchema = NULL,
                         cdmDatabaseSchema,
                         tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                         cohortTable = "cohort",
                         cohortDefinitionId,
                         cohortName = NULL,
                         sampleSize = 20,
                         personIds = NULL,
                         databaseId,
                         assignNewId = FALSE,
                         useAncestor = TRUE,
                         doi, 
                         comorbidities,
                         symptoms,
                         alternativeDiagnosis,
                         drugs,
                         diagnosticProcedures,
                         measurements,
                         treatmentProcedures,
                         complications
) {
  errorMessage <- checkmate::makeAssertCollection()
  
  # checking parameters
  
  checkmate::reportAssertions(collection = errorMessage)
  
  checkmate::assertCharacter(
    x = cohortDatabaseSchema,
    min.len = 0,
    max.len = 1,
    null.ok = TRUE,
    add = errorMessage
  )
  
  checkmate::assertCharacter(
    x = cdmDatabaseSchema,
    min.len = 1,
    add = errorMessage
  )
  
  checkmate::assertCharacter(
    x = cohortTable,
    min.len = 1,
    add = errorMessage
  )
  
  checkmate::assertCharacter(
    x = databaseId,
    min.len = 1,
    max.len = 1,
    add = errorMessage
  )
  
  checkmate::assertCharacter(
    x = tempEmulationSchema,
    min.len = 1,
    null.ok = TRUE,
    add = errorMessage
  )
  
  checkmate::assertIntegerish(
    x = cohortDefinitionId,
    lower = 0,
    len = 1,
    add = errorMessage
  )
  
  checkmate::assertIntegerish(
    x = sampleSize,
    lower = 0,
    len = 1,
    null.ok = TRUE,
    add = errorMessage
  )
 
  checkmate::reportAssertions(collection = errorMessage)
  
  originalDatabaseId <- databaseId
  
  cohortTableIsTemp <- FALSE
  if (is.null(cohortDatabaseSchema)) {
    if (grepl(
      pattern = "#",
      x = cohortTable,
      fixed = TRUE
    )) {
      cohortTableIsTemp <- TRUE
    } else {
      stop("cohortDatabaseSchema is NULL, but cohortTable is not temporary.")
    }
  }
  
  databaseId <- as.character(gsub(
    pattern = " ",
    replacement = "",
    x = databaseId
  ))
  
  if (nchar(databaseId) < nchar(originalDatabaseId)) {
    stop(paste0(
      "databaseId should not have space or underscore: ",
      originalDatabaseId
    ))
  }
  
  # Set up connection to server ----------------------------------------------------
  
  
  if (is.null(connection)) {
    if (!is.null(connectionDetails)) {
      connection <- DatabaseConnector::connect(connectionDetails)
      on.exit(DatabaseConnector::disconnect(connection))
    } else {
      stop("No connection or connectionDetails provided.")
    }
  }
  
  if (cohortTableIsTemp) {
    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = " DROP TABLE IF EXISTS #person_id_data;
                SELECT DISTINCT subject_id
                INTO #person_id_data
                FROM @cohort_table
                WHERE cohort_definition_id = @cohort_definition_id;",
      cohort_table = cohortTable,
      tempEmulationSchema = tempEmulationSchema,
      cohort_definition_id = cohortDefinitionId
    )
  } else { 
    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = " DROP TABLE IF EXISTS #person_id_data;
                  SELECT DISTINCT subject_id, cohort_start_date
                  INTO #person_id_data
                  FROM @cohort_database_schema.@cohort_table
                  WHERE cohort_definition_id = @cohort_definition_id;",
      cohort_table = cohortTable,
      cohort_database_schema = cohortDatabaseSchema,
      tempEmulationSchema = tempEmulationSchema,
      cohort_definition_id = cohortDefinitionId
    )
  }
  
  if (!is.null(personIds)) {
    DatabaseConnector::insertTable(
      connection = connection,
      tableName = "#persons_to_filter",
      createTable = TRUE,
      dropTableIfExists = TRUE,
      tempTable = TRUE,
      tempEmulationSchema = tempEmulationSchema,
      progressBar = TRUE,
      bulkLoad = (Sys.getenv("bulkLoad") == TRUE),
      camelCaseToSnakeCase = TRUE,
      data = tibble(subjectId = as.double(personIds) %>% unique())
    )
    
    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = "     DROP TABLE IF EXISTS #person_id_data2;
                  SELECT DISTINCT a.subject_id
                  INTO #person_id_data2
                  FROM #person_id_data a
                  INNER JOIN #persons_to_filter b
                  ON a.subject_id = b.subject_id;

                  DROP TABLE IF EXISTS #persons_filter;
                  SELECT new_id, subject_id as person_id
                  INTO #persons_filter
                  FROM
                  (
                  SELECT ROW_NUMBER() OVER (ORDER BY NEWID()) AS new_id, subject_id
                  FROM #person_id_data2
                  ) f;",
      tempEmulationSchema = tempEmulationSchema
    )
  } else {
    # assign new id and filter to sample size
    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = "DROP TABLE IF EXISTS #persons_filter;
              SELECT new_id, subject_id as person_id
              INTO #persons_filter
              FROM
              (
                SELECT *
                FROM
                (
                  SELECT ROW_NUMBER() OVER (ORDER BY NEWID()) AS new_id, subject_id
                  FROM #person_id_data
                ) f
    ) t
    WHERE new_id <= @sample_size;",
      tempEmulationSchema = tempEmulationSchema,
      sample_size = sampleSize
    )
  }
  
  if (cohortTableIsTemp) {
    writeLines("Getting cohort table.")
    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = " DROP TABLE IF EXISTS #pts_cohort;

              SELECT c.subject_id, p.new_id, c.cohort_start_date, c.cohort_end_date, c.cohort_definition_id
              INTO #pts_cohort
              FROM @cohort_table c
              INNER JOIN #persons_filter p
              ON c.subject_id = p.person_id
              WHERE c.cohort_definition_id = @cohort_definition_id
              ORDER BY c.subject_id, c.cohort_start_date;",
      cohort_table = cohortTable,
      tempEmulationSchema = tempEmulationSchema,
      cohort_definition_id = cohortDefinitionId
    ) 
  } else {
    writeLines("Getting cohort table.")
    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = " DROP TABLE IF EXISTS #pts_cohort;

              SELECT c.subject_id, p.new_id, cohort_start_date, cohort_end_date, c.cohort_definition_id
              INTO #pts_cohort
              FROM @cohort_database_schema.@cohort_table c
              INNER JOIN #persons_filter p
              ON c.subject_id = p.person_id
              WHERE cohort_definition_id = @cohort_definition_id
          ORDER BY c.subject_id, cohort_start_date;",
      cohort_database_schema = cohortDatabaseSchema,
      cohort_table = cohortTable,
      tempEmulationSchema = tempEmulationSchema,
      cohort_definition_id = cohortDefinitionId
    )
  }
  
  # check if patients exist
  cohort <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT count(*) FROM #pts_cohort;")
  
  if (nrow(cohort) == 0) {
    warning("Cohort does not have the selected subject ids.")
    return(NULL)
  }
  
  
  # keeperOutput code
  pullDataSql <- SqlRender::readSql(system.file("sql/sql_server/pullData.sql", package = "Keeper", mustWork = TRUE))
  
  writeLines("Getting patient data for keeperOutput.")
  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = pullDataSql,
    cdm_database_schema = cdmDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    use_ancestor = useAncestor,
    doi = doi,
    symptoms = symptoms,
    comorbidities = comorbidities,
    alternative_diagnosis = alternativeDiagnosis,
    complications = complications,
    diagnostic_procedures = diagnosticProcedures,
    treatment_procedures = treatmentProcedures,
    measurements = measurements,
    drugs = drugs
  ) 
  
  # loop for instantiating tables in R
  table_name_list = c("presentation", "visit_context", "comorbidities", "symptoms", "prior_disease",
                      "prior_drugs", "prior_treatment_procedures", "diagnostic_procedures", "measurements","alternative_diagnosis",
                      "after_disease", "after_drugs", "after_treatment_procedures", "death")
  tables <- list()
  for (table_name in table_name_list) {
    tables[[table_name]] <- DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = paste("SELECT * FROM #", table_name, ";", sep = ""),
      snakeCaseToCamelCase = TRUE) %>% 
      as_tibble()
  }
  
  subjects <- tables[["presentation"]] %>%
    select("personId", "newId", "age", "gender", "cohortDefinitionId", "cohortStartDate", "observationPeriod")
  
  tables[["presentation"]] <- tables[["presentation"]] %>%
    group_by(.data$cohortDefinitionId, .data$personId, .data$cohortStartDate) %>% 
    summarise(presentation = paste(.data$conceptName, collapse = " ")) 
  
  tables[["visit_context"]] <- tables[["visit_context"]] %>%
    group_by(.data$cohortDefinitionId, .data$personId, .data$cohortStartDate) %>% 
    summarise(visitContext = paste(.data$conceptName, collapse = " ")) 
  
  # loop for modifying tables 
  subset_name_list = c("comorbidities", "symptoms", "prior_disease", 
                       "prior_treatment_procedures", "diagnostic_procedures", "alternative_diagnosis",
                       "after_disease",  "after_treatment_procedures")
  
  for (subset_name in subset_name_list) {
    tables[[subset_name]] <- tables[[subset_name]] %>%
      group_by(.data$cohortDefinitionId, .data$personId, .data$cohortStartDate, .data$conceptName) %>% 
      summarise(dateComb = toString(sort(unique(.data$dateOrder))))%>%
      ungroup()%>%
      distinct()%>%
      mutate(dateName = paste(.data$conceptName, " (day ", .data$dateComb, ")", sep = ""))%>%
      group_by(.data$cohortDefinitionId, .data$personId, .data$cohortStartDate) %>% 
      summarise(!!(SqlRender::snakeCaseToCamelCase(subset_name)) := paste(.data$dateName, collapse = "; "))
  }
  
  # no aggregation of dates
  subset_name_list2 <- c("prior_drugs", "after_drugs", "measurements", "death")
  
  for (subset_name in subset_name_list2) {
    tables[[subset_name]] <- tables[[subset_name]] %>%
             group_by(.data$cohortDefinitionId, .data$personId, .data$cohortStartDate) %>% 
             summarise(!!(SqlRender::snakeCaseToCamelCase(subset_name)) := paste(.data$conceptName, collapse = " "))
  }
  
  
  # creating a joint dataframe
  # keeping cohort_definition_id to support lists in future
  keeperOutput <- subjects
  for (table_name in table_name_list) {
    keeperOutput <- keeperOutput %>%
      left_join(tables[[table_name]], by = c("personId", "cohortStartDate", "cohortDefinitionId"))
  }
  keeperOutput <- keeperOutput %>%
    select("personId", "newId", "age", "gender", "observationPeriod", "visitContext", "presentation", "comorbidities", "symptoms", "priorDisease", "priorDrugs", "priorTreatmentProcedures",
           "diagnosticProcedures", "measurements", "alternativeDiagnosis", "afterDisease", "afterTreatmentProcedures", "afterDrugs", "death")%>%
    distinct()
  # add columns for review
  #tibble::add_column(reviewer = NA, status = NA, index_misspecification = NA, notes = NA)
  
  keeperOutput <- replaceId(data = keeperOutput, useNewId = assignNewId)
  
  keeperOutput %>%
    replace(is.na(keeperOutput), "") %>%
    return()
}
