# Copyright 2023 Observational Health Data Sciences and Informatics
#
# This file is part of KEEPER
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

#' Creates a folder with the KEEPER to review person level profiles.
#'
#' @description
#' Export person level data from omop cdm tables for eligible persons in the cohort. Creates a folder with the KEEPER to review person level profiles.
#'
#' @template Connection
#'
#' @template CohortDatabaseSchema
#'
#' @template CdmDatabaseSchema
#'
#' @template VocabularyDatabaseSchema
#'
#' @template CohortTable
#'
#' @template TempEmulationSchema
#'
#' @param cohortDefinitionId          The cohort id to extract records.
#'
#' @param cohortName                  (optional) Cohort Name
#'
#' @param sampleSize                  (Optional, default = 20) The number of persons to randomly sample. Ignored, if personId is given.
#'
#' @param personIds                   (Optional) An array of personId's to look for in Cohort table and CDM.
#'
#' @param exportFolder                The folder where the output will be exported to. If this folder
#'                                    does not exist it will be created.
#' @param databaseId                  A short string for identifying the database (e.g. 'Synpuf'). This will be displayed
#'                                    in shiny app to toggle between databases. Should not have space or underscore (_).
#'
#' @param assignNewId                 (Default = FALSE) Do you want to assign a newId for persons. This will replace the personId in the source with a randomly assigned newId.
#'
#' @param userCovariates              (KEEPER: for future: turn recommendations for KEEPER on and off)
#'
#' @param drugs                       KEEPER: input string for concept_ids for drug exposures relevant to the disease of interest, to be used for prior exposures and treatment after the index date. 
#'                                    You may input drugs that are used to treat disease of interest and drugs used to treat alternative diagnosis
#'
#' @param doi                         KEEPER: input string for concept_ids for disease of interest
#'
#' @param comorbidities               KEEPER: input string for concept_ids for co-morbidities associated with the disease of interest (such as smoking or hypelipidemia for diabetes)
#'
#' @param symptoms                    KEEPER: input string for concept_ids for symptoms associated with the disease of interest (such as weight gain or loss for diabetes)
#'
#' @param diagnosticProcedures        KEEPER: input string for concept_ids for diagnostic procedures relevant to the condition of interest within a month prior and after the index date
#'
#' @param measurements	              KEEPER: input string for concept_ids for lab tests relevant to the disease of interest within a month prior and after the index date
#'
#' @param alternativeDiagnosis        KEEPER: input string for concept_ids for competing diagnosis within a month after the index date
#'
#' @param treatmentProcedures	        KEEPER: input string for concept_ids for treatment procedures relevant to the disease of interest within a month after the index date
#'
#' @param complications               KEEPER: input string for concept_ids for complications of the disease of interest within a year after the index date
#'
#' @param useAncestor                 KEEPER: a switch for using concept_ancestor to retrieve relevant terms vs using verbatim strings of codes
#'
#' @examples
#' \dontrun{
#' connectionDetails <- createConnectionDetails(
#'   dbms = "postgresql",
#'   server = "ohdsi.com",
#'   port = 5432,
#'   user = "me",
#'   password = "secure"
#' )
#'
#' XXX: to do: add windows as parameters, check ranges for measurements, fix NA, fix length of CSV string, add support for cdi as list
#' XXX: consider: adding a * for primary status
#' createKEEPER(
#'   connectionDetails = connectionDetails,
#'   exportFolder = "c:/temp/keeper",
#'   databaseId = "Synpuf",
#'   cdmDatabaseSchema = "dbo",
#'   cohortDatabaseSchema = "results",
#'   cohortTable = "cohort",
#'   cohortDefinitionId = 1234,
#'   cohortName = "DM type I",
#'   sampleSize = 100,
#'   assignNewId = TRUE,
#'   useAncestor = TRUE,
#'   doi = c(201820,442793,443238,4016045,4065354,45757392, 4051114, 433968, 375545, 29555009, 4209145, 4034964, 380834, 4299544, 4226354, 4159742, 43530690, 433736,
#'                     320128, 4170226, 40443308, 441267, 4163735, 192963, 85828009),
#'   symptoms = c(4232487, 4229881),
#'   comorbidities = c(432867, 436670),
#'   drugs = c(1730370, 21604490, 21601682, 21601855, 21601462, 21600280, 21602728, 1366773, 21602689, 21603923, 21603746),
#'   diagnosticProcedures = c(40756884, 4143852, 2746768, 2746766),
#'   measurements	= c(3034962, 3000483, 3034962, 3000483, 3004501, 3033408, 3005131, 3024629, 3031266, 3037110, 3009261, 3022548, 3019210, 3025232, 3033819,
#'                  3000845, 3002666, 3004077, 3026300, 3014737, 3027198, 3025398, 3010300, 3020399, 3007332, 3025673, 3027457, 3010084, 3004410, 3005673),
#'   alternativeDiagnosis = c(201820,442793,443238,4016045,4065354,45757392, 4051114, 433968, 375545, 29555009, 4209145, 4034964, 380834, 4299544, 4226354, 4159742, 43530690, 433736,
#'                          320128, 4170226, 40443308, 441267, 4163735, 192963, 85828009),
#'   treatmentProcedures = c(0),
#'   complications =  c(201820,442793,443238,4016045,4065354,45757392, 4051114, 433968, 375545, 29555009, 4209145, 4034964,
#'                       380834, 4299544, 4226354, 4159742, 43530690, 433736, 320128, 4170226, 40443308, 441267, 4163735, 192963, 85828009)                             
#' )
#' }
#'
# 
#' @export
createKEEPER <- function(connectionDetails = NULL,
                                    connection = NULL,
                                    cohortDatabaseSchema = NULL,
                                    cdmDatabaseSchema,
                                    vocabularyDatabaseSchema = cdmDatabaseSchema,
                                    tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                                    cohortTable = "cohort",
                                    cohortDefinitionId,
                                    cohortName = NULL,
                                    sampleSize = 20,
                                    personIds = NULL,
                                    exportFolder,
                                    databaseId,
                                    assignNewId = FALSE,
                                    #userCovariates = TRUE,
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
    x = vocabularyDatabaseSchema,
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

 # if (is.null(personIds)) {
 #   checkmate::assertIntegerish(
 #     x = sampleSize,
 #     lower = 0,
 #     len = 1,
 #     null.ok = TRUE,
 #     add = errorMessage
 #   )
 # } else {
 #   checkmate::assertIntegerish(
 #     x = personIds,
 #     lower = 0,
 #     min.len = 1,
 #     null.ok = TRUE
 #   )
 # }

# create export folder

  exportFolder <- normalizePath(exportFolder, mustWork = FALSE)

  dir.create(
    path = exportFolder,
    showWarnings = FALSE,
    recursive = TRUE
  )

  checkmate::assertDirectory(
    x = exportFolder,
    access = "x",
    add = errorMessage
  )

  checkmate::reportAssertions(collection = errorMessage)

  ParallelLogger::addDefaultFileLogger(
    fileName = file.path(exportFolder, "log.txt"),
    name = "keeper_file_logger"
  )
  ParallelLogger::addDefaultErrorReportLogger(
    fileName = file.path(exportFolder, "errorReportR.txt"),
    name = "keeper_error_logger"
  )
  on.exit(ParallelLogger::unregisterLogger("keeper_file_logger", silent = TRUE))
  on.exit(
    ParallelLogger::unregisterLogger("keeper_error_logger", silent = TRUE),
    add = TRUE
  )

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
      data = dplyr::tibble(subjectId = as.double(personIds) %>% unique())
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
    sql = "   DROP TABLE IF EXISTS #persons_filter;
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
      cohort_definition_id = cohortDefinitionId,
      snakeCaseToCamelCase = TRUE
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
      cohort_definition_id = cohortDefinitionId,
      snakeCaseToCamelCase = TRUE
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


# KEEPER code
pullDataSql <- SqlRender::readSql(system.file("sql/sql_server/pullData.sql", package = "KEEPER", mustWork = TRUE))

  writeLines("Getting patient data for KEEPER.")
   DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = pullDataSql,
    cdm_database_schema = cdmDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    snakeCaseToCamelCase = TRUE,
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
table_name_list = c('presentation', 'visit_context', 'comorbidities', 'symptoms', 'prior_disease',
'prior_drugs', 'prior_treatment_procedures', 'diagnostic_procedures', 'measurements','alternative_diagnosis',
'after_disease', 'after_drugs', 'after_treatment_procedures', 'death')

for (table_name in table_name_list) {
  assign(table_name, 
  DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = paste("SELECT * FROM #", table_name, ";", sep = ""),
    snakeCaseToCamelCase = TRUE) %>% as_tibble())
}

subjects = presentation%>%
 dplyr::select(personId, newId, age, gender, cohortDefinitionId, cohortStartDate, observationPeriod)%>%
 dplyr::rename(observation_period = observationPeriod)
      
presentation = presentation%>%
  dplyr::group_by(cohortDefinitionId, personId, cohortStartDate) %>% 
  dplyr::summarise(presentation = stringr::str_c(conceptName, collapse = " ")) 

visit_context = visit_context%>%
  dplyr::group_by(cohortDefinitionId, personId, cohortStartDate) %>% 
  dplyr::summarise(visit_context = stringr::str_c(conceptName, collapse = " ")) 

# loop for modifying tables 
subset_name_list = c( 'comorbidities', 'symptoms', 'prior_disease', 
'prior_treatment_procedures', 'diagnostic_procedures', 'alternative_diagnosis',
'after_disease',  'after_treatment_procedures')

for (subset_name in subset_name_list) {
  assign(subset_name, get(subset_name)%>%
    dplyr::group_by(cohortDefinitionId, personId, cohortStartDate, conceptName) %>% 
    dplyr::summarise(dateComb = toString(sort(unique(dateOrder))))%>%
    dplyr::ungroup()%>%
    dplyr::distinct()%>%
    dplyr::mutate(dateName = paste(conceptName, " (day ", dateComb, ")", sep = ""))%>%
    dplyr::group_by(cohortDefinitionId, personId, cohortStartDate) %>% 
    dplyr::summarise(!!(subset_name) := stringr::str_c(dateName, collapse = "; ")))
}

# no aggregation of dates
subset_name_list2 = c('prior_drugs', 'after_drugs', 'measurements', 'death')

for (subset_name in subset_name_list2) {
  assign(subset_name, get(subset_name)%>%
    dplyr::group_by(cohortDefinitionId, personId, cohortStartDate) %>% 
    dplyr::summarise(!!(subset_name) := stringr::str_c(conceptName, collapse = " ")))
}


# creating a joint dataframe
# keeping cohort_definition_id to support lists in future
  writeLines("Writing KEEPER file.")
  KEEPER = subjects%>%
  dplyr::left_join(presentation, by = c("personId", "cohortStartDate", "cohortDefinitionId"))%>%
  dplyr::left_join(visit_context, by = c("personId", "cohortStartDate", "cohortDefinitionId"))%>%
  dplyr::left_join(comorbidities, by = c("personId", "cohortStartDate", "cohortDefinitionId"))%>%
  dplyr::left_join(symptoms, by = c("personId", "cohortStartDate", "cohortDefinitionId"))%>%
  dplyr::left_join(prior_disease, by = c("personId", "cohortStartDate", "cohortDefinitionId"))%>%
  dplyr::left_join(prior_drugs, by = c("personId", "cohortStartDate", "cohortDefinitionId"))%>%
  dplyr::left_join(prior_treatment_procedures, by = c("personId", "cohortStartDate", "cohortDefinitionId"))%>%
  dplyr::left_join(diagnostic_procedures, by = c("personId", "cohortStartDate", "cohortDefinitionId"))%>%
  dplyr::left_join(measurements, by = c("personId", "cohortStartDate", "cohortDefinitionId"))%>%
  dplyr::left_join(alternative_diagnosis, by = c("personId", "cohortStartDate", "cohortDefinitionId"))%>%
  dplyr::left_join(after_disease, by = c("personId", "cohortStartDate", "cohortDefinitionId"))%>%
  dplyr::left_join(after_drugs, by = c("personId", "cohortStartDate", "cohortDefinitionId"))%>%
  dplyr::left_join(after_treatment_procedures, by = c("personId", "cohortStartDate", "cohortDefinitionId"))%>%
  dplyr::left_join(death, by = c("personId", "cohortStartDate", "cohortDefinitionId"))%>%
  dplyr::select(personId, newId, age, gender, observation_period, visit_context, presentation, comorbidities, symptoms, prior_disease, prior_drugs, prior_treatment_procedures,
                diagnostic_procedures, measurements, alternative_diagnosis, after_disease, after_treatment_procedures, after_drugs, death)%>%
  dplyr::distinct()
  # add columns for review
  #tibble::add_column(reviewer = NA, status = NA, index_misspecification = NA, notes = NA)

  KEEPER <- replaceId(data = KEEPER, useNewId = assignNewId)
  
  KEEPER%>%
  replace(is.na(KEEPER), "") %>%

  write.csv(paste0("KEEPER_cohort_", databaseId, "_", cohortDefinitionId,".csv"), row.names=F)


}
