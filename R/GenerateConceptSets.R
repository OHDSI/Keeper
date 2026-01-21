# Copyright 2026 Observational Health Data Sciences and Informatics
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

extractAndParseJson <- function(response) {
  jsonPart <- sub('.*?(\\{.*\\}).*', '\\1', gsub(".*</think>", "", response))
  parsed <- jsonlite::fromJSON(jsonPart)
  return(parsed)
}

vectorSearch <- function(term, domains, limit = 10) {
  params <- list(
    q = term,
    domain_id = paste(domains, collapse = ","),
    limit = limit
  )
  url <- "https://hecate.pantheon-hds.com/api/search_standard"
  response <- httr::GET(url, query = params)
  
  if (httr::status_code(response) == 200) {
    content_text <- httr::content(response, "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(content_text)
    data <- bind_rows(data$concepts) |>
      SqlRender::snakeCaseToCamelCaseNames()
    return(data)
  } else {
    stop(sprintf("Error in vector search for term '%s': %s", 
                 term,
                 httr::status_code(response)))
  }
}

phoebeSearch <- function(conceptId) {
  url <- sprintf("https://hecate.pantheon-hds.com/api/concepts/%d/phoebe", conceptId)
  response <- httr::GET(url)
  
  if (httr::status_code(response) == 200) {
    contextText <- httr::content(response, "text", encoding = "UTF-8")
    if (contextText == "[]") {
      return(NULL)
    }
    data <- jsonlite::fromJSON(contextText)
    data <- data |>
      SqlRender::snakeCaseToCamelCaseNames()
    return(data)
  } else {
    stop(sprintf("Error in phoebe search for concept %s: %s", 
                 conceptId,
                 httr::status_code(response)))
  }
}

removeChildren <- function(concepts, connection, vocabDatabaseSchema) {
  sql <- "
SELECT DISTINCT descendant_concept_id AS concept_id
FROM @database_schema.concept_ancestor
WHERE ancestor_concept_id IN (@concept_ids)
  AND descendant_concept_id IN (@concept_ids)
  AND ancestor_concept_id != descendant_concept_id;
"
  childrenConceptIds <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    database_schema = vocabDatabaseSchema,
    concept_ids = concepts$conceptId,
    snakeCaseToCamelCase = TRUE
  )$conceptId
  parentConcepts <- concepts |>
    filter(!.data$conceptId %in% childrenConceptIds)
  return(parentConcepts)
}

addNonChildren <- function(concepts, newConcepts, connection, vocabDatabaseSchema) {
  sql <- "
SELECT DISTINCT descendant_concept_id AS concept_id
FROM @database_schema.concept_ancestor
WHERE ancestor_concept_id IN (@concept_ids)
  AND descendant_concept_id IN (@new_concept_ids);
"
  childrenConceptIds <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    database_schema = vocabDatabaseSchema,
    concept_ids = concepts$conceptId,
    new_concept_ids = newConcepts$conceptId,
    snakeCaseToCamelCase = TRUE
  )$conceptId
  nonChildren <- newConcepts |>
    filter(!.data$conceptId %in% childrenConceptIds)
  concepts <- bind_rows(concepts, nonChildren)
  return(concepts)
}

removeNonStandard <- function(concepts, connection, vocabDatabaseSchema) {
  sql <- "
SELECT concept_id
FROM @database_schema.concept
WHERE concept_id IN (@concept_ids)
  AND standard_concept = 'S';
"
  standardConceptIds <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    database_schema = vocabDatabaseSchema,
    concept_ids = concepts$conceptId,
    snakeCaseToCamelCase = TRUE
  )$conceptId
  standardConcepts <- concepts |>
    filter(.data$conceptId %in% standardConceptIds)
  return(standardConcepts)
}

removeNonRelevantConcepts <- function(concepts, conditionPrompt, client, systemPrompt, batchSize = 25) {
  conceptIds <- c()
  for (start in seq(1, nrow(concepts), by = batchSize)) {
    batch <- concepts[start:min(start + batchSize - 1, nrow(concepts)), ]
    
    prompt <- paste0(conditionPrompt,
                    sprintf("\n\nConcepts:\n%s", jsonlite::toJSON(select(batch, "conceptId", "conceptName"))))
    client$set_system_prompt(systemPrompt)
    client$set_turns(list())
    response <- client$chat(prompt, echo = "none")  
    # writeLines(response)
    conceptIds <- c(conceptIds, extractAndParseJson(response)$conceptId)
  }
  concepts <- concepts |> 
    filter(conceptId %in% conceptIds) |>
    filter(!duplicated(.data$conceptId))
  return(concepts)
}


#' Generate KEEPER input concept sets
#' 
#' @description
#' Generate the concept sets used as input for KEEPER. This function uses LLMs, the OHDSI vocabulary vector store, and
#' Phoebe to populate concept sets for the disease of interest, symptoms, treatments etc. of the given medical 
#' condition.
#'
#' @param condition              A text string denoting the condition of interest, e.g. 'Type I Diabetes Mellitus 
#'                               (T1DM)'. This string is used as input for the LLM. 
#' @param client                 An LLM client created using the `ellmer` package.
#' @param vocabConnectionDetails Connection details for a database server hosting the OHDSI Vocabulary. Should be 
#'                               created using the \link[DatabaseConnector]{createConnectionDetails} function in the 
#'                               `DatabaseConnector` package.
#' @param vocabDatabaseSchema    The name of the database schema on the server where the vocabulary tables are located.
#'
#' @returns
#' A list of data frames. 
#' 
#' @export
generateKeeperConceptSets <- function(
    condition,
    client,
    vocabConnectionDetails,
    vocabDatabaseSchema) {
  connection <- DatabaseConnector::connect(vocabConnectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  
  yamlFileName <- "inst/ConceptSetGenerationPrompts.yaml"
  # yamlFileName <- system.file("ConceptSetGenerationPrompts.yaml", package = "Keeper")
  promptSets <- yaml::read_yaml(yamlFileName)
  
  conceptSets <- list()
  
  alternativeDiagnoses <- NULL
  for (i in seq_along(promptSets)) {
    promptSet <- promptSets[[i]]
    message(sprintf("Generating concept set: %s", promptSet$name))
    conceptSet <- generateConceptSet(
      condition = condition,
      alternativeDiagnoses = alternativeDiagnoses,
      promptSet = promptSet,
      connection = connection,
      vocabDatabaseSchema = vocabDatabaseSchema,
      client = client
    )
    if (promptSet$parameterName == "alternativeDiagnosis") {
      alternativeDiagnoses <- attr(conceptSet , "initialTerms")
    }
    conceptSets[[promptSet$parameterName]] <- conceptSet
  }
}


generateConceptSet <- function(condition,
                               alternativeDiagnoses = NULL,
                               promptSet,
                               client,
                               connection,
                               vocabDatabaseSchema) {
  conceptBatchSize <- 20
  minRecordCount <- 1e5
  
  if (is.null(alternativeDiagnoses)) {
    conditionPrompt <- sprintf("Condition: %s", condition)
  } else {
    conditionPrompt <- sprintf("Conditions:\n- %s", paste(c(condition, alternativeDiagnoses), collapse = "\n- "))
  }
  
  message("- Generating initial term list using LLM")
  client$set_system_prompt(promptSet$systemPromptTerms)
  client$set_turns(list())
  prompt <- conditionPrompt
  response <- client$chat(prompt, echo = "none")  
  #writeLines(response)
  terms <- extractAndParseJson(response)$terms
  message(sprintf("  Generated %d terms", length(terms)))
  
  message("- Searching standard concepts for terms using embedding vectors")
  concepts <- lapply(terms, vectorSearch, domains = promptSet$domains)
  concepts <- bind_rows(concepts) |>
    distinct() |>
    filter(.data$recordCount >= minRecordCount)
  message(sprintf("  Found %d unique concepts", nrow(concepts)))
  
  message("- Removing non-relevant concepts using LLM")
  concepts <- removeNonRelevantConcepts(
    concepts = concepts,
    conditionPrompt = conditionPrompt,
    client = client,
    systemPrompt = promptSet$systemPromptRemoveNonRelevant,
    batchSize = conceptBatchSize
  )
  message(sprintf("  Kept %d unique concepts", nrow(concepts)))
  
  message("- Removing children of included concepts")
  concepts <- removeChildren(concepts = concepts, 
                             connection = connection,
                             vocabDatabaseSchema = vocabDatabaseSchema)
  message(sprintf("  Kept %d unique concepts", nrow(concepts)))
  
  message("- Adding related concepts using Phoebe")
  newConcepts <- lapply(concepts$conceptId, phoebeSearch)
  newConcepts <- bind_rows(newConcepts) |>
    filter(!duplicated(conceptId))  |>
    filter(.data$recordCount >= minRecordCount)
  newConcepts <- removeNonStandard(concepts = newConcepts, 
                                   connection = connection,
                                   vocabDatabaseSchema = vocabDatabaseSchema)
  concepts <- addNonChildren(concepts = concepts, 
                             newConcepts = newConcepts,
                             connection = connection,
                             vocabDatabaseSchema = vocabDatabaseSchema)
  message(sprintf("  Now have a total of %d unique concepts", nrow(concepts)))
  
  message("- Removing non-relevant concepts using LLM")
  concepts <- removeNonRelevantConcepts(
    concepts = concepts,
    conditionPrompt = conditionPrompt,
    client = client,
    systemPrompt = promptSet$systemPromptRemoveNonRelevant,
    batchSize = conceptBatchSize
  )
  message(sprintf("  Kept %d unique concepts", nrow(concepts)))
  
  message("- Removing children of included concepts")
  concepts <- removeChildren(concepts = concepts, 
                             connection = connection,
                             vocabDatabaseSchema = vocabDatabaseSchema)
  message(sprintf("  Kept %d unique concepts", nrow(concepts)))
  
  concepts <- concepts |>
    select("conceptId", "conceptName", "vocabularyId")
  attr(concepts, "initialTerms") <- terms
  return(concepts)
}
