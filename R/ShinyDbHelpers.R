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

# Internal helpers used by the reviewer Shiny app (inst/shiny/server.R)
# to persist adjudication decisions to a database. These replace
# SqlRender @-token templates, whose textual substitution is unsafe for
# values that are user-supplied (`decision`, `certainty`, `index_day`)
# or that may legitimately contain apostrophes (phenotype names such as
# "Crohn's disease", database identifiers, etc.). Only the schema and
# column name are interpolated into the SQL — both after strict allowlist
# validation. Every other value is bound as a DBI parameter.

validateAdjudicationColumn <- function(column) {
  allowedColumns <- c("decision", "certainty", "index_day")
  if (!is.character(column) || length(column) != 1L ||
      is.na(column) || !column %in% allowedColumns) {
    stop(sprintf(
      "Invalid column '%s'; must be one of: %s",
      paste(column, collapse = ", "),
      paste(allowedColumns, collapse = ", ")
    ), call. = FALSE)
  }
  invisible(column)
}

validateDatabaseSchema <- function(databaseSchema) {
  pattern <- "^[A-Za-z_][A-Za-z0-9_]*(\\.[A-Za-z_][A-Za-z0-9_]*)?$"
  if (!is.character(databaseSchema) || length(databaseSchema) != 1L ||
      is.na(databaseSchema) || !grepl(pattern, databaseSchema)) {
    stop(sprintf(
      "Invalid databaseSchema: '%s'",
      paste(databaseSchema, collapse = ", ")
    ), call. = FALSE)
  }
  invisible(databaseSchema)
}

buildUpdateAdjudicationSql <- function(databaseSchema, column) {
  validateDatabaseSchema(databaseSchema)
  validateAdjudicationColumn(column)
  sprintf(
    paste(
      "UPDATE %s.adjudications",
      "SET %s = ?",
      "WHERE database_id = ?",
      "AND phenotype = ?",
      "AND generated_id = ?",
      "AND adjudicator = ?"
    ),
    databaseSchema, column
  )
}

# Thin wrapper over DBI::dbExecute so tests can stub out execution.
executeAdjudicationSql <- function(connection, sql, params) {
  DBI::dbExecute(connection, sql, params = params)
}

updateAdjudication <- function(connection,
                                databaseSchema,
                                column,
                                value,
                                databaseId,
                                phenotype,
                                generatedId,
                                adjudicator) {
  sql <- buildUpdateAdjudicationSql(databaseSchema, column)
  executeAdjudicationSql(
    connection = connection,
    sql = sql,
    params = list(
      value,
      databaseId,
      phenotype,
      as.integer(generatedId),
      adjudicator
    )
  )
  invisible(NULL)
}
