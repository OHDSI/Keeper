library(Keeper)
library(testthat)

# These tests guard against a SQL-injection regression in the reviewer
# Shiny app. The original `inst/shiny/server.R` built UPDATE statements by
# textual substitution (SqlRender `@parameter` tokens wrapped in single
# quotes), which both (a) broke on legitimate phenotype names containing an
# apostrophe (e.g. "Crohn's disease") and (b) allowed an attacker who could
# influence any interpolated value to inject arbitrary SQL. The new helpers
# allowlist the only two identifiers that MUST be interpolated (schema and
# column), and pass every user-supplied value to DBI as a bound parameter.

test_that("buildUpdateAdjudicationSql emits bind placeholders, not inline values", {
  for (column in c("decision", "certainty", "index_day")) {
    sql <- Keeper:::buildUpdateAdjudicationSql("public", column)
    expect_match(sql, sprintf("SET %s = \\?", column))
    expect_match(sql, "WHERE database_id = \\?")
    expect_match(sql, "AND phenotype = \\?")
    expect_match(sql, "AND generated_id = \\?")
    expect_match(sql, "AND adjudicator = \\?")
    expect_false(
      grepl("'", sql, fixed = TRUE),
      info = "SQL must not contain inline string literals"
    )
  }
})

test_that("buildUpdateAdjudicationSql rejects non-allowlisted columns", {
  expect_error(
    Keeper:::buildUpdateAdjudicationSql("public", "arbitrary"),
    "Invalid column"
  )
  expect_error(
    Keeper:::buildUpdateAdjudicationSql(
      "public", "decision = 'Case'; DROP TABLE users; --"),
    "Invalid column"
  )
  expect_error(
    Keeper:::buildUpdateAdjudicationSql("public", c("decision", "certainty")),
    "Invalid column"
  )
})

test_that("buildUpdateAdjudicationSql rejects unsafe schema names", {
  expect_error(
    Keeper:::buildUpdateAdjudicationSql(
      "public'; DROP TABLE users; --", "decision"),
    "Invalid databaseSchema"
  )
  expect_error(
    Keeper:::buildUpdateAdjudicationSql("", "decision"),
    "Invalid databaseSchema"
  )
  expect_error(
    Keeper:::buildUpdateAdjudicationSql("1public", "decision"),
    "Invalid databaseSchema"
  )
  expect_error(
    Keeper:::buildUpdateAdjudicationSql("public schema", "decision"),
    "Invalid databaseSchema"
  )
})

test_that("buildUpdateAdjudicationSql accepts database.schema form", {
  sql <- Keeper:::buildUpdateAdjudicationSql("my_db.my_schema", "decision")
  expect_match(sql, "UPDATE my_db\\.my_schema\\.adjudications")
})

test_that("updateAdjudication binds user-supplied values as parameters", {
  captured <- new.env(parent = emptyenv())
  local_mocked_bindings(
    executeAdjudicationSql = function(connection, sql, params) {
      captured$sql <- sql
      captured$params <- params
      1L
    },
    .package = "Keeper"
  )

  Keeper:::updateAdjudication(
    connection     = "fake_conn",
    databaseSchema = "public",
    column         = "decision",
    value          = "Case",
    databaseId     = "DB_1",
    phenotype      = "Crohn's disease",
    generatedId    = 42,
    adjudicator    = "ALICE"
  )

  # A phenotype legitimately containing an apostrophe must never appear
  # in the SQL text; it travels as a bound parameter instead.
  expect_false(grepl("Crohn", captured$sql, fixed = TRUE))
  expect_false(grepl("'", captured$sql, fixed = TRUE))
  expect_equal(
    captured$params,
    list("Case", "DB_1", "Crohn's disease", 42L, "ALICE")
  )
})

test_that("updateAdjudication neutralizes injection payloads in value", {
  captured <- new.env(parent = emptyenv())
  local_mocked_bindings(
    executeAdjudicationSql = function(connection, sql, params) {
      captured$sql <- sql
      captured$params <- params
      1L
    },
    .package = "Keeper"
  )

  injectionPayload <- "Case', adjudicator='attacker'; --"
  Keeper:::updateAdjudication(
    connection     = "fake_conn",
    databaseSchema = "public",
    column         = "decision",
    value          = injectionPayload,
    databaseId     = "DB_1",
    phenotype      = "AFib",
    generatedId    = 1,
    adjudicator    = "ALICE"
  )

  # The payload must not appear in the SQL; it rides in params.
  expect_false(grepl("attacker", captured$sql, fixed = TRUE))
  expect_false(grepl("DROP",     captured$sql, fixed = TRUE))
  expect_equal(captured$params[[1]], injectionPayload)
})

test_that("updateAdjudication coerces generatedId to integer", {
  captured <- new.env(parent = emptyenv())
  local_mocked_bindings(
    executeAdjudicationSql = function(connection, sql, params) {
      captured$params <- params
      1L
    },
    .package = "Keeper"
  )

  Keeper:::updateAdjudication(
    connection     = "fake_conn",
    databaseSchema = "public",
    column         = "index_day",
    value          = 5L,
    databaseId     = "DB_1",
    phenotype      = "AFib",
    generatedId    = "42",
    adjudicator    = "ALICE"
  )

  expect_identical(captured$params[[4]], 42L)
})

test_that("SqlRender interpolation is textual (documents why the old code broke)", {
  skip_if_not_installed("SqlRender")
  # This is not an assertion about Keeper's code — it documents the
  # vulnerability in the original implementation: SqlRender's @-token
  # substitution is textual and therefore unsafe for user-supplied values.
  vulnerable <- SqlRender::render(
    "SET phenotype = '@phenotype';",
    phenotype = "Crohn's disease"
  )
  expect_equal(vulnerable, "SET phenotype = 'Crohn's disease';")
})
