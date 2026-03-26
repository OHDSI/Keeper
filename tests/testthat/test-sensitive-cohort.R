library(Keeper)
library(testthat)

test_that("createReferenceCohortTableNames derives both table names", {
  tableNames <- createReferenceCohortTableNames("ref_cohort")

  expect_type(tableNames, "list")
  expect_named(tableNames, c("referenceCohortTable", "referenceCohortMetadataTable"))
  expect_equal(tableNames$referenceCohortTable, "ref_cohort")
  expect_equal(tableNames$referenceCohortMetadataTable, "ref_cohort_metadata")
})

test_that("uploadReferenceCohort enforces connection input", {
  reviews <- data.frame(
    generatedId = 1,
    phenotype = "GI bleed",
    isCase = "yes",
    indexDay = 0,
    certainty = "high",
    justification = "clear evidence",
    personId = 101,
    cohortStartDate = as.Date("2020-01-01"),
    cohortPrevalence = 0.2,
    model = "test-model",
    keeperVersion = "0.0.0",
    stringsAsFactors = FALSE
  )

  expect_error(
    uploadReferenceCohort(
      referenceCohortDatabaseSchema = "main",
      referenceCohortTableNames = createReferenceCohortTableNames("reference_cohort"),
      referenceCohortDefinitionId = 1,
      createReferenceCohortTables = FALSE,
      reviews = reviews
    ),
    "Must provide either connectionDetails or a connection"
  )
})

test_that("uploadReferenceCohort rejects non-unique metadata", {
  reviews <- data.frame(
    generatedId = c(1, 2),
    phenotype = c("GI bleed", "Another phenotype"),
    isCase = c("yes", "no"),
    indexDay = c(0, 1),
    certainty = c("high", "low"),
    justification = c("clear evidence", "uncertain evidence"),
    personId = c(101, 102),
    cohortStartDate = as.Date(c("2020-01-01", "2020-01-02")),
    cohortPrevalence = c(0.2, 0.3),
    model = c("test-model", "test-model-2"),
    keeperVersion = c("0.0.0", "0.0.0"),
    stringsAsFactors = FALSE
  )

  fakeConnectionDetails <- structure(list(), class = "ConnectionDetails")

  expect_error(
    uploadReferenceCohort(
      connectionDetails = fakeConnectionDetails,
      referenceCohortDatabaseSchema = "main",
      referenceCohortTableNames = createReferenceCohortTableNames("reference_cohort"),
      referenceCohortDefinitionId = 1,
      createReferenceCohortTables = FALSE,
      reviews = reviews
    ),
    "Non-unique metadata found"
  )
})

test_that("computePerformanceMetrics returns expected core measures", {
  metrics <- Keeper:::computePerformanceMetrics(
    tp = 8,
    tn = 9,
    fp = 1,
    fn = 2,
    cases = 10,
    nonCases = 10,
    cohortPrevalence = 0.2
  )

  expect_equal(as.vector(metrics$sensitivity), 0.8, tolerance = 1e-8)
  expect_equal(as.vector(metrics$specificity), 0.9, tolerance = 1e-8)
  expect_equal(as.vector(metrics$ppv), 8 / 9, tolerance = 1e-8)
  expect_equal(as.vector(metrics$auc), 0.85, tolerance = 1e-8)
  expect_equal(as.vector(metrics$kappa), 0.7, tolerance = 1e-8)
  expect_equal(as.vector(metrics$prevalence), 0.5, tolerance = 1e-8)
  expect_equal(as.vector(metrics$prevalenceOverall), 0.1, tolerance = 1e-8)
})

test_that("computePerformanceMetrics handles undefined sensitivity", {
  metrics <- Keeper:::computePerformanceMetrics(
    tp = 0,
    tn = 5,
    fp = 0,
    fn = 0,
    cases = 0,
    nonCases = 5,
    cohortPrevalence = 0.5
  )

  expect_true(is.na(metrics$sensitivity))
  expect_equal(as.vector(metrics$specificity), 1, tolerance = 1e-8)
  expect_equal(metrics$ppv, NA_real_)
})
