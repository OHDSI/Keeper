library(Keeper)
library(testthat)

test_that("generateKeeperConceptSets validates inputs", {
  expect_error(
    generateKeeperConceptSets(
      phenotype = "",
      client = list(),
      vocabConnectionDetails = structure(list(), class = "ConnectionDetails"),
      vocabDatabaseSchema = "main"
    )
  )
})

test_that("generateKeeperConceptSets orchestrates DOI and alternative diagnosis flows", {
  callLog <- list()
  fakePrompts <- list(
    list(name = "Disease", parameterName = "doi"),
    list(name = "Alternative diagnosis", parameterName = "alternativeDiagnosis"),
    list(name = "Symptoms", parameterName = "symptoms")
  )

  # Minimal client that satisfies checkmate::assertR6(client, "Chat")
  Chat <- R6::R6Class("Chat", public = list())
  client <- Chat$new()

  local_mocked_bindings(
    generateConceptSet = function(phenotype,
                                  promptSet,
                                  client,
                                  connection,
                                  vocabDatabaseSchema) {
      callLog[[length(callLog) + 1]] <<- list(
        phenotype = phenotype,
        parameterName = promptSet$parameterName,
        schema = vocabDatabaseSchema,
        hasConnection = !is.null(connection)
      )

      conceptSet <- data.frame(
        conceptId = as.integer(100 + length(callLog)),
        conceptName = paste("Concept", promptSet$parameterName, length(callLog)),
        vocabularyId = "SNOMED",
        stringsAsFactors = FALSE
      )
      attr(conceptSet, "cost") <- 1
      attr(conceptSet, "initialTerms") <- c("dx1", "dx2")
      conceptSet
    },
    .package = "Keeper"
  )
  local_mocked_bindings(
    read_yaml = function(...) fakePrompts,
    .package = "yaml"
  )
  local_mocked_bindings(
    connect = function(...) structure(list(fake = TRUE), class = "FakeConnection"),
    disconnect = function(...) invisible(NULL),
    .package = "DatabaseConnector"
  )

  result <- generateKeeperConceptSets(
    phenotype = "GI bleed",
    client = client,
    vocabConnectionDetails = structure(list(), class = "ConnectionDetails"),
    vocabDatabaseSchema = "main"
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 4)
  expect_named(result, c("conceptId", "conceptName", "vocabularyId", "conceptSetName", "target"))

  expect_equal(sum(result$target == "Disease of interest"), 3)
  expect_equal(sum(result$target == "Alternative diagnoses"), 1)
  expect_equal(sum(result$conceptSetName == "symptoms"), 2)

  expect_equal(length(callLog), 4)
  expect_equal(callLog[[1]]$parameterName, "doi")
  expect_equal(callLog[[2]]$parameterName, "alternativeDiagnosis")
  expect_equal(callLog[[3]]$parameterName, "symptoms")
  expect_equal(callLog[[4]]$parameterName, "symptoms")
  expect_equal(callLog[[4]]$phenotype, "\n- dx1\n- dx2")
  expect_true(callLog[[1]]$hasConnection)
  expect_equal(callLog[[1]]$schema, "main")
})
