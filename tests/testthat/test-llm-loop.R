library(Keeper)
library(testthat)

test_that("reviewCases validates Chat client input", {
  keeperTable <- data.frame(
    generatedId = 1,
    phenotype = "GI bleed",
    cohortPrevalence = 0.1,
    age = 50,
    gender = "Male",
    observationPeriod = "(day-365, day0)",
    visits = "Outpatient",
    presentation = "None",
    comorbidities = "None",
    symptoms = "None",
    priorDisease = "None",
    priorDrugs = "None",
    priorTreatmentProcedures = "None",
    diagnosticProcedures = "None",
    measurements = "None",
    alternativeDiagnosis = "None",
    afterDisease = "None",
    afterTreatmentProcedures = "None",
    afterDrugs = "None",
    death = "No",
    stringsAsFactors = FALSE
  )

  expect_error(
    reviewCases(
      keeper = keeperTable,
      settings = createPromptSettings(),
      client = list(),
      cacheFolder = tempdir()
    )
  )
})

test_that("reviewCases uses cache and avoids repeated LLM calls", {
  rdsPath <- system.file("shuffledKeeper.rds", package = "Keeper")
  shuffled <- readRDS(rdsPath)

  calls <- 0
  Chat <- R6::R6Class(
    "Chat",
    public = list(
      set_turns = function(turns) invisible(NULL),
      set_system_prompt = function(systemPrompt) invisible(NULL),
      chat_structured = function(prompt, echo, type) {
        calls <<- calls + 1
        list(
          justification = "mock justification",
          verdict = "yes",
          certainty = "high",
          day_of_onset = 3
        )
      },
      get_cost = function() 0.01,
      get_model = function() "mock-model"
    )
  )
  client <- Chat$new()

  local_mocked_bindings(
    createSystemPrompt = function(settings, phenotypeName) "System prompt",
    createPrompt = function(settings, phenotypeName, keeperTableRow) "User prompt",
    parseLlmResponse = function(response, noMatchIsInsufficientInformation = FALSE) {
      dplyr::tibble(
        isCase = tolower(response$verdict),
        certainty = tolower(response$certainty),
        indexDay = as.numeric(response$day_of_onset),
        justification = response$justification
      )
    },
    .package = "Keeper"
  )

  cacheFolder <- tempfile("keeper_cache_")
  resultFirst <- reviewCases(
    keeper = shuffled,
    settings = createPromptSettings(legacy = FALSE),
    client = client,
    cacheFolder = cacheFolder
  )

  expect_s3_class(resultFirst, "data.frame")
  expect_equal(nrow(resultFirst), length(unique(shuffled$generatedId)))
  expect_true(all(resultFirst$isCase == "yes"))
  expect_equal(unique(resultFirst$model), "mock-model")
  expect_equal(calls, length(unique(shuffled$generatedId)))

  expect_equal(length(list.files(cacheFolder, pattern = "\\.txt$")), 2 * length(unique(shuffled$generatedId)))
  unlink(cacheFolder)
})

test_that("generateCacheFileName sanitizes phenotype and supports prompt suffix", {
  responseFile <- Keeper:::generateCacheFileName(
    phenotypeName = "Acute bronchitis!",
    generatedId = 101,
    cacheFolder = "cache",
    type = "response"
  )
  promptFile <- Keeper:::generateCacheFileName(
    phenotypeName = "Acute bronchitis!",
    generatedId = 101,
    cacheFolder = "cache",
    type = "prompt"
  )

  expect_match(basename(responseFile), "^Acutebronchitis_p101\\.txt$")
  expect_match(basename(promptFile), "^Acutebronchitis_p101_prompt\\.txt$")
})
