library(Keeper)
library(testthat)

test_that("Parse LLM response", {
  expect_equal(parseLlmResponse("{\"verdict\": \"Yes\", \"day of onset\": 0}")$isCase, "yes")
  expect_equal(parseLlmResponse("{\"verdict\": \"No\", \"day of onset\": 0}")$isCase, "no")
  expect_equal(parseLlmResponse("{\"verdict\": \"Insufficient information\", \"day of onset\": 0}")$isCase, "insufficient information")
  expect_warning({isCase = parseLlmResponse("{\"verdict\": \"Maybe\", \"day of onset\": 0}", noMatchIsInsufficientInformation = FALSE)$isCase},
                 "Unable to parse response")
  expect_true(is.na(isCase))
  
  expect_equal(parseLlmResponse("{\"verdict\": \"Yes\", \"day of onset\": -7}")$indexDay, -7)
  
})
