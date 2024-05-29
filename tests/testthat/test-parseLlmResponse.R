library(Keeper)
library(testthat)

test_that("Parse LLM response", {
  expect_equal(parseLlmResponse("Summary: yes"), "yes")
  expect_equal(parseLlmResponse("Summary: no"), "no")
  expect_equal(parseLlmResponse("Summary: maybe"), "I don't know")
  expect_equal(parseLlmResponse("Summary: maybe", noMatchIsDontKnow = FALSE), NA)
})
