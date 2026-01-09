library(testthat)

test_that("ontology validation enforces required pattern", {
  expect_true(liss_validate_new_names("yf24a_wellbeing_mean"))
  expect_error(liss_validate_new_names("wellbeing_mean"), "Invalid new variable names")
  expect_error(liss_validate_new_names("YF24A_wellbeing_mean"), "Invalid new variable names")
})

test_that("forbidden token policy rejects analysis-role leakage", {
  expect_error(liss_validate_no_forbidden_tokens("yf24a_treatment_binary"), "forbidden tokens")
})
