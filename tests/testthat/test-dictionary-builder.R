library(testthat)

create_test_sav <- function(path) {
  df <- data.frame(
    var_a = c(1, 2, 3),
    var_b = c(4, 5, 6),
    stringsAsFactors = FALSE
  )
  haven::var_label(df$var_a) <- "Wellbeing scale (0-100)"
  haven::var_label(df$var_b) <- "Wellbeing scale (0-100)"
  haven::write_sav(df, path)
}

test_that("ontology validation enforces required pattern", {
  expect_true(liss_validate_new_names("wellbeing__mean"))
  expect_error(liss_validate_new_names("wellbeing_mean"), "Invalid new variable names")
  expect_error(liss_validate_new_names("Wellbeing__mean"), "Invalid new variable names")
})

test_that("forbidden token policy rejects analysis-role leakage", {
  expect_error(liss_validate_no_forbidden_tokens("treatment__binary"), "forbidden tokens")
})

test_that("duplicate new names are detected when building dictionaries", {
  sav_path <- tempfile(fileext = ".sav")
  create_test_sav(sav_path)
  overrides <- tibble::tribble(
    ~old,   ~new,
    "var_a", "wellbeing__mean",
    "var_b", "wellbeing__mean"
  )

  expect_error(
    liss_build_dictionary(
      sav_path = sav_path,
      module_id = "yf24a",
      overrides = overrides
    ),
    "Duplicate new names detected"
  )
})

test_that("drift report flags missing and added fields", {
  sav_path <- tempfile(fileext = ".sav")
  create_test_sav(sav_path)
  dict_existing <- tibble::tibble(
    old = c("var_a", "var_c"),
    new = c("wellbeing__mean", "income__raw")
  )

  drift <- liss_dictionary_drift_report(sav_path, dict_existing)

  expect_equal(drift$in_sav_not_in_dict[[1]], "var_b")
  expect_equal(drift$in_dict_not_in_sav[[1]], "var_c")
})
