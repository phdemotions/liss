library(testthat)

test_that("dictionary validation catches duplicates and missing names", {
  dict <- tibble::tibble(
    old = c("a", "b"),
    new = c("yf24a_construct_mean", "yf24a_construct_mean")
  )

  expect_error(liss_validate_dictionary(dict), "duplicated 'new'")

  dict$new <- c("yf24a_construct_mean", NA_character_)
  expect_error(liss_validate_dictionary(dict, strict = TRUE), "missing 'new'")
})

test_that("drift report flags missing and added fields", {
  sav_path <- tempfile(fileext = ".sav")
  df <- data.frame(
    nomem_encr2 = c(1, 2),
    yf24a010 = c(50, 75),
    stringsAsFactors = FALSE
  )
  haven::write_sav(df, sav_path)

  dict_existing <- tibble::tibble(
    old = c("nomem_encr2", "yf24a999"),
    new = c("yf24a_person_id_encrypted_raw", "yf24a_unknown_raw")
  )

  drift <- liss_dictionary_drift_report(sav_path, dict_existing)

  expect_equal(drift$in_sav_not_in_dict[[1]], "yf24a010")
  expect_equal(drift$in_dict_not_in_sav[[1]], "yf24a999")
})
