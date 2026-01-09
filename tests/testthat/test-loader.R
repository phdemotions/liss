library(testthat)

create_test_sav <- function(path) {
  df <- data.frame(
    nomem_encr2 = c(1, 2),
    yf24a010 = c(50, 75),
    stringsAsFactors = FALSE
  )
  haven::write_sav(df, path)
}

test_that("load_liss errors when module is missing", {
  with_test_extdata({
    sav_path <- tempfile(fileext = ".sav")
    create_test_sav(sav_path)
    expect_error(load_liss("missing", path = sav_path), "Unknown module")
  })
})

test_that("load_liss errors when versions are ambiguous", {
  with_test_extdata({
    sav_path <- tempfile(fileext = ".sav")
    create_test_sav(sav_path)
    expect_error(load_liss("ambig", path = sav_path), "Multiple versions available")
  })
})

test_that("load_liss errors when dictionary file is missing", {
  with_test_extdata({
    sav_path <- tempfile(fileext = ".sav")
    create_test_sav(sav_path)
    dict_path <- liss_extdata_path("dictionaries/dict_yf24a_1.0.rds")
    temp_path <- tempfile(fileext = ".rds")
    file.rename(dict_path, temp_path)
    on.exit(file.rename(temp_path, dict_path), add = TRUE)

    expect_error(
      load_liss("yf24a", version = "1.0", path = sav_path),
      "Extdata path not found"
    )
  })
})

test_that("load_liss errors when required ID keys are missing", {
  with_test_extdata({
    sav_path <- tempfile(fileext = ".sav")
    create_test_sav(sav_path)
    expect_error(
      load_liss("badid", version = "1.0", path = sav_path),
      "Dictionary missing id_key_new"
    )
  })
})
