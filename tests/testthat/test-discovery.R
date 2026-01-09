library(testthat)

test_that("crosswalk and vars return expected columns", {
  with_test_extdata({
    cw <- crosswalk("yf24a", version = "1.0")
    expect_true(all(c("module", "old", "new", "measurement_label", "label_raw") %in% names(cw)))

    analysis_vars <- vars("yf24a", version = "1.0", naming = "analysis")
    expect_true(all(c("new", "measurement_label", "orig_old") %in% names(analysis_vars)))

    raw_vars <- vars("yf24a", version = "1.0", naming = "raw")
    expect_true(all(c("old", "label_raw") %in% names(raw_vars)))
  })
})

test_that("search_vars finds matches across modules", {
  with_test_extdata({
    results <- search_vars("hire", fields = c("measurement_label"))
    expect_true(nrow(results) >= 1)
    expect_true(all(c("module", "version", "measurement_label") %in% names(results)))
  })
})
