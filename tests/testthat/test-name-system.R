testthat::test_that("provisional name generation is deterministic", {
  out <- liss_generate_provisional_name(
    study_id = "cp23o",
    old_name = "cp23o001",
    label = "Agreeable item 1"
  )

  testthat::expect_equal(out$new_name, "cp23o_agreeable_item_i01")
  testthat::expect_equal(out$construct, "agreeable_item")
  testthat::expect_equal(out$item_number, 1L)
})

testthat::test_that("rename_with_dictionary prevents duplicate new names", {
  df <- data.frame(a = 1:3, b = 4:6)
  dict <- tibble::tibble(
    old_name = c("a", "b"),
    new_name = c("study_construct", "study_construct")
  )

  testthat::expect_error(
    rename_with_dictionary(df, dict),
    "Duplicate new_name"
  )
})
