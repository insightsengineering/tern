testthat::test_that("wrap_text works with default settings", {
  rlang::local_options(lifecycle_verbosity = "quiet")
  text <- "This is a test with many words and more"
  result <- wrap_text(txt = text, width = grid::unit(4, "cm"), collapse = "\n")

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
