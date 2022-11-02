testthat::test_that("wrap_text works with default settings", {
  text <- "This is a test with many words and more"
  result <- wrap_text(txt = text, width = grid::unit(4, "cm"), collapse = "\n")
  expected <- "This is a test with\nmany words and\nmore"
  testthat::expect_identical(result, expected)
})
