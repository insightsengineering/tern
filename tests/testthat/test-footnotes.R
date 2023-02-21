testthat::test_that("footnotes works correctly", {
  x <- table(iris$Species)
  testthat::expect_warning(footnotes(x) <- "Species are equally distributed")

  testthat::expect_warning(res <- footnotes(x))
  testthat::expect_snapshot(res)
})

testthat::test_that("add_footnotes works correctly", {
  x <- table(iris$Species)
  testthat::expect_warning(footnotes(x) <- "Species are equally distributed")
  suppressWarnings(testthat::expect_warning(add_footnotes(x) <- "Add more footnotes"))

  testthat::expect_warning(res <- footnotes(x))
  testthat::expect_snapshot(res)
})
