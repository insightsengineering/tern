testthat::test_that("footnotes works correctly", {
  x <- table(iris$Species)
  footnotes(x) <- "Species are equally distributed"

  res <- testthat::expect_silent(footnotes(x))
  testthat::expect_snapshot(res)
})

testthat::test_that("add_footnotes works correctly", {
  x <- table(iris$Species)
  footnotes(x) <- "Species are equally distributed"
  add_footnotes(x) <- "Add more footnotes"

  res <- testthat::expect_silent(footnotes(x))
  testthat::expect_snapshot(res)
})
