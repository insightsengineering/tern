testthat::test_that("footnotes works correctly", {
  x <- table(iris$Species)
  footnotes(x) <- "Species are equally distributed"
  testthat::expect_identical(footnotes(x), "Species are equally distributed")
})

testthat::test_that("add_footnotes works correctly", {
  x <- table(iris$Species)
  footnotes(x) <- "Species are equally distributed"
  add_footnotes(x) <- "Add more footnotes"
  testthat::expect_identical(footnotes(x), c("Species are equally distributed", "Add more footnotes"))
})
