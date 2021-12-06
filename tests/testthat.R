pkg_name <- "tern"
if (requireNamespace("testthat", quietly = TRUE)) {
  library(testthat)
  reporter <- testthat::MultiReporter$new(list(
    testthat::CheckReporter$new(),
    testthat::JunitReporter$new(file = "junit-result.xml")
  ))
  test_results <- testthat::test_check(pkg_name, reporter = reporter)
  saveRDS(test_results, "unit_testing_results.rds")
}
