pkg_name <- "tern"
library(testthat)
test_check(pkg_name, reporter = ParallelProgressReporter$new())
