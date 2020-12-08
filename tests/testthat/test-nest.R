if (requireNamespace("test.nest", quietly = TRUE)) {
  library(test.nest)
  test_all(exclude_from_tests = c("test_strict", "test_importfrom"))
}
