skip_if_too_deep <- function(depth) { # nolintr
  stopifnot(length(depth) == 1 && is.numeric(depth) && depth >= 0 && depth <= 5)
  test_to_depth <- testing_depth() # by default 3 if there are no env variable

  testing_depth <- getOption("TESTING_DEPTH")
  if (is.null(testing_depth)) testing_depth <- Sys.getenv("TESTING_DEPTH")

  testing_depth <- tryCatch(
    as.numeric(testing_depth),
    error = function(error) 3,
    warning = function(warning) 3
  )

  if (length(testing_depth) != 1 || is.na(testing_depth)) testing_depth <- 3

  if (test_to_depth < depth) {
    testthat::skip(paste("testing depth", test_to_depth, "is below current testing specification", depth))
  }
}
