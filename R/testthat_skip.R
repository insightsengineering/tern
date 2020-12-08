
# nolint start
testing_depth <- function() { # nolintr # nousage

  TESTING_DEPTH <- getOption("TESTING_DEPTH")

  if (is.null(TESTING_DEPTH))
    TESTING_DEPTH <- Sys.getenv("TESTING_DEPTH")

  TESTING_DEPTH <- if (!is.null(TESTING_DEPTH) && grepl("^[[:digit:]]+$", TESTING_DEPTH)) {
    as.numeric(TESTING_DEPTH)
  } else if (is.numeric(TESTING_DEPTH) && length(TESTING_DEPTH) == 1) {
    TESTING_DEPTH
  } else {
    3
  }
  TESTING_DEPTH
}
# nolint end

# 1 Commit TEST Suite
# 2 Nightly Integreation TEST Suite
# 3 Extended Testing
#' @importFrom testthat skip
skip_if_too_deep <- function(depth) { # nolintr # nousage
  test_to_depth <- testing_depth()

  if (depth > test_to_depth)  {
    skip(paste("testing depth", depth, "is below current testing specification", test_to_depth))
  }
}

# used for tests at the moments
reapply_varlabels <- function(x, varlables, ...) { # nolintr # nousage
  do.call(var_relabel, c(list(x = x), as.list(varlables), list(...)))
}
