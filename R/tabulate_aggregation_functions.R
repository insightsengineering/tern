
# Collection of reusable function that can be reused in rtabulate

success_and_proportion <- function(x) {
  if (!is.logical(x)) stop("x is required to be logical")
  sum(x) * c(1, 1/length(x))
}

