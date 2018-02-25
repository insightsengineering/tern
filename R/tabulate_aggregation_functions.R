
# Collection of reusable function that can be reused in rtabulate

positives_and_proportion <- function(x) {
  if (!is.logical(x)) stop("x is required to be logical")
  sum(x) * c(1, 1/length(x))
}


n_not_na <- function(x) {
  sum(!is.na(x))
}

mean_sd <- function(x, na.rm = TRUE) {
  c(mean(x, na.rm = na.rm), sd(x, na.rm = na.rm))
}

median_t <- function(x, na.rm = TRUE) {
  median(x, na.rm = TRUE)
}

range_t <- function(x, na.rm = TRUE) {
  range(x, na.rm = TRUE)
}