
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

iqr_num <- function(x, na.rm = TRUE) {
  quantile(x, probs = c(.25, .75), na.rm = TRUE)
}


# Version with test for if-all-NA, then display empty cell

positives_and_proportion2 <- function(x) {
  if (!is.logical(x)) stop("x is required to be logical")
  if (all(is.na(x))) rcell(" ", format="xx") else sum(x) * c(1, 1/length(x))
}

n_not_na2 <- function(x) {
  if (all(is.na(x))) rcell(" ", format="xx") else sum(!is.na(x))
}

mean_sd2 <- function(x, na.rm = TRUE) {
  if (all(is.na(x))) rcell(" ", format="xx") else c(mean(x, na.rm = na.rm), sd(x, na.rm = na.rm))
}

median_t2 <- function(x, na.rm = TRUE) {
  if (all(is.na(x))) rcell(" ", format="xx") else median(x, na.rm = TRUE)
}

range_t2 <- function(x, na.rm = TRUE) {
  if (all(is.na(x))) rcell(" ", format="xx") else range(x, na.rm = TRUE)
}

iqr_num2 <- function(x, na.rm = TRUE) {
  if (all(is.na(x))) rcell(" ", format="xx") else quantile(x, probs = c(.25, .75), na.rm = TRUE)
}


# Version with test for if-all-NA, then display NE

positives_and_proportion3 <- function(x) {
  if (!is.logical(x)) stop("x is required to be logical")
  if (all(is.na(x))) rcell("NE", format="xx") else sum(x) * c(1, 1/length(x))
}

n_not_na3 <- function(x) {
  sum(!is.na(x))
}

mean_sd3 <- function(x, na.rm = TRUE) {
  if (all(is.na(x))) rcell("NE", format = function(x, output) paste0(x," (",x,")")) else c(mean(x, na.rm = na.rm), sd(x, na.rm = na.rm))
}

median_t3 <- function(x, na.rm = TRUE) {
  if (all(is.na(x))) rcell("NE", format="xx") else median(x, na.rm = TRUE)
}

range_t3 <- function(x, na.rm = TRUE) {
  if (all(is.na(x))) rcell("NE", format = function(x, output) paste0(x," - ",x)) else range(x, na.rm = TRUE)
}

iqr_num3 <- function(x, na.rm = TRUE) {
  if (all(is.na(x))) rcell("NE", format = function(x, output) paste0(x," - ",x)) else quantile(x, probs = c(.25, .75), na.rm = TRUE)
}