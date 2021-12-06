#' Control Function for Subgroup Treatment Effect Pattern (STEP) Calculations
#'
#' This is an auxiliary function for controlling arguments for STEP calculations.
#'
#' @param biomarker (`numeric` or `NULL`)\cr optional provision of the numeric biomarker variable, which
#'   could be used to infer `bandwidth`, see below.
#' @param use_percentile (`flag`)\cr if `TRUE`, the running windows are created according to
#'   quantiles rather than actual values, i.e. the bandwidth refers to the percentage of data
#'   covered in each window. Suggest `TRUE` if the biomarker variable is not uniformly
#'   distributed.
#' @param bandwidth (`number` or `NULL`)\cr indicating the bandwidth of each window.
#'    Depending on the argument `use_percentile`, it can be either the length of actual-value
#'    windows on the real biomarker scale, or percentage windows.
#'    If `use_percentile = TRUE`, it should be a number between 0 and 1.
#'    If `NULL`, treat the bandwidth to be infinity, which means only one global model will be fitted.
#'    By default, `0.25` is used for percentage windows and one quarter of the range of the `biomarker`
#'    variable for actual-value windows.
#' @param degree (`count`)\cr the degree of polynomial function of the biomarker as an interaction term
#'   with the treatment arm fitted at each window. If 0 (default), then the biomarker variable
#'   is not included in the model fitted in each biomarker window.
#' @param num_points (`count`)\cr the number of points at which the hazard ratios are estimated. The
#'   smallest number is 2.
#' @return A list of components with the same names as the arguments, except `biomarker` which is
#'   just used to calculate the `bandwidth` in case that actual biomarker windows are requested.
#' @export
#' @examples
#' # Provide biomarker values and request actual values to be used,
#' # so that bandwidth is chosen from range.
#' control_step(biomarker = 1:10, use_percentile = FALSE)
#'
#' # Use a global model with quadratic biomarker interaction term.
#' control_step(bandwidth = NULL, degree = 2)
#'
#' # Reduce number of points to be used.
#' control_step(num_points = 10)
#'
control_step <- function(biomarker = NULL,
                         use_percentile = TRUE,
                         bandwidth,
                         degree = 0L,
                         num_points = 39L) {
  assertthat::assert_that(
    is.null(biomarker) || is.numeric(biomarker),
    assertthat::is.flag(use_percentile),
    is_nonnegative_count(degree),
    assertthat::is.count(num_points) && num_points >= 2
  )
  if (missing(bandwidth)) {
    # Infer bandwidth.
    bandwidth <- if (use_percentile) {
      0.25
    } else if (!is.null(biomarker)) {
      diff(range(biomarker, na.rm = TRUE)) / 4
    } else {
      NULL
    }
  } else {
    # Check bandwidth.
    assertthat::assert_that(
      is.null(bandwidth) ||
        (use_percentile && is_proportion(bandwidth)) ||
        (!use_percentile && assertthat::is.scalar(bandwidth) && bandwidth > 0)
    )
  }
  list(
    use_percentile = use_percentile,
    bandwidth = bandwidth,
    degree = as.integer(degree),
    num_points = as.integer(num_points)
  )
}
