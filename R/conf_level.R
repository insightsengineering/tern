#' Check Confidence Levels for Different Confidence Intervals and Tests
#'
#' Often a table contains multiple confidence intervals or p-values from tests.
#'
#' @param conf_level a single value or a named vector with values between 0 and 1
#' @param type names expected in the returned values
#'
#' @return a named vector with the confidence levels
#'
#' @importFrom stats setNames
#'
#' @examples
#'
#' tern:::check_conf_level(0.95, NULL)
#' tern:::check_conf_level(0.95, c("survfit", "coxph", "ztest"))
#' tern:::check_conf_level(conf_level = c(coxph = 0.99, survfit = 0.95, ztest = 0.98),
#'                         type = c("survfit", "coxph", "ztest"))
#'
#' \dontrun{
#' # will fail because mismatching size of both
#' # arguments and neither has length 1
#' tern:::check_conf_level(c(survfit = 0.95, ztest = 0.98),
#'                         c("survfit", "coxph", "ztest"))
#' }
check_conf_level <- function(conf_level, type = NULL) {

  stopifnot(
    is_numeric_vector(conf_level),
    all(conf_level > 0 & conf_level < 1)
  )

  n_conf_level <- length(conf_level)
  if (is.null(type) && n_conf_level == 1) {
    conf_level
  } else if (is_character_vector(type) && n_conf_level == 1) {
    setNames(rep(conf_level, length(type)), type)
  } else if (is_character_vector(type) && n_conf_level > 0 && setequal(names(conf_level), type)) {
    conf_level[type]
  } else {
    stop(
      "conf_level is not specified correct for types: ",
      paste(type, collapse = ", "), " -- conf_level: ",
      paste(conf_level, collapse = ", ")
    )
  }

}
