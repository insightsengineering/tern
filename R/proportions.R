#' Proportions
#'
#' Estimate proportion for the study of binary outcomes.
#'
#' @inheritParams argument_convention
#' @name prop
#'
NULL


#' @describeIn prop The Wilson interval calls [stats::prop.test()]
#'   with option `correct = FALSE`. Also referred to as Wilson score interval.
#' @importFrom stats prop.test
#' @export
#' @examples
#' rsp <- c(
#'   TRUE, TRUE, TRUE, TRUE, TRUE,
#'   FALSE, FALSE, FALSE, FALSE, FALSE
#' )
#' prop_wilson(rsp, conf_level = 0.9)
#'
prop_wilson <- function(rsp, conf_level){

  y <- stats::prop.test(
    sum(rsp),
    length(rsp),
    correct = FALSE,
    conf.level = conf_level
    )

  as.numeric(y$conf.int)

}
