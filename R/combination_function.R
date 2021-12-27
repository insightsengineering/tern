#' Combination Functions Class
#'
#' `CombinationFunction` is an S4 class which extends standard functions. These are special functions that
#' can be combined and negated with the logical operators.
#'
#' @param e1 (`CombinationFunction`)\cr left hand side of logical operator.
#' @param e2 (`CombinationFunction`)\cr right hand side of logical operator.
#' @param x (`CombinationFunction`)\cr the function which should be negated.
#'
#' @exportClass CombinationFunction
#' @export CombinationFunction
#'
#' @aliases CombinationFunction-class
#' @name combination_function
#'
#' @examples
#' higher <- function(a) {
#'   force(a)
#'   CombinationFunction(
#'     function(x) {
#'       x > a
#'     }
#'   )
#' }
#'
#' lower <- function(b) {
#'   force(b)
#'   CombinationFunction(
#'     function(x) {
#'       x < b
#'     }
#'   )
#' }
#'
#' c1 <- higher(5)
#' c2 <- lower(10)
#' c3 <- higher(5) & lower(10)
#' c3(7)
CombinationFunction <- methods::setClass("CombinationFunction", contains = "function") # nolint

#' @describeIn combination_function Logical "AND" combination of `CombinationFunction` functions.
#'   The resulting object is of the same class, and evaluates the two argument functions. The result
#'   is then the "AND" of the two individual results.
#' @export
#'
methods::setMethod(
  "&",
  signature = c(e1 = "CombinationFunction", e2 = "CombinationFunction"),
  definition = function(e1, e2) {
    CombinationFunction(function(...) {
      e1(...) && e2(...)
    })
  }
)

#' @describeIn combination_function Logical "OR" combination of `CombinationFunction` functions.
#'   The resulting object is of the same class, and evaluates the two argument functions. The result
#'   is then the "OR" of the two individual results.
#' @export
#'
methods::setMethod(
  "|",
  signature = c(e1 = "CombinationFunction", e2 = "CombinationFunction"),
  definition = function(e1, e2) {
    CombinationFunction(function(...) {
      e1(...) || e2(...)
    })
  }
)

#' @describeIn combination_function Logical negation of `CombinationFunction` functions.
#'   The resulting object is of the same class, and evaluates the original function. The result
#'   is then the opposite of this results.
#' @export
#'
methods::setMethod(
  "!",
  signature = c(x = "CombinationFunction"),
  definition = function(x) {
    CombinationFunction(function(...) {
      !x(...)
    })
  }
)
