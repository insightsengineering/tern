#' Get cached `pool` class object from `rbmi` package
#'
#' @export
#'
#' @examples
#' get_pool_object()
get_pool_object <- function() {
  if (!("package:tern" %in% search())) {
    stop("cached rbmi `pool` class object will loaded if the tern package is attached.",
         "Please run library(tern) before loading cached object.", call. = FALSE)
  } else {
    get("pool_obj", envir = asNamespace("tern"))
  }
}
