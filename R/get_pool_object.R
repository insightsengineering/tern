#' Get cached `rbmi` S3 `pool` object
#'
#' @export
#'
#' @examples
#' get_pool_object()
get_pool_object <- function() {
  if (!("package:tern" %in% search())) {
    stop("cached rbmi S3 pool object will loaded if the tern package is attached.",
         "Please run library(tern) before loading cached object.", call. = FALSE)
  } else {
    get("pool_obj", envir = asNamespace("tern"))
  }
}
