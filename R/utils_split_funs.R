#' Custom Split Functions
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Collection of useful functions that are expanding on the core list of functions
#' provided by `rtables`. See `?rtables::custom_split_funs` and [rtables::make_split_fun()]
#' for more information on how to make a custom split function. All these functions
#' work with [split_rows_by()] argument `split_fun` to modify the way the split
#' happens.
#'
#' @seealso [rtables::make_split_fun()]
#'
#' @name utils_split_funs
NULL

#' @describeIn utils_split_funs split function to place reference group facet last
#'  during post-processing stage.
#'
#' @return
#' * `ref_group_last` returns an utility function that puts the reference group
#'  as last and needs to be assigned to `split_fun`.
#'
#' @examples
#' library(dplyr)
#'
#' dat <- data.frame(
#'   x = factor(letters[1:5], levels = letters[5:1]),
#'   y = 1:5
#' )
#' # ref_group_last
#'
#' # With rtables layout functions
#' basic_table() %>%
#'   split_cols_by("x", ref_group = "c", split_fun = ref_group_last) %>%
#'   analyze("y") %>%
#'   build_table(dat)
#'
#' # With tern layout funcitons
#' adtte_f <- tern_ex_adtte %>%
#'   filter(PARAMCD == "OS") %>%
#'   mutate(
#'     AVAL = day2month(AVAL),
#'     is_event = CNSR == 0
#'   )
#'
#' basic_table() %>%
#'   split_cols_by(var = "ARMCD", ref_group = "ARM B", split_fun = ref_group_last) %>%
#'   add_colcounts() %>%
#'   surv_time(
#'     vars = "AVAL",
#'     var_labels = "Survival Time (Months)",
#'     is_event = "is_event",
#'   ) %>%
#'   build_table(df = adtte_f)
#'
#' @export
ref_group_last <- make_split_fun(
  post = list(
    function(splret, spl, fulldf) {
      if (!"ref_group_value" %in% slotNames(spl)) {
        stop("Reference group is undefined.")
      }

      spl_var <- spl@payload # can be substituted by splret$labels
      init_lvls <- levels(fulldf[[spl_var]])

      if (!all(names(splret$values) %in% init_lvls)) {
        stop("This split function does not work with combination facets.")
      }

      ref_group_pos <- which(init_lvls == spl@ref_group_value)
      reord_lvls <- c(init_lvls[-ref_group_pos], init_lvls[ref_group_pos])
      ord <- match(reord_lvls, names(splret$values))

      make_split_result(
        splret$values[ord],
        splret$datasplit[ord],
        splret$labels[ord]
      )
    }
  )
)
#' @describeIn utils_split_funs split function to keep original order of factor
#'  levels in the split.
#'
#' @return
#' * `keep_level_order` returns an utility function that keeps the original levels'.
#'  It needs to be assigned to `split_fun`.
#'
#' @examples
#' # keep_level_order --------
#' # Even if default would bring ref_group first, the original order puts it last
#' basic_table() %>%
#'   split_cols_by("Species", ref_group = "virginica", split_fun = keep_level_order) %>%
#'   analyze("Sepal.Length") %>%
#'   build_table(iris)
#'
#' @export
keep_level_order <- make_split_fun(
  post = list(
    function(splret, spl, fulldf, ...) {
      ord <- order(names(splret$values))
      make_split_result(
        splret$values[ord],
        splret$datasplit[ord],
        splret$labels[ord]
      )
    }
  )
)
#' @describeIn utils_split_funs split function to change level order based on a integerish
#'   vector or a character vector that represent the split variable's factor levels.
#'
#' @param order (`character` or `integer`)\cr vector of ordering indexes for the split facets.
#'
#' @return
#' * `keep_level_order` returns an utility function that changes the original levels' order,
#'   depending on input `order` and split levels.
#'
#' @examples
#' # level_order --------
#' # Even if default would bring ref_group first, the original order puts it last
#' basic_table() %>%
#'   split_cols_by("Species", split_fun = level_order(c(1, 3, 2))) %>%
#'   analyze("Sepal.Length") %>%
#'   build_table(iris)
#'
#' # character vector
#' new_order <- level_order(levels(iris$Species)[c(1, 3, 2)])
#' basic_table() %>%
#'   split_cols_by("Species", ref_group = "virginica", split_fun = new_order) %>%
#'   analyze("Sepal.Length") %>%
#'   build_table(iris)
#'
#' @export
level_order <- function(order) {
  make_split_fun(
    post = list(
      function(splret, spl, fulldf, ...) {
        if (checkmate::test_integerish(order)) {
          checkmate::assert_integerish(order, lower = 1, upper = length(splret$values))
          ord <- order
        } else {
          checkmate::assert_character(order, len = length(splret$values))
          checkmate::assert_set_equal(order, names(splret$values), ordered = FALSE)
          ord <- match(order, names(splret$values))
        }
        make_split_result(splret$values[ord],
                          splret$datasplit[ord],
                          splret$labels[ord])
      }
    )
  )
}
