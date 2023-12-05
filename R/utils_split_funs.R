#' Custom Split Functions
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Collection of useful functions that are expanding on the core list of functions
#' provided by `rtables`. See [rtables::custom_split_funs] and [rtables::make_split_fun()]
#' for more information on how to make a custom split function. All these functions
#' work with [split_rows_by()] argument `split_fun` to modify the way the split
#' happens. For other split functions, consider consulting [`rtables::split_funcs`].
#'
#' @seealso [rtables::make_split_fun()]
#'
#' @name utils_split_funs
NULL

#' @describeIn utils_split_funs split function to place reference group facet at a specific position
#'  during post-processing stage.
#'
#' @param position (`string` or `integer`)\cr should it be `"first"` or `"last"` or in a specific position?
#'
#' @return
#' * `ref_group_position` returns an utility function that puts the reference group
#'  as first, last or at a certain position and needs to be assigned to `split_fun`.
#'
#' @examples
#' library(dplyr)
#'
#' dat <- data.frame(
#'   x = factor(letters[1:5], levels = letters[5:1]),
#'   y = 1:5
#' )
#'
#' # With rtables layout functions
#' basic_table() %>%
#'   split_cols_by("x", ref_group = "c", split_fun = ref_group_position("last")) %>%
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
#'   split_cols_by(var = "ARMCD", ref_group = "ARM B", split_fun = ref_group_position("first")) %>%
#'   add_colcounts() %>%
#'   surv_time(
#'     vars = "AVAL",
#'     var_labels = "Survival Time (Months)",
#'     is_event = "is_event",
#'   ) %>%
#'   build_table(df = adtte_f)
#'
#' basic_table() %>%
#'   split_cols_by(var = "ARMCD", ref_group = "ARM B", split_fun = ref_group_position(2)) %>%
#'   add_colcounts() %>%
#'   surv_time(
#'     vars = "AVAL",
#'     var_labels = "Survival Time (Months)",
#'     is_event = "is_event",
#'   ) %>%
#'   build_table(df = adtte_f)
#'
#' @export
ref_group_position <- function(position = "first") {
  make_split_fun(
    post = list(
      function(splret, spl, fulldf) {
        if (!"ref_group_value" %in% methods::slotNames(spl)) {
          stop("Reference group is undefined.")
        }

        spl_var <- rtables:::spl_payload(spl)
        fulldf[[spl_var]] <- factor(fulldf[[spl_var]])
        init_lvls <- levels(fulldf[[spl_var]])

        if (!all(names(splret$values) %in% init_lvls)) {
          stop("This split function does not work with combination facets.")
        }

        ref_group_pos <- which(init_lvls == rtables:::spl_ref_group(spl))
        pos_choices <- c("first", "last")
        if (checkmate::test_choice(position, pos_choices) && position == "first") {
          pos <- 0
        } else if (checkmate::test_choice(position, pos_choices) && position == "last") {
          pos <- length(init_lvls)
        } else if (checkmate::test_int(position, lower = 1, upper = length(init_lvls))) {
          pos <- position - 1
        } else {
          stop("Wrong input for ref group position. It must be 'first', 'last', or a integer.")
        }

        reord_lvls <- append(init_lvls[-ref_group_pos], init_lvls[ref_group_pos], after = pos)
        ord <- match(reord_lvls, names(splret$values))

        make_split_result(
          splret$values[ord],
          splret$datasplit[ord],
          splret$labels[ord]
        )
      }
    )
  )
}

#' @describeIn utils_split_funs split function to change level order based on a `integer`
#'   vector or a `character` vector that represent the split variable's factor levels.
#'
#' @param order (`character` or `integer`)\cr vector of ordering indexes for the split facets.
#'
#' @return
#' * `level_order` returns an utility function that changes the original levels' order,
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
      function(splret, spl, fulldf) {
        if (checkmate::test_integerish(order)) {
          checkmate::assert_integerish(order, lower = 1, upper = length(splret$values))
          ord <- order
        } else {
          checkmate::assert_character(order, len = length(splret$values))
          checkmate::assert_set_equal(order, names(splret$values), ordered = FALSE)
          ord <- match(order, names(splret$values))
        }
        make_split_result(
          splret$values[ord],
          splret$datasplit[ord],
          splret$labels[ord]
        )
      }
    )
  )
}
