#' Split Function to Place Reference Group Last
#'
#' @param splret
#' @param spl
#' @param fulldf
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' dat2 <- data.frame(
#' x = factor(letters[1:5], levels = letters[5:1]),
#' y = 1:5
#' )
#'
#' basic_table() %>%
#'   split_cols_by("x", ref_group = "a", split_fun = make_split_fun(post = list(ref_last))) %>%
#'   analyze("y") %>%
#'   build_table(dat2)
#'
#' basic_table() %>%
#'   split_cols_by("x", ref_group = "c", split_fun = make_split_fun(post = list(ref_last))) %>%
#'   analyze("y") %>%
#'   build_table(dat2)
#'
#' basic_table() %>%
#'   split_cols_by("x", ref_group = "e", split_fun = make_split_fun(post = list(ref_last))) %>%
#'   analyze("y") %>%
#'   build_table(dat2)
#'
ref_last <- function(splret, spl, fulldf, ...) {

  spl_var <- spl@payload
  init_lvls <- levels(fulldf[[spl_var]])
  ref_group_pos <- which(init_lvls == spl@ref_group_value)
  reord_lvls <- c(init_lvls[-ref_group_pos], init_lvls[ref_group_pos])

  if (identical(names(splret$values), init_lvls)) {

    ord <- match(reord_lvls, init_lvls)

  } else {

    ord <- match(reord_lvls, names(splret$values))
  }

  make_split_result(splret$values[ord],
                    splret$datasplit[ord],
                    splret$labels[ord])
}

# THis isn't quite working yet.
# sf <- make_split_fun(
#   post = list(add_combo_facet("A_C", "Arms A+C", c("A: Drug X", "C: Combination")),
#               ref_last)
# )
#
# basic_table(show_colcounts = TRUE) %>%
#   split_cols_by("ARM", split_fun = sf, ref_group = "B: Placebo") %>%
#   analyze("AGE") %>%
#   build_table(DM)
