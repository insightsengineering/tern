#' Convert List of Groups to Data Frame
#'
#' This converts a list of group levels into a data frame format which is expected by
#' [rtables::add_combo_levels()].
#'
#' @param groups_list (named `list` of `character`)\cr specifies the new group levels via the names and the
#'   levels that belong to it in the character vectors that are elements of the list.
#'
#' @return [tibble::tibble()] in the required format.
#' @importFrom tibble tibble
#' @export
#'
#' @examples
#' grade_groups <- list(
#'   "Any Grade (%)" = c("1", "2", "3", "4", "5"),
#'   "Grade 3-4 (%)" = c("3", "4"),
#'   "Grade 5 (%)" = "5"
#' )
#' groups_list_to_df(grade_groups)
#'
groups_list_to_df <- function(groups_list) {
  assert_that(
    is_fully_named_list(groups_list),
    all(sapply(groups_list, is.character))
  )
  tibble::tibble(
    valname = make_names(names(groups_list)),
    label = names(groups_list),
    levelcombo = unname(groups_list),
    exargs = replicate(length(groups_list), list())
  )
}


#' Reference and Treatment Group Combination
#'
#' Facilitate the re-combination of groups divided as reference and
#' treatment groups; it helps in arranging groups of
#' columns in the `rtables` framework and teal modules.
#'
#' @inheritParams base::paste
#' @param fct (`factor`)\cr the variable with levels which needs to be grouped.
#' @param ref (`string`)\cr the reference level(s).
#'
#' @return a `list` with first item `ref` (reference) and second item `trt`
#'   (treatment).
#'
#' @export
#' @examples
#'
#' library(rtables)
#'
#' groups <- combine_groups(
#'   fct = DM$ARM,
#'   ref = c("B: Placebo")
#' )
#'
#' basic_table() %>%
#'   split_cols_by_groups("ARM", groups) %>%
#'   add_colcounts() %>%
#'   summarize_vars("AGE") %>%
#'   build_table(DM)
#'
combine_groups <- function(fct,
                           ref = NULL,
                           collapse = "/") {

  assert_that(
    (is.null(ref) || is_valid_character(ref)),
    is_character_or_factor(fct),
    is.string(collapse)
  )

  fct <- as_factor_keep_attributes(fct)

  group_levels <- levels(fct)
  if (is.null(ref)) {
    ref <- group_levels[1]
  } else {
    assert_that(all_elements_in_ref(x = ref, ref = group_levels))
  }

  groups <- list(
    ref = group_levels[group_levels %in% ref],
    trt = group_levels[!group_levels %in% ref]
  )
  setNames(groups, nm = lapply(groups, paste, collapse = collapse))
}

#' Split Columns by Groups of Levels
#'
#' @inheritParams argument_convention
#' @inheritParams groups_list_to_df
#' @param ... additional arguments, see _note_ section.
#'
#' @note The ellipse (`...`) conveys arguments to [rtables::split_cols_by()]
#'   in order, for instance, to control formats (`format`), add a joint column
#'   for all groups (`incl_all`).
#'
#' @seealso [rtables::split_cols_by()]
#' @return the modified layout.
#' @export
#'
#' @examples
#'
#' # 1 - Basic use
#'
#' # Without group combination `split_cols_by_groups` is
#' # equivalent to [rtables::split_cols_by()].
#' basic_table() %>%
#'   split_cols_by_groups("ARM") %>%
#'   add_colcounts() %>%
#'   analyze("AGE") %>%
#'   build_table(DM)
#'
#' # Add a reference column.
#' basic_table() %>%
#'   split_cols_by_groups("ARM", ref_group = "B: Placebo") %>%
#'   add_colcounts() %>%
#'   analyze(
#'     "AGE",
#'     afun = function(x, .ref_group, .in_ref_col){
#'       if (.in_ref_col) {
#'         in_rows("Diff Mean" = rcell(NULL))
#'       } else {
#'         in_rows("Diff Mean" = rcell(mean(x) - mean(.ref_group), format = "xx.xx"))
#'       }
#'     }
#'   ) %>%
#'   build_table(DM)
#'
#' # 2 - Adding group specification
#'
#' # Manual preparation of the groups.
#' groups <- list(
#'   "Arms A+B" = c("A: Drug X", "B: Placebo"),
#'   "Arms A+C" = c("A: Drug X", "C: Combination")
#' )
#'
#' # Use of split_cols_by_groups without reference column.
#' basic_table() %>%
#'   split_cols_by_groups("ARM", groups) %>%
#'   add_colcounts() %>%
#'   analyze("AGE") %>%
#'   build_table(DM)
#'
#' # Including differentiated output in the reference column.
#' basic_table() %>%
#'   split_cols_by_groups("ARM", groups_list = groups, ref_group = "Arms A+B") %>%
#'   analyze(
#'     "AGE",
#'     afun = function(x, .ref_group, .in_ref_col){
#'       if (.in_ref_col) {
#'         in_rows("Diff. of Averages" = rcell(NULL))
#'       } else {
#'         in_rows("Diff. of Averages" = rcell(mean(x) - mean(.ref_group), format = "xx.xx"))
#'       }
#'     }
#'   ) %>%
#'   build_table(DM)
#'
#' # 3 - Binary list dividing factor levels into reference and treatment
#'
#' # `combine_groups` defines reference and treatment.
#' groups <- combine_groups(
#'   fct = DM$ARM,
#'   ref = c("A: Drug X", "B: Placebo")
#' )
#' groups
#'
#' # Use group definition without reference column.
#' basic_table() %>%
#'   split_cols_by_groups("ARM", groups_list = groups) %>%
#'   add_colcounts() %>%
#'   analyze("AGE") %>%
#'   build_table(DM)
#'
#'
#' # Use group definition with reference column (first item of groups).
#' basic_table() %>%
#'   split_cols_by_groups("ARM", groups, ref_group = names(groups)[1]) %>%
#'   add_colcounts() %>%
#'   analyze(
#'     "AGE",
#'     afun = function(x, .ref_group, .in_ref_col){
#'       if (.in_ref_col) {
#'         in_rows("Diff Mean" = rcell(NULL))
#'       } else {
#'         in_rows("Diff Mean" = rcell(mean(x) - mean(.ref_group), format = "xx.xx"))
#'       }
#'     }
#'   ) %>%
#'   build_table(DM)
#'
split_cols_by_groups <- function(lyt,
                                 var,
                                 groups_list = NULL,
                                 ref_group = NULL,
                                 ...) {

  if (is.null(groups_list)) {
    split_cols_by(
      lyt = lyt,
      var = var,
      ref_group = ref_group,
      ...
    )
  } else {
    groups_df <- groups_list_to_df(groups_list)
    if (!is.null(ref_group)) {
      ref_group <- groups_df$valname[groups_df$label == ref_group]
    }
    split_cols_by(
      lyt = lyt,
      var = var,
      split_fun = add_combo_levels(groups_df, keep_levels = groups_df$valname),
      ref_group = ref_group,
      ...
    )
  }
}
