#' Patient Counts with the Most Extreme Post-baseline Toxicity Grade per Direction of Abnormality
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Primary analysis variable `.var` indicates the toxicity grade (factor), and additional
#' analysis variables are `id` (character or factor), `param` (`factor`) and `grade_dir` (`factor`).
#' The pre-processing steps are crucial when using this function.
#' For a certain direction (e.g. high or low) this function counts
#' patients in the denominator as number of patients with at least one valid measurement during treatment,
#' and patients in the numerator as follows:
#'  * `1` to `4`: Numerator is number of patients with worst grades 1-4 respectively;
#'  * `Any`: Numerator is number of patients with at least one abnormality, which means grade is different from 0.
#'
#' @details
#' The pre-processing steps are crucial when using this function. From the standard
#' lab grade variable `ATOXGR`, derive the following two variables:
#' * A grade direction variable (e.g. `GRADE_DIR`) is required in order to obtain
#'   the correct denominators when building the layout as it is used to define row splitting.
#' * A toxicity grade variable (e.g. `GRADE_ANL`) where all negative values from
#'   `ATOXGR` are replaced by their absolute values.
#' * Prior to tabulation, `df` must filtered to include only post-baseline records with worst grade flags.
#'
#' @inheritParams argument_convention
#'
#' @name abnormal_by_worst_grade
#'
NULL

#' @describeIn abnormal_by_worst_grade Statistics function which counts patients with worst grade,
#'   consisting of counts and percentages of patients with worst grade
#'   `1` to `4`, and `Any` non-zero grade.
#' @return [s_count_abnormal_by_worst_grade()] the single statistic `count_fraction` with grade 1 to 4
#'   and "Any" results.
#'
#' @examples
#' library(scda)
#' library(dplyr)
#' library(forcats)
#'
#' adlb <- synthetic_cdisc_data("latest")$adlb
#'
#' # Data is modified in order to have some parameters with grades only in one direction
#' # and simulate the real data.
#' adlb$ATOXGR[adlb$PARAMCD == "ALT" & adlb$ATOXGR %in% c("1", "2", "3", "4")] <- "-1"
#' adlb$ANRIND[adlb$PARAMCD == "ALT" & adlb$ANRIND == "HIGH"] <- "LOW"
#' adlb$WGRHIFL[adlb$PARAMCD == "ALT"] <- ""
#'
#' adlb$ATOXGR[adlb$PARAMCD == "IGA" & adlb$ATOXGR %in% c("-1", "-2", "-3", "-4")] <- "1"
#' adlb$ANRIND[adlb$PARAMCD == "IGA" & adlb$ANRIND == "LOW"] <- "HIGH"
#' adlb$WGRLOFL[adlb$PARAMCD == "IGA"] <- ""
#'
#' # Here starts the real pre-processing.
#' adlb_f <- adlb %>%
#'   filter(!AVISIT %in% c("SCREENING", "BASELINE")) %>%
#'   mutate(
#'     GRADE_DIR = factor(case_when(
#'       ATOXGR %in% c("-1", "-2", "-3", "-4") ~ "LOW",
#'       ATOXGR == "0" ~ "ZERO",
#'       ATOXGR %in% c("1", "2", "3", "4") ~ "HIGH"
#'     ),
#'     levels = c("LOW", "ZERO", "HIGH")
#'     ),
#'     GRADE_ANL = fct_relevel(
#'       fct_recode(ATOXGR, `1` = "-1", `2` = "-2", `3` = "-3", `4` = "-4"),
#'       c("0", "1", "2", "3", "4")
#'     )
#'   ) %>%
#'   filter(WGRLOFL == "Y" | WGRHIFL == "Y") %>%
#'   droplevels()
#'
#' adlb_f_alt <- adlb_f %>%
#'   filter(PARAMCD == "ALT") %>%
#'   droplevels()
#' full_parent_df <- list(adlb_f_alt, "not_needed")
#' cur_col_subset <- list(rep(TRUE, nrow(adlb_f_alt)), "not_needed")
#'
#' # This mimics a split structure on PARAM and GRADE_DIR for a total column
#' spl_context <- data.frame(
#'   split = c("PARAM", "GRADE_DIR"),
#'   full_parent_df = I(full_parent_df),
#'   cur_col_subset = I(cur_col_subset)
#' )
#'
#' # Internal function - s_count_abnormal_by_worst_grade
#' \dontrun{
#' tern:::s_count_abnormal_by_worst_grade(
#'   df = adlb_f_alt,
#'   .spl_context = spl_context,
#'   .var = "GRADE_ANL"
#' )
#' }
#'
#' @keywords internal
s_count_abnormal_by_worst_grade <- function(df, # nolint
                                            .var = "GRADE_ANL",
                                            .spl_context,
                                            variables = list(
                                              id = "USUBJID",
                                              param = "PARAM",
                                              grade_dir = "GRADE_DIR"
                                            )) {
  checkmate::assert_string(.var)
  assert_valid_factor(df[[.var]])
  assert_valid_factor(df[[variables$param]])
  assert_valid_factor(df[[variables$grade_dir]])
  assert_df_with_variables(df, c(a = .var, variables))
  checkmate::assert_multi_class(df[[variables$id]], classes = c("factor", "character"))

  # To verify that the `split_rows_by` are performed with correct variables.
  checkmate::assert_subset(c(variables[["param"]], variables[["grade_dir"]]), .spl_context$split)
  first_row <- .spl_context[.spl_context$split == variables[["param"]], ] # nolint
  x_lvls <- c(setdiff(levels(df[[.var]]), "0"), "Any")
  result <- split(numeric(0), factor(x_lvls))

  subj <- first_row$full_parent_df[[1]][[variables[["id"]]]]
  subj_cur_col <- subj[first_row$cur_col_subset[[1]]]
  # Some subjects may have a record for high and low directions but
  # should be counted only once.
  denom <- length(unique(subj_cur_col))

  for (lvl in x_lvls) {
    if (lvl != "Any") {
      num <- sum(df[[.var]] == lvl)
      fraction <- ifelse(denom == 0, 0, num / denom)
    } else {
      num <- sum(df[[.var]] != 0)
      fraction <- ifelse(denom == 0, 0, num / denom)
    }
    result[[lvl]] <- formatters::with_label(c(count = num, fraction = fraction), lvl)
  }

  result <- list(count_fraction = result)
  result
}

#' @describeIn abnormal_by_worst_grade Formatted Analysis function which can be further customized by calling
#'   [rtables::make_afun()] on it. It is used as `afun` in [rtables::analyze()].
#'
#' @return [a_count_abnormal_by_worst_grade()] returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @examples
#' # Internal function - a_count_abnormal_by_worst_grade
#' \dontrun{
#' # Use the Formatted Analysis function for `analyze()`. We need to ungroup `count_fraction` first
#' # so that the `rtables` formatting function `format_count_fraction()` can be applied correctly.
#' afun <- make_afun(tern:::a_count_abnormal_by_worst_grade, .ungroup_stats = "count_fraction")
#' afun(df = adlb_f_alt, .spl_context = spl_context)
#' }
#'
#' @keywords internal
a_count_abnormal_by_worst_grade <- make_afun( # nolint
  s_count_abnormal_by_worst_grade,
  .formats = c(count_fraction = format_count_fraction)
)

#' @describeIn abnormal_by_worst_grade Layout creating function which can be used for creating tables,
#'    which can take statistics function arguments and additional format arguments (see below).
#'
#' @examples
#' # Map excludes records without abnormal grade since they should not be displayed
#' # in the table.
#' map <- unique(adlb_f[adlb_f$GRADE_DIR != "ZERO", c("PARAM", "GRADE_DIR", "GRADE_ANL")]) %>%
#'   lapply(as.character) %>%
#'   as.data.frame() %>%
#'   arrange(PARAM, desc(GRADE_DIR), GRADE_ANL)
#'
#' basic_table() %>%
#'   split_cols_by("ARMCD") %>%
#'   split_rows_by("PARAM") %>%
#'   split_rows_by("GRADE_DIR", split_fun = trim_levels_to_map(map)) %>%
#'   count_abnormal_by_worst_grade(
#'     var = "GRADE_ANL",
#'     variables = list(id = "USUBJID", param = "PARAM", grade_dir = "GRADE_DIR")
#'   ) %>%
#'   build_table(df = adlb_f)
#'
#' @export
count_abnormal_by_worst_grade <- function(lyt,
                                          var,
                                          ...,
                                          .stats = NULL,
                                          .formats = NULL,
                                          .labels = NULL,
                                          .indent_mods = NULL) {
  afun <- make_afun(
    a_count_abnormal_by_worst_grade,
    .stats = .stats,
    .formats = .formats,
    .labels = .labels,
    .indent_mods = .indent_mods,
    .ungroup_stats = "count_fraction"
  )
  analyze(
    lyt = lyt,
    vars = var,
    afun = afun,
    extra_args = list(...),
    show_labels = "hidden"
  )
}
