#' Patient Counts with the Most Extreme Post-baseline Toxicity Grade per Direction of Abnormality
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Primary analysis variable `.var` indicates the toxicity grade (`factor`), and additional
#' analysis variables are `id` (`character` or `factor`), `param` (`factor`) and `grade_dir` (`factor`).
#' The pre-processing steps are crucial when using this function.
#' For a certain direction (e.g. high or low) this function counts
#' patients in the denominator as number of patients with at least one valid measurement during treatment,
#' and patients in the numerator as follows:
#'   * `1` to `4`: Numerator is number of patients with worst grades 1-4 respectively;
#'   * `Any`: Numerator is number of patients with at least one abnormality, which means grade is different from 0.
#'
#' Pre-processing is crucial when using this function and can be done automatically using the
#' [h_adlb_abnormal_by_worst_grade()] helper function. See the description of this function for details on the
#' necessary pre-processing steps.
#'
#' @inheritParams argument_convention
#' @param .stats (`character`)\cr statistics to select for the table. Run `get_stats("abnormal_by_worst_grade")`
#'   to see available statistics for this function.
#'
#' @seealso [h_adlb_abnormal_by_worst_grade()] which pre-processes `ADLB` data frames to be used in
#'   [count_abnormal_by_worst_grade()].
#'
#' @name abnormal_by_worst_grade
#' @order 1
NULL

#' @describeIn abnormal_by_worst_grade Statistics function which counts patients by worst grade.
#'
#' @return
#' * `s_count_abnormal_by_worst_grade()` returns the single statistic `count_fraction` with grades 1 to 4 and
#'   "Any" results.
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
  first_row <- .spl_context[.spl_context$split == variables[["param"]], ]
  x_lvls <- c(setdiff(levels(df[[.var]]), "0"), "Any")
  result <- split(numeric(0), factor(x_lvls))

  subj <- first_row$full_parent_df[[1]][[variables[["id"]]]]
  subj_cur_col <- subj[first_row$cur_col_subset[[1]]]
  # Some subjects may have a record for high and low directions but
  # should be counted only once.
  denom <- length(unique(subj_cur_col))

  for (lvl in x_lvls) {
    if (lvl != "Any") {
      df_lvl <- df[df[[.var]] == lvl, ]
    } else {
      df_lvl <- df[df[[.var]] != 0, ]
    }
    num <- length(unique(df_lvl[[variables[["id"]]]]))
    fraction <- ifelse(denom == 0, 0, num / denom)
    result[[lvl]] <- formatters::with_label(c(count = num, fraction = fraction), lvl)
  }

  result <- list(count_fraction = result)
  result
}

#' @describeIn abnormal_by_worst_grade Formatted analysis function which is used as `afun`
#'   in `count_abnormal_by_worst_grade()`.
#'
#' @return
#' * `a_count_abnormal_by_worst_grade()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @keywords internal
a_count_abnormal_by_worst_grade <- make_afun( # nolint
  s_count_abnormal_by_worst_grade,
  .formats = c(count_fraction = format_count_fraction)
)

#' @describeIn abnormal_by_worst_grade Layout-creating function which can take statistics function arguments
#'   and additional format arguments. This function is a wrapper for [rtables::analyze()].
#'
#' @return
#' * `count_abnormal_by_worst_grade()` returns a layout object suitable for passing to further layouting functions,
#'   or to [rtables::build_table()]. Adding this function to an `rtable` layout will add formatted rows containing
#'   the statistics from `s_count_abnormal_by_worst_grade()` to the table layout.
#'
#' @examples
#' library(dplyr)
#' library(forcats)
#' adlb <- tern_ex_adlb
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
#' # Pre-processing
#' adlb_f <- adlb %>% h_adlb_abnormal_by_worst_grade()
#'
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
#' @order 2
count_abnormal_by_worst_grade <- function(lyt,
                                          var,
                                          variables = list(
                                            id = "USUBJID",
                                            param = "PARAM",
                                            grade_dir = "GRADE_DIR"
                                          ),
                                          na_str = default_na_str(),
                                          nested = TRUE,
                                          ...,
                                          .stats = NULL,
                                          .formats = NULL,
                                          .labels = NULL,
                                          .indent_mods = NULL) {
  extra_args <- list(variables = variables, ...)

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
    na_str = na_str,
    nested = nested,
    extra_args = extra_args,
    show_labels = "hidden"
  )
}

#' Helper function to prepare `ADLB` for [count_abnormal_by_worst_grade()]
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Helper function to prepare an `ADLB` data frame to be used as input in
#' [count_abnormal_by_worst_grade()]. The following pre-processing steps are applied:
#'
#' 1. `adlb` is filtered on variable `avisit` to only include post-baseline visits.
#' 2. `adlb` is filtered on variables `worst_flag_low` and `worst_flag_high` so that only
#'    worst grades (in either direction) are included.
#' 3. From the standard lab grade variable `atoxgr`, the following two variables are derived
#'    and added to `adlb`:
#'   * A grade direction variable (e.g. `GRADE_DIR`). The variable takes value `"HIGH"` when
#'     `atoxgr > 0`, `"LOW"` when `atoxgr < 0`, and `"ZERO"` otherwise.
#'   * A toxicity grade variable (e.g. `GRADE_ANL`) where all negative values from `atoxgr` are
#'     replaced by their absolute values.
#' 4. Unused factor levels are dropped from `adlb` via [droplevels()].
#'
#' @param adlb (`data.frame`)\cr `ADLB` dataframe.
#' @param atoxgr (`character`)\cr Analysis toxicity grade variable. This must be a `factor`
#'   variable.
#' @param avisit (`character`)\cr Analysis visit variable.
#' @param worst_flag_low (`character`)\cr Worst low lab grade flag variable. This variable is
#'   set to `"Y"` when indicating records of worst low lab grades.
#' @param worst_flag_high (`character`)\cr Worst high lab grade flag variable. This variable is
#'   set to `"Y"` when indicating records of worst high lab grades.
#'
#' @return `h_adlb_abnormal_by_worst_grade()` returns the `adlb` data frame with two new
#'   variables: `GRADE_DIR` and `GRADE_ANL`.
#'
#' @seealso [abnormal_by_worst_grade]
#'
#' @examples
#' h_adlb_abnormal_by_worst_grade(tern_ex_adlb) %>%
#'   dplyr::select(ATOXGR, GRADE_DIR, GRADE_ANL) %>%
#'   head(10)
#'
#' @export
h_adlb_abnormal_by_worst_grade <- function(adlb,
                                           atoxgr = "ATOXGR",
                                           avisit = "AVISIT",
                                           worst_flag_low = "WGRLOFL",
                                           worst_flag_high = "WGRHIFL") {
  adlb %>%
    dplyr::filter(
      !.data[[avisit]] %in% c("SCREENING", "BASELINE"),
      .data[[worst_flag_low]] == "Y" | .data[[worst_flag_high]] == "Y"
    ) %>%
    dplyr::mutate(
      GRADE_DIR = factor(
        dplyr::case_when(
          .data[[atoxgr]] %in% c("-1", "-2", "-3", "-4") ~ "LOW",
          .data[[atoxgr]] == "0" ~ "ZERO",
          .data[[atoxgr]] %in% c("1", "2", "3", "4") ~ "HIGH"
        ),
        levels = c("LOW", "ZERO", "HIGH")
      ),
      GRADE_ANL = forcats::fct_relevel(
        forcats::fct_recode(.data[[atoxgr]], `1` = "-1", `2` = "-2", `3` = "-3", `4` = "-4"),
        c("0", "1", "2", "3", "4")
      )
    ) %>%
    droplevels()
}
