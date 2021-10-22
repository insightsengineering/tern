#' Patient Counts with the Most Extreme Post-baseline Toxicity Grade per Direction of Abnormality
#'
#' Primary analysis variable `.var` indicates the toxicity grade (numeric), and additional
#' analysis variables are `id` (character or factor), `param` (`factor`) and `anrind` (`factor`).
#' The pre-processing steps are crucial when using this function.
#' For a certain direction (e.g. high or low) this function counts
#' patients in the denominator as number of patients with at least one valid measurement during treatment,
#' and patients in the numerator as follows:
#'  * `1` to `4`: Numerator is number of patients with worst grades 1-4 respectively;
#'  * `Any`: Numerator is number of patients with at least one abnormality, which means grade is different from 0.
#'
#' @details
#' The pre-processing steps are crucial when using this function.
#' * New Reference Indicator variable `GRADE_DIR` is created in order not to consider
#'  the records where grade is 0 and range indicator is HIGH/LOW. This will replace `ANRIND`.
#' * New Toxicity Grade variable `GRADE_ANL` is created to replace `ATOXGR` where all negative
#' `ATOXGR` values are replaced by their absolute values.
#' * `df` is filtered out to include only post-baseline records with worst grade flags.
#'
#' @inheritParams argument_convention
#' @param abnormal (`character`)\cr identifying the abnormality direction.
#'
#' @name abnormal_by_worst_grade
#'
NULL

#' @describeIn abnormal_by_worst_grade Statistics function which counts patients with worst grade
#'   for a single `abnormal` level, consisting of counts and percentages of patients with worst grade
#'   `1` to `4`, and `Any` non-zero grade.
#' @return [s_count_abnormal_by_worst_grade()] the single statistic `count_fraction` with grade 1 to 4
#'   and "Any" results.
#'
#' @importFrom stats setNames
#'
#' @export
#'
#' @examples
#' library(scda)
#' library(dplyr)
#' library(forcats)
#'
#' adlb <- synthetic_cdisc_data("latest")$adlb
#'
#' # Data set is modified in order to have some
#'  parameters with grades only in one direction
#' # and simulate the real data.
#' adlb$ATOXGR[
#' adlb$PARAMCD == "ALT" & adlb$ATOXGR %in% c("1", "2", "3", "4")
#' ] <- "-1"
#' adlb$ANRIND[adlb$PARAMCD == "ALT" & adlb$ANRIND == "HIGH"] <- "LOW"
#' adlb$WGRHIFL[adlb$PARAMCD == "ALT"] <- ""
#'
#' adlb$ATOXGR[
#' adlb$PARAMCD == "IGA" & adlb$ATOXGR %in% c("-1", "-2", "-3", "-4")
#' ] <- "1"
#' adlb$ANRIND[adlb$PARAMCD == "IGA" & adlb$ANRIND == "LOW"] <- "HIGH"
#' adlb$WGRLOFL[adlb$PARAMCD == "IGA"] <- ""
#'
#' # Here starts the real preprocessing.
#' adlb_f <- adlb %>%
#'   filter(!AVISIT %in% c("SCREENING", "BASELINE")) %>%
#'   mutate(
#'     WGRLOFL = case_when(WGRLOFL == "Y" ~ TRUE, TRUE ~ FALSE),
#'     WGRHIFL = case_when(WGRHIFL == "Y" ~ TRUE, TRUE ~ FALSE),
#'     GRADE_DIR = factor(case_when(
#'      ATOXGR %in% c("-1", "-2", "-3", "-4") ~ "LOW",
#'      ATOXGR == "0" ~ "ZERO",
#'      ATOXGR %in% c("1", "2", "3", "4") ~ "HIGH"),
#'      levels = c("LOW", "ZERO", "HIGH")),
#'     GRADE_ANL = fct_relevel(
#'      fct_recode(ATOXGR,
#'       `1` = "-1", `2` = "-2", `3` = "-3", `4` = "-4"),
#'        c("0", "1", "2", "3", "4")
#'      )
#'    ) %>%
#'   filter(WGRLOFL == TRUE | WGRHIFL == TRUE) %>%
#'   droplevels()
#'
#' adlb_f_alt <- adlb_f %>% filter(PARAMCD == "ALT") %>% droplevels()
#' full_parent_df <- list(adlb_f_alt, "not_needed")
#' cur_col_subset <- list(adlb_f_alt$ARMCD == "ARM A", "not_needed")
#'
#' spl_context <- data.frame(
#'   split = c("PARAM", "GRADE_DIR"),
#'   full_parent_df = I(full_parent_df),
#'   cur_col_subset = I(cur_col_subset)
#' )
#' s_count_abnormal_by_worst_grade(
#'   df = adlb_f %>% filter(
#'   ARMCD == "ARM A" & PARAMCD == "ALT" & GRADE_DIR == "LOW") %>%
#'   droplevels(),
#'   .spl_context = spl_context,
#'   .var = "GRADE_ANL",
#'   variables = list(id = "USUBJID", param  = "PARAM",  anrind = "GRADE_DIR"))
#'
s_count_abnormal_by_worst_grade <- function(df = adlb_f, #nolint
                                            .var = "GRADE_ANL",
                                            .spl_context,
                                            variables = list(
                                              id = "USUBJID",
                                              param  = "PARAM",
                                              anrind = "GRADE_DIR"
                                            )) {

  assert_that(
    is.string(.var),
    is.list(variables),
    is.string(variables$id),
    is.string(variables$param),
    is.string(variables$anrind),
    all(names(variables) %in% c("id", "param", "anrind")),
    is_df_with_variables(df, c(a = .var, variables)),
    is.factor(df[[.var]]),
    is_character_or_factor(df[[variables$id]]),
    is.factor(df[[variables[["param"]]]]),
    all(!is.na(df[[.var]]))
  )
  #To verify that the `split_rows_by` are performed with correct variables.
  #Informative message
  assert_that(all(c(variables[["param"]], variables[["anrind"]]) %in% .spl_context$split),
              msg = paste(
                "variabes$param and variables$anrind are not taking the",
                "same variables as the ones used for splitting rows in the layout.")
             )
  first_row <- .spl_context[.spl_context$split == variables[["param"]], ] #nolint
  x_lvls <- c(setdiff(levels(df[[.var]]), "0"), "Any")
  result <- split(numeric(0), factor(x_lvls))

  subj <- first_row$full_parent_df[[1]][[variables[["id"]]]]
  subj_cur_col <- subj[first_row$cur_col_subset[[1]]]
  # some subject may have a record for HI flag and LO flag but
  #should be counted only once.
  denom <- length(unique(subj_cur_col))

  for (lvl in x_lvls) {
    if (lvl != "Any") {

      num <- sum(df[[.var]] == lvl)
      fraction <- ifelse(denom == 0, 0, num / denom)

    } else {

      num <- sum(df[[.var]] != 0)
      fraction <- ifelse(denom == 0, 0, num / denom)

    }
    result[[lvl]] <- with_label(c(count = num, fraction = fraction), lvl)
  }

  result <- list(count_fraction = result)
  result
}

#' @describeIn abnormal_by_worst_grade Formatted Analysis function which can be further customized by calling
#'   [rtables::make_afun()] on it. It is used as `afun` in [rtables::analyze()].
#' @return [a_count_abnormal_by_worst_grade()] returns the corresponding list with formatted [rtables::CellValue()].
#' @export
#' @examples
#' # Use the Formatted Analysis function for `analyze()`. We need to ungroup `count_fraction` first
#' # so that the rtables formatting function `format_count_fraction()` can be applied correctly.
#' afun <- make_afun(a_count_abnormal_by_worst_grade, .ungroup_stats = "count_fraction", .spl_context = spl_context)
#' afun(
#'   df = adlb_f %>% filter(
#'   ARMCD == "ARM A" & PARAMCD == "ALT" & GRADE_DIR == "LOW") %>%
#'   droplevels()
#' )
#'
a_count_abnormal_by_worst_grade <- make_afun(  #nolint
  s_count_abnormal_by_worst_grade,
  .formats = c(count_fraction = format_count_fraction)
)

#' @describeIn abnormal_by_worst_grade Layout creating function which can be used for creating tables,
#'    which can take statistics function arguments and additional format arguments (see below).
#' @export
#' @examples
#'
#' map <- unique(adlb_f[adlb_f$GRADE_DIR != "ZERO", c("PARAM", "GRADE_DIR", "GRADE_ANL")]) %>%
#'  lapply(as.character) %>%
#'  as.data.frame() %>%
#'  arrange(PARAM, desc(GRADE_DIR), GRADE_ANL)
#'
#' basic_table() %>%
#'   split_cols_by("ARMCD") %>%
#'   split_rows_by("PARAM", split_fun = trim_levels_to_map(map)) %>%
#'   split_rows_by("GRADE_DIR") %>%
#'   count_abnormal_by_worst_grade(
#'     var = "GRADE_ANL",
#'     variables = list(id = "USUBJID", param  = "PARAM", anrind = "GRADE_DIR")
#'   ) %>%
#'   build_table(df = adlb_f)
#'
#'
count_abnormal_by_worst_grade <- function(lyt,
                                          var,
                                          variables,
                                          ...,
                                          table_names = var,
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
    table_names = var,
    extra_args = c(list(variables = variables, list(...))),
    show_labels = "hidden"
  )
}
