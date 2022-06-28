#' Patient Counts for Laboratory Events (Worsen From Baseline) by Highest Grade Post-Baseline
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Patient count and fraction for laboratory events (worsen from baseline) shift table.
#'
#' @inheritParams argument_convention
#'
#' @name abnormal_by_worst_grade_worsen
NULL

#' Helper Function to Prepare ADLB with Worst Labs
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Helper function to prepare a `df` for generate the patient count shift table
#'
#' @param adlb (`data frame`) \cr `ADLB` dataframe
#' @param worst_flag_low (named `vector`) \cr
#' Worst low post-baseline lab grade flag variable
#' @param worst_flag_high (named `vector`) \cr
#' Worst high post-baseline lab grade flag variable
#' @param direction_var (`string`) \cr
#' Direction variable specifying the direction of the shift table of interest.
#' Only lab records flagged by `L`, `H` or `B` are included in the shift table.
#'  * `L`: low direction only
#'  * `H`: high direction only
#'  * `B`: both low and high directions
#'
#' @return [h_adlb_worsen()] returns the `adlb` `data frame` containing only the
#' worst labs specified according to `worst_flag_low` or `worst_flag_high` for the
#' direction specified according to `direction_var`. For instance, for a lab that is
#' needed for the low direction only, only records flagged by `worst_flag_low` are
#' selected. For a lab that is needed for both low and high directions, the worst
#' low records are selected for the low direction, and the worst high record are selected
#' for the high direction.
#'
#' @examples
#' library(scda)
#' library(dplyr)
#' adlb <- synthetic_cdisc_data("latest")$adlb
#' adsl <- synthetic_cdisc_data("latest")$adsl
#'
#' # The direction variable, GRADDR, is based on metadata
#' adlb <- adlb %>%
#'   mutate(
#'     GRADDR = case_when(
#'       PARAMCD == "ALT" ~ "B",
#'       PARAMCD == "CRP" ~ "L",
#'       PARAMCD == "IGA" ~ "H"
#'     )
#'   ) %>%
#'   filter(SAFFL == "Y" & ONTRTFL == "Y" & GRADDR != "")
#'
#' df <- h_adlb_worsen(
#'   adlb,
#'   worst_flag_low = c("WGRLOFL" = "Y"),
#'   worst_flag_high = c("WGRHIFL" = "Y"),
#'   direction_var = "GRADDR"
#' )
#'
#' @export
h_adlb_worsen <- function(adlb,
                          worst_flag_low = NULL,
                          worst_flag_high = NULL,
                          direction_var) {
  assertthat::assert_that(
    assertthat::is.string(direction_var),
    all(unique(adlb[[direction_var]]) %in% c("B", "L", "H"))
  )
  assert_df_with_variables(adlb, list("Col" = direction_var))

  if (any(unique(adlb[[direction_var]]) == "H")) {
    assert_df_with_variables(adlb, list("High" = names(worst_flag_high)))
  }

  if (any(unique(adlb[[direction_var]]) == "L")) {
    assert_df_with_variables(adlb, list("Low" = names(worst_flag_low)))
  }

  if (any(unique(adlb[[direction_var]]) == "B")) {
    assert_df_with_variables(
      adlb,
      list(
        "Low" = names(worst_flag_low),
        "High" = names(worst_flag_high)
      )
    )
  }

  # extract patients with worst post-baseline lab, either low or high or both
  worst_flag <- c(worst_flag_low, worst_flag_high)
  col_names <- names(worst_flag)
  filter_values <- worst_flag
  temp <- Map(
    function(x, y) which(adlb[[x]] == y),
    col_names,
    filter_values
  )
  position_satisfy_filters <- Reduce(union, temp)

  # select variables of interest
  adlb_f <- adlb[position_satisfy_filters, ]

  # generate subsets for different directionality
  adlb_f_h <- adlb_f[which(adlb_f[[direction_var]] == "H"), ]
  adlb_f_l <- adlb_f[which(adlb_f[[direction_var]] == "L"), ]
  adlb_f_b <- adlb_f[which(adlb_f[[direction_var]] == "B"), ]

  # for labs requiring both high and low, data is duplicated and will be stacked on top of each other
  adlb_f_b_h <- adlb_f_b
  adlb_f_b_l <- adlb_f_b

  # extract data with worst lab
  if (!is.null(worst_flag_high) && !is.null(worst_flag_low)) {

    # change H to High, L to Low
    adlb_f_h[[direction_var]] <- rep("High", nrow(adlb_f_h))
    adlb_f_l[[direction_var]] <- rep("Low", nrow(adlb_f_l))

    # change, B to High and Low
    adlb_f_b_h[[direction_var]] <- rep("High", nrow(adlb_f_b_h))
    adlb_f_b_l[[direction_var]] <- rep("Low", nrow(adlb_f_b_l))

    adlb_out_h <- adlb_f_h[which(adlb_f_h[[names(worst_flag_high)]] == worst_flag_high), ]
    adlb_out_b_h <- adlb_f_b_h[which(adlb_f_b_h[[names(worst_flag_high)]] == worst_flag_high), ]
    adlb_out_l <- adlb_f_l[which(adlb_f_l[[names(worst_flag_low)]] == worst_flag_low), ]
    adlb_out_b_l <- adlb_f_b_l[which(adlb_f_b_l[[names(worst_flag_low)]] == worst_flag_low), ]

    out <- rbind(adlb_out_h, adlb_out_b_h, adlb_out_l, adlb_out_b_l)
  } else if (!is.null(worst_flag_high)) {
    adlb_f_h[[direction_var]] <- rep("High", nrow(adlb_f_h))
    adlb_f_b_h[[direction_var]] <- rep("High", nrow(adlb_f_b_h))

    adlb_out_h <- adlb_f_h[which(adlb_f_h[[names(worst_flag_high)]] == worst_flag_high), ]
    adlb_out_b_h <- adlb_f_b_h[which(adlb_f_b_h[[names(worst_flag_high)]] == worst_flag_high), ]

    out <- rbind(adlb_out_h, adlb_out_b_h)
  } else if (!is.null(worst_flag_low)) {
    adlb_f_l[[direction_var]] <- rep("Low", nrow(adlb_f_l))
    adlb_f_b_l[[direction_var]] <- rep("Low", nrow(adlb_f_b_l))

    adlb_out_l <- adlb_f_l[which(adlb_f_l[[names(worst_flag_low)]] == worst_flag_low), ]
    adlb_out_b_l <- adlb_f_b_l[which(adlb_f_b_l[[names(worst_flag_low)]] == worst_flag_low), ]

    out <- rbind(adlb_out_l, adlb_out_b_l)
  }

  # label
  formatters::var_labels(out) <- formatters::var_labels(adlb_f, fill = FALSE)
  # NA
  out
}

#' Helper Function to Analyze Patients for [s_count_abnormal_lab_worsen_by_baseline()]
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Helper function to count the number of patients and the fraction of patients according to
#' highest post-baseline lab grade variable `.var`, baseline lab grade variable `baseline_var`,
#' and the direction of interest specified in `direction_var`.
#'
#' @inheritParams argument_convention
#' @inheritParams h_adlb_worsen
#' @param baseline_var (`string`) \cr baseline lab grade variable
#'
#' @return [h_worsen_counter()] returns the counts and fraction of patients
#' whose worst post-baseline lab grades are worse than their baseline grades, for
#' post-baseline worst grades "1", "2", "3", "4" and "Any".
#'
#' @examples
#' # `h_worsen_counter`
#' h_worsen_counter(
#'   df %>% filter(PARAMCD == "CRP" & GRADDR == "Low"),
#'   id = "USUBJID",
#'   .var = "ATOXGR",
#'   baseline_var = "BTOXGR",
#'   direction_var = "GRADDR"
#' )
#'
#' @export
h_worsen_counter <- function(df, id, .var, baseline_var, direction_var) {
  assertthat::assert_that(
    assertthat::is.string(id),
    assertthat::is.string(.var),
    assertthat::is.string(baseline_var),
    length(unique(df[[direction_var]])) == 1,
    unique(df[[direction_var]]) %in% c("High", "Low")
  )
  assert_df_with_variables(df, list(val = c(id, .var, baseline_var, direction_var)))

  # remove post-baseline missing
  df <- df[df[[.var]] != "<Missing>", ]

  # obtain directionality
  direction <- unique(df[[direction_var]])

  if (direction == "Low") {
    grade <- -1:-4
    worst_grade <- -4
  } else if (direction == "High") {
    grade <- 1:4
    worst_grade <- 4
  }

  if (nrow(df) > 0) {
    by_grade <- lapply(grade, function(i) {
      # filter baseline values that is less than i or <Missing>
      df_temp <- df[df[[baseline_var]] %in% c((i + sign(i) * -1):(-1 * worst_grade), "<Missing>"), ]
      # num: number of patients with post-baseline worst lab equal to i
      num <- length(unique(df_temp[df_temp[[.var]] %in% i, id, drop = TRUE]))
      # denom: number of patients with baseline values less than i or <missing> and post-baseline in the same direction
      denom <- length(unique(df_temp[[id]]))
      rm(df_temp)
      c(num = num, denom = denom)
    })
  } else {
    by_grade <- lapply(1, function(i) {
      c(num = 0, denom = 0)
    })
  }

  names(by_grade) <- as.character(seq_along(by_grade))

  # baseline grade less 4 or missing
  df_temp <- df[!df[[baseline_var]] %in% worst_grade, ]

  # denom: number of patients with baseline values less than 4 or <missing> and post-baseline in the same direction
  denom <- length(unique(df_temp[, id, drop = TRUE]))

  # condition 1: missing baseline and in the direction of abnormality
  con1 <- which(df_temp[[baseline_var]] == "<Missing>" & df_temp[[.var]] %in% grade)
  df_temp_nm <- df_temp[which(df_temp[[baseline_var]] != "<Missing>" & df_temp[[.var]] %in% grade), ]

  # condition 2: if post-baseline values are present then post-baseline values must be worse than baseline
  if (direction == "Low") {
    con2 <- which(as.numeric(as.character(df_temp_nm[[.var]])) < as.numeric(as.character(df_temp_nm[[baseline_var]])))
  } else {
    con2 <- which(as.numeric(as.character(df_temp_nm[[.var]])) > as.numeric(as.character(df_temp_nm[[baseline_var]])))
  }

  # number of patients satisfy either conditions 1 or 2
  num <- length(unique(df_temp[union(con1, con2), id, drop = TRUE]))

  list(fraction = c(by_grade, list("Any" = c(num = num, denom = denom))))
}

#' @describeIn abnormal_by_worst_grade_worsen Statistics function which calculates the
#' counts and fraction of patients whose worst post-baseline lab grades are worse than
#' their baseline grades, for post-baseline worst grades "1", "2", "3", "4" and "Any".
#'
#' @param variables (named `list` of `string`) \cr list of additional analysis variables including:
#' * `id` (`string`): \cr subject variable name
#' * `baseline_var` (`string`): \cr name of the data column containing baseline toxicity variable
#' * `direction_var` (`string`): See `direction_var` for more detail
#' @return [s_count_abnormal_lab_worsen_by_baseline()] returns the
#' counts and fraction of patients whose worst post-baseline lab grades are worse than
#' their baseline grades, for post-baseline worst grades "1", "2", "3", "4" and "Any".
#'
#' @examples
#' library(scda)
#' library(dplyr)
#' adlb <- synthetic_cdisc_data("latest")$adlb
#' adsl <- synthetic_cdisc_data("latest")$adsl
#'
#' # The direction variable, GRADDR, is based on metadata
#' adlb <- adlb %>%
#'   mutate(
#'     GRADDR = case_when(
#'       PARAMCD == "ALT" ~ "B",
#'       PARAMCD == "CRP" ~ "L",
#'       PARAMCD == "IGA" ~ "H"
#'     )
#'   ) %>%
#'   filter(SAFFL == "Y" & ONTRTFL == "Y" & GRADDR != "")
#'
#' df <- h_adlb_worsen(
#'   adlb,
#'   worst_flag_low = c("WGRLOFL" = "Y"),
#'   worst_flag_high = c("WGRHIFL" = "Y"),
#'   direction_var = "GRADDR"
#' )
#'
#' # Patients with worsening lab grade for CRP in the direction of low
#' tern:::s_count_abnormal_lab_worsen_by_baseline(
#'   df = df %>% filter(ARMCD == "ARM A" & PARAMCD == "CRP"),
#'   .var = "ATOXGR",
#'   variables = list(
#'     id = "USUBJID",
#'     baseline_var = "BTOXGR",
#'     direction_var = "GRADDR"
#'   )
#' )
#'
#' @keywords internal
s_count_abnormal_lab_worsen_by_baseline <- function(df, # nolint
                                                    .var = "ATOXGR",
                                                    variables = list(
                                                      id = "USUBJID",
                                                      baseline_var = "BTOXGR",
                                                      direction_var = "GRADDR"
                                                    )) {
  assertthat::assert_that(
    assertthat::is.string(.var),
    assertthat::is.string(variables$id),
    assertthat::is.string(variables$baseline_var),
    assertthat::is.string(variables$direction_var),
    setequal(names(variables), c("id", "baseline_var", "direction_var"))
  )
  assert_df_with_variables(df, c(aval = .var, variables[1:3]))
  assert_list_of_variables(variables)

  h_worsen_counter(df, variables$id, .var, variables$baseline_var, variables$direction_var)
}


#' @describeIn abnormal_by_worst_grade_worsen
#' Formatted Analysis function which can be further customized by
#' calling [rtables::make_afun()] on it. It is used as `afun` in [rtables::analyze()].
#'
#' @return [a_count_abnormal_lab_worsen_by_baseline()] returns
#' the corresponding list with formatted [rtables::CellValue()].
#'
#' @examples
#' tern:::a_count_abnormal_lab_worsen_by_baseline(
#'   df = df %>% filter(ARMCD == "ARM A" & PARAMCD == "CRP"),
#'   .var = "ATOXGR",
#'   variables = list(id = "USUBJID", baseline_var = "BTOXGR", direction_var = "GRADDR")
#' )
#'
#' @keywords internal
a_count_abnormal_lab_worsen_by_baseline <- make_afun( # nolint
  s_count_abnormal_lab_worsen_by_baseline,
  .formats = c(fraction = format_fraction),
  .ungroup_stats = "fraction"
)

#' @describeIn abnormal_by_worst_grade_worsen Layout
#' creating function which can be used for creating tables, which can take statistics
#' function arguments and additional format arguments (see below).
#'
#' @examples
#' basic_table() %>%
#'   split_cols_by("ARMCD") %>%
#'   add_colcounts() %>%
#'   split_rows_by("PARAMCD") %>%
#'   split_rows_by("GRADDR") %>%
#'   count_abnormal_lab_worsen_by_baseline(
#'     var = "ATOXGR",
#'     variables = list(
#'       id = "USUBJID",
#'       baseline_var = "BTOXGR",
#'       direction_var = "GRADDR"
#'     )
#'   ) %>%
#'   append_topleft("Direction of Abnormality") %>%
#'   build_table(df = df, alt_counts_df = adsl)
#'
#' @export
count_abnormal_lab_worsen_by_baseline <- function(lyt, # nolint
                                                  var,
                                                  ...,
                                                  table_names = NULL,
                                                  .stats = NULL,
                                                  .formats = NULL,
                                                  .labels = NULL,
                                                  .indent_mods = NULL) {
  checkmate::assert_string(var)

  afun <- make_afun(
    a_count_abnormal_lab_worsen_by_baseline,
    .stats = .stats,
    .formats = .formats,
    .labels = .labels,
    .indent_mods = .indent_mods
  )

  lyt <- analyze(
    lyt = lyt,
    vars = var,
    afun = afun,
    extra_args = list(...),
    show_labels = "hidden"
  )

  lyt
}
