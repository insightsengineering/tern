#' Counting Patients Summing Exposure Across All Patients in Columns
#'
#' Counting the number of patients and summing analysis value (i.e exposure values) across all patients
#' when a column table layout is required.
#'
#' @name summarize_patients_exposure_in_cols
#'
NULL

#' @describeIn summarize_patients_exposure_in_cols Statistics function which counts numbers
#'  of patients and the sum of exposure across all patients.
#' @inheritParams argument_convention
#' @param custom_label (`string` or `NULL`)\cr if provided and `labelstr` is empty then this will
#' be used as label.
#' @export
#' @return [s_count_patients_sum_exposure()] returns a list with the statistics:\cr
#' - `n_patients`: number of unique patients in `df`.
#' - `sum_exposure`: sum of `.var` across all patients in `df`.
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   USUBJID = c(paste("id", seq(1, 12), sep = "")),
#'   ARMCD = c(rep("ARM A", 6), rep("ARM B", 6)),
#'   SEX = c(rep("Female", 6), rep("Male", 6)),
#'   AVAL = as.numeric(sample(seq(1, 20), 12)),
#'   stringsAsFactors = TRUE
#' )
#' adsl <- data.frame(
#'   USUBJID = c(paste("id", seq(1, 12), sep = "")),
#'   ARMCD = c(rep("ARM A", 2), rep("ARM B", 2)),
#'   SEX = c(rep("Female", 2), rep("Male", 2)),
#'   stringsAsFactors = TRUE
#' )
#'
#' s_count_patients_sum_exposure(df = df, .N_col = nrow(adsl))
#' s_count_patients_sum_exposure(
#'   df = df,
#'   .N_col = nrow(adsl),
#'   custom_label = "some user's custom label"
#' )
s_count_patients_sum_exposure <- function(df, # nolintr
                                          .var = "AVAL",
                                          id = "USUBJID",
                                          labelstr = "",
                                          .N_col, # nolintr
                                          custom_label = NULL) {
  assertthat::assert_that(
    is.data.frame(df),
    assertthat::is.string(id),
    assertthat::is.string(labelstr),
    is.null(custom_label) || assertthat::is.string(custom_label),
    is_df_with_variables(df, list(.var = .var, id = id)),
    is.numeric(df[[.var]])
  )

  row_label <- if (labelstr != "") {
    labelstr
  } else if (!is.null(custom_label)) {
    custom_label
  } else {
    "Total patients numbers/person time"
  }

  y <- list()

  y$n_patients <-
    formatable::with_label(
      s_num_patients_content(
        df = df,
        .N_col = .N_col, # nolintr
        .var = id,
        labelstr = ""
      )$unique,
      row_label
    )

  y$sum_exposure <- formatable::with_label(sum(df[[.var]]), row_label)
  y
}

#' @describeIn summarize_patients_exposure_in_cols Layout creating function which adds the count
#'   statistics of patients and the sum of analysis value in the column layout as content rows.
#' @inheritParams argument_convention
#' @param col_split (`flag`)\cr whether the columns should be split.
#'  Set to `FALSE` when the required column split has been done already earlier in the layout pipe.
#' @export
#' @examples
#'
#' lyt <- basic_table() %>%
#'   split_cols_by("ARMCD", split_fun = add_overall_level("Total", first = FALSE)) %>%
#'   summarize_patients_exposure_in_cols(var = "AVAL", col_split = TRUE) %>%
#'   split_rows_by("SEX") %>%
#'   summarize_patients_exposure_in_cols(var = "AVAL", col_split = FALSE)
#' result <- build_table(lyt, df = df, alt_counts_df = adsl)
#' result
#'
#' lyt2 <- basic_table() %>%
#'   split_cols_by("ARMCD", split_fun = add_overall_level("Total", first = FALSE)) %>%
#'   summarize_patients_exposure_in_cols(
#'     var = "AVAL", col_split = TRUE,
#'     .stats = "n_patients", custom_label = "some custom label"
#'   ) %>%
#'   split_rows_by("SEX") %>%
#'   summarize_patients_exposure_in_cols(var = "AVAL", col_split = FALSE)
#' result2 <- build_table(lyt2, df = df, alt_counts_df = adsl)
#' result2
#'
#' lyt3 <- basic_table() %>%
#'   summarize_patients_exposure_in_cols(var = "AVAL", col_split = TRUE)
#' result3 <- build_table(lyt3, df = df, alt_counts_df = adsl)
#' result3
#'
#' lyt4 <- basic_table() %>%
#'   summarize_patients_exposure_in_cols(var = "AVAL", col_split = TRUE, .stats = "sum_exposure")
#' result4 <- build_table(lyt4, df = df, alt_counts_df = adsl)
#' result4
summarize_patients_exposure_in_cols <- function(lyt, # nolintr
                                                var,
                                                ...,
                                                .stats = c("n_patients", "sum_exposure"),
                                                .labels = c(n_patients = "Patients", sum_exposure = "Person time"),
                                                .indent_mods = NULL,
                                                col_split = TRUE) {
  afun_list <- Map(function(stat) {
    make_afun(
      s_count_patients_sum_exposure,
      .stats = stat,
      .formats = ifelse(stat == "n_patients", "xx (xx.x%)", "xx")
    )
  },
  stat = .stats
  )

  if (col_split) {
    lyt <- split_cols_by_multivar(
      lyt = lyt,
      vars = rep(var, length(.stats)),
      varlabels = .labels[.stats]
    )
  }
  summarize_row_groups(
    lyt = lyt,
    var = var,
    cfun = afun_list,
    extra_args = list(...)
  )
}
