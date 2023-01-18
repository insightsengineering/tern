# Test the single variant for VST01

gen_advs <- function() {
  advs <- advs_raw # nolintr
  advs_label <- formatters::var_labels(advs)

  advs <- advs %>%
    dplyr::filter(PARAMCD == "DIABP" & PARAM == "Diastolic Blood Pressure") %>%
    dplyr::mutate(PARAMCD = droplevels(PARAMCD), PARAM = droplevels(PARAM)) # nolint

  # post-baseline
  advs_pb <- advs %>% dplyr::filter(ABLFL != "Y" & ABLFL2 != "Y") # nolint

  advs_pb_max <- advs_pb %>%
    dplyr::group_by(PARAM, USUBJID) %>%
    dplyr::arrange(dplyr::desc(AVAL)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(AVISIT = "Post-Baseline Maximum")

  advs_pb_min <- advs_pb %>%
    dplyr::group_by(PARAM, USUBJID) %>%
    dplyr::arrange(AVAL) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(AVISIT = "Post-Baseline Minimum")

  advs_pb_last <- advs_pb %>%
    dplyr::group_by(PARAM, USUBJID) %>%
    dplyr::arrange(dplyr::desc(AVISITN)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(AVISIT = "Post-Baseline Last")

  advs_f <- rbind(
    advs,
    advs_pb_last,
    advs_pb_min,
    advs_pb_max
  )

  formatters::var_labels(advs_f) <- advs_label
  advs_f <- advs_f %>% dplyr::mutate(AVISIT = droplevels(AVISIT)) # nolint
  advs_f
}

testthat::test_that("VST01 default variant is produced correctly", {
  skip_if_too_deep(3)

  advs <- gen_advs()
  advs_baseline <- advs %>% dplyr::filter(ABLFL == "Y") # nolint
  df_adsl <- unique(advs[c("USUBJID", "ARM")])

  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    split_rows_by("AVISIT") %>%
    split_cols_by_multivar(
      vars = c("AVAL", "CHG"),
      varlabels = c("Value at Visit", "Change from Baseline")
    ) %>%
    summarize_colvars(.labels = c(range = "Min - Max")) %>%
    build_table(df = advs, alt_counts_df = df_adsl)

  res <- expect_silent(result)
  expect_snapshot(res)
})
