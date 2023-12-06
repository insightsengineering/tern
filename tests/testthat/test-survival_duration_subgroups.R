preprocess_adtte <- function(adtte) {
  # Save variable labels before data processing steps.
  adtte_labels <- formatters::var_labels(adtte)

  adtte_mod <- adtte %>%
    dplyr::filter(
      PARAMCD == "OS",
      ARM %in% c("B: Placebo", "A: Drug X"),
      SEX %in% c("M", "F")
    ) %>%
    dplyr::mutate(
      # Reorder levels of ARM to display reference arm before treatment arm.
      ARM = droplevels(forcats::fct_relevel(ARM, "B: Placebo")),
      SEX = droplevels(SEX),
      AVALU = as.character(AVALU),
      is_event = CNSR == 0
    )
  reapply_varlabels(adtte_mod, adtte_labels, is_event = "Event Flag")
}

adtte_local <- tern_ex_adtte %>%
  preprocess_adtte()

testthat::test_that("extract_survival_subgroups functions as expected with valid input and default arguments", {
  adtte <- adtte_local

  result <- extract_survival_subgroups(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM", subgroups = c("SEX", "BMRKR2")),
    data = adtte
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("extract_survival_subgroups works as expected with groups_lists", {
  adtte <- adtte_local

  result <- extract_survival_subgroups(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM", subgroups = c("SEX", "BMRKR2")),
    data = adtte,
    groups_lists = list(
      BMRKR2 = list(
        "low" = "LOW",
        "low/medium" = c("LOW", "MEDIUM"),
        "low/medium/high" = c("LOW", "MEDIUM", "HIGH")
      )
    )
  )
  survtime <- result$survtime
  res <- testthat::expect_silent(survtime[survtime$var == "BMRKR2", "subgroup"])
  testthat::expect_snapshot(res)

  hr <- result$hr
  res <- testthat::expect_silent(hr[hr$var == "BMRKR2", "subgroup"])
  testthat::expect_snapshot(res)
})

testthat::test_that("extract_survival_subgroups functions as expected with NULL subgroups", {
  adtte <- adtte_local

  result <- extract_survival_subgroups(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM"),
    data = adtte
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("a_survival_subgroups functions as expected with valid input", {
  df <- data.frame(
    hr = c(0.1234, 0.5678),
    pval = c(0.00001, 1.302309),
    subgroup = c("M", "F"),
    stringsAsFactors = FALSE
  )

  afun <- a_survival_subgroups(.formats = list("hr" = "xx.xx", pval = "x.xxxx | (<0.0001)"))

  result <- basic_table() %>%
    split_cols_by_multivar(c("hr", "pval")) %>%
    analyze_colvars(afun) %>%
    build_table(df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("tabulate_survival_subgroups functions as expected with valid input", {
  adtte <- adtte_local

  df <- extract_survival_subgroups(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM", subgroups = c("SEX", "BMRKR2")),
    data = adtte
  )

  result <- basic_table() %>%
    tabulate_survival_subgroups(df, time_unit = adtte$AVALU[1])

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("tabulate_survival_subgroups functions as expected with NULL subgroups", {
  adtte <- adtte_local

  df <- extract_survival_subgroups(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM"),
    data = adtte
  )

  result <- basic_table() %>%
    tabulate_survival_subgroups(df, time_unit = adtte$AVALU[1])

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("tabulate_survival_subgroups functions as expected with extreme values in subgroups", {
  adtte <- adtte_local %>%
    dplyr::slice(1:30) %>%
    reapply_varlabels(formatters::var_labels(adtte_local))

  df <- extract_survival_subgroups(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM", subgroups = "REGION1"),
    data = adtte
  )

  result <- basic_table() %>%
    tabulate_survival_subgroups(df, time_unit = adtte$AVALU[1])

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("tabulate_survival_subgroups functions as expected when one arm has 0 records", {
  adtte <- adtte_local

  suppressWarnings(expect_warning(df <- extract_survival_subgroups(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM", subgroups = "RACE"),
    data = adtte
  )))

  result <- basic_table() %>%
    tabulate_survival_subgroups(
      df = df,
      vars = c("n_tot_events", "n", "n_events", "median", "hr", "ci", "pval"),
      time_unit = adtte$AVALU[1]
    )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("tabulate_survival_subgroups works correctly with both `n_tot` and `n_tot_events` in `vars`", {
  adtte <- adtte_local

  suppressWarnings(expect_warning(df <- extract_survival_subgroups(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM", subgroups = "RACE"),
    data = adtte
  )))

  # Both n_tot variables, but no surv time vars.
  result_both <- basic_table() %>%
    tabulate_survival_subgroups(
      df = df,
      vars = c("hr", "ci", "n_tot", "pval", "n_tot_events"),
      time_unit = adtte$AVALU[1]
    )
  # Check that the column indices attributes are correct.
  result_cols_both <- attributes(result_both)[c("col_x", "col_ci", "col_symbol_size")]

  res <- testthat::expect_silent(result_cols_both)
  testthat::expect_snapshot(res)

  # Both n_tot variables and also surv time vars, so we have a reordering of the vars in the table.
  result_both_survtime <- basic_table() %>%
    tabulate_survival_subgroups(
      df = df,
      vars = c("hr", "median", "n_events", "ci", "pval", "n_tot_events", "n", "n_tot"),
      time_unit = adtte$AVALU[1]
    )
  # Check that the column indices attributes are correct.
  result_cols_both_survtime <- attributes(result_both_survtime)[c("col_x", "col_ci", "col_symbol_size")]

  res <- testthat::expect_silent(result_cols_both_survtime)
  testthat::expect_snapshot(res)

  # Check header of table.
  result_header_both_survtime <- to_string_matrix(result_both_survtime,
    with_spaces = FALSE, print_txt_to_copy = FALSE
  )[2, ]

  res <- testthat::expect_silent(result_header_both_survtime)
  testthat::expect_snapshot(res)
})

testthat::test_that("d_survival_subgroups_colvars functions as expected with valid input", {
  vars <- c("n", "n_events", "median", "n_tot_events", "hr", "ci", "pval")

  result <- d_survival_subgroups_colvars(
    vars = vars,
    conf_level = 0.9,
    method = "p-value (log-rank)",
    time_unit = "Months"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("tabulate_survival_subgroups na_str argument works as expected", {
  adtte <- adtte_local

  df <- extract_survival_subgroups(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM", subgroups = c("SEX", "BMRKR2")),
    data = adtte
  )
  df$hr$hr[2:5] <- NA

  result <- basic_table() %>%
    tabulate_survival_subgroups(
      df,
      time_unit = adtte$AVALU[1],
      na_str = "<No data>"
    )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
