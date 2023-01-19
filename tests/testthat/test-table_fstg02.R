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
      is_event = CNSR == 0,
      # Convert time to MONTH
      AVAL = day2month(AVAL),
      AVALU = "Months"
    )

  reapply_varlabels(adtte_mod, adtte_labels, AVAL = adtte_labels["AVAL"])
}

adtte_local <- adtte_raw %>%
  preprocess_adtte()

testthat::test_that("FSTG02 table variant 1 (Subgroup Analysis of Survival Duration) is produced correctly", {
  anl1 <- adtte_local

  df <- extract_survival_subgroups(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM", subgroups = c("SEX", "BMRKR2")),
    data = anl1
  )

  result <- basic_table() %>%
    tabulate_survival_subgroups(
      df = df,
      vars = c("n_tot", "n", "median", "hr", "ci"),
      time_unit = anl1$AVALU[1]
    )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Add plot.
  g_forest(
    tbl = result,
    draw = FALSE
  )
})

testthat::test_that("FSTG02 table variant 2 (specifying class variables and options for the treatment variable)", {
  anl2 <- adtte_local %>%
    dplyr::mutate(
      # Recode levels of arm.
      ARM = forcats::fct_recode(
        ARM,
        "Placebo" = "B: Placebo",
        "Drug X" = "A: Drug X"
      ),
      # Reorder levels of `SEX`.
      SEX = forcats::fct_relevel(SEX, "M", "F"),
      # Reorder levels of `STRATA1`` by frequency.
      STRATA1 = forcats::fct_infreq(STRATA1)
    )

  df <- extract_survival_subgroups(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM", subgroups = c("SEX", "STRATA1")),
    data = anl2
  )

  result <- basic_table() %>%
    tabulate_survival_subgroups(
      df = df,
      vars = c("n_tot", "n", "median", "hr", "ci"),
      time_unit = anl2$AVALU[1]
    )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Add plot.
  g_forest(
    tbl = result,
    draw = FALSE
  )
})

testthat::test_that("FSTG02 table variant 3 (selecting columns and changing the alpha level)", {
  anl3 <- adtte_local

  df <- extract_survival_subgroups(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM", subgroups = c("SEX", "BMRKR2")),
    control = control_coxph(conf_level = 0.9),
    data = anl3
  )

  result <- basic_table() %>%
    tabulate_survival_subgroups(
      df = df,
      vars = c("n_tot", "hr", "ci")
    )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Add plot.
  g_forest(
    tbl = result,
    draw = FALSE
  )
})

testthat::test_that("FSTG02 table variant 4 (fixed symbol size) is produced correctly", {
  anl4 <- adtte_local

  df <- extract_survival_subgroups(
    variables = list(
      tte = "AVAL",
      is_event = "is_event",
      arm = "ARM",
      subgroups = c("SEX", "BMRKR2")
    ),
    data = anl4
  )

  result <- basic_table() %>%
    tabulate_survival_subgroups(
      df = df,
      vars = c("n_tot", "n", "median", "hr", "ci"),
      time_unit = anl4$AVALU[1]
    )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Add plot.
  g_forest(
    tbl = result,
    draw = FALSE
  )
})
