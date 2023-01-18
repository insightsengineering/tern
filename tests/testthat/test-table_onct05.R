preprocess_adrs <- function(adrs, n_records = 20) {
  adrs_labels <- formatters::var_labels(adrs)
  adrs <- adrs %>%
    dplyr::filter(PARAMCD == "BESRSPI") %>%
    dplyr::filter(ARM %in% c("A: Drug X", "B: Placebo")) %>%
    dplyr::slice(seq_len(n_records)) %>%
    droplevels() %>%
    dplyr::mutate(
      # Reorder levels of factor to make the placebo group the reference arm.
      ARM = forcats::fct_relevel(ARM, "B: Placebo"),
      rsp = AVALC == "CR"
    )
  formatters::var_labels(adrs) <- c(adrs_labels, "Response")

  adrs
}

adrs <- adrs_raw

testthat::test_that("ONCT05 variant 1 (Objective Response Rate by Subgroup) is produced correctly", {
  adrs <- adrs %>%
    preprocess_adrs(n_records = 200)

  df <- extract_rsp_subgroups(
    variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "STRATA2")),
    data = adrs
  )

  result <- basic_table() %>%
    tabulate_rsp_subgroups(df)

  res <- expect_silent(result)
  expect_snapshot(res)
})

testthat::test_that("ONCT05 variant 2 (Specifying class variables) is produced correctly", {
  adrs <- adrs %>%
    preprocess_adrs(n_records = 200)

  adrs <- adrs %>%
    dplyr::mutate(
      # Reorder levels of SEX.
      SEX = forcats::fct_relevel(SEX, "M", "F"),
      # Reorder levels of STRATA1 by frequency.
      STRATA1 = forcats::fct_infreq(STRATA1)
    )

  df <- extract_rsp_subgroups(
    variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "STRATA1")),
    data = adrs
  )

  result <- basic_table() %>%
    tabulate_rsp_subgroups(df)

  res <- expect_silent(result)
  expect_snapshot(res)
})

testthat::test_that("ONCT05 variant 3 (selecting columns and changing the alpha level) is produced correctly", {
  adrs <- adrs %>%
    preprocess_adrs(n_records = 200)

  df <- extract_rsp_subgroups(
    variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "STRATA2")),
    data = adrs,
    conf_level = 0.9,
    method = "chisq"
  )

  result <- basic_table() %>%
    tabulate_rsp_subgroups(df, vars = c("n_tot", "or", "ci", "pval"))

  res <- expect_silent(result)
  expect_snapshot(res)
})

testthat::test_that("ONCT05 variant 4 (setting values indicating response) is produced correctly", {
  # Define new criteria for responder.
  adrs <- adrs %>%
    preprocess_adrs(n_records = 200) %>%
    dplyr::mutate(
      new_rsp = AVALC %in% c("CR", "PR")
    )

  df <- extract_rsp_subgroups(
    variables = list(rsp = "new_rsp", arm = "ARM", subgroups = c("SEX", "STRATA2")),
    data = adrs
  )

  # Response table.
  result <- basic_table() %>%
    tabulate_rsp_subgroups(df)

  res <- expect_silent(result)
  expect_snapshot(res)
})
