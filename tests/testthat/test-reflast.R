split_fun <- make_split_fun(post = list(ref_group_last))

testthat::test_that("ref_group_last split fun gives error when ref group is undefined", {
  lyt <- basic_table() %>%
    split_cols_by(var = "ARMCD", split_fun = split_fun) %>%
    analyze("AGE")

  testthat::expect_error(build_table(lyt, df = tern_ex_adsl))
})

testthat::test_that("ref_group_last split fun gives error when used with combo facets", {
  custom_splitfun <- make_split_fun(
    post = list(
      add_combo_facet("A_C", "Arms A+C", c("A: Drug X", "C: Combination")),
      ref_group_last
    )
  )

  lyt <- basic_table() %>%
    split_cols_by("ARM", ref_group = "B: Placebo", split_fun = custom_splitfun) %>%
    analyze("AGE")

  testthat::expect_error(build_table(lyt, df = tern_ex_adsl))
})

testthat::test_that("analyze_vars works as expected with ref_group_last split fun", {
  result <- basic_table() %>%
    split_cols_by(var = "ARMCD", ref_group = "ARM B", split_fun = split_fun) %>%
    add_colcounts() %>%
    analyze_vars(c("AGE", "STRATA2")) %>%
    build_table(df = tern_ex_adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("compare_vars works as expected with ref_group_last split fun", {
  result <- basic_table() %>%
    split_cols_by(var = "ARMCD", ref_group = "ARM B", split_fun = split_fun) %>%
    add_colcounts() %>%
    compare_vars("AGE") %>%
    build_table(df = tern_ex_adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("summarize_ancova works as expected with ref_group_last split fun", {
  result <- basic_table() %>%
    split_cols_by(var = "ARMCD", ref_group = "ARM B", split_fun = split_fun) %>%
    add_colcounts() %>%
    summarize_ancova(
      vars = "BMRKR1",
      variables = list(arm = "ARM"),
      var_labels = "Unadjusted comparison",
      conf_level = 0.95
    ) %>%
    build_table(tern_ex_adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("binary endpoint layouts work as expected with ref_group_last split fun", {
  adrs_f <- tern_ex_adrs %>%
    dplyr::filter(PARAMCD == "INVET") %>%
    dplyr::mutate(is_rsp = AVALC %in% c("CR", "PR"))

  result <- basic_table() %>%
    split_cols_by(var = "ARM", ref_group = "B: Placebo", split_fun = split_fun) %>%
    add_colcounts() %>%
    estimate_odds_ratio(vars = "is_rsp") %>%
    estimate_proportion_diff(vars = "is_rsp", table_names = "prop_diff") %>%
    test_proportion_diff(vars = "is_rsp", table_names = "test_prop_diff") %>%
    build_table(adrs_f)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("time to event layouts works as expected with ref_group_last split fun", {
  adtte_f <- tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "PFS") %>%
    dplyr::mutate(
      AVAL = day2month(AVAL),
      is_event = CNSR == 0
    )

  result <- basic_table() %>%
    split_cols_by(var = "ARMCD", ref_group = "ARM B", split_fun = split_fun) %>%
    add_colcounts() %>%
    coxph_pairwise(
      vars = "AVAL",
      is_event = "is_event"
    ) %>%
    surv_timepoint(
      vars = "AVAL",
      var_labels = "Months",
      time_point = 6,
      is_event = "is_event",
      method = "both"
    ) %>%
    build_table(adtte_f)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("summarize_ancova works as expected with ref_group_last split fun", {
  anl <- tern_ex_adtte %>% filter(PARAMCD == "TNE")

  result <- basic_table() %>%
    split_cols_by(var = "ARMCD", ref_group = "ARM B", split_fun = split_fun) %>%
    add_colcounts() %>%
    summarize_glm_count(
      vars = "AVAL",
      variables = list(arm = "ARM", offset = "lgTMATRSK", covariates = NULL),
      conf_level = 0.95,
      distribution = "poisson",
      rate_mean_method = "emmeans",
      var_labels = "Unadjusted rate (per year)",
      .stats = c("rate"),
      .labels = c(rate = "Rate")
    ) %>%
    build_table(anl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
