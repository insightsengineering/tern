testthat::test_that("ref_group_position last split fun gives error when ref group is undefined", {
  lyt <- basic_table() %>%
    split_cols_by(var = "ARMCD", split_fun = ref_group_position("last")) %>%
    analyze("AGE")

  testthat::expect_error(build_table(lyt, df = tern_ex_adsl))
})

testthat::test_that("analyze_vars works as expected with ref_group_position last split fun", {
  # Default behavior
  result <- basic_table() %>%
    split_cols_by(var = "ARMCD", ref_group = "ARM C") %>%
    add_colcounts() %>%
    analyze_vars(c("AGE", "STRATA2")) %>%
    build_table(df = tern_ex_adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_identical(names(res), c("ARM A", "ARM B", "ARM C"))

  result <- basic_table() %>%
    split_cols_by(var = "ARMCD", ref_group = "ARM C", split_fun = ref_group_position("last")) %>%
    add_colcounts() %>%
    analyze_vars(c("AGE", "STRATA2")) %>%
    build_table(df = tern_ex_adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res[3:4, ])
})

testthat::test_that("compare_vars works as expected with ref_group first split fun", {
  result <- basic_table() %>%
    split_cols_by(var = "ARMCD", ref_group = "ARM B", split_fun = ref_group_position("first")) %>%
    add_colcounts() %>%
    compare_vars("AGE") %>%
    build_table(df = tern_ex_adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_identical(names(res), c("ARM B", "ARM A", "ARM C"))
  testthat::expect_snapshot(res[1:2, ])
})

testthat::test_that("summarize_ancova works as expected with ref_group position split fun", {
  result <- basic_table() %>%
    split_cols_by(var = "ARMCD", ref_group = "ARM B", split_fun = ref_group_position(2)) %>%
    add_colcounts() %>%
    summarize_ancova(
      vars = "BMRKR1",
      variables = list(arm = "ARM"),
      var_labels = "Unadjusted comparison",
      conf_level = 0.95
    ) %>%
    build_table(tern_ex_adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_identical(names(res), c("ARM A", "ARM B", "ARM C"))
  testthat::expect_snapshot(res[1:2, ])
})

testthat::test_that("binary endpoint layouts work as expected with ref_group_position last split fun", {
  adrs_f <- tern_ex_adrs %>%
    dplyr::filter(PARAMCD == "INVET") %>%
    dplyr::mutate(is_rsp = AVALC %in% c("CR", "PR"))

  result <- basic_table() %>%
    split_cols_by(var = "ARM", ref_group = "B: Placebo", split_fun = ref_group_position("last")) %>%
    add_colcounts() %>%
    estimate_odds_ratio(vars = "is_rsp") %>%
    estimate_proportion_diff(vars = "is_rsp", table_names = "prop_diff") %>%
    test_proportion_diff(vars = "is_rsp", table_names = "test_prop_diff") %>%
    build_table(adrs_f)

  res <- testthat::expect_silent(result)
  testthat::expect_identical(names(res), c("A: Drug X", "C: Combination", "B: Placebo"))
  testthat::expect_snapshot(res)
})

testthat::test_that("time to event layouts works as expected with ref_group_position last split fun", {
  adtte_f <- tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "PFS") %>%
    dplyr::mutate(
      AVAL = day2month(AVAL),
      is_event = CNSR == 0
    )

  result <- basic_table() %>%
    split_cols_by(var = "ARMCD", ref_group = "ARM B", split_fun = ref_group_position("last")) %>%
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

testthat::test_that("summarize_ancova works as expected with ref_group_position last split fun", {
  anl <- tern_ex_adtte %>% filter(PARAMCD == "TNE")

  result <- basic_table() %>%
    split_cols_by(var = "ARMCD", ref_group = "ARM B", split_fun = ref_group_position("last")) %>%
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

testthat::test_that("level_order works for integerish and characters", {
  tbl_int <- basic_table() %>%
    split_cols_by("Species", split_fun = level_order(c(1, 3, 2))) %>%
    analyze("Sepal.Length") %>%
    build_table(iris)

  # character vector
  new_order <- level_order(levels(iris$Species)[c(1, 3, 2)])
  tbl_chr <- basic_table() %>%
    split_cols_by("Species", ref_group = "virginica", split_fun = new_order) %>%
    analyze("Sepal.Length") %>%
    build_table(iris)

  testthat::expect_identical(toString(tbl_int), toString(tbl_chr))
})
