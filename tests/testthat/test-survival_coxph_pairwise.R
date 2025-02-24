testthat::test_that("s_coxph_pairwise works with default arguments and no stratification factors", {
  adtte_f <- tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)
  df <- adtte_f %>% dplyr::filter(ARMCD == "ARM A")
  df_ref <- adtte_f %>% dplyr::filter(ARMCD == "ARM B")

  result <- s_coxph_pairwise(
    df = df,
    .ref_group = df_ref,
    .in_ref_col = FALSE,
    .var = "AVAL",
    is_event = "is_event",
    strata = NULL
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_coxph_pairwise works with customized arguments and no stratification factors", {
  adtte_f <- tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)
  df <- adtte_f %>% dplyr::filter(ARMCD == "ARM A")
  df_ref <- adtte_f %>% dplyr::filter(ARMCD == "ARM B")

  result <- s_coxph_pairwise(
    df = df,
    .ref_group = df_ref,
    .in_ref_col = FALSE,
    .var = "AVAL",
    is_event = "is_event",
    strata = NULL,
    control = control_coxph(pval_method = "wald", ties = "breslow", conf_level = 0.9)
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_coxph_pairwise works with default arguments and stratification factors", {
  adtte_f <- tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)
  df <- adtte_f %>% dplyr::filter(ARMCD == "ARM A")
  df_ref <- adtte_f %>% dplyr::filter(ARMCD == "ARM B")

  result <- s_coxph_pairwise(
    df = df,
    .ref_group = df_ref,
    .in_ref_col = FALSE,
    .var = "AVAL",
    is_event = "is_event",
    strata = c("SEX", "RACE")
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_coxph_pairwise works with customized arguments and stratification factors", {
  adtte_f <- tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)
  df <- adtte_f %>% dplyr::filter(ARMCD == "ARM A")
  df_ref <- adtte_f %>% dplyr::filter(ARMCD == "ARM B")

  result <- s_coxph_pairwise(
    df = df,
    .ref_group = df_ref,
    .in_ref_col = FALSE,
    .var = "AVAL",
    is_event = "is_event",
    strata = c("SEX", "RACE"),
    control = control_coxph(pval_method = "wald", ties = "breslow", conf_level = 0.9)
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("coxph_pairwise works with default arguments and no stratification factors", {
  adtte_f <- tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)

  result <- basic_table() %>%
    split_cols_by(
      var = "ARMCD",
      ref_group = "ARM A"
    ) %>%
    coxph_pairwise(
      vars = "AVAL",
      is_event = "is_event",
      var_labels = c("Unstratified Analysis"),
      strata = NULL
    ) %>%
    build_table(df = adtte_f)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("coxph_pairwise works with customized arguments and no stratification factors", {
  adtte_f <- tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)

  result <- basic_table() %>%
    split_cols_by(
      var = "ARMCD",
      ref_group = "ARM A"
    ) %>%
    coxph_pairwise(
      vars = "AVAL",
      is_event = "is_event",
      var_labels = c("Unstratified Analysis"),
      control = control_coxph(pval_method = "likelihood", conf_level = 0.99),
      strata = NULL
    ) %>%
    build_table(df = adtte_f)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("coxph_pairwise works with default arguments and stratification factors", {
  adtte_f <- tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)

  result <- basic_table() %>%
    split_cols_by(
      var = "ARMCD",
      ref_group = "ARM A"
    ) %>%
    coxph_pairwise(
      vars = "AVAL",
      is_event = "is_event",
      var_labels = c("Stratified Analysis"),
      strata = "SEX"
    ) %>%
    build_table(df = adtte_f)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("coxph_pairwise works with customized arguments and stratification factors", {
  adtte_f <- tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)

  result <- basic_table() %>%
    split_cols_by(
      var = "ARMCD",
      ref_group = "ARM A"
    ) %>%
    coxph_pairwise(
      vars = "AVAL",
      is_event = "is_event",
      var_labels = c("Stratified Analysis"),
      control = control_coxph(pval_method = "likelihood", conf_level = 0.99),
      strata = c("SEX", "RACE"),
      .stats = c("hr", "hr_ci"),
      .formats = c(hr = "xx.xxx", hr_ci = "(xx.xxx, xx.xxx)")
    ) %>%
    build_table(df = adtte_f)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_coxph_pairwise gets p-value (log-rank) calculated by survival::survdiff()", {
  x1 <- runif(1, 200, 400) %>% floor()
  adtte_f <- tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)
  adtte_f <- adtte_f[1:x1, ]
  df <- adtte_f %>% dplyr::filter(ARMCD == "ARM A")
  df_ref <- adtte_f %>% dplyr::filter(ARMCD == "ARM B")

  result <- s_coxph_pairwise(
    df = df,
    .ref_group = df_ref,
    .in_ref_col = FALSE,
    .var = "AVAL",
    is_event = "is_event",
    strata = NULL
  )

  data <- rbind(df_ref, df)
  group <- factor(rep(c("ref", "x"), c(nrow(df_ref), nrow(df))), levels = c("ref", "x"))
  .in_ref_col <- FALSE
  .var <- "AVAL"
  is_event <- "is_event"
  assert_df_with_variables(df, list(tte = .var, is_event = is_event))
  df_cox <- data.frame(
    tte = data[[.var]],
    is_event = data[[is_event]],
    arm = group
  )
  orginal_survdiff <- survival::survdiff(
    survival::Surv(tte, is_event) ~ arm,
    data = df_cox
  )
  log_rank_pvalue <- 1 - pchisq(orginal_survdiff$chisq, length(orginal_survdiff$n) - 1)

  testthat::expect_equal(as.numeric(result$pvalue), log_rank_pvalue, tolerance = 1e-6)
})

testthat::test_that("coxph_pairwise works with NA values", {
  adtte_f <- tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = FALSE)

  testthat::expect_warning(testthat::expect_warning(
    result <- basic_table() %>%
      split_cols_by(
        var = "ARMCD",
        ref_group = "ARM A"
      ) %>%
      coxph_pairwise(
        vars = "AVAL",
        is_event = "is_event",
        var_labels = c("Unstratified Analysis"),
        strata = NULL,
        na_str = "empty"
      ) %>%
      build_table(df = adtte_f)
  ))

  testthat::expect_snapshot(result)
})
