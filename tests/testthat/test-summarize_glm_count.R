testthat::test_that("h_glm_poisson glm-fit works with healthy input", {
  anl <- tern_ex_adtte %>%
    filter(PARAMCD == "TNE")
  anl$AVAL_f <- as.factor(anl$AVAL)

  result <- h_glm_poisson(
    .var = "AVAL",
    .df_row = anl,
    variables = list(arm = "ARM", offset = "lgTMATRSK", covariates = NULL)
  )

  mat1 <- summary(result$glm_fit)$coefficients %>% as.data.frame()
  mat1$coefs <- row.names(mat1)
  rownames(mat1) <- NULL
  names(mat1) <- c("Estimate", "SE", "z_value", "Pr", "coefs")

  res <- testthat::expect_silent(mat1)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_glm_poisson emmeans-fit works with healthy input", {
  anl <- tern_ex_adtte %>%
    filter(PARAMCD == "TNE")
  anl$AVAL_f <- as.factor(anl$AVAL)

  result <- h_glm_count(
    .var = "AVAL",
    .df_row = anl,
    variables = list(arm = "ARMCD", offset = "lgTMATRSK", covariates = NULL),
    distribution = "poisson"
  )
  mat1 <- as.data.frame(broom::tidy(result$emmeans_fit))

  res <- testthat::expect_silent(mat1)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_glm_poisson fails wrong inputs", {
  testthat::expect_error(
    h_glm_poisson(
      .var = "wrong.var",
      .df_row = anl,
      variables = list(arm = "ARM", offset = "lgTMATRSK", covariates = NULL)
    )
  )

  testthat::expect_error(
    h_glm_poisson(
      .var = "AVAL",
      .df_row = anl,
      variables = list(arm = "ARM", offset = "lgTMATRSK", covariates = c("wrong.var"))
    )
  )
})

testthat::test_that("h_glm_poisson glm-fit works with healthy input with covariates", {
  anl <- tern_ex_adtte %>%
    filter(PARAMCD == "TNE")
  anl$AVAL_f <- as.factor(anl$AVAL)

  result <- h_glm_poisson(
    .var = "AVAL",
    .df_row = anl,
    variables = list(arm = "ARM", offset = "lgTMATRSK", covariates = c("REGION1"))
  )

  mat1 <- summary(result$glm_fit)$coefficients %>% as.data.frame()
  mat1$coefs <- row.names(mat1)
  rownames(mat1) <- NULL
  names(mat1) <- c("Estimate", "SE", "z_value", "Pr", "coefs")

  res <- testthat::expect_silent(mat1)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_glm_poisson emmeans-fit works with healthy input with covariates", {
  anl <- tern_ex_adtte %>%
    filter(PARAMCD == "TNE")
  anl$AVAL_f <- as.factor(anl$AVAL)

  result <- h_glm_count(
    .var = "AVAL",
    .df_row = anl,
    variables = list(arm = "ARMCD", offset = "lgTMATRSK", covariates = c("REGION1")),
    distribution = "poisson"
  )
  mat1 <- as.data.frame(broom::tidy(result$emmeans_fit))

  res <- testthat::expect_silent(mat1)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_glm_quasipoisson glm-fit works with healthy input", {
  anl <- tern_ex_adtte %>%
    filter(PARAMCD == "TNE")
  anl$AVAL_f <- as.factor(anl$AVAL)

  result <- h_glm_quasipoisson(
    .var = "AVAL",
    .df_row = anl,
    variables = list(arm = "ARM", offset = "lgTMATRSK", covariates = c("REGION1"))
  )

  mat1 <- summary(result$glm_fit)$coefficients %>% as.data.frame()
  mat1$coefs <- row.names(mat1)
  rownames(mat1) <- NULL
  names(mat1) <- c("Estimate", "SE", "z_value", "Pr", "coefs")

  res <- testthat::expect_silent(mat1)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_glm_quasipoisson emmeans-fit works with healthy input", {
  anl <- tern_ex_adtte %>%
    filter(PARAMCD == "TNE")
  anl$AVAL_f <- as.factor(anl$AVAL)

  result <- h_glm_quasipoisson(
    .var = "AVAL",
    .df_row = anl,
    variables = list(arm = "ARM", offset = "lgTMATRSK", covariates = c("REGION1"))
  )
  mat1 <- as.data.frame(broom::tidy(result$emmeans_fit))

  res <- testthat::expect_silent(mat1)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_glm_quasipoisson fails wrong inputs", {
  testthat::expect_error(
    h_glm_quasipoisson(
      .var = "wrong.var",
      .df_row = anl,
      variables = list(arm = "ARM", offset = "lgTMATRSK", covariates = NULL)
    )
  )

  testthat::expect_error(
    h_glm_quasipoisson(
      .var = "AVAL",
      .df_row = anl,
      variables = list(arm = "ARM", offset = "lgTMATRSK", covariates = c("wrong.var"))
    )
  )
})

testthat::test_that("h_glm_negbin glm-fit works with healthy input", {
  anl <- tern_ex_adtte %>%
    filter(PARAMCD == "TNE")
  anl$AVAL_f <- as.factor(anl$AVAL)

  result <- h_glm_negbin(
    .var = "AVAL",
    .df_row = anl,
    variables = list(arm = "ARM", offset = "lgTMATRSK", covariates = c("REGION1"))
  )

  mat1 <- summary(result$glm_fit)$coefficients %>% as.data.frame()
  mat1$coefs <- row.names(mat1)
  rownames(mat1) <- NULL
  names(mat1) <- c("Estimate", "SE", "z_value", "Pr", "coefs")

  res <- testthat::expect_silent(mat1)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_glm_negbin emmeans-fit works with healthy input", {
  anl <- tern_ex_adtte %>%
    filter(PARAMCD == "TNE")
  anl$AVAL_f <- as.factor(anl$AVAL)

  result <- h_glm_negbin(
    .var = "AVAL",
    .df_row = anl,
    variables = list(arm = "ARM", offset = "lgTMATRSK", covariates = c("REGION1"))
  )
  mat1 <- as.data.frame(broom::tidy(result$emmeans_fit))

  res <- testthat::expect_silent(mat1)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_glm_negbin fails wrong inputs", {
  testthat::expect_error(
    h_glm_negbin(
      .var = "wrong.var",
      .df_row = anl,
      variables = list(arm = "ARM", offset = "lgTMATRSK", covariates = NULL)
    )
  )

  testthat::expect_error(
    h_glm_negbin(
      .var = "AVAL",
      .df_row = anl,
      variables = list(arm = "ARM", offset = "lgTMATRSK", covariates = c("wrong.var"))
    )
  )
})

testthat::test_that("h_glm_count glm-fit works with healthy input", {
  anl <- tern_ex_adtte %>%
    filter(PARAMCD == "TNE")
  anl$AVAL_f <- as.factor(anl$AVAL)

  result <- h_glm_count(
    .var = "AVAL",
    .df_row = anl,
    variables = list(arm = "ARMCD", offset = "lgTMATRSK", covariates = NULL),
    distribution = "poisson"
  )

  mat1 <- summary(result$glm_fit)$coefficients %>% as.data.frame()
  mat1$coefs <- row.names(mat1)
  rownames(mat1) <- NULL
  names(mat1) <- c("Estimate", "SE", "z_value", "Pr", "coefs")

  res <- testthat::expect_silent(mat1)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_glm_count emmeans-fit works with healthy input", {
  anl <- tern_ex_adtte %>%
    filter(PARAMCD == "TNE")
  anl$AVAL_f <- as.factor(anl$AVAL)

  result <- h_glm_count(
    .var = "AVAL",
    .df_row = anl,
    variables = list(arm = "ARMCD", offset = "lgTMATRSK", covariates = NULL),
    distribution = "poisson"
  )
  mat1 <- as.data.frame(broom::tidy(result$emmeans_fit))

  res <- testthat::expect_silent(mat1)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_glm_count fails wrong inputs", {
  testthat::expect_error(
    h_glm_count(
      .var = "wrong.var",
      .df_row = anl,
      variables = list(arm = "ARM", offset = "lgTMATRSK", covariates = NULL)
    )
  )

  testthat::expect_error(
    h_glm_count(
      .var = "AVAL",
      .df_row = anl,
      variables = list(arm = "ARM", offset = "lgTMATRSK", covariates = c("wrong.var"))
    )
  )
})

testthat::test_that("h_ppmeans works with healthy input", {
  set.seed(2)
  anl <- tern_ex_adtte %>%
    filter(PARAMCD == "TNE")
  anl$AVAL_f <- as.factor(anl$AVAL)

  fits <- h_glm_count(
    .var = "AVAL",
    .df_row = anl,
    variables = list(arm = "ARMCD", offset = "lgTMATRSK", covariates = c("REGION1")),
    distribution = "poisson"
  )

  testthat::expect_snapshot(fits)

  fits2 <- h_glm_count(
    .var = "AVAL",
    .df_row = anl,
    variables = list(arm = "ARMCD", offset = "lgTMATRSK", covariates = c("REGION1")),
    distribution = "quasipoisson"
  )

  testthat::expect_snapshot(fits2)

  # XXX ppmeans fails snapshot diff in integration tests
  testthat::expect_silent(
    result <- h_ppmeans(
      obj = fits$glm_fit,
      .df_row = anl,
      arm = "ARM",
      conf_level = 0.95
    ) # diff
  )
})

testthat::test_that("s_glm_count works with healthy input", {
  set.seed(2)
  anl <- tern_ex_adtte %>%
    filter(PARAMCD == "TNE")
  anl$AVAL_f <- as.factor(anl$AVAL)

  result <- s_glm_count(
    df = anl %>%
      filter(ARMCD == "ARM B"),
    .df_row = anl,
    .var = "AVAL",
    .in_ref_col = TRUE,
    variables = list(arm = "ARMCD", offset = "lgTMATRSK", covariates = c("REGION1")),
    conf_level = 0.95,
    distribution = "poisson",
    rate_mean_method = "emmeans" # XXX ppmeans fails snapshot diff in integration tests
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res) # diff
})

testthat::test_that("s_glm_count (negative binomial) works with healthy input", {
  set.seed(2)
  anl <- tern_ex_adtte %>%
    filter(PARAMCD == "TNE")
  anl$AVAL_f <- as.factor(anl$AVAL)

  result <- s_glm_count(
    df = anl %>%
      filter(ARMCD == "ARM B"),
    .df_row = anl,
    .var = "AVAL",
    .in_ref_col = TRUE,
    variables = list(arm = "ARMCD", offset = "lgTMATRSK", covariates = c("REGION1")),
    conf_level = 0.95,
    distribution = "negbin",
    rate_mean_method = "emmeans" # XXX ppmeans fails snapshot diff in integration tests
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res) # diff
})

testthat::test_that("s_glm_count works with no reference group selected.", {
  set.seed(2)
  anl <- tern_ex_adtte %>%
    filter(PARAMCD == "TNE")
  anl$AVAL_f <- as.factor(anl$AVAL)

  result <- s_glm_count(
    df = anl %>%
      filter(ARMCD == "ARM B"),
    .df_row = anl,
    .var = "AVAL",
    .in_ref_col = FALSE,
    .ref_group = anl %>%
      filter(ARMCD == "ARM B"),
    variables = list(arm = "ARMCD", offset = "lgTMATRSK", covariates = c("REGION1")),
    conf_level = 0.95,
    distribution = "poisson",
    rate_mean_method = "emmeans" # XXX ppmeans fails snapshot diff in integration tests
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res) # diff
})

testthat::test_that("s_glm_count (negative binomial) works with no reference group selected.", {
  set.seed(2)
  anl <- tern_ex_adtte %>%
    filter(PARAMCD == "TNE")
  anl$AVAL_f <- as.factor(anl$AVAL)

  result <- s_glm_count(
    df = anl %>%
      filter(ARMCD == "ARM B"),
    .df_row = anl,
    .var = "AVAL",
    .in_ref_col = FALSE,
    .ref_group = anl %>%
      filter(ARMCD == "ARM B"),
    variables = list(arm = "ARMCD", offset = "lgTMATRSK", covariates = c("REGION1")),
    conf_level = 0.95,
    distribution = "negbin",
    rate_mean_method = "emmeans" # XXX ppmeans fails snapshot diff in integration tests
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res) # diff
})

testthat::test_that("s_glm_count fails wrong inputs", {
  testthat::expect_error(s_glm_count(
    df = anl %>%
      filter(ARMCD == "ARM B"),
    .df_row = anl,
    .var = "AVAL",
    .in_ref_col = FALSE,
    variables = list(arm = "ARMCD", offset = "lgTMATRSK", covariates = c("REGION1")),
    conf_level = 0.95,
    distribution = "quasipoisson",
    rate_mean_method = "ppmeans"
  ))
})

testthat::test_that("summarize_glm_count works with healthy inputs", {
  anl <- tern_ex_adtte %>%
    filter(PARAMCD == "TNE")
  anl$AVAL_f <- as.factor(anl$AVAL)
  result <- basic_table() %>%
    split_cols_by("ARM", ref_group = "B: Placebo", split_fun = ref_group_position("first")) %>%
    add_colcounts() %>%
    analyze_vars(
      "AVAL_f",
      var_labels = "Number of exacerbations per patient",
      .stats = c("count_fraction"),
      .formats = c("count_fraction" = "xx (xx.xx%)"),
      .label = c("Number of exacerbations per patient")
    ) %>%
    summarize_glm_count(
      vars = "AVAL",
      variables = list(arm = "ARM", offset = "lgTMATRSK", covariates = NULL),
      conf_level = 0.95,
      distribution = "poisson",
      rate_mean_method = "emmeans",
      var_labels = "Unadjusted exacerbation rate (per year)",
      table_names = "unadj",
      .stats = c("rate"),
      .labels = c(rate = "Rate")
    ) %>%
    build_table(anl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("summarize_glm_count (negative binomial) works with healthy inputs", {
  anl <- tern_ex_adtte %>%
    filter(PARAMCD == "TNE")
  anl$AVAL_f <- as.factor(anl$AVAL)
  result <- basic_table() %>%
    split_cols_by("ARM", ref_group = "B: Placebo", split_fun = ref_group_position("first")) %>%
    add_colcounts() %>%
    analyze_vars(
      "AVAL_f",
      var_labels = "Number of exacerbations per patient",
      .stats = c("count_fraction"),
      .formats = c("count_fraction" = "xx (xx.xx%)"),
      .label = c("Number of exacerbations per patient")
    ) %>%
    summarize_glm_count(
      vars = "AVAL",
      variables = list(arm = "ARM", offset = "lgTMATRSK", covariates = NULL),
      conf_level = 0.95,
      distribution = "negbin",
      rate_mean_method = "emmeans",
      var_labels = "Unadjusted exacerbation rate (per year)",
      table_names = "unadj",
      .stats = c("rate"),
      .labels = c(rate = "Rate")
    ) %>%
    build_table(anl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
