testthat::test_that("h_glm_poisson glm-fit works with healthy input", {
  library(scda)
  library(dplyr)
  anl <- synthetic_cdisc_dataset("latest", "adtte") %>%
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


  expected <- structure(
    data.frame(
      "Estimate" = as.numeric(c(2.31126468, -0.06290975, -0.14532927)),
      "SE" = as.numeric(c(0.04981355, 0.07112460, 0.07210257)),
      "z_value" = as.numeric(c(46.3983147, -0.8845006, -2.0155907)),
      "Pr" = as.numeric(c(0.000000, 0.37642606, 0.04384279)),
      "coefs" = c("(Intercept)", "ARMB: Placebo", "ARMC: Combination")
    )
  )

  testthat::expect_equal(mat1, expected, tolerance = 0.0000001)
})

testthat::test_that("h_glm_poisson emmeans-fit works with healthy input", {
  library(scda)
  library(dplyr)
  anl <- synthetic_cdisc_dataset("latest", "adtte") %>%
    filter(PARAMCD == "TNE")
  anl$AVAL_f <- as.factor(anl$AVAL)

  result <- h_glm_count(
    .var = "AVAL",
    .df_row = anl,
    variables = list(arm = "ARMCD", offset = "lgTMATRSK", covariates = NULL),
    distribution = "poisson"
  )
  mat1 <- as.data.frame(broom::tidy(result$emmeans_fit))

  expected <-
    data.frame(
      ARMCD = as.character(c("ARM A", "ARM B", "ARM C")),
      rate = as.numeric(c(10.087174, 9.472141, 8.722757)),
      std.error = as.numeric(c(0.5024779, 0.4808751, 0.4547052)),
      df = as.numeric(c(Inf, Inf, Inf)),
      null = as.numeric(c(1, 1, 1)),
      statistic = as.numeric(c(46.39831, 44.28746, 41.54985)),
      p.value = as.numeric(c(0, 0, 0))
    )

  testthat::expect_equal(mat1$ARMCD, expected$ARMCD, tolerance = 0.0000001)
  testthat::expect_equal(mat1$rate, expected$rate, tolerance = 0.0000001)
  testthat::expect_equal(mat1$std.error, expected$std.error, tolerance = 0.0000001)
  testthat::expect_equal(mat1$df, expected$df, tolerance = 0.0000001)
  testthat::expect_equal(mat1$null, expected$null, tolerance = 0.0000001)
  testthat::expect_equal(mat1$statistic, expected$statistic, tolerance = 0.0000001)
  testthat::expect_equal(mat1$p.value, expected$p.value, tolerance = 0.0000001)
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

testthat::test_that("h_glm_quasipoisson glm-fit works with healthy input", {
  library(scda)
  library(dplyr)
  anl <- synthetic_cdisc_dataset("latest", "adtte") %>%
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


  expected <- structure(
    data.frame(
      "Estimate" = as.numeric(c(
        2.6422450, -0.3607871, -0.3453530, -0.3197254,
        -0.4146191, -0.2957926, -0.0414498, -0.1453444
      )),
      "SE" = as.numeric(c(
        0.4289370, 0.4332569, 0.6608601, 0.8896327, 0.5264360,
        0.5811470, 0.2715915, 0.2725934
      )),
      "z_value" = as.numeric(c(
        6.1599841, -0.8327325, -0.5225811, -0.3593904,
        -0.7875964, -0.5089806, -0.1526182, -0.5331911
      )),
      "Pr" = as.numeric(c(
        1.805018e-09, 4.055027e-01, 6.015610e-01, 7.194966e-01,
        4.314088e-01, 6.110522e-01, 8.787779e-01, 5.942035e-01
      )),
      "coefs" = c(
        "(Intercept)", "REGION1Asia", "REGION1Eurasia", "REGION1Europe",
        "REGION1North America", "REGION1South America", "ARMB: Placebo",
        "ARMC: Combination"
      )
    )
  )

  testthat::expect_equal(mat1, expected, tolerance = 0.0000001)
})

testthat::test_that("h_glm_quasipoisson emmeans-fit works with healthy input", {
  library(scda)
  library(dplyr)
  anl <- synthetic_cdisc_dataset("latest", "adtte") %>%
    filter(PARAMCD == "TNE")
  anl$AVAL_f <- as.factor(anl$AVAL)

  result <- h_glm_quasipoisson(
    .var = "AVAL",
    .df_row = anl,
    variables = list(arm = "ARM", offset = "lgTMATRSK", covariates = c("REGION1"))
  )
  mat1 <- as.data.frame(broom::tidy(result$emmeans_fit))

  expected <-
    data.frame(
      ARM = as.character(c("A: Drug X", "B: Placebo", "C: Combination")),
      rate = as.numeric(c(10.515659, 10.088697, 9.093147)),
      std.error = as.numeric(c(2.596110, 2.608489, 2.241374)),
      df = as.numeric(c(Inf, Inf, Inf)),
      null = as.numeric(c(1, 1, 1)),
      statistic = as.numeric(c(9.530385, 8.939723, 8.955809)),
      p.value = as.numeric(c(1.567003e-21, 3.901468e-19, 3.372532e-19))
    )


  testthat::expect_equal(mat1$ARMCD, expected$ARMCD, tolerance = 0.0000001)
  testthat::expect_equal(mat1$rate, expected$rate, tolerance = 0.0000001)
  testthat::expect_equal(mat1$std.error, expected$std.error, tolerance = 0.1)
  testthat::expect_equal(mat1$df, expected$df, tolerance = 0.0000001)
  testthat::expect_equal(mat1$null, expected$null, tolerance = 0.0000001)
  testthat::expect_equal(mat1$statistic, expected$statistic, tolerance = 0.0000001)
  testthat::expect_equal(mat1$p.value, expected$p.value, tolerance = 0.0000001)
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

testthat::test_that("h_glm_count glm-fit works with healthy input", {
  library(scda)
  library(dplyr)
  anl <- synthetic_cdisc_dataset("latest", "adtte") %>%
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


  expected <- structure(
    data.frame(
      "Estimate" = as.numeric(c(2.31126468, -0.06290975, -0.14532927)),
      "SE" = as.numeric(c(0.04981355, 0.07112460, 0.07210257)),
      "z_value" = as.numeric(c(46.3983147, -0.8845006, -2.0155907)),
      "Pr" = as.numeric(c(0.00000000, 0.37642606, 0.04384279)),
      "coefs" = c("(Intercept)", "ARMCDARM B", "ARMCDARM C")
    )
  )

  testthat::expect_equal(mat1, expected, tolerance = 0.0000001)
})

testthat::test_that("h_glm_count emmeans-fit works with healthy input", {
  library(scda)
  library(dplyr)
  anl <- synthetic_cdisc_dataset("latest", "adtte") %>%
    filter(PARAMCD == "TNE")
  anl$AVAL_f <- as.factor(anl$AVAL)

  result <- h_glm_count(
    .var = "AVAL",
    .df_row = anl,
    variables = list(arm = "ARMCD", offset = "lgTMATRSK", covariates = NULL),
    distribution = "poisson"
  )
  mat1 <- as.data.frame(broom::tidy(result$emmeans_fit))

  expected <-
    data.frame(
      ARMCD = as.character(c("ARM A", "ARM B", "ARM C")),
      rate = as.numeric(c(10.087174, 9.472141, 8.722757)),
      std.error = as.numeric(c(0.5024779, 0.4808751, 0.4547052)),
      df = as.numeric(c(Inf, Inf, Inf)),
      null = as.numeric(c(1, 1, 1)),
      statistic = as.numeric(c(46.39831, 44.28746, 41.54985)),
      p.value = as.numeric(c(0, 0, 0))
    )


  testthat::expect_equal(mat1$ARMCD, expected$ARMCD, tolerance = 0.0000001)
  testthat::expect_equal(mat1$rate, expected$rate, tolerance = 0.0000001)
  testthat::expect_equal(mat1$std.error, expected$std.error, tolerance = 0.0000001)
  testthat::expect_equal(mat1$df, expected$df, tolerance = 0.0000001)
  testthat::expect_equal(mat1$null, expected$null, tolerance = 0.0000001)
  testthat::expect_equal(mat1$statistic, expected$statistic, tolerance = 0.0000001)
  testthat::expect_equal(mat1$p.value, expected$p.value, tolerance = 0.0000001)
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
  library(scda)
  library(dplyr)
  anl <- synthetic_cdisc_dataset("latest", "adtte") %>%
    filter(PARAMCD == "TNE")
  anl$AVAL_f <- as.factor(anl$AVAL)

  fits <- h_glm_count(
    .var = "AVAL",
    .df_row = anl,
    variables = list(arm = "ARMCD", offset = "lgTMATRSK", covariates = c("REGION1")),
    distribution = "quasipoisson"
  )
  result <- h_ppmeans(
    obj = fits$glm_fit,
    .df_row = anl,
    arm = "ARM",
    conf_level = 0.95
  )

  expected <-
    data.frame(
      rate = as.numeric(c(9.48327, 9.48327, 9.48327)),
      asymp.LCL = as.numeric(c(7.625358, 7.625358, 7.625358)),
      asymp.UCL = as.numeric(c(11.79386, 11.79386, 11.79386)),
      ARM = c("A: Drug X", "B: Placebo", "C: Combination")
    )
  row.names(expected) <- c("A: Drug X", "B: Placebo", "C: Combination")

  testthat::expect_equal(result, expected, tolerance = 0.0000001)
})

testthat::test_that("s_glm_count works with healthy input", {
  library(scda)
  library(dplyr)
  anl <- synthetic_cdisc_dataset("latest", "adtte") %>%
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
    distribution = "quasipoisson",
    rate_mean_method = "ppmeans"
  )

  expected <- list(
    n = 134L,
    rate = formatters::with_label(9.662708, "Adjusted Rate"),
    rate_ci = formatters::with_label(c(6.593273, 14.161091), "95% CI"),
    rate_ratio = formatters::with_label(character(0), "Adjusted Rate Ratio"),
    rate_ratio_ci = formatters::with_label(character(0), "95% CI"),
    pval = formatters::with_label(character(0), "p-value")
  )

  testthat::expect_equal(result, expected, tolerance = 0.0000001)
})

testthat::test_that("s_glm_count fails wrong inputs", {
  testthat::expect_error(
    df = anl %>%
      filter(ARMCD == "ARM B"),
    .df_row = anl,
    .var = "AVAL",
    .in_ref_col = FALSE,
    variables = list(arm = "ARMCD", offset = "lgTMATRSK", covariates = c("REGION1")),
    conf_level = 0.95,
    distribution = "quasipoisson",
    rate_mean_method = "ppmeans"
  )
})

testthat::test_that("summarize_glm_count works with healthy inputs", {
  library(scda)
  library(dplyr)
  anl <- synthetic_cdisc_dataset("latest", "adtte") %>%
    filter(PARAMCD == "TNE")
  anl$AVAL_f <- as.factor(anl$AVAL)
  result <- basic_table() %>%
    split_cols_by("ARM", ref_group = "B: Placebo") %>%
    add_colcounts() %>%
    summarize_vars(
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

  result_matrix <- to_string_matrix(result)

  expected_matrix <- t(structure(
    c(
      "", "B: Placebo", "A: Drug X", "C: Combination", "", "(N=134)", "(N=134)",
      "(N=132)", "Number of exacerbations per patient", "", "", "", "0", "6 (4.48%)",
      "7 (5.22%)", "12 (9.09%)", "1", "19 (14.18%)", "24 (17.91%)", "23 (17.42%)", "2",
      "34 (25.37%)", "20 (14.93%)", "25 (18.94%)", "3", "31 (23.13%)", "36 (26.87%)",
      "30 (22.73%)", "4", "21 (15.67%)", "20 (14.93%)", "22 (16.67%)", "5", "16 (11.94%)",
      "16 (11.94%)", "9 (6.82%)", "6", "6 (4.48%)", "6 (4.48%)", "6 (4.55%)", "7",
      "0 (0.00%)", "5 (3.73%)", "4 (3.03%)", "8", "1 (0.75%)", "0 (0.00%)",
      "1 (0.76%)", "Unadjusted exacerbation rate (per year)", "", "", "", "Rate", "9.4721", "10.0872", "8.7228"
    ),
    .Dim = c(4L, 14L)
  ))

  testthat::expect_identical(result_matrix, expected_matrix)
})
