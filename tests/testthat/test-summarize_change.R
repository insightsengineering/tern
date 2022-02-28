testthat::test_that("s_change_from_baseline handles empty data (complete missing for a visit)", {
  test_data <- data.frame(
    chg = numeric(),
    aval = numeric(),
    ablfl = logical()
  )

  result <- s_change_from_baseline(
    test_data,
    .var = "chg",
    variables = list(value = "aval", baseline_flag = "ablfl"),
    na.rm = TRUE
  )
  expected <- list(
    n = c(n = 0L),
    mean = c(mean = NA_real_),
    sd = c(sd = NA_real_),
    se = c(se = NA_real_),
    mean_sd = c(mean = NA_real_, sd = NA_real_),
    mean_ci = with_label(c(mean_ci_lwr = NA_real_, mean_ci_upr = NA_real_), "Mean 95% CI"),
    mean_sei = with_label(c(mean_sei_lwr = NA_real_, mean_sei_upr = NA_real_), "Mean -/+ 1xSE"),
    mean_sdi = with_label(c(mean_sdi_lwr = NA_real_, mean_sdi_upr = NA_real_), "Mean -/+ 1xSD"),
    median = c(median = NA_real_),
    mad = c(mad = NA_real_),
    median_ci = with_label(
      `attr<-`(c(median_ci_lwr = NA_real_, median_ci_upr = NA_real_), "conf_level", NA_real_),
      "Median 95% CI"
    ),
    quantiles = with_label(c(quantile_0.25 = NA_real_, quantile_0.75 = NA_real_), "25% and 75%-ile"),
    iqr = c(iqr = NA_real_),
    range = c(min = NA_real_, max = NA_real_),
    min = c(min = NA_real_),
    max = c(max = NA_real_),
    cv = c(cv = NA_real_),
    geom_mean = c(geom_mean = NaN),
    geom_mean_ci = with_label(c(mean_ci_lwr = NA_real_, mean_ci_upr = NA_real_), "Geometric Mean 95% CI"),
    geom_cv = c(geom_cv = NA_real_)
  )

  testthat::expect_identical(result, expected)
})

testthat::test_that("s_change_from_baseline handles NA in baseline values", {
  test_data <- data.frame(
    chg = c(0, 0, 0, NA),
    aval = c(0, 3, 6, NA),
    ablfl = c(TRUE, TRUE, TRUE, TRUE)
  )

  result <- s_change_from_baseline(
    test_data,
    .var = "chg",
    variables = list(value = "aval", baseline_flag = "ablfl"),
    na.rm = TRUE
  )
  expected <- list(
    n = c(n = 3L),
    mean = c(mean = 3),
    sd = c(sd = 3),
    se = c(se = 1.732051),
    mean_sd = c(mean = 3, sd = 3),
    mean_ci = with_label(c(mean_ci_lwr = -4.452413, mean_ci_upr = 10.452413), "Mean 95% CI"),
    mean_sei = with_label(c(mean_sei_lwr = 1.267949, mean_sei_upr = 4.732051), "Mean -/+ 1xSE"),
    mean_sdi = with_label(c(mean_sdi_lwr = 0, mean_sdi_upr = 6), "Mean -/+ 1xSD"),
    median = c(median = 3),
    mad = c(mad = 0),
    median_ci = with_label(
      `attr<-`(c(median_ci_lwr = NA_real_, median_ci_upr = NA_real_), "conf_level", NA_real_),
      "Median 95% CI"
    ),
    quantiles = with_label(c(quantile_0.25 = 0, quantile_0.75 = 6), "25% and 75%-ile"),
    iqr = c(iqr = 6),
    range = c(min = 0, max = 6),
    min = c(min = 0),
    max = c(max = 6),
    cv = c(cv = 100),
    geom_mean = c(geom_mean = NA_real_),
    geom_mean_ci = with_label(c(mean_ci_lwr = NA_real_, mean_ci_upr = NA_real_), "Geometric Mean 95% CI"),
    geom_cv = c(geom_cv = NA_real_)
  )

  testthat::expect_equal(result, expected, tol = 1e-6)
})

testthat::test_that("s_change_from_baseline handles baseline substitution", {
  test_data <- data.frame(
    chg = c(3, 1, 2, 5),
    aval = c(1, 3, 6, 4),
    ablfl = c(TRUE, FALSE, FALSE, TRUE)
  )

  result <- test_data %>%
    split(test_data$ablfl) %>%
    lapply(
      s_change_from_baseline,
      .var = "chg",
      variables = list(value = "aval", baseline_flag = "ablfl")
    )
  expected <- list(
    # Here we take the summary of the 2 change values.
    `FALSE` = list(
      n = c(n = 2L),
      mean = c(mean = 1.5),
      sd = c(sd = 0.7071068),
      se = c(se = 0.5),
      mean_sd = c(mean = 1.5, sd = 0.7071068),
      mean_ci = with_label(c(mean_ci_lwr = -4.853102, mean_ci_upr = 7.853102), "Mean 95% CI"),
      mean_sei = with_label(c(mean_sei_lwr = 1, mean_sei_upr = 2), "Mean -/+ 1xSE"),
      mean_sdi = with_label(c(mean_sdi_lwr = 0.7928932, mean_sdi_upr = 2.2071068), "Mean -/+ 1xSD"),
      median = c(median = 1.5),
      mad = c(mad = 0),
      median_ci = with_label(
        `attr<-`(c(median_ci_lwr = NA_real_, median_ci_upr = NA_real_), "conf_level", NA_real_),
        "Median 95% CI"
      ),
      quantiles = with_label(c(quantile_0.25 = 1, quantile_0.75 = 2), "25% and 75%-ile"),
      iqr = c(iqr = 1),
      range = c(min = 1, max = 2),
      min = c(min = 1),
      max = c(max = 2),
      cv = c(cv = 47.14045),
      geom_mean = c(geom_mean = 1.414214),
      geom_mean_ci = with_label(c(mean_ci_lwr = 0.01729978, mean_ci_upr = 115.60839614), "Geometric Mean 95% CI"),
      geom_cv = c(geom_cv = 52.10922)
    ),
    # Here we take the summary of the 2 baseline values.
    `TRUE` = list(
      n = c(n = 2L),
      mean = c(mean = 2.5),
      sd = c(sd = 2.12132),
      se = c(se = 1.5),
      mean_sd = c(mean = 2.5, sd = 2.12132),
      mean_ci = with_label(c(mean_ci_lwr = -16.55931, mean_ci_upr = 21.55931), "Mean 95% CI"),
      mean_sei = with_label(c(mean_sei_lwr = 1, mean_sei_upr = 4), "Mean -/+ 1xSE"),
      mean_sdi = with_label(c(mean_sdi_lwr = 0.3786797, mean_sdi_upr = 4.6213203), "Mean -/+ 1xSD"),
      median = c(median = 2.5),
      mad = c(mad = 0),
      median_ci = with_label(
        `attr<-`(c(median_ci_lwr = NA_real_, median_ci_upr = NA_real_), "conf_level", NA_real_),
        "Median 95% CI"
      ),
      quantiles = with_label(c(quantile_0.25 = 1, quantile_0.75 = 4), "25% and 75%-ile"),
      iqr = c(iqr = 3),
      range = c(min = 1, max = 4),
      min = c(min = 1),
      max = c(max = 4),
      cv = c(cv = 84.85281),
      geom_mean = c(geom_mean = 2),
      geom_mean_ci = with_label(c(mean_ci_lwr = 2.992824e-04, mean_ci_upr = 1.336530e+04), "Geometric Mean 95% CI"),
      geom_cv = c(geom_cv = 127.0458)
    )
  )

  testthat::expect_equal(result, expected, tolerance = 1e-4)
})

testthat::test_that("summarize_change works as expected", {
  library(dplyr)
  dta_test <- data.frame(
    USUBJID = rep(1:6, each = 3),
    AVISIT = rep(paste0("V", 1:3), 6),
    AVAL = c(9:1, rep(NA, 9))
  ) %>%
    dplyr::mutate(
      ABLFLL = AVISIT == "V1"
    ) %>%
    dplyr::group_by(USUBJID) %>%
    dplyr::mutate(
      BLVAL = AVAL[ABLFLL],
      CHG = AVAL - BLVAL
    ) %>%
    dplyr::ungroup()

  result <- basic_table() %>%
    split_rows_by("AVISIT") %>%
    summarize_change("CHG", variables = list(value = "AVAL", baseline_flag = "ABLFLL")) %>%
    build_table(dta_test)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "V1", "n", "Mean (SD)", "Median", "Min - Max",
      "V2", "n", "Mean (SD)", "Median", "Min - Max", "V3", "n", "Mean (SD)",
      "Median", "Min - Max", "all obs", "", "3", "6 (3)", "6", "3 - 9",
      "", "3", "-1 (0)", "-1", "-1 - -1", "", "3", "-2 (0)", "-2",
      "-2 - -2"
    ),
    .Dim = c(16L, 2L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})
