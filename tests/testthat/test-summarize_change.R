test_that("s_change_from_baseline handles empty data (complete missing for a visit)", {

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
  expected <-  list(
    n = 0L,
    mean_sd = c(mean = NA_real_, sd = NA_real_),
    median = NA_real_,
    quantiles = with_label(c(NA_real_, NA_real_), "25% and 75%-ile"),
    range = c(NA_real_, NA_real_),
    mean_ci = with_label(c(mean_ci_lwr = NA_real_, mean_ci_upr = NA_real_), "Mean 95% CI"),
    median_ci =  with_label(c(median_ci_lwr = NA_real_, median_ci_upr = NA_real_), "Median 95% CI")
  )

  expect_identical(result, expected)
})

test_that("s_change_from_baseline handles NA in baseline values", {

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
  expected <-  list(
    n = 3L,
    mean_sd = c(mean = 3, sd = 3),
    median = 3,
    quantiles = with_label(c(0, 6), "25% and 75%-ile"),
    range = c(0, 6),
    mean_ci = with_label(c(mean_ci_lwr = -4.452413, mean_ci_upr = 10.452413), "Mean 95% CI"),
    median_ci =  with_label(c(median_ci_lwr = NA_real_, median_ci_upr = NA_real_), "Median 95% CI")
  )

  expect_equal(result, expected, tol = 1e-6)
})

test_that("s_change_from_baseline handles baseline substitution", {

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
      n = 2L,
      mean_sd = c(mean = 1.5, sd = 0.7071068),
      median = 1.5,
      quantiles = with_label(c(1, 2), "25% and 75%-ile"),
      range = c(1, 2),
      mean_ci = with_label(c(mean_ci_lwr = -4.853102, mean_ci_upr = 7.853102), "Mean 95% CI"),
      median_ci =  with_label(c(median_ci_lwr = NA_real_, median_ci_upr = NA_real_), "Median 95% CI")
    ),
    # Here we take the summary of the 2 baseline values.
    `TRUE` = list(
      n = 2L,
      mean_sd = c(mean = 2.5, sd = 2.12132),
      median = 2.5,
      quantiles = with_label(c(1, 4), "25% and 75%-ile"),
      range = c(1, 4),
      mean_ci = with_label(c(mean_ci_lwr = -16.55931, mean_ci_upr = 21.55931), "Mean 95% CI"),
      median_ci =  with_label(c(median_ci_lwr = NA_real_, median_ci_upr = NA_real_), "Median 95% CI")
    )
  )

  expect_equal(result, expected, tolerance = 1e-4)
})

test_that("summarize_change works as expected", {
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
    c("", "V1", "n", "Mean (SD)", "Median", "Min - Max",
      "V2", "n", "Mean (SD)", "Median", "Min - Max", "V3", "n", "Mean (SD)",
      "Median", "Min - Max", "all obs", "", "3", "6 (3)", "6", "3 - 9",
      "", "3", "-1 (0)", "-1", "-1 - -1", "", "3", "-2 (0)", "-2",
      "-2 - -2"),
    .Dim = c(16L, 2L)
  )

  expect_identical(result_matrix, expected_matrix)
})
