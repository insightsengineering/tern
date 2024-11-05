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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("summarize_change works as expected", {
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})


testthat::test_that("summarize_change works with custom statistical functions", {
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

  testthat::expect_error(
    basic_table() %>%
      split_rows_by("AVISIT") %>%
      summarize_change(
        "CHG",
        variables = list(value = "AVAL", baseline_flag = "ABLFLL"),
        .stats = c("n", "my_stat" = function(x) mean(x))
      ) %>%
      build_table(dta_test),
    "custom function has x as first parameter, while the default function has df"
  )
  testthat::expect_error(
    basic_table() %>%
      split_rows_by("AVISIT") %>%
      summarize_change(
        "CHG",
        variables = list(value = "AVAL", baseline_flag = "ABLFLL"),
        .stats = c("n", "my_stat" = function(df) mean(df$AVAL))
      ) %>%
      build_table(dta_test),
    "The custom statistical function needs to have "
  )

  result <- basic_table() %>%
    split_rows_by("AVISIT") %>%
    summarize_change(
      "CHG",
      variables = list(value = "AVAL", baseline_flag = "ABLFLL"),
      .stats = c("n", "my_stat" = function(df, ...) mean(df$AVISIT, na.rm = TRUE))
    ) %>%
    build_table(dta_test)
})
