get_surv_df <- function() {
  adtte_f <- tibble::tribble(
    ~AVAL, ~is_event, ~ARM, ~SEX,
    20, TRUE, "T", "F",
    30, FALSE, "C", "M",
    50, TRUE, "C", "F",
    70, FALSE, "T", "M",
    10, TRUE, "T", "M",
    11, TRUE, "C", "M",
    70, TRUE, "T", "F",
    100, FALSE, "T", "M"
  ) %>%
    df_explicit_na()

  extract_survival_subgroups(
    variables = list(
      tte = "AVAL",
      is_event = "is_event",
      arm = "ARM",
      subgroups = "SEX"
    ),
    data = adtte_f
  )
}

get_rsp_df <- function() {
  adrs_f <- tibble::tribble(
    ~rsp, ~ARM, ~SEX,
    TRUE, "T", "F",
    FALSE, "C", "M",
    TRUE, "C", "F",
    FALSE, "T", "M",
    TRUE, "T", "M",
    TRUE, "C", "M",
    TRUE, "T", "F",
    FALSE, "T", "M"
  ) %>%
    df_explicit_na()

  extract_rsp_subgroups(
    variables = list(
      rsp = "rsp",
      arm = "ARM",
      subgroups = "SEX"
    ),
    data = adrs_f
  )
}

test_that("h_default_forest_header gives correct result for full survival table", {
  df <- get_surv_df()

  tbl <- basic_table() %>%
    tabulate_survival_subgroups(df, time_unit = "DAYS")

  result <- h_default_forest_header(tbl)
  expected <- c("T\nBetter", "C\nBetter")
  expect_identical(result, expected)
})

test_that("h_default_forest_header gives NULL for survival table not mentioning arms", {
  df <- get_surv_df()

  tbl <- basic_table() %>%
    tabulate_survival_subgroups(df, vars = c("hr", "n_tot", "ci"), time_unit = "DAYS")

  result <- h_default_forest_header(tbl)
  expected <- NULL
  expect_identical(result, expected)
})

test_that("h_default_forest_header gives correct result for full response table", {
  df <- get_rsp_df()

  tbl <- basic_table() %>%
    tabulate_rsp_subgroups(df)

  result <- h_default_forest_header(tbl)
  expected <- c("C\nBetter", "T\nBetter")
  expect_identical(result, expected)
})

test_that("h_default_forest_header gives NULL for response table not mentioning arms", {
  df <- get_rsp_df()

  tbl <- basic_table() %>%
    tabulate_rsp_subgroups(df, vars = c("or", "n_tot", "ci"))

  result <- h_default_forest_header(tbl)
  expected <- NULL
  expect_identical(result, expected)
})

test_that("h_default_forest_header gives error when not exactly two arms could be inferred", {
  # Construct a table with two column splits so we have "arms" inferred, but more than two.
  tbl <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_cols_by("SEX") %>%
    analyze(c("AGE", "BMRKR2")) %>%
    build_table(ex_adsl)
  expect_error(
    h_default_forest_header(tbl),
    "Default `forest_header` could not be constructed"
  )
})
