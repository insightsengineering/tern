dta_local <- data.frame(
  ARM = rep(c("A", "B"), 9),
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

testthat::test_that("summarize_colvars works as expected without column split and default behavior", {
  dta <- dta_local

  l <- basic_table() %>%
    split_rows_by("AVISIT") %>%
    split_cols_by_multivar(vars = c("AVAL", "CHG")) %>%
    summarize_colvars()

  result <- build_table(l, dta)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("summarize_colvars works as expected with column split", {
  dta <- dta_local

  l <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("AVISIT") %>%
    split_cols_by_multivar(vars = c("AVAL", "CHG")) %>%
    summarize_colvars()

  result <- build_table(l, dta)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("summarize_colvars works when selecting statistics and custom formatting", {
  dta <- dta_local

  l <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("AVISIT") %>%
    split_cols_by_multivar(vars = c("AVAL", "CHG")) %>%
    summarize_colvars(
      .stats = c("n", "mean_sd"),
      .formats = c("mean_sd" = "xx.x, xx.x"),
      .labels = c(n = "n", mean_sd = "Mean, SD"),
      .indent_mods = c(n = 2L, mean_sd = 5L)
    )

  result <- build_table(l, dta)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
