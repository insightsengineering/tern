# Data generation
adpp <- adpp_raw
adpp_plasma <- adpp %>% dplyr::filter(PPSPEC == "Plasma", AVISIT == "CYCLE 1 DAY 1")

# Define template layout
l <- basic_table() %>%
  split_cols_by(
    var = "ARMCD",
    split_fun = trim_levels_in_group("ARMCD"),
    # label_pos = "topleft", # nolint
    split_label = "Treatment Arm"
  ) %>%
  split_rows_by(
    var = "PKPARAM",
    label_pos = "topleft",
    split_label = "PK Parameter"
  ) %>%
  summarize_vars(
    vars = "AVAL",
    .stats = c("n", "mean_sd", "cv", "geom_mean", "geom_cv", "median", "range"),
    .formats = c(
      n = "xx.",
      mean_sd = sprintf_format("%.3e (%.3e)"),
      cv = "xx.x",
      geom_mean = sprintf_format("%.3e"),
      geom_cv = "xx.x",
      median = sprintf_format("%.3e"),
      range = sprintf_format("%.3e - %.3e")
    )
  )

# PKPT02 Drug X
testthat::test_that("PKPT02 is produced correctly for Drug X", {
  adpp0 <- adpp_plasma %>%
    filter(PPCAT == "Plasma Drug X") %>%
    h_pkparam_sort() %>%
    mutate(PKPARAM = factor(paste0(TLG_DISPLAY, " (", AVALU, ")")))

  result <- build_table(l, df = adpp0)

  res <- expect_silent(result)
  expect_snapshot(res)
})

# PKPT02 Drug Y
testthat::test_that("PKPT02 is produced correctly for Drug Y", {
  adpp1 <- adpp_plasma %>%
    filter(PPCAT == "Plasma Drug Y") %>%
    h_pkparam_sort() %>%
    mutate(PKPARAM = factor(paste0(TLG_DISPLAY, " (", AVALU, ")")))

  result <- build_table(l, df = adpp1)

  res <- expect_silent(result)
  expect_snapshot(res)
})
