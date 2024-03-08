testthat::test_that("g_waterfall default plot works", {
  testthat::skip_if_not_installed("vdiffr")
  g_waterfall <- g_waterfall(
    height = c(3, 5, -1),
    id = letters[1:3],
    col = NULL
  )
  expect_snapshot_ggplot(title = "g_waterfall", fig = g_waterfall, width = 10, height = 8)
})

testthat::test_that("g_waterfall plot with labels and colors works", {
  set.seed(123)
  testthat::skip_if_not_installed("vdiffr")

  adrs <- tern_ex_adrs
  adrs_f <- head(dplyr::filter(adrs, PARAMCD == "OVRINV"), 30)
  adrs_f$pchg <- rnorm(30, 10, 50)
  adrs_f <- adrs_f[!duplicated(adrs_f$USUBJID), ]

  g_waterfall_decorated <-
    g_waterfall(
      height = adrs_f$pchg,
      id = paste("asdfdsfdsfsd", adrs_f$USUBJID),
      col_var = adrs_f$SEX,
      col = "blue",
      xlab = "ID",
      ylab = "Percentage Change",
      title = "Waterfall plot"
    )

  expect_snapshot_ggplot(title = "g_waterfall_decorated", fig = g_waterfall_decorated, width = 10, height = 8)
})
