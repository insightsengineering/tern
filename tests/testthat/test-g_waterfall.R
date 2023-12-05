testthat::test_that("g_waterfall default plot works", {
  g_waterfall <- g_waterfall(
    height = c(3, 5, -1),
    id = letters[1:3],
    col = NULL
  )
  vdiffr::expect_doppelganger(title = "g_waterfall", fig = g_waterfall)
})

testthat::test_that("g_waterfall plot with labels and colors works", {
  set.seed(123)
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

  vdiffr::expect_doppelganger(title = "g_waterfall_decorated", fig = g_waterfall_decorated)
})
