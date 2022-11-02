testthat::test_that("g_waterfall default plot works", {
  result <- testthat::expect_silent(
    g_waterfall(
      height = c(3, 5, -1),
      id = letters[1:3],
      col = NULL
    )
  )
})

testthat::test_that("g_waterfall plot with labels and colors works", {
  adrs <- adrs_raw
  adrs_f <- head(dplyr::filter(adrs, PARAMCD == "OVRINV"), 30)
  adrs_f$pchg <- rnorm(30, 10, 50)
  adrs_f <- adrs_f[!duplicated(adrs_f$USUBJID), ]

  result <- testthat::expect_silent(
    g_waterfall(
      height = adrs_f$pchg,
      id = paste("asdfdsfdsfsd", adrs_f$USUBJID),
      col_var = adrs_f$SEX,
      xlab = "ID",
      ylab = "Percentage Change",
      title = "Waterfall plot"
    )
  )
})
