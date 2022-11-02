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
  ADRS <- adrs_raw
  ADRS_f <- head(dplyr::filter(ADRS, PARAMCD == "OVRINV"), 30)
  ADRS_f$pchg <- rnorm(30, 10, 50)
  ADRS_f <- ADRS_f[!duplicated(ADRS_f$USUBJID), ]

  result <- testthat::expect_silent(
    g_waterfall(
      height = ADRS_f$pchg,
      id = paste("asdfdsfdsfsd", ADRS_f$USUBJID),
      col_var = ADRS_f$SEX,
      xlab = "ID",
      ylab = "Percentage Change",
      title = "Waterfall plot"
    )
  )
})
