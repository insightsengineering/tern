testthat::test_that("h_adsl_adlb_merge_using_worst_flag generates missing for patients without any lab", {
  adsl_f <- tern_ex_adsl %>%
    dplyr::filter(USUBJID %in% c("AB12345-CHN-3-id-128", "AB12345-CHN-11-id-175"))
  adlb_f <- tern_ex_adlb %>%
    dplyr::filter(USUBJID %in% c("AB12345-CHN-3-id-128"))

  testthat::expect_silent(
    result <- h_adsl_adlb_merge_using_worst_flag(adsl_f, adlb_f, worst_flag = c("WGRHIFL" = "Y"))
  )
  res <- df_explicit_na(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_adsl_adlb_merge_using_worst_flag generates missing for patients missing baseline lab", {
  adsl_f <- tern_ex_adsl %>%
    dplyr::filter(USUBJID %in% c("AB12345-CHN-3-id-128", "AB12345-CHN-11-id-175"))
  adlb_f <- tern_ex_adlb %>%
    dplyr::filter(USUBJID %in% c("AB12345-CHN-3-id-128", "AB12345-CHN-11-id-175"))

  adlb_f[adlb_f$USUBJID == "AB12345-CHN-11-id-175", ]$BTOXGR <- NA

  result <- h_adsl_adlb_merge_using_worst_flag(adsl_f, adlb_f, worst_flag = c("WGRHIFL" = "Y"))

  res <- df_explicit_na(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_adsl_adlb_merge_using_worst_flag generates missing for patients missing post-baseline lab", {
  adsl_f <- tern_ex_adsl %>%
    dplyr::filter(USUBJID %in% c("AB12345-CHN-3-id-128", "AB12345-CHN-11-id-175"))
  adlb_f <- tern_ex_adlb %>%
    dplyr::filter(USUBJID %in% c("AB12345-CHN-3-id-128", "AB12345-CHN-11-id-175"))

  adlb_f[adlb_f$USUBJID == "AB12345-CHN-11-id-175", ]$ATOXGR <- NA

  result <- h_adsl_adlb_merge_using_worst_flag(adsl_f, adlb_f, worst_flag = c("WGRHIFL" = "Y"))

  res <- df_explicit_na(result)
  testthat::expect_snapshot(res)
})

testthat::test_that(
  "h_adsl_adlb_merge_using_worst_flag generates missing for patients without any worst flagged post-baseline values",
  code = {
    adsl_f <- tern_ex_adsl %>%
      dplyr::filter(USUBJID %in% c("AB12345-CHN-3-id-128", "AB12345-CHN-11-id-175"))
    adlb_f <- tern_ex_adlb %>%
      dplyr::filter(USUBJID %in% c("AB12345-CHN-3-id-128", "AB12345-CHN-11-id-175"))

    adlb_f[adlb_f$USUBJID == "AB12345-CHN-11-id-175", ]$WGRHIFL <- ""

    result <- h_adsl_adlb_merge_using_worst_flag(adsl_f, adlb_f, worst_flag = c("WGRHIFL" = "Y"))

    res <- df_explicit_na(result)
    testthat::expect_snapshot(res)
  }
)

testthat::test_that("h_adsl_adlb_merge_using_worst_flag generates missing and by visit lab results", {
  adsl_f <- tern_ex_adsl %>%
    dplyr::filter(USUBJID %in% c("AB12345-CHN-3-id-128", "AB12345-CHN-11-id-175"))
  adlb_f <- tern_ex_adlb %>%
    dplyr::filter(USUBJID %in% c("AB12345-CHN-3-id-128"))

  result <- h_adsl_adlb_merge_using_worst_flag(adsl_f, adlb_f, worst_flag = c("WGRHIVFL" = "Y"), by_visit = TRUE)

  res <- df_explicit_na(result)
  testthat::expect_snapshot(res)
})
