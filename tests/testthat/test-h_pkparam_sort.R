adpp <- scda::synthetic_cdisc_data("rcd_2022_02_28")$adpp
expected_tlg_display <- c(
  "Cmax", "AUCinf obs", "CL obs", "Ae", "Fe", "CLR",
  "Rmax", "Tonset", "RENALCLD"
)

testthat::test_that("h_pkparam_sort with PARAMCD", {
  sorted_adpp <- adpp %>% h_pkparam_sort()
  result_tlg_display <- levels(sorted_adpp$TLG_DISPLAY)
  testthat::expect_identical(result_tlg_display, expected_tlg_display)
})

testthat::test_that("h_pkparam_sort with out PARAMCD", {
  adpp_1 <- adpp
  adpp_1[["pktest"]] <- adpp$PARAMCD
  adpp_1$PARAMCD <- NULL # nolint
  sorted_adpp <- adpp_1 %>% h_pkparam_sort("pktest")
  result_tlg_display <- levels(sorted_adpp$TLG_DISPLAY)
  testthat::expect_identical(result_tlg_display, expected_tlg_display)
})
