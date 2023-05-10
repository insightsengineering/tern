testthat::test_that("h_pkparam_sort with PARAMCD", {
  sorted_adpp <- tern_ex_adpp %>% h_pkparam_sort()
  result_tlg_display <- levels(sorted_adpp$TLG_DISPLAY)

  res <- testthat::expect_silent(result_tlg_display)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_pkparam_sort with out PARAMCD", {
  adpp_1 <- tern_ex_adpp
  adpp_1[["pktest"]] <- tern_ex_adpp$PARAMCD
  adpp_1$PARAMCD <- NULL
  sorted_adpp <- adpp_1 %>% h_pkparam_sort("pktest")
  result_tlg_display <- levels(sorted_adpp$TLG_DISPLAY)

  res <- testthat::expect_silent(result_tlg_display)
  testthat::expect_snapshot(res)
})
