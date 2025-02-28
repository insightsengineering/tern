## Deprecated ------------------------------------------------------------

testthat::test_that("h_tab_rsp_one_biomarker works as expected", {
  df <- data.frame(
    n_tot = c(48L, 48L),
    n_rsp = c(24L, 24L),
    prop = c(0.5, 0.5),
    or = c(0.992727618706316, 1.00485769099575),
    lcl = c(0.859391304891713, 0.950491104268725),
    ucl = c(1.14675133356916, 1.06233396043214),
    conf_level = c(0.95, 0.95),
    pval = c(0.920991170690111, 0.864415775291559),
    pval_label = c("p-value (Wald)", "p-value (Wald)"),
    subgroup = c("All patients", "All patients"),
    row_type = c("content", "content"),
    var = c("ALL", "ALL"),
    var_label = c("All patients", "All patients")
  )
  lifecycle::expect_deprecated(lifecycle::expect_deprecated(
    res <- h_tab_rsp_one_biomarker(
      df = df,
      vars = c("n_tot", "or", "ci")
    )
  ))

  testthat::expect_snapshot(res)
})

testthat::test_that("h_tab_surv_one_biomarker works as expected", {
  df <- data.frame(
    n_tot = c(48L, 48L),
    n_tot_events = c(25L, 25L),
    median = c(1269.40388857211, 1269.40388857211),
    hr = c(0.992727618706316, 1.00485769099575),
    lcl = c(0.859391304891713, 0.950491104268725),
    ucl = c(1.14675133356916, 1.06233396043214),
    conf_level = c(0.95, 0.95),
    pval = c(0.920991170690111, 0.864415775291559),
    pval_label = c("p-value (Wald)", "p-value (Wald)"),
    subgroup = c("All patients", "All patients"),
    row_type = c("content", "content"),
    var = c("ALL", "ALL"),
    var_label = c("All patients", "All patients")
  )
  lifecycle::expect_deprecated(lifecycle::expect_deprecated(
    result <- h_tab_surv_one_biomarker(
      df = df,
      vars = c("n_tot", "hr", "ci"),
      time_unit = "months"
    )
  ))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
