test_that("`prop_diff_ha` (proportion difference by Anderson-Hauck)", {

  # "Mid" case: 3/4 respond in group A, 1/2 respond in group B.
  rsp <- c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)
  grp <- factor(c("A", "B", "A", "B", "A", "A"), levels = c("B", "A"))

  result <- prop_diff_ha(rsp = rsp, grp = grp, conf_level = 0.90)
  expected <- list(# according to SAS.
    diff = 0.25,
    diff_ci = c(-0.9195, 1)
  )
  expect_equal(result, expected, tol = 0.0001)


  # Edge case: Same proportion of response in A and B.
  rsp <- c(TRUE, FALSE, TRUE, FALSE)
  grp <- factor(c("A", "A", "B", "B"), levels = c("A", "B"))
  result <- prop_diff_ha(rsp = rsp, grp = grp, conf_level = 0.6)

  expected <- list(# according to SAS.
    diff = 0,
    diff_ci = c(-0.8451, 0.8451)
  )
  expect_equal(result, expected, tol = 0.0001)

})


test_that("`prop_diff_nc` (proportion difference by Newcombe)", {

  # "Mid" case: 3/4 respond in group A, 1/2 respond in group B.
  rsp <- c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)
  grp <- factor(c("A", "B", "A", "B", "A", "A"), levels = c("B", "A"))

  result <- suppressWarnings(
    prop_diff_nc(rsp = rsp, grp = grp, conf_level = 0.9)
  )
  expected <- list(# according to SAS.
    diff = 0.25,
    diff_ci = c(-0.2967, 0.6750)
  )
  expect_equal(result, expected, tol = 0.0001)

  # Edge case: Same proportion of response in A and B.
  rsp <- c(TRUE, FALSE, TRUE, FALSE)
  grp <- factor(c("A", "A", "B", "B"), levels = c("A", "B"))
  result <- expect_warning(
    prop_diff_nc(rsp = rsp, grp = grp, conf_level = 0.6)
  )
  expected <- list(# according to SAS.
    diff = 0,
    diff_ci = c(-0.3616, 0.3616)
  )
  expect_equal(result, expected, tol = 0.0001)

})


test_that("`prop_diff_cmh` (proportion difference by CMH)", {

  set.seed(2)
  rsp <- sample(c(TRUE, FALSE), 100, TRUE)
  grp <- sample(c("Placebo", "Treatment"), 100, TRUE)
  grp <- factor(grp, levels = c("Placebo", "Treatment"))
  strata_data <- data.frame(
    "f1" = sample(c("a", "b"), 100, TRUE),
    "f2" = sample(c("x", "y", "z"), 100, TRUE),
    stringsAsFactors = TRUE)

  result <- prop_diff_cmh(
    rsp = rsp, grp = grp, strata = interaction(strata_data),
    conf_level = 0.90
  )

  expected <- list(
    prop = c(Placebo = 0.5331117, Treatment = 0.3954251),
    prop_ci = list(
      Placebo = c(0.4306536, 0.6355698),
      Treatment = c(0.2890735, 0.5017768)
    ),
    diff = -0.1376866,
    diff_ci = c(-0.285363076, 0.009989872)
  )
  expect_equal(result, expected, tol = 0.0001)

})


test_that("`estimate_proportion_diff` is compatible with `rtables`", {

  # "Mid" case: 3/4 respond in group A, 1/2 respond in group B.
  dta <- data.frame(
    rsp = c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE),
    grp = factor(c("A", "B", "A", "B", "A", "A"), levels = c("B", "A"))
  )

  l <- split_cols_by(lyt = NULL, var = "grp", ref_group = "B") %>%
    estimate_proportion_diff(
      vars = "rsp",
      conf_level = 0.90,
      method = "ha"
    )

  result <- build_table(l, df = dta)
  expected <- structure(
    c(
      "", "Difference in Response rate (%)", "90% CI (Anderson-Hauck)",
      "B", "", "", "A", "25", "(-92, 100)"
    ),
    .Dim = c(3L, 3L)
  )

  expect_identical(to_string_matrix(result), expected)
})
