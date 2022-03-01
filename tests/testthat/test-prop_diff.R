testthat::test_that("`prop_diff_ha` (proportion difference by Anderson-Hauck)", {

  # "Mid" case: 3/4 respond in group A, 1/2 respond in group B.
  rsp <- c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)
  grp <- factor(c("A", "B", "A", "B", "A", "A"), levels = c("B", "A"))

  result <- prop_diff_ha(rsp = rsp, grp = grp, conf_level = 0.90)
  # according to SAS.
  expected <- list(
    diff = 0.25,
    diff_ci = c(-0.9195, 1)
  )
  testthat::expect_equal(result, expected, tol = 0.0001)


  # Edge case: Same proportion of response in A and B.
  rsp <- c(TRUE, FALSE, TRUE, FALSE)
  grp <- factor(c("A", "A", "B", "B"), levels = c("A", "B"))
  result <- prop_diff_ha(rsp = rsp, grp = grp, conf_level = 0.6)
  # according to SAS.
  expected <- list(
    diff = 0,
    diff_ci = c(-0.8451, 0.8451)
  )
  testthat::expect_equal(result, expected, tol = 0.0001)
})


testthat::test_that("`prop_diff_nc` (proportion difference by Newcombe)", {

  # "Mid" case: 3/4 respond in group A, 1/2 respond in group B.
  rsp <- c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)
  grp <- factor(c("A", "B", "A", "B", "A", "A"), levels = c("B", "A"))

  result <- suppressWarnings(
    prop_diff_nc(rsp = rsp, grp = grp, conf_level = 0.9)
  )
  # according to SAS.
  expected <- list(
    diff = 0.25,
    diff_ci = c(-0.2967, 0.6750)
  )
  testthat::expect_equal(result, expected, tol = 0.0001)

  # Edge case: Same proportion of response in A and B.
  rsp <- c(TRUE, FALSE, TRUE, FALSE)
  grp <- factor(c("A", "A", "B", "B"), levels = c("A", "B"))
  result <- testthat::expect_warning(
    prop_diff_nc(rsp = rsp, grp = grp, conf_level = 0.6)
  )
  # according to SAS.
  expected <- list(
    diff = 0,
    diff_ci = c(-0.3616, 0.3616)
  )
  testthat::expect_equal(result, expected, tol = 0.0001)
})


testthat::test_that("`prop_diff_cmh` (proportion difference by CMH)", {
  set.seed(2, kind = "Mersenne-Twister")
  rsp <- sample(c(TRUE, FALSE), 100, TRUE)
  grp <- sample(c("Placebo", "Treatment"), 100, TRUE)
  grp <- factor(grp, levels = c("Placebo", "Treatment"))
  strata_data <- data.frame(
    "f1" = sample(c("a", "b"), 100, TRUE),
    "f2" = sample(c("x", "y", "z"), 100, TRUE),
    stringsAsFactors = TRUE
  )

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
  testthat::expect_equal(result, expected, tol = 0.0001)
})

testthat::test_that("prop_diff_cmh works correctly when some strata don't have both groups", {
  set.seed(2, kind = "Mersenne-Twister")
  rsp <- sample(c(TRUE, FALSE), 100, TRUE)
  grp <- sample(c("Placebo", "Treatment"), 100, TRUE)
  grp <- factor(grp, levels = c("Placebo", "Treatment"))
  strata_data <- data.frame(
    "f1" = sample(c("a", "b"), 100, TRUE),
    "f2" = sample(c("x", "y", "z"), 100, TRUE),
    stringsAsFactors = TRUE
  )

  # Deliberately remove all `Treatment` patients from one stratum.
  grp[strata_data$f1 == "a" & strata_data$f2 == "x"] <- "Placebo"

  result <- testthat::expect_silent(prop_diff_cmh(
    rsp = rsp, grp = grp, strata = interaction(strata_data),
    conf_level = 0.90
  ))

  testthat::expect_false(is.na(result$diff))
  testthat::expect_false(any(is.na(result$diff_ci)))

  expected <- list(
    prop = c(Placebo = 0.569842, Treatment = 0.398075),
    prop_ci = list(
      Placebo = c(0.4637119, 0.6759721),
      Treatment = c(0.2836122, 0.5125378)
    ),
    diff = -0.171767,
    diff_ci = c(-0.32786094, -0.01567301)
  )
  testthat::expect_equal(result, expected, tol = 0.000001)
})


testthat::test_that("`estimate_proportion_diff` is compatible with `rtables`", {
  skip_if_fail_rtables_refactor()

  # "Mid" case: 3/4 respond in group A, 1/2 respond in group B.
  dta <- data.frame(
    rsp = c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE),
    grp = factor(c("A", "B", "A", "B", "A", "A"), levels = c("B", "A"))
  )

  l <- basic_table() %>%
    split_cols_by(var = "grp", ref_group = "B") %>%
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

  testthat::expect_identical(to_string_matrix(result), expected)
})
