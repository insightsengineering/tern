testthat::test_that("`prop_diff_ha` (proportion difference by Anderson-Hauck)", {

  # "Mid" case: 3/4 respond in group A, 1/2 respond in group B.
  rsp <- c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)
  grp <- factor(c("A", "B", "A", "B", "A", "A"), levels = c("B", "A"))

  result <- prop_diff_ha(rsp = rsp, grp = grp, conf_level = 0.90)
  # according to SAS.
  expected <- list(
    diff = 0.25,
    diff_ci = c(-0.9195, 1.0000)
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
  result <- prop_diff_nc(rsp = rsp, grp = grp, conf_level = 0.6)

  # according to SAS.
  expected <- list(
    diff = 0,
    diff_ci = c(-0.3616, 0.3616)
  )
  testthat::expect_equal(result, expected, tol = 0.0001)
})

testthat::test_that("`prop_diff_wald` (proportion difference by Wald's test: with correction)", {

  # "Mid" case: 3/4 respond in group A, 1/2 respond in group B.
  rsp <- c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)
  grp <- factor(c("A", "B", "A", "B", "A", "A"), levels = c("B", "A"))

  result <- prop_diff_wald(rsp = rsp, grp = grp, conf_level = 0.9, correct = TRUE)

  expected <- list(
    diff = 0.25,
    diff_ci = c(-0.8069, 1.0000)
  )
  testthat::expect_equal(result, expected, tol = 0.0001)

  # Edge case: Same proportion of response in A and B.
  rsp <- c(TRUE, FALSE, TRUE, FALSE)
  grp <- factor(c("A", "A", "B", "B"), levels = c("A", "B"))
  result <- prop_diff_wald(rsp = rsp, grp = grp, conf_level = 0.6, correct = TRUE)

  expected <- list(
    diff = 0,
    diff_ci = c(-0.9208, 0.9208)
  )
  testthat::expect_equal(result, expected, tol = 0.0001)
})

testthat::test_that("`prop_diff_wald` (proportion difference by Wald's test: without correction)", {

  # "Mid" case: 3/4 respond in group A, 1/2 respond in group B.
  rsp <- c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)
  grp <- factor(c("A", "B", "A", "B", "A", "A"), levels = c("B", "A"))

  result <- suppressWarnings(
    prop_diff_wald(rsp = rsp, grp = grp, conf_level = 0.9, correct = FALSE)
  )
  expected <- list(
    diff = 0.25,
    diff_ci = c(-0.4319, 0.9319)
  )
  testthat::expect_equal(result, expected, tol = 0.0001)

  # Edge case: Same proportion of response in A and B.
  rsp <- c(TRUE, FALSE, TRUE, FALSE)
  grp <- factor(c("A", "A", "B", "B"), levels = c("A", "B"))
  result <- prop_diff_wald(rsp = rsp, grp = grp, conf_level = 0.6, correct = FALSE)

  expected <- list(
    diff = 0,
    diff_ci = c(-0.4208, 0.4208)
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
    diff_ci = c(-0.285363076, 0.009989872),
    weights = c(0.1148388, 0.2131696, 0.1148388, 0.2131696, 0.1767914, 0.1671918),
    n1 = c(4, 11, 8, 11, 13, 11),
    n2 = c(8, 9, 4, 9, 6, 6)
  )

  names(expected$weights) <- names(expected$n1) <- names(expected$n2) <- levels(interaction(strata_data))
  testthat::expect_equal(result, expected, tol = 0.0001)
  testthat::expect_warning(prop_diff_cmh(
    rsp = rsp[1:4], grp = grp[1:4], strata = interaction(strata_data[1:4, ]),
    conf_level = 0.90
  ))
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
    diff_ci = c(-0.32786094, -0.01567301),
    weights = c(0.2408257, 0.1297378, 0.2408257, 0.1997279, 0.1888829),
    n1 = c(11, 8, 11, 13, 11),
    n2 = c(9, 4, 9, 6, 6)
  )
  names(expected$weights) <- names(expected$n1) <- names(expected$n2) <- levels(interaction(strata_data))[-1]

  testthat::expect_equal(result, expected, tol = 0.000001)
})

testthat::test_that("`prop_strat_nc` (proportion difference by stratified Newcombe) with cmh weights", {
  set.seed(1)
  rsp <- c(
    sample(c(TRUE, FALSE), size = 40, prob = c(3 / 4, 1 / 4), replace = TRUE),
    sample(c(TRUE, FALSE), size = 40, prob = c(1 / 2, 1 / 2), replace = TRUE)
  ) # response to the treatment
  grp <- factor(rep(c("A", "B"), each = 40), levels = c("B", "A")) # treatment group
  strata_data <- data.frame(
    "f1" = sample(c("a", "b"), 80, TRUE),
    "f2" = sample(c("x", "y", "z"), 80, TRUE),
    stringsAsFactors = TRUE
  )
  strata <- interaction(strata_data)

  results <- prop_diff_strat_nc(
    rsp = rsp,
    grp = grp,
    strata = strata,
    conf_level = 0.95
  )

  # Values externally validated
  expect_equal(results$diff, 0.2539, tol = 1e-4)
  expect_equal(array(results$diff_ci), array(c(0.0347, 0.4454)), tol = 1e-4)
})

testthat::test_that("`prop_strat_nc` (proportion difference by stratified Newcombe) with wilson_h weights", {
  set.seed(1)
  rsp <- c(
    sample(c(TRUE, FALSE), size = 40, prob = c(3 / 4, 1 / 4), replace = TRUE),
    sample(c(TRUE, FALSE), size = 40, prob = c(1 / 2, 1 / 2), replace = TRUE)
  ) # response to the treatment
  grp <- factor(rep(c("A", "B"), each = 40), levels = c("B", "A")) # treatment group
  strata_data <- data.frame(
    "f1" = sample(c("a", "b"), 80, TRUE),
    "f2" = sample(c("x", "y", "z"), 80, TRUE),
    stringsAsFactors = TRUE
  )
  strata <- interaction(strata_data)

  results <- prop_diff_strat_nc(
    rsp = rsp,
    grp = grp,
    strata = strata,
    weights_method = "wilson_h",
    conf_level = 0.95
  )

  # Values internally checked (no reference yet)
  expect_equal(results$diff, 0.2587, tol = 1e-4)
  expect_equal(array(results$diff_ci), array(c(0.0391, 0.4501)), tol = 1e-4)
})

testthat::test_that("`estimate_proportion_diff` is compatible with `rtables`", {
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
      "B", "", "", "A", "25.0", "(-92.0, 100.0)"
    ),
    .Dim = c(3L, 3L)
  )

  testthat::expect_identical(to_string_matrix(result), expected)
})

testthat::test_that("`estimate_proportion_diff` and cmh is compatible with `rtables`", {
  set.seed(1)
  nex <- 100 # Number of test rows
  dta <- data.frame(
    "rsp" = sample(c(TRUE, FALSE), nex, TRUE),
    "grp" = sample(c("A", "B"), nex, TRUE),
    "f1" = sample(c("a1", "a2"), nex, TRUE),
    "f2" = sample(c("x", "y", "z"), nex, TRUE),
    stringsAsFactors = TRUE
  )
  l <- basic_table() %>%
    split_cols_by(var = "grp", ref_group = "B") %>%
    estimate_proportion_diff(
      vars = "rsp",
      variables = list(strata = c("f1", "f2")),
      conf_level = 0.90,
      .formats = c("xx.xxxx", "(xx.xxxx, xx.xxxx)"),
      method = "cmh"
    )

  result <- build_table(l, df = dta)
  result <- to_string_matrix(result)
  expected <- structure(
    c(
      "", "Difference in Response rate (%)", "90% CI (CMH, without correction)",
      "B", "", "", "A", "-4.2133", "(-20.0215, 11.5950)"
    ),
    .Dim = c(3L, 3L)
  )
  testthat::expect_identical(result, expected)
})

testthat::test_that("`estimate_proportion_diff` and strat_newcombe is compatible with `rtables`", {
  set.seed(1)
  rsp <- c(
    sample(c(TRUE, FALSE), size = 40, prob = c(3 / 4, 1 / 4), replace = TRUE),
    sample(c(TRUE, FALSE), size = 40, prob = c(1 / 2, 1 / 2), replace = TRUE)
  ) # response to the treatment
  grp <- factor(rep(c("A", "B"), each = 40), levels = c("B", "A")) # treatment group
  strata_data <- data.frame(
    "f1" = sample(c("a", "b"), 80, TRUE),
    "f2" = sample(c("x", "y", "z"), 80, TRUE),
    stringsAsFactors = TRUE
  )
  strata <- interaction(strata_data)
  dta <- cbind(rsp, grp, strata_data)
  l <- basic_table() %>%
    split_cols_by(var = "grp", ref_group = "B") %>%
    estimate_proportion_diff(
      vars = "rsp",
      variables = list(strata = c("f1", "f2")),
      conf_level = 0.95,
      .formats = c("xx.xx", "(xx.xx, xx.xx)"),
      method = "strat_newcombe"
    )
  result <- build_table(l, df = dta)
  result <- to_string_matrix(result)
  expected <- structure(
    c(
      "", "Difference in Response rate (%)",
      "95% CI (Stratified Newcombe, without correction)",
      "B", "", "", "A", "25.39", "(3.47, 44.54)"
    ),
    .Dim = c(3L, 3L)
  )

  # Values externally validated
  testthat::expect_identical(result, expected)
})
