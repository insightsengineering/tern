testthat::test_that("`prop_diff_ha` (proportion difference by Anderson-Hauck)", {
  # "Mid" case: 3/4 respond in group A, 1/2 respond in group B.
  rsp <- c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)
  grp <- factor(c("A", "B", "A", "B", "A", "A"), levels = c("B", "A"))

  result <- prop_diff_ha(rsp = rsp, grp = grp, conf_level = 0.90)
  # according to SAS.
  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Edge case: Same proportion of response in A and B.
  rsp <- c(TRUE, FALSE, TRUE, FALSE)
  grp <- factor(c("A", "A", "B", "B"), levels = c("A", "B"))
  result <- prop_diff_ha(rsp = rsp, grp = grp, conf_level = 0.6)
  # according to SAS.
  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("`prop_diff_nc` (proportion difference by Newcombe)", {
  # "Mid" case: 3/4 respond in group A, 1/2 respond in group B.
  rsp <- c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)
  grp <- factor(c("A", "B", "A", "B", "A", "A"), levels = c("B", "A"))

  result <- suppressWarnings(
    prop_diff_nc(rsp = rsp, grp = grp, conf_level = 0.9)
  )
  # according to SAS.
  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Edge case: Same proportion of response in A and B.
  rsp <- c(TRUE, FALSE, TRUE, FALSE)
  grp <- factor(c("A", "A", "B", "B"), levels = c("A", "B"))
  result <- prop_diff_nc(rsp = rsp, grp = grp, conf_level = 0.6)
  # according to SAS.
  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("`prop_diff_wald` (proportion difference by Wald's test: with correction)", {
  # "Mid" case: 3/4 respond in group A, 1/2 respond in group B.
  rsp <- c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)
  grp <- factor(c("A", "B", "A", "B", "A", "A"), levels = c("B", "A"))

  result <- prop_diff_wald(rsp = rsp, grp = grp, conf_level = 0.9, correct = TRUE)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Edge case: Same proportion of response in A and B.
  rsp <- c(TRUE, FALSE, TRUE, FALSE)
  grp <- factor(c("A", "A", "B", "B"), levels = c("A", "B"))
  result <- prop_diff_wald(rsp = rsp, grp = grp, conf_level = 0.6, correct = TRUE)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Edge case: All respond in all groups.
  rsp <- c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
  grp <- factor(c("A", "B", "A", "B", "A", "A"), levels = c("B", "A"))

  result <- suppressWarnings(
    prop_diff_wald(rsp = rsp, grp = grp, conf_level = 0.9, correct = TRUE)
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("`prop_diff_wald` (proportion difference by Wald's test: without correction)", {
  # "Mid" case: 3/4 respond in group A, 1/2 respond in group B.
  rsp <- c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)
  grp <- factor(c("A", "B", "A", "B", "A", "A"), levels = c("B", "A"))

  result <- suppressWarnings(
    prop_diff_wald(rsp = rsp, grp = grp, conf_level = 0.9, correct = FALSE)
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Edge case: Same proportion of response in A and B.
  rsp <- c(TRUE, FALSE, TRUE, FALSE)
  grp <- factor(c("A", "A", "B", "B"), levels = c("A", "B"))
  result <- prop_diff_wald(rsp = rsp, grp = grp, conf_level = 0.6, correct = FALSE)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Edge case: All respond in all groups.
  rsp <- c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
  grp <- factor(c("A", "B", "A", "B", "A", "A"), levels = c("B", "A"))

  result <- suppressWarnings(
    prop_diff_wald(rsp = rsp, grp = grp, conf_level = 0.9, correct = FALSE)
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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
  expect_equal(results$diff, 0.2539, tolerance = 1e-4)
  expect_equal(as.numeric(results$diff_ci), c(0.0347, 0.4454), tolerance = 1e-3)
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
  expect_equal(results$diff, 0.2587, tolerance = 1e-4)
  expect_equal(as.numeric(results$diff_ci), c(0.0391, 0.4501), tolerance = 1e-3)
})

testthat::test_that("prop_diff_strat_nc output matches equivalent SAS function output", {
  set.seed(1)
  rsp <- c(
    sample(c(TRUE, FALSE), size = 40, prob = c(3 / 4, 1 / 4), replace = TRUE),
    sample(c(TRUE, FALSE), size = 40, prob = c(1 / 2, 1 / 2), replace = TRUE)
  )
  grp <- factor(rep(c("A", "B"), each = 40), levels = c("B", "A"))
  strata_data <- data.frame(
    "f1" = sample(c("a", "b"), 80, TRUE),
    "f2" = sample(c("x", "y", "z"), 80, TRUE),
    stringsAsFactors = TRUE
  )
  strata <- interaction(strata_data)
  weights <- 1:6 / sum(1:6)

  nc <- prop_diff_strat_nc(rsp = rsp, grp = grp, strata = strata, conf_level = 0.95)
  result <- c(value = nc$diff, nc$diff_ci)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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
    split_cols_by(var = "grp", ref_group = "B", split_fun = ref_group_position("first")) %>%
    estimate_proportion_diff(
      vars = "rsp",
      variables = list(strata = c("f1", "f2")),
      conf_level = 0.90,
      .formats = c("xx.xxxx", "(xx.xxxx, xx.xxxx)"),
      method = "cmh"
    )

  result <- build_table(l, df = dta)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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
  result <- to_string_matrix(result, with_spaces = FALSE, print_txt_to_copy = FALSE)
  expected <- structure(
    c(
      "", "Difference in Response rate (%)",
      "  95% CI (Stratified Newcombe, without correction)",
      "B", "", "", "A", "25.39", "(3.47, 44.54)"
    ),
    .Dim = c(3L, 3L)
  )

  # Values externally validated
  testthat::expect_identical(result, expected)
})

testthat::test_that("s_proportion_diff works with no strata", {
  nex <- 100
  set.seed(2)
  dta <- data.frame(
    "rsp" = sample(c(TRUE, FALSE), nex, TRUE),
    "grp" = sample(c("A", "B"), nex, TRUE),
    "f1" = sample(c("a1", "a2"), nex, TRUE),
    "f2" = sample(c("x", "y", "z"), nex, TRUE),
    stringsAsFactors = TRUE
  )
  result <- s_proportion_diff(
    df = subset(dta, grp == "A"),
    .var = "rsp",
    .ref_group = subset(dta, grp == "B"),
    .in_ref_col = FALSE,
    conf_level = 0.90,
    method = "ha"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_proportion_diff works with strata", {
  nex <- 100
  set.seed(2)
  dta <- data.frame(
    "rsp" = sample(c(TRUE, FALSE), nex, TRUE),
    "grp" = sample(c("A", "B"), nex, TRUE),
    "f1" = sample(c("a1", "a2"), nex, TRUE),
    "f2" = sample(c("x", "y", "z"), nex, TRUE),
    stringsAsFactors = TRUE
  )
  result <- s_proportion_diff(
    df = subset(dta, grp == "A"),
    .var = "rsp",
    .ref_group = subset(dta, grp == "B"),
    .in_ref_col = FALSE,
    variables = list(strata = c("f1", "f2")),
    conf_level = 0.90,
    method = "cmh"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
