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

testthat::test_that("`prop_diff_cmh` with Sato variance estimator for difference", {
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
    conf_level = 0.90, diff_se = "sato"
  )

  # Comparison values from SAS PROC FREQ TABLES with COMMONRISKDIFF option:
  expect_equal(result$se_diff, 0.1081, tolerance = 1e-3)
  expect_equal(result$diff_ci, c(-0.3154, 0.0400), tolerance = 1e-3)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_miettinen_nurminen_var_est works as expected", {
  result <- h_miettinen_nurminen_var_est(
    n1 = 10, n2 = 15,
    x1 = 4, x2 = 6, diff_par = 0.1
  )
  expect_snapshot_value(result, style = "deparse", tolerance = 1e-4)

  result2 <- h_miettinen_nurminen_var_est(
    n1 = c(10, 12), n2 = c(15, 18),
    x1 = c(4, 2), x2 = c(6, 8), diff_par = 0.1
  )
  expect_snapshot_value(result2, style = "deparse", tolerance = 1e-4)
})

testthat::test_that("prop_diff_cmh works correctly with Miettinen-Nurminen variance estimator", {
  # Example from the Lu (2008) paper, described in Melikov and Mosier (2025),
  # https://pharmasug.org/proceedings/2025/SA/PharmaSUG-2025-SA-198.pdf

  # Summary data
  df_sum <- data.frame(
    Stratum = c(1, 1, 2, 2, 3, 3),
    Trt     = c(1, 2, 1, 2, 1, 2),
    N       = c(15, 5, 10, 10, 25, 35),
    Events  = c(1, 3, 3, 4, 2, 18)
  )

  # Expand into subject-level binary outcomes
  df_bin <- do.call(
    rbind,
    lapply(seq_len(nrow(df_sum)), function(i) {
      with(df_sum[i, ], {
        data.frame(
          Stratum = Stratum,
          Trt     = Trt,
          Outcome = c(rep(1, Events), rep(0, N - Events))
        )
      })
    })
  )

  result <- prop_diff_cmh(
    rsp = as.logical(df_bin$Outcome),
    grp = factor(df_bin$Trt, levels = c(2, 1)),
    strata = factor(df_bin$Stratum),
    diff_se = "miettinen_nurminen",
    conf_level = 0.95
  )
  # Compare with result on p.5 from Melikov and Mosier (2025).
  expect_equal(result$diff, -0.37857, tolerance = 1e-4)
  expect_equal(result$diff_ci, c(-0.5381, -0.1962), tolerance = 1e-3)

  # Reasonable result for the standard error (was not reported).
  expect_equal(result$se_diff, 0.08753, tolerance = 1e-4)
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
  testthat::expect_false(anyNA(result$diff_ci))

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


testthat::test_that("`prop_diff_uncond_exact` matches reference values", {
  mk_data <- function(n11, n21, n1, n2) {
    rsp <- c(rep(TRUE, n21), rep(FALSE, n2 - n21), rep(TRUE, n11), rep(FALSE, n1 - n11))
    grp <- factor(c(rep("B", n2), rep("A", n1)), levels = c("B", "A"))
    list(rsp = rsp, grp = grp)
  }

  case1 <- mk_data(n11 = 40, n21 = 5, n1 = 78, n2 = 17)
  result1 <- prop_diff_uncond_exact(rsp = case1$rsp, grp = case1$grp, conf_level = 0.95)
  expect_equal(result1$diff, 0.2187, tolerance = 1e-4)
  expect_equal(result1$diff_ci, c(-0.0466, 0.4676), tolerance = 1e-4)

  case2 <- mk_data(n11 = 27, n21 = 3, n1 = 57, n2 = 3)
  result2 <- prop_diff_uncond_exact(rsp = case2$rsp, grp = case2$grp, conf_level = 0.95)
  expect_equal(result2$diff, -0.5263, tolerance = 1e-4)
  expect_equal(result2$diff_ci, c(-0.9057, 0.1197), tolerance = 1e-4)

  result3 <- prop_diff_uncond_exact(rsp = case2$rsp, grp = case2$grp, conf_level = 0.99)
  expect_equal(result3$diff, -0.5263, tolerance = 1e-4)
  expect_equal(result3$diff_ci, c(-0.9586, 0.2677), tolerance = 1e-4)

  case4 <- mk_data(n11 = 0, n21 = 2, n1 = 2, n2 = 2)
  result4 <- prop_diff_uncond_exact(rsp = case4$rsp, grp = case4$grp, conf_level = 0.90)
  expect_equal(result4$diff, -1, tolerance = 1e-8)
  expect_equal(result4$diff_ci, c(-1, 0.0543), tolerance = 1e-3)
})

testthat::test_that("h_worst_case_tail_probability returns valid tail probabilities", {
  n1 <- 2
  n2 <- 2
  t0 <- 0
  tables <- expand.grid(n11 = 0:n1, n21 = 0:n2)
  t_values <- tables$n11 / n1 - tables$n21 / n2

  p_upper <- h_worst_case_tail_probability(
    d_star = 0,
    n1 = n1,
    n2 = n2,
    t_values = t_values,
    t0 = t0,
    tables = tables,
    tail = "upper"
  )
  p_lower <- h_worst_case_tail_probability(
    d_star = 0,
    n1 = n1,
    n2 = n2,
    t_values = t_values,
    t0 = t0,
    tables = tables,
    tail = "lower"
  )

  expect_gte(p_upper, 0)
  expect_lte(p_upper, 1)
  expect_gte(p_lower, 0)
  expect_lte(p_lower, 1)
})

testthat::test_that("h_worst_case_tail_probability handles degenerate p2 interval", {
  n1 <- 2
  n2 <- 2
  t0 <- 0
  tables <- expand.grid(n11 = 0:n1, n21 = 0:n2)
  t_values <- tables$n11 / n1 - tables$n21 / n2

  p_upper <- h_worst_case_tail_probability(
    d_star = 1,
    n1 = n1,
    n2 = n2,
    t_values = t_values,
    t0 = t0,
    tables = tables,
    tail = "upper"
  )
  p_lower <- h_worst_case_tail_probability(
    d_star = 1,
    n1 = n1,
    n2 = n2,
    t_values = t_values,
    t0 = t0,
    tables = tables,
    tail = "lower"
  )

  expect_equal(p_upper, 1, tolerance = 1e-10)
  expect_equal(p_lower, 0, tolerance = 1e-10)
})

testthat::test_that("h_find_ci_bound_uniroot finds expected roots", {
  p_increasing <- function(d) (d + 1) / 2
  p_decreasing <- function(d) (1 - d) / 2

  lower <- h_find_ci_bound_uniroot(
    p_value_function = p_increasing,
    cutoff = 0.25,
    direction = "increasing"
  )
  upper <- h_find_ci_bound_uniroot(
    p_value_function = p_decreasing,
    cutoff = 0.25,
    direction = "decreasing"
  )

  expect_equal(lower, -0.5, tolerance = 1e-6)
  expect_equal(upper, 0.5, tolerance = 1e-6)
})

testthat::test_that("h_find_ci_bound_uniroot handles boundary case", {
  always_large <- function(d) 0.9

  boundary <- h_find_ci_bound_uniroot(
    p_value_function = always_large,
    cutoff = 0.25,
    direction = "increasing",
    interval = c(-1, 1)
  )

  expect_equal(boundary, -1)
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
      .formats = c(diff = "xx.xxxx", diff_ci = "(xx.xxxx, xx.xxxx)"),
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
      .formats = c(diff = "xx.xx", diff_ci = "(xx.xx, xx.xx)"),
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

testthat::test_that("s_proportion_diff works with CMH Sato method", {
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
    method = "cmh_sato"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_proportion_diff works with CMH Miettinen and Nurminen method", {
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
    method = "cmh_mn"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot_value(res, style = "deparse", tolerance = 1e-4)
})


testthat::test_that("s_proportion_diff works with uncond_exact_diff", {
  dta <- data.frame(
    rsp = c(rep(TRUE, 5), rep(FALSE, 12), rep(TRUE, 40), rep(FALSE, 38)),
    grp = c(rep("B", 17), rep("A", 78)),
    stringsAsFactors = FALSE
  )

  result <- s_proportion_diff(
    df = subset(dta, grp == "A"),
    .var = "rsp",
    .ref_group = subset(dta, grp == "B"),
    .in_ref_col = FALSE,
    conf_level = 0.95,
    method = "uncond_exact_diff"
  )

  expect_equal(as.numeric(result$diff), 21.87, tolerance = 1e-2)
  expect_equal(as.numeric(result$diff_ci), c(-4.66, 46.76), tolerance = 1e-2)
  expect_identical(attr(result$diff_ci, "label"), "95% CI (Unconditional exact)")
})

testthat::test_that("s_proportion_diff rejects uncond_exact_diff with strata", {
  dta <- data.frame(
    rsp = c(TRUE, FALSE, TRUE, FALSE),
    grp = c("A", "A", "B", "B"),
    strata = c("S1", "S2", "S1", "S2"),
    stringsAsFactors = FALSE
  )

  expect_error(
    s_proportion_diff(
      df = subset(dta, grp == "A"),
      .var = "rsp",
      .ref_group = subset(dta, grp == "B"),
      .in_ref_col = FALSE,
      variables = list(strata = "strata"),
      conf_level = 0.95,
      method = "uncond_exact_diff"
    ),
    "only available for unstratified analyses"
  )
})

testthat::test_that("check_diff_prop_ci is silent with healthy input", {
  # "Mid" case: 3/4 respond in group A, 1/2 respond in group B.
  rsp <- c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)
  grp <- factor(c("A", "B", "A", "B", "A", "A"), levels = c("B", "A"))

  testthat::expect_silent(check_diff_prop_ci(
    rsp = rsp, grp = grp, conf_level = 0.90
  ))
})

testthat::test_that("check_diff_prop_ci fails with wrong input", {
  rsp <- c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)
  grp <- c("A", "B", "A", "B", "A", "A")

  testthat::expect_error(check_diff_prop_ci(
    rsp = rsp, grp = grp, conf_level = "0.90"
  ))
})
