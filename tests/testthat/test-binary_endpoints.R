context("test binary endpoint functions")

test_that("`s_odds_ratio` works for unstratified analysis", {
  set.seed(1)
  rsp <- sample(c(TRUE, FALSE), 100, TRUE)
  groups <- c("Placebo", "Treatment", "Combination")
  trt <- sample(groups, 100, TRUE)
  trt <- factor(trt, levels = groups)

  result <- s_odds_ratio(
    rsp = rsp,
    col_by = trt
  )
  expected <- structure(
    data.frame(
      level = c("Treatment", "Combination"),
      odds_ratio = c(2.671, 0.658),
      ci_lower = c(0.955, 0.247),
      ci_upper = c(7.476, 1.749)
    ),
    conf_level = 0.95
  )
  expect_equal(result, expected, tol = 0.001)
})

test_that("`s_odds_ratio` works for stratified analysis", {
  set.seed(1)
  rsp <- sample(c(TRUE, FALSE), 100, TRUE)
  groups <- c("Placebo", "Treatment", "Combination")
  trt <- sample(groups, 100, TRUE)
  trt <- factor(trt, levels = groups)
  strata_data <- data.frame(
    "f1" = sample(c("a", "b"), 100, TRUE),
    "f2" = sample(c("x", "y", "z"), 100, TRUE),
    stringsAsFactors = TRUE
  )
  strat <- interaction(strata_data)

  result <- s_odds_ratio(
    rsp = rsp,
    col_by = trt,
    strat = strat
  )
  expected <- structure(
    data.frame(
      level = c("Treatment", "Combination"),
      odds_ratio = c(2.711, 0.652),
      ci_lower = c(0.963, 0.244),
      ci_upper = c(7.637, 1.740)
    ),
    conf_level = 0.95
  )
  expect_equal(result, expected, tol = 0.001)
})

test_that("`s_odds_ratio` returns NA values when `rsp` has only one unique value", {
  set.seed(1)
  rsp <- rep(TRUE, 100)
  groups <- c("Placebo", "Treatment", "Combination")
  trt <- sample(groups, 100, TRUE)
  trt <- factor(trt, levels = groups)

  result <- s_odds_ratio(
    rsp = rsp,
    col_by = trt
  )
  expected <- structure(
    data.frame(
      level = c("Treatment", "Combination"),
      odds_ratio = c(NA, NA),
      ci_lower = c(NA, NA),
      ci_upper = c(NA, NA)
    ),
    conf_level = 0.95
  )
  expect_equal(result, expected, tol = 0.001)
})

test_that("`s_odds_ratio` works also when there are only 2 `col_by` levels", {
  set.seed(1)
  rsp <- sample(c(TRUE, FALSE), 100, TRUE)
  groups <- c("Placebo", "Treatment")
  trt <- sample(groups, 100, TRUE)
  trt <- factor(trt, levels = groups)

  result <- s_odds_ratio(
    rsp = rsp,
    col_by = trt
  )
  expected <- structure(
    data.frame(
      level = c("Treatment"),
      odds_ratio = c(1.169),
      ci_lower = c(0.532),
      ci_upper = c(2.565)
    ),
    conf_level = 0.95
  )
  expect_equal(result, expected, tol = 0.001)
})

test_that("`s_proportion` works with Agresti-Coull CI", {
  # "Mid" case.
  rsp1 <- c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)
  result1 <- s_proportion(
    x = rsp1,
    conf_level = 0.9,
    prop_ci_method = "agresti-coull"
  )
  expected1 <- list(
    prop = 4 / 6,
    prop_ci = c(0.3436, 0.8862),  # From SAS.
    label_ci = "90% CI for Response Rates (Agresti-Coull)"
  )
  expect_equal(result1, expected1, tol = 0.0001)

  # Corner case: Only responders.
  rsp2 <- c(TRUE, TRUE, TRUE, TRUE)
  result2 <- s_proportion(
    x = rsp2,
    conf_level = 0.95,
    prop_ci_method = "agresti-coull"
  )
  expected2 <- list(
    prop = 1,
    prop_ci = c(0.4540, 1),  # From SAS.
    label_ci = "95% CI for Response Rates (Agresti-Coull)"
  )
  expect_equal(result2, expected2, tol = 0.00011)  # Small additional difference here.
})

test_that("`s_proportion` works with Jeffreys CI", {
  # "Mid" case.
  rsp1 <- c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)
  result1 <- s_proportion(
    x = rsp1,
    conf_level = 0.9,
    prop_ci_method = "jeffreys"
  )
  expected1 <- list(
    prop = 4 / 6,
    prop_ci = c(0.3408, 0.8957),  # From SAS.
    label_ci = "90% CI for Response Rates (Jeffreys)"
  )
  expect_equal(result1, expected1, tol = 0.0001)

  # Corner case: Only responders.
  rsp2 <- c(TRUE, TRUE, TRUE, TRUE)
  result2 <- s_proportion(
    x = rsp2,
    conf_level = 0.95,
    prop_ci_method = "jeffreys"
  )
  expected2 <- list(
    prop = 1,
    prop_ci = c(0.5552, 1),  # From SAS.
    label_ci = "95% CI for Response Rates (Jeffreys)"
  )
  expect_equal(result2, expected2, tol = 0.0001)
})
