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

get_example_data <- function() {
  read.table(
    header = TRUE,
    sep = ";",
    stringsAsFactors = FALSE,
    text = '
    "USUBJID";"AGE";"RESPONSE";"ARMCD";"COUNTRY"
    "AB12345-XYZ1-id-1";"33";"Y";"ARM C";"AFG"
    "AB12345-XYZ2-id-2";"20";"Y";"ARM C";"BES"
    "AB12345-XYZ1-id-3";"27";"Y";"ARM C";"AFG"
    "AB12345-XYZ1-id-4";"64";"N";"ARM A";"CUW"
    "AB12345-XYZ2-id-5";"43";"N";"ARM B";"SXM"
    "AB12345-XYZ2-id-6";"28";"Y";"ARM B";"CUW"
    "AB12345-XYZ1-id-7";"21";"Y";"ARM B";"BES"
    "AB12345-XYZ1-id-8";"35";"Y";"ARM B";"BES"
    "AB12345-XYZ2-id-9";"20";"Y";"ARM C";"CUW"
    "AB12345-XYZ2-id-10";"30";"N";"ARM A";"BES"
    "AB12345-XYZ2-id-11";"25";"N";"ARM A";"AFG"
    "AB12345-XYZ2-id-12";"63";"N";"ARM B";"SXM"
    "AB12345-XYZ2-id-13";"60";"Y";"ARM B";"CUW"
    "AB12345-XYZ2-id-14";"38";"N";"ARM A";"AFG"
    "AB12345-XYZ2-id-15";"20";"N";"ARM C";"AFG"
    "AB12345-XYZ2-id-16";"58";"Y";"ARM B";"AFG"
    "AB12345-XYZ1-id-17";"57";"Y";"ARM A";"AFG"
    "AB12345-XYZ2-id-18";"54";"Y";"ARM A";"BES"
    "AB12345-XYZ2-id-19";"54";"Y";"ARM B";"AFG"
    "AB12345-XYZ1-id-20";"32";"Y";"ARM B";"SXM"
    "AB12345-XYZ1-id-21";"54";"Y";"ARM B";"CUW"
    "AB12345-XYZ1-id-22";"66";"Y";"ARM A";"BES"
    "AB12345-XYZ1-id-23";"40";"Y";"ARM C";"SXM"
    "AB12345-XYZ1-id-24";"20";"Y";"ARM C";"AFG"
    "AB12345-XYZ1-id-25";"55";"N";"ARM C";"BES"
    "AB12345-XYZ2-id-26";"55";"N";"ARM A";"SXM"
    "AB12345-XYZ2-id-27";"33";"N";"ARM B";"SXM"
    "AB12345-XYZ2-id-28";"73";"N";"ARM C";"AFG"
    "AB12345-XYZ2-id-29";"24";"Y";"ARM B";"BES"
    "AB12345-XYZ2-id-30";"46";"Y";"ARM A";"SXM"'
  ) %>%
#nolint start
  dplyr::mutate(
    ARMCD = factor(ARMCD),
    RESPONSE = factor(RESPONSE),
    COUNTRY = factor(COUNTRY)
  )
#nolint end
}

test_that("Elementary Odds Ratio table works correctly with stratification", {
  anl <- get_example_data()
  result <- t_el_odds_ratio(
    rsp = anl$RESPONSE == "Y",
    col_by = anl$ARMCD,
    conf_level = 0.9,
    strata_data = anl[, c("COUNTRY"), drop = FALSE]
  )
  expected <- rtable(
    header = rheader(
      rrowl(NULL, "ARM A", "ARM B", "ARM C"),
      rrowl(NULL, "(N=9)", "(N=12)", "(N=9)")
    ),
    rrowl(
      "Responders",
      list(c(4, 0.444), c(9, 0.75), c(6, 0.667)),
      format = "xx.xx (xx.xx%)"
    ),
    rrowl(
      "Odds Ratio",
      list(NULL, c(3.858, 0.772, 19.284), c(2.266, 0.466, 11.021)),
      format = "xx.xx (xx.xx - xx.xx)"
    )
  )
  comp <- compare_rtables(result, expected, comp.attr = FALSE)
  expect_true(all(comp == "."), "t_el_odds_ratio does not produce same results any longer")
})

test_that("Elementary Odds Ratio table works correctly without stratification", {
  anl <- get_example_data()
  result <- t_el_odds_ratio(
    rsp = anl$RESPONSE == "Y",
    col_by = anl$ARMCD,
    conf_level = 0.99
  )
  expected <- rtable(
    header = rheader(
      rrowl(NULL, "ARM A", "ARM B", "ARM C"),
      rrowl(NULL, "(N=9)", "(N=12)", "(N=9)")
    ),
    rrowl(
      "Responders",
      list(c(4, 0.444), c(9, 0.75), c(6, 0.667)),
      format = "xx.xx (xx.xx%)"
    ),
    rrowl(
      "Odds Ratio",
      list(NULL, c(3.75, 0.328, 42.856), c(2.5, 0.203, 30.781)),
      format = "xx.xx (xx.xx - xx.xx)"
    )
  )
  comp <- compare_rtables(result, expected, comp.attr = FALSE)
  expect_true(all(comp == "."), "t_el_odds_ratio does not produce the same results any longer")
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

test_that("`s_proportion_diff` works with Anderson-Hauck CI", {
  # "Mid" case: 3/4 respond in group A, 1/2 respond in group B.
  rsp1 <- c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)
  trt1 <- factor(c("A", "B", "A", "B", "A", "A"), levels = c("B", "A"))
  result1 <- s_proportion_diff(
    x = rsp1,
    grp = trt1,
    conf_level = 0.9,
    diff_ci_method = "anderson-hauck"
  )
  expected1 <- list(
    diff = 0.25,
    diff_ci = c(-0.9195, 1),  # From SAS.
    label_ci = "90% CI for difference (Anderson-Hauck)"
  )
  expect_equal(result1, expected1, tol = 0.0001)

  # Corner case: Same proportion of response in A and B.
  rsp2 <- c(TRUE, FALSE, TRUE, FALSE)
  trt2 <- factor(c("A", "A", "B", "B"), levels = c("A", "B"))
  result2 <- s_proportion_diff(
    x = rsp2,
    grp = trt2,
    conf_level = 0.6,
    diff_ci_method = "anderson-hauck"
  )
  expected2 <- list(
    diff = 0,
    diff_ci = c(-0.8451, 0.8451),  # From SAS.
    label_ci = "60% CI for difference (Anderson-Hauck)"
  )
  expect_equal(result2, expected2, tol = 0.0001)
})

test_that("`s_proportion_diff` works with Newcombe CI", {
  # "Mid" case: 3/4 respond in group A, 1/2 respond in group B.
  rsp1 <- c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)
  trt1 <- factor(c("A", "B", "A", "B", "A", "A"), levels = c("B", "A"))
  result1 <- s_proportion_diff(
    x = rsp1,
    grp = trt1,
    conf_level = 0.9,
    diff_ci_method = "newcombe"
  )
  expected1 <- list(
    diff = 0.25,
    diff_ci = c(-0.2967, 0.6750),  # From SAS.
    label_ci = "90% CI for difference (Newcombe)"
  )
  expect_equal(result1, expected1, tol = 0.0001)

  # Corner case: Same proportion of response in A and B.
  rsp2 <- c(TRUE, FALSE, TRUE, FALSE)
  trt2 <- factor(c("A", "A", "B", "B"), levels = c("A", "B"))
  result2 <- s_proportion_diff(
    x = rsp2,
    grp = trt2,
    conf_level = 0.6,
    diff_ci_method = "newcombe"
  )
  expected2 <- list(
    diff = 0,
    diff_ci = c(-0.3616, 0.3616),  # From SAS.
    label_ci = "60% CI for difference (Newcombe)"
  )
  expect_equal(result2, expected2, tol = 0.0001)
})

test_that("`s_test_proportion_diff` works with Fisher's Exact Test", {
  # "Mid" case: 3/10 respond in group A, 4/8 respond in group B.
  rsp1 <- c(
    rep(c(TRUE, FALSE), c(3, 7)),
    rep(c(TRUE, FALSE), c(4, 4))
  )
  trt1 <- factor(
    rep(c("A", "B"), c(10, 8)),
    levels = c("B", "A")
  )
  result1 <- s_test_proportion_diff(
    x = rsp1,
    grp = trt1,
    test = "fisher"
  )
  expected1 <- list(
    p_value = 0.6305,  # From SAS.
    test_name = "p-value (Fisher's Exact Test)"
  )
  expect_equal(result1, expected1, tol = 0.0001)

  # Corner case: Same proportion of response in A and B.
  rsp2 <- c(TRUE, FALSE, TRUE, FALSE)
  trt2 <- factor(c("A", "A", "B", "B"), levels = c("A", "B"))
  result2 <- s_test_proportion_diff(
    x = rsp2,
    grp = trt2,
    test = "fisher"
  )
  expected2 <- list(
    p_value = 1,  # From SAS.
    test_name = "p-value (Fisher's Exact Test)"
  )
  expect_equal(result2, expected2, tol = 0.0001)
})

# Alternative implementation of the Schouten p-value, given a 2x2 contingency table.
# Source:
# https://www.phusewiki.org/wiki/index.php?title=Analysis_of_Response_(Proportions)
schouten_pval <- function(t_tbl) {
  r1 <- t_tbl[1, "TRUE"]
  r2 <- t_tbl[2, "TRUE"]
  n1 <- sum(t_tbl[1, ])
  n2 <- sum(t_tbl[2, ])
  test_num <- (n1 + n2 - 1) * (abs(r2 * (n1 - r1) - r1 * (n2 - r2)) - min(n1, n2) / 2)^2
  test_denom <- n1 * n2 * (r1 + r2) * (n1 + n2 - r1 - r2)
  test_stat <- test_num / test_denom
  p_value <- pchisq(test_stat, df = 1, lower.tail = FALSE)
  return(p_value)
}

test_that("`s_test_proportion_diff` works with Chi-Squared Test with Schouten Correction", {
  # "Mid" case: 3/10 respond in group A, 4/8 respond in group B.
  rsp1 <- c(
    rep(c(TRUE, FALSE), c(3, 7)),
    rep(c(TRUE, FALSE), c(4, 4))
  )
  trt1 <- factor(
    rep(c("A", "B"), c(10, 8)),
    levels = c("B", "A")
  )
  result1 <- s_test_proportion_diff(
    x = rsp1,
    grp = trt1,
    test = "schouten"
  )
  expected1 <- list(
    p_value = schouten_pval(table(trt1, rsp1)),
    test_name = "p-value (Chi-Squared Test with Schouten Correction)"
  )
  expect_equal(result1, expected1, tol = 0.0001)

  # Corner case: Same proportion of response in A and B.
  rsp2 <- c(TRUE, FALSE, TRUE, FALSE)
  trt2 <- factor(c("A", "A", "B", "B"), levels = c("A", "B"))
  result2 <- s_test_proportion_diff(
    x = rsp2,
    grp = trt2,
    test = "schouten"
  )
  expected2 <- list(
    p_value = schouten_pval(table(trt2, rsp2)),
    test_name = "p-value (Chi-Squared Test with Schouten Correction)"
  )
  expect_equal(result2, expected2, tol = 0.0001)
})
