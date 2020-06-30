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
      "Odds Ratio*",
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
