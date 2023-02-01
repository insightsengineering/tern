testthat::test_that("pairwise works correctly", {
  suppressWarnings(testthat::expect_warning(result <- lm(SEX ~ pairwise(ARM), data = tern_ex_adsl)))
  expected <- c(
    "(Intercept)" = 1.4492754, "pairwise(ARM)B: Placebo" = 0.0027794,
    "pairwise(ARM)C: Combination" = -0.0009995
  )
  testthat::expect_equal(result$coefficients, expected, tolerance = 1e-4)
})

testthat::test_that("univariate works correctly", {
  result <- testthat::expect_silent(univariate(c("SEX", "AGE", "RACE")))
  expected <- c("SEX", "AGE", "RACE")
  attr(expected, "varname") <- "c(\"SEX\", \"AGE\", \"RACE\")"
  testthat::expect_identical(result, expected)
})

testthat::test_that("rht works correctly", {
  result <- testthat::expect_silent(rht(as.formula("y ~ m * x + b")))
  expected <- c("+", "m * x", "b")
  testthat::expect_identical(result, expected)
})

testthat::test_that("estimate_coef works correctly", {
  adtte <- tern_ex_adtte %>%
    filter(PARAMCD == "PFS") %>%
    mutate(
      ARMCD = droplevels(ARMCD),
      SEX = droplevels(SEX)
    )
  mod <- survival::coxph(
    formula = survival::Surv(time = AVAL, event = 1 - CNSR) ~ (SEX + ARMCD)^2,
    data = adtte
  )

  mmat <- stats::model.matrix(mod)[1, ]
  mmat[!mmat == 0] <- 0

  result <- estimate_coef(
    variable = "ARMCD", given = "SEX", lvl_var = "ARM A", lvl_given = "M",
    coef = stats::coef(mod), mmat = mmat, vcov = stats::vcov(mod), conf_level = .95
  )
  expected_armcd <- matrix(
    c(0, 0, 1, 1, 1),
    nrow = 1,
    dimnames = list("ARMCD/SEXM", c("coef", "se(coef)", "hr", "lcl", "ucl"))
  )
  expected_details <- "Estimations of ARMCD hazard ratio given the level of SEX compared to ARMCD level ARM A."

  testthat::expect_identical(result$ARMCD, expected_armcd)
  testthat::expect_identical(attr(result, "details"), expected_details)
})

testthat::test_that("try_car_anova works correctly", {
  mod <- survival::coxph(
    formula = survival::Surv(time = futime, event = fustat) ~ factor(rx) + survival::strata(ecog.ps),
    data = survival::ovarian
  )
  result <- try_car_anova(mod = mod, test.statistic = "Wald")
  result_aov <- c(result$aov$Df, result$aov$Chisq, result$aov$`Pr(>Chisq)`)

  testthat::expect_equal(result_aov,
    c(1, 1, 0.9678, 0.3970, 0.3252, 0.5286),
    tolerance = 1e-3
  )
  testthat::expect_identical(result$warn_text, NULL)
})

testthat::test_that("check_formula returns correct error", {
  testthat::expect_error(check_formula("y ~ m * x + b"))
})

testthat::test_that("check_covariate_formulas works correctly", {
  testthat::expect_error(check_covariate_formulas(NULL))
})

testthat::test_that("name_covariate_names works correctly", {
  result <- name_covariate_names(c(
    A = formula("y ~ A"),
    B = formula("y ~ B"),
    C = formula("y ~ C")
  ))
  expected <- list(A = formula("y ~ A"), B = formula("y ~ B"), C = formula("y ~ C"))
  testthat::expect_identical(result, expected)
})

testthat::test_that("check_increments gives correct warning", {
  testthat::expect_warning(check_increments(c(A = "A", B = "B", D = "C"), c(
    A = formula("y ~ A"),
    B = formula("y ~ B"),
    C = formula("y ~ C")
  )))
})

testthat::test_that("s_cox_multivariate works correctly with character input", {
  adtte_f <- tern_ex_adtte %>%
    subset(PARAMCD == "OS") %>%
    filter(
      PARAMCD == "OS" &
        SEX %in% c("F", "M") &
        RACE %in% c("ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE")
    ) %>%
    mutate(
      SEX = droplevels(SEX),
      RACE = droplevels(RACE),
      ARMCD = as.character(ARMCD)
    )

  result <- s_cox_multivariate(
    formula = survival::Surv(time = AVAL, event = 1 - CNSR) ~ (ARMCD + RACE + AGE)^2, data = adtte_f
  )
  expected_aov <- matrix(
    c(2, 2, 1, 4, 2, 2, 1.1569, 1.7917, 0.0108, 3.1853, 1.1363, 1.1686, 0.5608, 0.4083, 0.9174, 0.5273, 0.5666, 0.5576),
    dimnames = list(
      c("ARMCD", "RACE", "AGE", "ARMCD:RACE", "ARMCD:AGE", "RACE:AGE"),
      c("Df", "Chisq", "Pr(>Chisq)")
    ),
    ncol = 3
  )
  testthat::expect_equal(as.matrix(result$aov), expected_aov, tolerance = 1e-3)
})
