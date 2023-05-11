testthat::test_that("univariate works correctly", {
  result <- testthat::expect_silent(univariate(c("SEX", "AGE", "RACE")))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  suppressWarnings(testthat::expect_warning(result2 <- lm(SEX ~ univariate(ARM), data = tern_ex_adsl)))

  res2 <- testthat::expect_silent(result2)
  testthat::expect_snapshot(res2)
})

testthat::test_that("rht works correctly", {
  result <- testthat::expect_silent(rht(as.formula("y ~ m * x + b")))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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

  res <- testthat::expect_silent(result$ARMCD)
  testthat::expect_snapshot(res)

  res <- testthat::expect_silent(attr(result, "details"))
  testthat::expect_snapshot(res)
})

testthat::test_that("try_car_anova works correctly", {
  mod <- survival::coxph(
    formula = survival::Surv(time = futime, event = fustat) ~ factor(rx) + survival::strata(ecog.ps),
    data = survival::ovarian
  )
  result <- try_car_anova(mod = mod, test.statistic = "Wald")
  result_aov <- c(result$aov$Df, result$aov$Chisq, result$aov$`Pr(>Chisq)`)

  res <- testthat::expect_silent(result_aov)
  testthat::expect_snapshot(res)

  res <- testthat::expect_silent(result$warn_text)
  testthat::expect_snapshot(res)
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

  res <- testthat::expect_silent(result$aov)
  testthat::expect_snapshot(res)
})
