testthat::test_that("estimate_coef works correctly", {
  adtte <- adtte_raw %>%
    filter(PARAMCD == "PFS") %>%
    mutate(
      ARMCD = droplevels(ARMCD),
      SEX = droplevels(SEX)
    )
  mod <- coxph(
    formula = Surv(time = AVAL, event = 1 - CNSR) ~ (SEX + ARMCD)^2,
    data = adtte
  )

  mmat <- stats::model.matrix(mod)[1, ]
  mmat[!mmat == 0] <- 0

  result <- estimate_coef(
    variable = "ARMCD", given = "SEX", lvl_var = "ARM A", lvl_given = "M",
    coef = stats::coef(mod), mmat = mmat, vcov = stats::vcov(mod), conf_level = .95
  )
  expected_armcd <- matrix(
    c(0, 0, 1, 1, 1), nrow = 1,
    dimnames = list("ARMCD/SEXM", c("coef", "se(coef)", "hr", "lcl", "ucl"))
  )
  expected_details <- "Estimations of ARMCD hazard ratio given the level of SEX compared to ARMCD level ARM A."

  testthat::expect_identical(result$ARMCD, expected_armcd)
  testthat::expect_identical(attr(result, "details"), expected_details)
})

testthat::test_that("try_car_anova works correctly", {
  mod <- coxph(
    formula = Surv(time = futime, event = fustat) ~ factor(rx) + strata(ecog.ps),
    data = ovarian
  )
  result <- try_car_anova(mod = mod, test.statistic = "Wald")
  result_aov <- c(result$aov$Df, result$aov$Chisq, result$aov$`Pr(>Chisq)`)

  testthat::expect_equal(result_aov, c(1, 0.7521, 0.3858), tolerance = 1e-3)
  testthat::expect_identical(result$warn_text, NULL)
})

testthat::test_that("s_cox_multivariate works correctly", {
  adtte_f <- adtte_raw %>% subset(PARAMCD == "OS") %>%
    filter(
      PARAMCD == "OS" &
        SEX %in% c("F", "M") &
        RACE %in% c("ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE")
    ) %>%
    mutate(
      SEX = droplevels(SEX),
      RACE = droplevels(RACE)
    )

  result <- s_cox_multivariate(
    formula = Surv(time = AVAL, event = 1 - CNSR) ~ (ARMCD + RACE + AGE)^2, data = adtte_f
  )
  expected_aov <- matrix(
    c(2, 2, 1, 4, 2, 2, 1.5484, 0.4485, 0.0668, 3.3770, 0.2824, 0.6636, 0.4611, 0.7991, 0.7961, 0.4968, 0.8683, 0.7176),
    dimnames = list(c("ARMCD", "RACE", "AGE", "ARMCD:RACE", "ARMCD:AGE", "RACE:AGE"),
                    c("Df", "Chisq", "Pr(>Chisq)")),
    ncol = 3
  )
  testthat::expect_equal(as.matrix(result$aov), expected_aov, tolerance = 1e-3)
})
