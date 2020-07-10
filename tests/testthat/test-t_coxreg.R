context("The Cox (PH) regression models.")

test_that(
  "Warning issues by `car::Anova` are well accounted for.",
  code = {

    # Example data set and associated cox regression model including a `strata`.
    library(survival)
    form <- Surv(time = futime, event = fustat) ~ factor(rx) + strata(ecog.ps)
    mod <- coxph(formula = form, data = ovarian)
    app_fit <- suppressMessages(
      fit_n_aov(
        formula = form, data = ovarian, conf_level = 0.95,
        pval_method = c("wald", "likelihood")[2]
      )
    )

    warn_txt <- "Variable in global environment remains unchanged."
    expected_warning <- paste0(
      "Warning in `try_car_anova`: simpleWarning in ",
      "Anova.coxph(mod, test.statistic = test.statistic, type = \"III\"): ",
      "LR tests not supported for models with clusters or strata\n",
      " Wald tests substituted"
    )

    # Test car::Anova for Wald or LR test.
    with_wald <- tern:::try_car_anova(mod = mod, test.statistic = "Wald")
    with_lr <- tern:::try_car_anova(mod = mod, test.statistic = "LR")

    # Expect an error to be handled when test is "LR".
    expect_true(
      all(

        # No warning expected.
        is.null(with_wald$warn_text),

        # Warning expected (LR + strata in formula).
        !is.null(with_lr$warn_text) && with_lr$warn_text == expected_warning,

        # A variable warn_txt is unchanged.
        warn_txt == "Variable in global environment remains unchanged.",

        # The warning message is passed through fit_n_aov.
        attr(app_fit, "message") == expected_warning
      )
    )
  }
)
