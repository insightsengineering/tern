# [`get_simple`]: simple fabricated dataset for test scennarios.
raw_data <- data.frame(
  time = c(5, 5, 10, 10, 5, 5, 10, 10),
  status = c(0, 0, 1, 0, 0, 1, 1, 1),
  armcd  = factor(LETTERS[c(1, 1, 1, 1, 2, 2, 2, 2)], levels = c("A", "B")),
  age = c(15, 68, 65, 17, 12, 33, 45, 20),
  stage = factor(
    c("1", "2", "1", "1", "1", "2", "1", "2"),
    levels = c("1", "2")
  )
)

# [`get_bladder`]: survival dataset derived for test scenarios.
get_bladder <- function() {
  library(survival)
  set.seed(1, kind = "Mersenne-Twister")
  dta_bladder <- with(
    data = bladder[bladder$enum < 5, ],
    data.frame(
      time = stop,
      status = event,
      arm = paste("ARM:", as.factor(rx)),
      armcd = as.factor(rx),
      covar1 = as.factor(enum),
      covar2 = factor(
        sample(as.factor(enum)), levels = 1:4, labels = c("F", "F", "M", "M")
      )
    )
  )
  attr(dta_bladder$armcd, "label") <- "ARM"
  attr(dta_bladder$covar1, "label") <- "A Covariate Label"
  attr(dta_bladder$covar2, "label") <- "Sex (F/M)"
  dta_bladder$age <- sample( # nolint
    20:60, size = nrow(dta_bladder), replace = TRUE
  )
  dta_bladder
}

# h_coxreg_univar_formulas ----

testthat::test_that("h_coxreg_univar_formulas creates formulas with covariate", {
  result <- h_coxreg_univar_formulas(
    variables = list(
      time = "time", event = "status", arm = "armcd", covariates = c("X", "y")
    )
  )
  expected <- c(
    ref = "survival::Surv(time, status) ~ armcd",
    X = "survival::Surv(time, status) ~ armcd + X",
    y = "survival::Surv(time, status) ~ armcd + y"
  )
  testthat::expect_identical(result, expected)
})

testthat::test_that("h_coxreg_univar_formulas creates formulas with strata", {
  result <- h_coxreg_univar_formulas(
    variables = list(
      time = "time", event = "status", arm = "armcd", covariates = c("X", "y"),
      strata = "SITE"
    )
  )
  expected <- c(
    ref = "survival::Surv(time, status) ~ armcd + strata(SITE)",
    X = "survival::Surv(time, status) ~ armcd + X + strata(SITE)",
    y = "survival::Surv(time, status) ~ armcd + y + strata(SITE)"
  )
  testthat::expect_identical(result, expected)
})

testthat::test_that("h_coxreg_univar_formulas creates formula for reference when treatment is only considered", {
  result <- h_coxreg_univar_formulas(
    variables = list(
      time = "time", event = "status", arm = "armcd"
    )
  )
  expected <- c(
    ref = "survival::Surv(time, status) ~ armcd"
  )
  testthat::expect_identical(result, expected)
})

testthat::test_that("h_coxreg_univar_formulas creates formulas with interactions", {
  result <- h_coxreg_univar_formulas(
    variables = list(
      time = "time", event = "status", arm = "armcd", covariates = c("X", "y"),
      strata = "SITE"
    ),
    interaction = TRUE
  )
  expected <- c(
    ref = "survival::Surv(time, status) ~ armcd + strata(SITE)",
    X = "survival::Surv(time, status) ~ armcd * X + strata(SITE)",
    y = "survival::Surv(time, status) ~ armcd * y + strata(SITE)"
  )
  testthat::expect_identical(result, expected)
})

testthat::test_that("h_coxreg_univar_formulas creates formula without treatment arm", {
  result <- h_coxreg_univar_formulas(
    variables = list(
      time = "time", event = "status", covariates = c("X", "y"),
      strata = "SITE"
    )
  )
  expected <- c(
    X = "survival::Surv(time, status) ~ 1 + X + strata(SITE)",
    y = "survival::Surv(time, status) ~ 1 + y + strata(SITE)"
  )
  testthat::expect_identical(result, expected)
})

testthat::test_that("h_coxreg_univar_formulas fails when requesting interaction without treatment arm", {
  testthat::expect_error(h_coxreg_univar_formulas(
    variables = list(
      time = "time", event = "status", covariates = c("X", "y"),
      strata = "SITE"
    ),
    interaction = TRUE
  ))
})

testthat::test_that("h_coxreg_univar_formulas fails when requesting interaction without covariates", {
  testthat::expect_error(h_coxreg_univar_formulas(
    variables = list(
      time = "time", event = "status", arm = "armcd",
      strata = "SITE"
    ),
    interaction = TRUE
  ))
})

testthat::test_that("h_coxreg_univar_formulas creates formulas with multiple strata", {
  result <- h_coxreg_univar_formulas(
    variables = list(
      time = "time", event = "status", arm = "armcd", covariates = c("X", "y"),
      strata = c("SITE", "COUNTRY")
    )
  )
  expected <- c(
    ref = "survival::Surv(time, status) ~ armcd + strata(SITE, COUNTRY)",
    X = "survival::Surv(time, status) ~ armcd + X + strata(SITE, COUNTRY)",
    y = "survival::Surv(time, status) ~ armcd + y + strata(SITE, COUNTRY)"
  )
  testthat::expect_identical(result, expected)
})

# h_coxreg_multivar_extract ----

testthat::test_that("h_coxreg_multivar_extract extracts correct coxph results when covariate names overlap", {
  library(survival)
  set.seed(1, kind = "Mersenne-Twister")
  dta_simple <- raw_data
  mod <- survival::coxph(survival::Surv(time, status) ~ age + stage, data = dta_simple)
  result <- h_coxreg_multivar_extract(var = "age", mod = mod, data = dta_simple)
  expected <- structure(
    list(
      pval = 0.261168055675453,
      hr = 1.02693066913884,
      lcl = 0.980414799123199,
      ucl = 1.07565348887132,
      level = "age",
      n = 8L,
      term = "age",
      term_label = "age"
    ),
    row.names = 1L,
    class = "data.frame"
  )
  testthat::expect_equal(result, expected, tolerance = 0.2)
})

testthat::test_that("h_coxreg_multivar_extract extracts correct coxph results when covariate is a factor", {
  library(survival)
  set.seed(1, kind = "Mersenne-Twister")
  dta_simple <- raw_data
  mod <- survival::coxph(survival::Surv(time, status) ~ age + stage, data = dta_simple)
  result <- h_coxreg_multivar_extract(var = "stage", mod = mod, data = dta_simple)
  expected <- structure(
    list(
      term = c("stage", "stage"),
      pval = c(NA, 0.194768110455291),
      term_label = c("stage (reference = 1)", "2"),
      hr = c(NA, 4.98956427177951),
      lcl = c(NA, 0.439400256215144),
      ucl = c(NA, 56.6584822609408),
      level = c(NA, "2"),
      n = c(NA, 8L)
      ),
    row.names = c(NA, -2L),
    class = "data.frame"
    )
  attributes(result)$heading <- NULL
  attributes(expected)$heading <- NULL
  testthat::expect_equal(result, expected, tolerance = 0.2)
})

# h_coxreg_multivar_formula ----

testthat::test_that("h_coxreg_multivar_formula creates formula without covariate", {
  result <- h_coxreg_multivar_formula(
    variables = list(arm = "ARMCD", event = "EVNT", time = "TIME", covariates = character())
  )
  expected <- "survival::Surv(TIME, EVNT) ~ ARMCD"
  testthat::expect_identical(result, expected)
})

testthat::test_that("h_coxreg_multivar_formula creates formulas with a strata", {
  result <- h_coxreg_multivar_formula(
    variables = list(
      time = "time", event = "status", arm = "armcd", covariates = c("X", "y"),
      strata = "SITE"
    )
  )
  expected <- "survival::Surv(time, status) ~ armcd + X + y + strata(SITE)"
  testthat::expect_identical(result, expected)
})

testthat::test_that("h_coxreg_multivar_formula creates formulas with multiple strata", {
  result <- h_coxreg_multivar_formula(
    variables = list(
      time = "time", event = "status", arm = "armcd", covariates = c("X", "y"),
      strata = c("SITE", "COUNTRY")
    )
  )
  expected <- "survival::Surv(time, status) ~ armcd + X + y + strata(SITE, COUNTRY)"
  testthat::expect_identical(result, expected)
})

testthat::test_that("h_coxreg_multivar_formula creates formula with covariate", {
  result <- h_coxreg_multivar_formula(
    variables = list(
      time = "time", event = "status", arm = "armcd", covariates = c("covar1", "covar2")
    )
  )
  expected <- "survival::Surv(time, status) ~ armcd + covar1 + covar2"
  testthat::expect_identical(result, expected)
})

testthat::test_that("h_coxreg_multivar_formula creates formula without treatment arm", {
  result <- h_coxreg_multivar_formula(
    variables = list(
      time = "time", event = "status", covariates = c("covar1", "covar2")
    )
  )
  expected <- "survival::Surv(time, status) ~ 1 + covar1 + covar2"
  testthat::expect_identical(result, expected)
})

testthat::test_that("h_coxreg_multivar_formula creates formulas with multiple strata and without arm", {
  result <- h_coxreg_multivar_formula(
    variables = list(
      time = "time", event = "status", covariates = c("X", "y"),
      strata = c("SITE", "COUNTRY")
    )
  )
  expected <- "survival::Surv(time, status) ~ 1 + X + y + strata(SITE, COUNTRY)"
  testthat::expect_identical(result, expected)
})

# control_coxreg ----

testthat::test_that("control_coxreg returns a standard list of parameters", {
  result <- control_coxreg()
  expected <- list(
    pval_method = "wald", ties = "exact", conf_level = 0.95,
    interaction = FALSE
  )
  testthat::expect_identical(result, expected)
})

# fit_coxreg_univar ----

testthat::test_that("fit_coxreg_univar returns model results as expected", {
  data <- get_bladder()
  control <- control_coxreg(conf_level = 0.91)
  variables <- list(
    time = "time", event = "status", arm = "armcd",
    covariates = "covar1"
  )
  forms <- h_coxreg_univar_formulas(
    variables = variables,
  )

  result <- fit_coxreg_univar(
    variables = variables,
    data = data,
    control = control
  )

  expected <- list(
    mod = lapply(
      forms, function(x) {
        survival::coxph(formula = stats::as.formula(x), data = data, ties = control$ties)
      }
    ),
    data = data,
    control = control,
    vars = variables,
    at = list()
  )
  testthat::expect_equal(result$mod, expected$mod)
})

testthat::test_that("fit_coxreg_univar runs with non-represented level of a factor", {
  data <- get_bladder() %>%
    dplyr::filter(covar1 %in% 1:3)

  variables <- list(
    time = "time", event = "status", arm = "armcd",
    covariates = "covar1"
  )

  testthat::expect_silent(fit_coxreg_univar(variables = variables, data = data))
})

testthat::test_that("fit_coxreg_univar is stopped when there are not 2 arms", {
  data <- get_bladder() %>%
    dplyr::filter(covar1 %in% 1:3)

  variables <- list(
    time = "time", event = "status", arm = "covar1", covariates = "covar2"
  )

  testthat::expect_error(fit_coxreg_univar(variables = variables, data = data))
})

testthat::test_that("fit_coxreg_univar is stopped when likelihood method is used together with strata", {
  data <- get_bladder()

  variables <- list(
    time = "time", event = "status", arm = "armcd", covariates = "age", strata = "covar1"
  )

  testthat::expect_error(
    fit_coxreg_univar(
      variables = variables, data = data, control = control_coxreg(pval_method = "likelihood")
    )
  )
})

testthat::test_that("fit_coxreg_univar works without treatment arm", {
  data <- get_bladder()

  variables <- list(
    time = "time", event = "status", covariates = "age", strata = "covar1"
  )

  result <- testthat::expect_silent(fit_coxreg_univar(variables = variables, data = data))
  testthat::expect_named(result$mod, "age")
})


testthat::test_that("fit_coxreg_univar's result are identical to soon deprecated s_cox_univariate (no interaction)", {
  dta_bladder <- get_bladder()

  univar_model <- fit_coxreg_univar(
    variables = list(
      time = "time", event = "status", arm = "armcd",
      covariates = "covar1"
    ),
    data = dta_bladder,
    control = control_coxreg(
      pval_method = c("wald", "likelihood")[1],
      ties = c("exact", "efron", "breslow")[2],
      conf_level = 0.95,
      interaction = FALSE
    )
  )
  df <- broom::tidy(univar_model)
  result <- df[c("n", "hr", "pval", "ci")]

  expected <- with(
    data = testthat::expect_warning(s_cox_univariate(
      formula = survival::Surv(time, status) ~ arm(armcd),
      data = dta_bladder,
      covariates = list(~ covar1)
    )),
    expr = {
      y <- list(
        n = as.list(n),
        hr = as.list(hr),
        pval = as.list(pval)
      )
      y$ci <- lapply(ci, as.numeric)
      y
    }
  )

  testthat::expect_equivalent(result, expected)
})

testthat::test_that("fit_coxreg_univar's result are identical to soon deprecated s_cox_univariate (with interaction)", {

  dta_bladder <- get_bladder()
  expected <- with(
    data = testthat::expect_warning(s_cox_univariate(
      formula = survival::Surv(time, status) ~ arm(armcd),
      data = dta_bladder,
      covariates = list(~ covar1),
      interactions = TRUE,
      pval_method = c("wald", "likelihood")[2]
    )),
    expr = {
      y <- list(
        n = as.list(n),
        hr = as.list(do.call(c, hr)),
        pval = pval[c(1)],
        pval_inter = lrt
      )
      y$ci <- ci
      y$ci <- do.call(
        c, list(
          list(c(y$ci[[1]])),
          split(y$ci[[2]], f = seq_len(nrow(y$ci[[2]])))
        )
      )
      lapply(y, unname)
    }
  )

  clear <- function(x) x[sapply(x, function(x) length(x) > 0)]
  result <- lapply(
    X = broom::tidy(
      fit_coxreg_univar(
        variables = list(
          time = "time", event = "status", arm = "armcd",
          covariates = "covar1"
        ),
        data = dta_bladder,
        control = control_coxreg(
          pval_method = c("wald", "likelihood")[2],
          ties = c("exact", "efron", "breslow")[2],
          conf_level = 0.95,
          interaction = TRUE
        )
      )
    )[c("n", "hr", "pval", "pval_inter", "ci")],
    clear
  )

  testthat::expect_equivalent(result, expected)
})

# tidy.summary.coxph ----

testthat::test_that("tidy.summary.coxph method tidies up the Cox regression model", {
  dta_simple <- raw_data
  mod <- summary(survival::coxph(survival::Surv(time, status) ~ armcd, data = dta_simple))
  result <- broom::tidy(mod)
  expected <- dplyr::tibble(
    "Pr(>|z|)" = 0.2472383,
    "exp(coef)" = 3.846606,
    "exp(-coef)" = 0.2599694,
    "lower .95" = 0.3926671,
    "upper .95" = 37.68173,
    "level" = "armcdB",
    "n" = 8L
  ) %>%
    as.data.frame()
  testthat::expect_equal(result, expected, tolerance = 1e-5)
})

# h_coxreg_univar_extract ----

testthat::test_that("h_coxreg_univar_extract extracts coxph results", {
  dta_simple <- raw_data
  mod <- survival::coxph(survival::Surv(time, status) ~ armcd, data = dta_simple)
  result <- h_coxreg_univar_extract(effect = "armcd", covar = "armcd", mod = mod, data = dta_simple)
  expected <- data.frame(
    effect = "Treatment:",
    term = "armcd",
    term_label = "B vs control (A)",
    level = "B",
    n = 8L,
    hr = 3.84660587023879,
    lcl = 0.392667098727247, ucl = 37.6817328696881,
    pval = 0.247238279140636,
    stringsAsFactors = FALSE
  )
  testthat::expect_equal(result, expected, tolerance = 1e-4)
})

# muffled_car_anova ----

testthat::test_that("muffled_car_anova muffles notes about dropped strata term", {
  bladder1 <- bladder[bladder$enum < 5, ]
  mod <- survival::coxph(
    survival::Surv(stop, event) ~ (rx + size + number) * strata(enum) + cluster(id),
    bladder1
  )
  expect_message(car::Anova(mod, test.statistic = "Wald"))
  testthat::expect_silent(muffled_car_anova(mod, test_statistic = "Wald"))
})

testthat::test_that("muffled_car_anova gives a hint in the error message when an error occurs", {
  bladder2 <- bladder[1:20, ]
  mod <- survival::coxph(
    survival::Surv(stop, event) ~ (rx + size + number) * strata(enum) + cluster(id),
    bladder2
  )
  testthat::expect_error(
    muffled_car_anova(mod, test_statistic = "Wald"),
    "the model seems to have convergence problems"
  )
})

# tidy.coxreg.univar ----

testthat::test_that("tidy.coxreg.univar method tidies up the univariate Cox regression model", {
  univar_model <- fit_coxreg_univar(
    variables = list(
      time = "time", event = "status", arm = "armcd",
      covariates = c("covar1", "covar2")
    ),
    data = get_bladder()
  )
  result <- broom::tidy(univar_model)

  expected <- structure(
    list(
      effect = c("Treatment:", "Covariate:", "Covariate:"),
      term = c("armcd", "covar1", "covar2"),
      term_label = c("2 vs control (1)", "A Covariate Label", "Sex (F/M)"),
      level = c("2", "2", "2"),
      n = list(340L, 340L, 340L),
      hr = list(0.638642559520787, 0.607036963131032, 0.624273826370828),
      lcl = c(0.432384366384154, 0.410167532672898, 0.422242256428962),
      ucl = c(0.943291086683029, 0.898398447595346, 0.922972071975087),
      pval = list(0.0242380486470984, 0.012573389581111, 0.0181887572605306),
      ci = list(c(0.432384366384154, 0.943291086683029),
                c(0.410167532672898, 0.898398447595346),
                c(0.422242256428962, 0.922972071975087)
      )
    ),
    row.names = c("ref", "covar1", "covar2"),
    class = "data.frame",
    conf_level = 0.95
  )
  testthat::expect_equal(result, expected, tolerance = 1e-5)
})

testthat::test_that("tidy.coxreg.univar method works with only numeric covariates with strata", {
  univar_model <- fit_coxreg_univar(
    variables = list(
      time = "time", event = "status", arm = "armcd",
      covariates = "age", strata = c("covar1", "covar2")
    ),
    data = get_bladder()
  )
  result <- broom::tidy(univar_model)

  expected <- structure(
    list(
      effect = c("Treatment:", "Covariate:"),
      term = c("armcd", "age"),
      term_label = c("2 vs control (1)", "age"),
      level = c("2", "2"),
      n = list(340L, 340L),
      hr = list(0.6208343, 0.6076894),
      lcl = c(0.4164994, 0.4072018),
      ucl = c(0.9254162, 0.9068879),
      pval = list(0.01925561, 0.01475084),
      ci = list(c(0.4164994, 0.9254162),
                c(0.4072018, 0.9068879)
      )
    ),
    row.names = c("ref", "age"),
    class = "data.frame",
    conf_level = 0.95
  )
  testthat::expect_equal(result, expected, tolerance = 1e-5)
})

testthat::test_that("tidy.coxreg.univar method works without treatment arm", {
  univar_model <- fit_coxreg_univar(
    variables = list(
      time = "time", event = "status",
      covariates = c("age", "covar1"), strata = "covar2"
    ),
    data = get_bladder()
  )
  result <- testthat::expect_silent(broom::tidy(univar_model))
  testthat::expect_identical(result$term, c("age", "covar1", rep("A Covariate Label", 3)))
})

# h_coxreg_extract_interaction ----

testthat::test_that("h_coxreg_extract_interaction works with factor as covariate", {
  mod <- survival::coxph(survival::Surv(time, status) ~ armcd * covar1, data = get_bladder())
  testthat::expect_silent(
    h_coxreg_extract_interaction(
      effect = "armcd", covar = "covar1", mod = mod, data = get_bladder(),
      control = control_coxreg()
    )
  )
  testthat::expect_silent(
    h_coxreg_inter_effect(
      x = get_bladder()[["covar1"]],
      effect = "armcd", covar = "covar1", mod = mod, data = get_bladder(),
      control = control_coxreg()
    )
  )
})

# h_coxreg_inter_effect ----

testthat::test_that("h_coxreg_inter_effect works with numerics as covariate", {
  mod1 <- survival::coxph(survival::Surv(time, status) ~ armcd * age, data = get_bladder())
  testthat::expect_silent(
    h_coxreg_extract_interaction(
      effect = "armcd", covar = "age", mod = mod1, control = control_coxreg(),
      at = list(), data = get_bladder()
    )
  )
  testthat::expect_silent(
    h_coxreg_inter_effect(
      x = get_bladder()[["age"]],
      effect = "armcd", covar = "age", mod = mod1, control = control_coxreg(),
      at = list(), data = get_bladder()
    )
  )

  mod2 <- survival::coxph(survival::Surv(time, status) ~ armcd * age + strata(covar1), data = get_bladder())
  testthat::expect_silent(
    h_coxreg_inter_effect(
      x = get_bladder()[["age"]],
      effect = "armcd", covar = "age", mod = mod2, data = get_bladder(),
      at = list(), control = control_coxreg()
    )
  )
})

testthat::test_that("h_coxreg_inter_effect.numerics works with _:_ in effect levels", {
  mod <- survival::coxph(survival::Surv(time, status) ~ armcd * age, data = get_bladder())
  expected <- testthat::expect_silent(
    h_coxreg_extract_interaction(
      effect = "armcd", covar = "age", mod = mod, control = control_coxreg(),
      at = list(), data = get_bladder()
    )
  )

  mod <- survival::coxph(survival::Surv(time, status) ~ arm * age, data = get_bladder())
  result <-  testthat::expect_silent(
    h_coxreg_extract_interaction(
      effect = "arm", covar = "age", mod = mod, control = control_coxreg(),
      at = list(), data = get_bladder()
    )
  )
  # The first column in the effect (arm/armcd) and expected to vary.
  testthat::expect_equal(result[, -1], expected[, -1], check.attributes = FALSE)
})

# h_coxreg_inter_estimations ----

testthat::test_that("h_coxreg_inter_estimations' results identical to soon deprecated estimate_coef", {
  # Testing dataset [survival::bladder].
  library(survival)
  set.seed(1, kind = "Mersenne-Twister")
  dta_bladder <- get_bladder()

  mod <- survival::coxph(survival::Surv(time, status) ~ armcd * covar1, data = dta_bladder)

  result <- h_coxreg_inter_estimations(
    variable = "armcd", given = "covar1",
    lvl_var = levels(dta_bladder$armcd),
    lvl_given = levels(dta_bladder$covar1),
    mod = mod, conf_level = .95
  )

  mmat <- model.matrix(mod)[1, ]
  mmat[!mmat == 0] <- 0

  expected <- estimate_coef(
    variable = "armcd", given = "covar1",
    lvl_var = levels(dta_bladder$armcd),
    lvl_given = levels(dta_bladder$covar1),
    coef = coef(mod), mmat = mmat, vcov = vcov(mod),
    conf_level = .95
  )
  testthat::expect_identical(result, expected)
})

# fit_coxreg_multivar ----

testthat::test_that("fit_coxreg_multivar returns model results as expected", {
  data <- get_bladder()
  control <- control_coxreg(conf_level = 0.91)
  variables <- list(
    time = "time", event = "status", arm = "armcd",
    covariates = c("covar1", "covar2")
  )
  form <- h_coxreg_multivar_formula(variables = variables)

  result <- fit_coxreg_multivar(
    variables = variables,
    data = data,
    control = control
  )

  expected <- list(
    mod = survival::coxph(
      formula = stats::as.formula(form),
      data = data, ties = control$ties
    ),
    data = data,
    control = control,
    vars = variables
  )
  testthat::expect_equal(result$mod, expected$mod)
})

testthat::test_that("fit_coxreg_multivar is stopped when likelihood method is used together with strata", {
  data <- get_bladder()

  variables <- list(
    time = "time", event = "status", arm = "armcd", covariates = "age", strata = "covar1"
  )

  testthat::expect_error(
    fit_coxreg_multivar(
      variables = variables, data = data, control = control_coxreg(pval_method = "likelihood")
    )
  )
})


testthat::test_that("fit_coxreg_multivar works correctly also without treatment arm", {
  data <- get_bladder()
  control <- control_coxreg(conf_level = 0.9)
  variables <- list(
    time = "time", event = "status",
    covariates = c("covar1", "covar2")
  )
  result <- testthat::expect_silent(fit_coxreg_multivar(
    variables = variables,
    data = data,
    control = control
  ))
  testthat::expect_is(result$mod, "coxph")
  testthat::expect_equal(formula(result$mod), survival::Surv(time, status) ~ 1 + covar1 + covar2)
})

# tidy.coxreg.multivar ----

testthat::test_that("tidy.coxreg.multivar method tidies up the multi-variable Cox regression model", {
  library(survival)
  set.seed(1, kind = "Mersenne-Twister")
  dta_bladder <- get_bladder()

  multivar_model <- fit_coxreg_multivar(
    variables = list(
      time = "time", event = "status", arm = "armcd",
      covariates = c("covar1", "covar2")
    ),
    data = dta_bladder,
    control = control_coxreg(ties = "efron")
  )
  result <- broom::tidy(multivar_model)
  expected <- structure(
    list(
      term = c(
        "armcd", "ARM", "covar1", "A Covariate Label", "A Covariate Label",
        "A Covariate Label", "covar2", "Sex (F/M)"
      ),
      pval = list(
        numeric(0), 0.01274101, 7.121178e-09, 0.001145167,
        6.519833e-06, 3.296958e-08, numeric(0), 0.1979248
      ),
      term_label = c(
        "ARM (reference = 1)", "2", "A Covariate Label (reference = 1)",
        "2", "3", "4", "Sex (F/M) (reference = F)", "M"
      ),
      hr = list(
        numeric(0), 0.6106495, numeric(0), 0.46139,
        0.3111114, 0.1847729, numeric(0), 1.28109
      ),
      lcl = c(NA, 0.4142327, NA, 0.2894783, 0.1872782, 0.1015025, NA, 0.8786364),
      ucl = c(NA, 0.9002013, NA, 0.7353945, 0.5168263, 0.3363563, NA, 1.8678847),
      level = c(NA, "2", NA, "2", "3", "4", NA, "M"),
      ci = list(
        numeric(0), c(0.4142327, 0.9002013), numeric(0), c(0.2894783, 0.7353945),
        c(0.1872782, 0.5168263), c(0.1015025, 0.3363563), numeric(0), c(0.8786364, 1.8678847)
      )
    ),
    row.names = c(
      "armcd.1", "armcd.2", "covar1.1", "covar1.2",
      "covar1.3", "covar1.4", "covar2.1", "covar2.2"
    ),
    class = "data.frame",
    conf_level = 0.95
  )
  attr(expected, "conf_level") <- 0.95
  testthat::expect_equal(result, expected, tolerance = 1e-4)
})

# s_coxreg ----

testthat::test_that("s_coxreg converts tabulated results in a list", {
  univar_model <- fit_coxreg_univar(
    variables = list(
      time = "time", event = "status", arm = "armcd",
      covariates = c("covar1", "covar2")
    ),
    data = get_bladder()
  )
  df <- broom::tidy(univar_model)
  result <- s_coxreg(df = df, .var = "hr")
  expected <- list(
    hr = list(`2 vs control (1)` = 0.638642559520787),
    hr = list(`A Covariate Label` = 0.607036963131032),
    hr = list(`Sex (F/M)` = 0.62427)
  )
  testthat::expect_equal(result, expected, tolerance = 1e-4)
})

# summarize_coxreg ----

testthat::test_that("summarize_coxreg adds the univariate Cox regression layer to rtables", {
  conf_level <- 0.90
  univar_model <- fit_coxreg_univar(
    variables = list(
      time = "time", event = "status", arm = "armcd",
      covariates = c("covar1", "covar2")
    ),
    data = get_bladder(),
    control = control_coxreg(ties = "breslow", conf_level = conf_level)
  )
  df <- broom::tidy(univar_model)
  result <- basic_table() %>%
    split_rows_by("effect") %>%
    split_rows_by("term", child_labels = "hidden") %>%
    summarize_coxreg(conf_level = conf_level) %>%
    build_table(df = df)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "Treatment:", "2 vs control (1)", "Covariate:", "A Covariate Label",
      "Sex (F/M)", "n", "", "340", "", "340", "340", "Hazard Ratio", "", "0.64", "",
      "0.61", "0.63", "90% CI", "", "(0.46, 0.89)", "", "(0.44, 0.85)",
      "(0.45, 0.87)", "p-value", "", "0.0253", "", "0.0136", "0.0191"
    ),
    .Dim = 6:5
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("summarize_coxreg adds the multi-variable Cox regression layer to rtables", {
  library(survival)
  set.seed(1, kind = "Mersenne-Twister")
  dta_bladder <- get_bladder()
  conf_level <- 0.90

  multivar_model <- fit_coxreg_multivar(
    variables = list(
      time = "time", event = "status", arm = "armcd",
      covariates = c("covar1", "covar2")
    ),
    data = dta_bladder
  )
  df <- broom::tidy(multivar_model)
  result <- basic_table() %>%
    split_rows_by("term", child_labels = "hidden") %>%
    summarize_coxreg(multivar = TRUE, conf_level = conf_level) %>%
    build_table(df = df)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "ARM (reference = 1)", "2", "A Covariate Label (reference = 1)",
      "2", "3", "4", "Sex (F/M) (reference = F)", "M", "Hazard Ratio",
      "", "0.61", "", "0.46", "0.31", "0.18", "", "1.29", "90% CI",
      "", "(0.41, 0.9)", "", "(0.28, 0.73)", "(0.18, 0.51)", "(0.1, 0.33)",
      "", "(0.88, 1.89)", "p-value", "", "0.0123", "<0.0001", "0.0011",
      "<0.0001", "<0.0001", "", "0.1911"
    ),
    .Dim = c(9L, 4L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("summarize_coxreg works without treatment arm in univariate case", {
  library(survival)
  set.seed(1, kind = "Mersenne-Twister")
  dta_bladder <- get_bladder()
  conf_level <- 0.90

  univar_covs_model <- fit_coxreg_univar(
    variables = list(
      time = "time", event = "status",
      covariates = c("covar1", "covar2")
    ),
    data = dta_bladder
  )
  df <- broom::tidy(univar_covs_model)
  result <- basic_table() %>%
    split_rows_by("term", child_labels = "hidden") %>%
    summarize_coxreg(multivar = TRUE, conf_level = conf_level) %>%
    build_table(df = df)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c("", "A Covariate Label (reference = 1)", "2", "3",
      "4", "Sex (F/M) (reference = F)", "M", "Hazard Ratio", "", "0.45",
      "0.31", "0.18", "", "1.33", "90% CI", "", "(0.28, 0.71)", "(0.19, 0.52)",
      "(0.1, 0.33)", "", "(0.91, 1.94)", "p-value", "<0.0001", "0.0007",
      "<0.0001", "<0.0001", "", "0.1414"),
    .Dim = c(7L, 4L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
