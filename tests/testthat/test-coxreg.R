
# [`get_simple`]: simple fabricated dataset for test scennarios.
get_simple <- function() {
  data.frame(
    time = c(5, 5, 10, 10, 5, 5, 10, 10),
    status = c(0, 0, 1, 0, 0, 1, 1, 1),
    armcd  = factor(LETTERS[c(1, 1, 1, 1, 2, 2, 2, 2)], levels = c("A", "B"))
  )
}

# [`get_bladder`]: survival dataset derived for test scenarios.
get_bladder <- function() {
  library(survival)
  set.seed(1, kind = "Mersenne-Twister")
  dta_bladder <- with(
    data = bladder[bladder$enum < 5, ],
    data.frame(
      time = stop,
      status = event,
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
  dta_bladder$AGE <- sample( # nolint
    20:60, size = nrow(dta_bladder), replace = TRUE
  )
  dta_bladder
}

test_that("h_coxreg_univ_formulas creates formulas with covariate", {
  result <- h_coxreg_univ_formulas(
    variables = list(
      time = "time", event = "status", arm = "armcd", covariates = c("X", "y")
    )
  )
  expected <- c(
    ref = "Surv( time , status ) ~  armcd",
    X = "Surv( time , status ) ~  armcd + X ",
    y = "Surv( time , status ) ~  armcd + y "
  )
  expect_identical(result, expected)

})

test_that("h_coxreg_univ_formulas creates formulas with strata", {
  result <- h_coxreg_univ_formulas(
    variables = list(
      time = "time", event = "status", arm = "armcd", covariates = c("X", "y"),
      strata = "SITE"
    )
  )
  expected <- c(
    ref = "Surv( time , status ) ~  armcd",
    X = "Surv( time , status ) ~  armcd + X + strata( SITE )",
    y = "Surv( time , status ) ~  armcd + y + strata( SITE )"
  )
  expect_identical(result, expected)
})

test_that("h_coxreg_univ_formulas creates formulas with interactions", {
  result <- h_coxreg_univ_formulas(
    variables = list(
      time = "time", event = "status", arm = "armcd", covariates = c("X", "y"),
      strata = "SITE"
    ),
    interaction = TRUE
  )
  expected <- c(
    ref = "Surv( time , status ) ~  armcd",
    X = "Surv( time , status ) ~  armcd * X + strata( SITE )",
    y = "Surv( time , status ) ~  armcd * y + strata( SITE )"
  )
  expect_identical(result, expected)
})

test_that("control_coxreg returns a standard list of parameters", {
  result <- control_coxreg()
  expected <- list(
    pval_method = "wald", ties = "exact", conf_level = 0.95,
    interaction = FALSE
  )
  expect_identical(result, expected)
})

test_that("h_coxreg_univ_extract extracts coxph results", {
  dta_simple <- get_simple()
  mod <- coxph(Surv(time, status) ~ armcd, data = dta_simple)
  result <- h_coxreg_univ_extract(effect = "armcd", covar = "armcd", mod = mod, data = dta_simple)
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
  expect_equal(result, expected, tolerance = 1e-4)
})

test_that("h_coxreg tabulates univariate Cox regression results", {
  result <- h_coxreg(
    variables = list(
      time = "time", event = "status", arm = "armcd",
      covariates = c("covar1", "covar2")
    ),
    data = get_bladder()
  )

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
  expect_equal(result, expected, tolerance = 1e-5)
})

test_that("s_coxreg converts tabulated results in a list", {
  df <- h_coxreg(
    variables = list(
      time = "time", event = "status", arm = "armcd",
      covariates = c("covar1", "covar2")
    ),
    data = get_bladder()
  )
  result <- s_coxreg(df = df, .var = "hr")
  expected <- list(
    hr = list(`2 vs control (1)` = 0.638642559520787),
    hr = list(`A Covariate Label` = 0.607036963131032),
    hr = list(`Sex (F/M)` = 0.62427)
  )
  expect_equal(result, expected, tolerance = 1e-4)
})

test_that("fit_coxreg adds the univariate Cox regression layer to rtables", {
  conf_level <- 0.90
  df <- h_coxreg(
    variables = list(
      time = "time", event = "status", arm = "armcd",
      covariates = c("covar1", "covar2")
    ),
    data = get_bladder(),
    control = control_coxreg(ties = "breslow", conf_level = conf_level)
  )
  result <- split_rows_by(lyt = NULL, "effect") %>%
    split_rows_by("term", child_labels = "hidden") %>%
    fit_coxreg(conf_level = conf_level) %>%
    build_table(df = df)
  result_matrix <- to_string_matrix(result)
  dput(result_matrix)
  expected_matrix <- structure(
    c(
      "", "Treatment:", "2 vs control (1)", "Covariate:", "A Covariate Label",
      "Sex (F/M)", "n", "", "340", "", "340", "340", "HR", "", "0.64", "",
      "0.61", "0.63", "90% CI", "", "(0.46, 0.89)", "", "(0.44, 0.85)",
      "(0.45, 0.87)", "pval", "", "0.0253", "", "0.0136", "0.0191"
    ),
    .Dim = 6:5
  )
  expect_identical(result_matrix, expected_matrix)
})


test_that("h_coxreg_inter_effect works with factor as covariate", {
  mod <- coxph(Surv(time, status) ~ armcd * covar1, data = get_bladder())
  expect_silent(
    h_coxreg_extract_interaction(
      effect = "armcd", covar = "covar1", mod = mod, data = get_bladder(),
      control = control_coxreg()
    )
  )
  expect_silent(
    h_coxreg_inter_effect(
      x = get_bladder()[["covar1"]],
      effect = "armcd", covar = "covar1", mod = mod, data = get_bladder(),
      control = control_coxreg()
    )
  )
})

test_that("h_coxreg_inter_effect works with numerics as covariate", {
  mod <- coxph(Surv(time, status) ~ armcd * AGE, data = get_bladder())
  expect_silent(
    h_coxreg_extract_interaction(
      effect = "armcd", covar = "AGE", mod = mod, control = control_coxreg(),
      at = list(), data = get_bladder()
    )
  )
  expect_silent(
    h_coxreg_inter_effect(
      x = get_bladder()[["AGE"]],
      effect = "armcd", covar = "AGE", mod = mod, control = control_coxreg(),
      at = list(), data = get_bladder()
    )
  )
})

test_that("h_coxreg_inter_estimations' results identical to soon deprecated estimate_coef", {
  # Testing dataset [survival::bladder].
  library(survival)
  set.seed(1, kind = "Mersenne-Twister")
  dta_bladder <- get_bladder()

  mod <- coxph(Surv(time, status) ~ armcd * covar1, data = dta_bladder)

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
  expect_identical(result, expected)
})

test_that("h_coxreg's result are identical to soon deprecated s_cox_univariate (no interaction)", {

  dta_bladder <- get_bladder()

  result <- h_coxreg(
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
  )[c("n", "hr", "pval", "ci")]

  expected <- with(
    data = s_cox_univariate(
      formula = Surv(time, status) ~ arm(armcd),
      data = dta_bladder,
      covariates = list(~ covar1)
    ),
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

  expect_equivalent(result, expected)
})

test_that("h_coxreg's result are identical to soon deprecated s_cox_univariate (with interaction)", {

  dta_bladder <- get_bladder()
  expected <- with(
    data = s_cox_univariate(
      formula = Surv(time, status) ~ arm(armcd),
      data = dta_bladder,
      covariates = list(~ covar1),
      interactions = TRUE,
      pval_method = c("wald", "likelihood")[2]
    ),
    expr = {
      y <- list(
        n = as.list(n),
        hr = as.list(do.call(c, hr)),
        pval = pval[c(1, 3)],
        pval_inter = lrt
      )
      y$ci <- ci
      y$ci <- do.call(
        c, list(
          list(c(y$ci[[1]])),
          split(y$ci[[2]], f = 1:nrow(y$ci[[2]]))
        )
      )
      lapply(y, unname)
    }
  )

  clear <- function(x) x[sapply(x, function(x) length(x) > 0)]
  result <- lapply(
    X = h_coxreg(
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
    )[c("n", "hr", "pval", "pval_inter", "ci")],
    clear
  )

  expect_equivalent(result, expected)
})
