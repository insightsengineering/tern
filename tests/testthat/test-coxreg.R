
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
  set.seed(1, kind = "Mersenne-Twister")
  bladder <- survival::bladder
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
  attr(dta_bladder$covar1, "label") <- "A Covar"
  attr(dta_bladder$covar2, "label") <- "Sex (F/M)"
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
  result <- h_coxreg_univ_extract(mod, var = "armcd", data = dta_simple)
  expected <- data.frame(
    term = "armcd",
    term_label = "armcd",
    n = 8L,
    hr = 3.84660587023879,
    lcl = 0.392667098727247, ucl = 37.6817328696881,
    pval = 0.247238279140636
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
      mod = c("Treatment", "Covariate", "Covariate"),
      term = structure(
        1:3, .Label = c("armcd", "covar1", "covar2"), class = "factor"
      ),
      term_label = structure(
        1:3, .Label = c("armcd", "A Covar", "Sex (F/M)"), class = "factor"
      ),
      n = c(340L, 340L, 340L),
      hr = c(0.638642559520787, 0.607036963131032, 0.624273826370828),
      lcl = c(0.432384366384154, 0.410167532672898, 0.422242256428962),
      ucl = c(0.943291086683029, 0.898398447595346, 0.922972071975087),
      pval = c(0.0242380486470984, 0.012573389581111, 0.0181887572605306),
      ci = list(
        c(0.432384366384154, 0.943291086683029),
        c(0.410167532672898, 0.898398447595346),
        c(0.422242256428962, 0.922972071975087)
      )
    ),
    row.names = c("ref", "covar1", "covar2"),
    class = "data.frame", conf_level = 0.95
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
    hr = list(armcd = 0.638642559520787),
    hr = list(`A Covar` = 0.607036963131032),
    hr = list(`Sex (F/M)` = 0.624273826370828)
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
  result <- split_rows_by(lyt = NULL, "mod", child_labels = "visible") %>%
    fit_coxreg(conf_level = conf_level) %>%
    build_table(df = df)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "Treatment", "armcd", "Covariate", "A Covar",
      "Sex (F/M)", "n", "", "340", "", "340", "340", "HR", "", "0.64",
      "", "0.61", "0.63", "90% CI", "", "(0.46, 0.89)", "", "(0.44, 0.85)",
      "(0.45, 0.87)", "pval", "", "0.0253", "", "0.0136", "0.0191"
    ),
    .Dim = 6:5
  )
  expect_identical(result_matrix, expected_matrix)
})
