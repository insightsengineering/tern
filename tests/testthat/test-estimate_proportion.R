
testthat::test_that("prop_wilson returns right result", {
  rsp <- c(
    TRUE, TRUE, TRUE, TRUE, TRUE,
    FALSE, FALSE, FALSE, FALSE, FALSE
  )

  expected <- c(0.2692718, 0.7307282)
  result <- prop_wilson(rsp, conf_level = 0.9)

  testthat::expect_equal(expected, result, tolerance = 1e-5)
})

testthat::test_that("prop_strat_wilson returns right result", {
  set.seed(1)

  # Testing data set
  rsp <- sample(c(TRUE, FALSE), 100, TRUE)
  strata_data <- data.frame(
    "f1" = sample(c("a", "b"), 100, TRUE),
    "f2" = sample(c("x", "y", "z"), 100, TRUE),
    stringsAsFactors = TRUE
  )
  strata <- interaction(strata_data)
  table_strata <- table(rsp, strata)
  n_ws <- ncol(table_strata) # Number of weights

  expected <- list(
    conf.int = c(lower = 0.4073299, upper = 0.5647475),
    weights = c(0.2074199, 0.1776464, 0.1915610, 0.1604678, 0.1351096, 0.1277952)
  )
  names(expected$weights) <- colnames(table_strata)

  result <- prop_strat_wilson(
    rsp = rsp, strata = strata,
    conf_level = 0.90
  )

  testthat::expect_equal(expected, result, tolerance = 1e-5)

  # Test without estimating weights (all equal here)
  expected <- list(conf.int = c(lower = 0.4190436, upper = 0.5789733))
  result <- prop_strat_wilson(
    rsp = rsp, strata = strata,
    weights = rep(1 / n_ws, n_ws), # Not automatic setting of weights
    conf_level = 0.90
  )

  testthat::expect_equal(expected, result, tolerance = 1e-5)
})

testthat::test_that("prop_clopper_pearson returns right result", {
  rsp <- c(
    TRUE, TRUE, TRUE, TRUE, TRUE,
    FALSE, FALSE, FALSE, FALSE, FALSE
  )

  result <- prop_clopper_pearson(rsp, conf_level = .95)
  expected <- c(0.1871, 0.8129)
  testthat::expect_equal(expected, result, tolerance = 1e-4)
})

testthat::test_that("prop_wald returns right result", {
  rsp <- c(
    TRUE, TRUE, TRUE, TRUE, TRUE,
    FALSE, FALSE, FALSE, FALSE, FALSE
  )

  result <- prop_wald(rsp, conf_level = 0.95, correct = TRUE)
  expected <- c(0.1401, 0.8599)
  testthat::expect_equal(expected, result, tolerance = 1e-4)

  result <- prop_wald(rsp, conf_level = 0.95, correct = FALSE)
  expected <- c(0.1901, 0.8099)
  testthat::expect_equal(expected, result, tolerance = 1e-4)
})

testthat::test_that("prop_agresti_coull returns right result", {
  rsp <- c(
    TRUE, TRUE, TRUE, TRUE, TRUE,
    FALSE, FALSE, FALSE, FALSE, FALSE
  )

  result <- prop_agresti_coull(rsp, conf_level = 0.95)
  expected <- c(0.2366, 0.7634)
  testthat::expect_equal(expected, result, tolerance = 1e-4)
})


testthat::test_that("prop_jeffreys returns right result", {
  rsp <- c(
    TRUE, TRUE, TRUE, TRUE, TRUE,
    FALSE, FALSE, FALSE, FALSE, FALSE
  )

  result <- prop_jeffreys(rsp, conf_level = 0.95)
  expected <- c(0.2235, 0.7765)
  testthat::expect_equal(expected, result, tolerance = 1e-4)
})

testthat::test_that("s_proportion returns right result", {
  result <- s_proportion(c(1, 0, 1, 0))
  expected <- list(
    n_prop = c(2, .5),
    prop_ci = c(0, 100)
  )
  testthat::expect_equal(expected, result, tolerance = 1e-4, check.attributes = FALSE)
})

testthat::test_that("`s_proportion` works with Jeffreys CI", {

  # "Mid" case.
  rsp <- c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)
  result <- s_proportion(
    df = rsp,
    conf_level = 0.9,
    method = "jeffreys"
  )
  # according to SAS.
  expected <- list(
    n_prop = c(4, 4 / 6),
    prop_ci = formatters::with_label(
      c(34.0802, 89.5730),
      label = "90% CI for Response Rates (Jeffreys)"
    )
  )
  testthat::expect_equal(result, expected, tol = 0.0001, check.attributes = FALSE)

  # Corner case: Only responders.
  rsp <- c(TRUE, TRUE, TRUE, TRUE)
  result <- s_proportion(
    df = rsp,
    conf_level = 0.95,
    method = "jeffreys"
  )
  # according to SAS.
  expected <- list(
    n_prop = c(4, 1),
    prop_ci = formatters::with_label(
      c(55.5237, 100),
      label = "95% CI for Response Rates (Jeffreys)"
    )
  )
  testthat::expect_equal(result, expected, tol = 0.0001, check.attributes = FALSE)
})

testthat::test_that("`s_proportion` works with Agresti-Coull CI", {

  # "Mid" case.
  rsp <- c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)
  result <- s_proportion(
    df = rsp,
    conf_level = 0.9,
    method = "agresti-coull"
  )
  # according to SAS.
  expected <- list(
    n_prop = c(4, 4 / 6),
    prop_ci = formatters::with_label(
      c(34.3585, 88.6154),
      label = "90% CI for Response Rates (Agresti-Coull)"
    )
  )
  testthat::expect_equal(result, expected, tol = 0.0001, check.attributes = FALSE)

  # Edge case: Only responders.
  rsp <- c(TRUE, TRUE, TRUE, TRUE)
  result <- s_proportion(
    df = rsp,
    conf_level = 0.95,
    method = "agresti-coull"
  )
  # according to SAS.
  expected <- list(
    n_prop = c(4, 1),
    prop_ci = formatters::with_label(
      c(45.4050, 100),
      label = "95% CI for Response Rates (Agresti-Coull)"
    )
  )
  # Small additional difference acknowledged here.
  testthat::expect_equal(result, expected, tol = 0.00011, check.attributes = FALSE)
})

testthat::test_that("`estimate_proportion` is compatible with `rtables`", {

  # Data loading and processing
  adrs <- scda::synthetic_cdisc_data("rcd_2022_06_27")$adrs
  anl <- adrs %>%
    dplyr::filter(PARAMCD == "BESRSPI") %>%
    dplyr::mutate(is_rsp = AVALC %in% c("CR", "PR"))

  result <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    add_colcounts() %>%
    add_overall_col(label = "All") %>%
    estimate_proportion(
      vars = "is_rsp",
      conf_level = 0.95,
      method = "wilson",
      .formats = c("xx.xx (xx.xx%)", "(xx.xxxx, xx.xxxx)")
    ) %>%
    build_table(anl)
  result <- get_formatted_cells(result)
  expected <- rbind(
    c("133.00 (99.25%)", "127.00 (94.78%)", "131.00 (99.24%)", "391.00 (97.75%)"),
    c("(95.8940, 99.8681)", "(89.6097, 97.4468)", "(95.8337, 99.8661)", "(95.7797, 98.8118)")
  )

  testthat::expect_equal(result, expected, tol = 0.0001)
})

testthat::test_that("`estimate_proportion` and strat_wilson is compatible with `rtables`", {
  set.seed(1)

  # Data loading and processing
  adrs <- scda::synthetic_cdisc_data("rcd_2022_06_27")$adrs
  anl <- adrs %>%
    dplyr::filter(PARAMCD == "BESRSPI") %>%
    dplyr::mutate(DTHFL = DTHFL == "Y") # Death flag yes

  result <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    add_colcounts() %>%
    add_overall_col(label = "All") %>%
    estimate_proportion(
      vars = "DTHFL",
      conf_level = 0.95,
      method = "strat_wilson",
      variables = list(strata = c("SEX", "REGION1")),
      .formats = c("xx.xx (xx.xx%)", "(xx.xxxx, xx.xxxx)")
    ) %>%
    build_table(anl)

  result <- get_formatted_cells(result)

  expected <- rbind(
    c("32.00 (23.88%)", "25.00 (18.66%)", "21.00 (15.91%)", "78.00 (19.50%)"),
    c("(13.8757, 28.6814)", "(9.5263, 25.5939)", "(6.3043, 19.3884)", "(14.4206, 22.2236)")
  )

  testthat::expect_equal(result, expected)

  # Changing other variables (weights and max_nt)
  n_ws <- length(unique(anl$SEX)) * length(unique(anl$REGION1))
  result <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    add_colcounts() %>%
    add_overall_col(label = "All") %>%
    estimate_proportion(
      vars = "DTHFL",
      conf_level = 0.95,
      method = "strat_wilson",
      variables = list(strata = c("SEX", "REGION1"), weights = rep(1 / n_ws, n_ws), max_nit = 1),
      .formats = c("xx.xx (xx.xx%)", "(xx.xxxx, xx.xxxx)")
    ) %>%
    build_table(anl)

  result <- get_formatted_cells(result)

  expected <- rbind(
    c("32.00 (23.88%)", "25.00 (18.66%)", "21.00 (15.91%)", "78.00 (19.50%)"),
    c("(13.3442, 37.8055)", "(12.0757, 38.7593)", "(9.1163, 32.7794)", "(13.7473, 27.0375)")
  )

  testthat::expect_equal(result, expected)
})
