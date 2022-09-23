testthat::test_that("prop_wilson returns right result", {
  rsp <- c(
    TRUE, TRUE, TRUE, TRUE, TRUE,
    FALSE, FALSE, FALSE, FALSE, FALSE
  )

  expected <- c(0.2692718, 0.7307282)
  result <- prop_wilson(rsp, conf_level = 0.9)

  testthat::expect_equal(expected, result, tolerance = 1e-5)
})

testthat::test_that("strata_normal_quantile works with general factor table", {
  set.seed(1)

  strata_data <- table(data.frame(
    "f1" = sample(c(TRUE, FALSE), 100, TRUE),
    "f2" = sample(c("x", "y", "z"), 100, TRUE),
    stringsAsFactors = TRUE
  ))

  ns <- colSums(strata_data)
  ests <- strata_data["TRUE", ] / ns
  vars <- ests * (1 - ests) / ns
  weights <- rep(1 / length(ns), length(ns))

  result <- strata_normal_quantile(vars, weights, 0.95)

  testthat::expect_equal(result, 1.133272, tol = 0.000001)
})

testthat::test_that("update_weights_strat_wilson works with general inputs", {
  set.seed(1)

  vs <- c(0.011, 0.013, 0.012, 0.014, 0.017, 0.018)
  sq <- 0.674
  ws <- rep(1 / length(vs), length(vs))
  ns <- c(22, 18, 17, 17, 14, 12)

  result <- update_weights_strat_wilson(vs, sq, ws, ns, 100, 0.95, 0.001)
  expected <- list(
    "n_it" = 3,
    "weights" = c(0.2067191, 0.1757727, 0.1896962, 0.1636346, 0.1357615, 0.1284160)
  )

  testthat::expect_equal(result[1:2], expected, tol = 0.000001)
})

testthat::test_that("update_weights_strat_wilson convergence test", {
  set.seed(1)

  # Important parameters
  n_to_test <- 1000 # Number of entries
  n_ltrs <- 15 # Number of centers/strata = n_ltrs * 3 (i.e. x, y, z)

  # Table creation
  strata_data <- data.frame(
    "rsp" = sample(c(TRUE, FALSE), n_to_test, TRUE),
    "f1" = sample(letters[1:n_ltrs], n_to_test, TRUE),
    "f2" = sample(c("x", "y", "z"), n_to_test, TRUE),
    stringsAsFactors = TRUE
  )
  strata <- interaction(strata_data[2:3])
  tbl_strata <- table(strata_data[[1]], strata)
  n_strata <- ncol(tbl_strata) # Number of weights or centers

  # Defining inputs
  xs <- tbl_strata["TRUE", ]
  ns <- colSums(tbl_strata)
  ests <- xs / ns
  vs <- ests * (1 - ests) / ns
  ws <- rep(1 / length(vs), length(vs))
  cl <- 0.95 # Confidence level
  ni <- 1000 # Maximum number of allowed iterations
  tol <- 0.0001 # Tolerance for convergence
  sq <- strata_normal_quantile(vs, ws, cl) # Initial quantiles

  result <- update_weights_strat_wilson(vs, sq, ws, ns, ni, cl, tol)
  testthat::expect_equal(result$n_it, 2)
  warning_message <- "The heuristic to find weights did not converge with max_iterations = 2"
  testthat::expect_warning(update_weights_strat_wilson(vs, sq, ws, ns, 2, cl, 0.000000001),
    regexp = warning_message
  )
})

testthat::test_that("prop_strat_wilson returns right results", {
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

  result <- prop_strat_wilson(
    rsp = rsp, strata = strata,
    conf_level = 0.90
  )

  expected <- list(
    conf_int = c(lower = 0.4072891, upper = 0.5647887),
    weights = c(0.2074199, 0.1776464, 0.1915610, 0.1604678, 0.1351096, 0.1277952)
  )
  names(expected$weights) <- colnames(table_strata)

  testthat::expect_equal(result, expected, tolerance = 1e-5)
})

testthat::test_that("prop_strat_wilson returns right result with inserted weights", {
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

  # Test without estimating weights (all equal here)
  expected <- list(conf_int = c(lower = 0.4190436, upper = 0.5789733))
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
  anl <- adrs_raw %>%
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
  anl <- adrs_raw %>%
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
    c("25.00 (18.66%)", "23.00 (17.16%)", "22.00 (16.67%)", "70.00 (17.50%)"),
    c("(10.7692, 23.5028)", "(8.5334, 24.7046)", "(9.6237, 23.1385)", "(12.8136, 19.6358)")
  )

  testthat::expect_equal(result, expected)
})

testthat::test_that("`estimate_proportion` and strat_wilson with equal weights
                    and specific number of interactions is compatible with `rtables`", {
  set.seed(1)

  # Data loading and processing
  anl <- adrs_raw %>%
    dplyr::filter(PARAMCD == "BESRSPI") %>%
    dplyr::mutate(DTHFL = DTHFL == "Y") # Death flag yes

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
      variables = list(strata = c("SEX", "REGION1")),
      weights = rep(1 / n_ws, n_ws),
      max_iterations = 1,
      .formats = c("xx.xx (xx.xx%)", "(xx.xxxx, xx.xxxx)")
    ) %>%
    build_table(anl)

  result <- get_formatted_cells(result)

  expected <- rbind(
    c("25.00 (18.66%)", "23.00 (17.16%)", "22.00 (16.67%)", "70.00 (17.50%)"),
    c("(13.0819, 37.5306)", "(13.0280, 40.8632)", "(8.5252, 30.6413)", "(14.1413, 27.6395)")
  )

  testthat::expect_equal(result, expected)
})
