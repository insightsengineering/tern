testthat::test_that("prop_wilson returns right result", {
  rsp <- c(
    TRUE, TRUE, TRUE, TRUE, TRUE,
    FALSE, FALSE, FALSE, FALSE, FALSE
  )
  result <- prop_wilson(rsp, conf_level = 0.9)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("update_weights_strat_wilson works with general inputs", {
  set.seed(1)

  vs <- c(0.011, 0.013, 0.012, 0.014, 0.017, 0.018)
  sq <- 0.674
  ws <- rep(1 / length(vs), length(vs))
  ns <- c(22, 18, 17, 17, 14, 12)

  result <- update_weights_strat_wilson(vs, sq, ws, ns, 100, 0.95, 0.001)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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

  res <- testthat::expect_silent(result$n_it)
  testthat::expect_snapshot(res)

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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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
  result <- prop_strat_wilson(
    rsp = rsp, strata = strata,
    weights = rep(1 / n_ws, n_ws), # Not automatic setting of weights
    conf_level = 0.90
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("prop_clopper_pearson returns right result", {
  rsp <- c(
    TRUE, TRUE, TRUE, TRUE, TRUE,
    FALSE, FALSE, FALSE, FALSE, FALSE
  )
  result <- prop_clopper_pearson(rsp, conf_level = .95)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("prop_wald returns right result", {
  rsp <- c(
    TRUE, TRUE, TRUE, TRUE, TRUE,
    FALSE, FALSE, FALSE, FALSE, FALSE
  )

  result <- prop_wald(rsp, conf_level = 0.95, correct = TRUE)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  result <- prop_wald(rsp, conf_level = 0.95, correct = FALSE)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("prop_agresti_coull returns right result", {
  rsp <- c(
    TRUE, TRUE, TRUE, TRUE, TRUE,
    FALSE, FALSE, FALSE, FALSE, FALSE
  )
  result <- prop_agresti_coull(rsp, conf_level = 0.95)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("prop_jeffreys returns right result", {
  rsp <- c(
    TRUE, TRUE, TRUE, TRUE, TRUE,
    FALSE, FALSE, FALSE, FALSE, FALSE
  )
  result <- prop_jeffreys(rsp, conf_level = 0.95)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("prop_strat_wilson output matches equivalent SAS function output", {
  set.seed(1)
  rsp <- c(
    sample(c(TRUE, FALSE), size = 40, prob = c(3 / 4, 1 / 4), replace = TRUE),
    sample(c(TRUE, FALSE), size = 40, prob = c(1 / 2, 1 / 2), replace = TRUE)
  )
  grp <- factor(rep(c("A", "B"), each = 40), levels = c("B", "A"))
  strata_data <- data.frame(
    "f1" = sample(c("a", "b"), 80, TRUE),
    "f2" = sample(c("x", "y", "z"), 80, TRUE),
    stringsAsFactors = TRUE
  )
  strata <- interaction(strata_data)
  weights <- 1:6 / sum(1:6)

  wilson <- prop_strat_wilson(rsp, strata, weights)
  result <- wilson$conf_int

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_proportion returns right result", {
  result <- s_proportion(c(1, 0, 1, 0))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("`s_proportion` works with Jeffreys CI", {
  # "Mid" case.
  rsp <- c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)
  result <- s_proportion(
    df = rsp,
    conf_level = 0.9,
    method = "jeffreys"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Corner case: Only responders.
  rsp <- c(TRUE, TRUE, TRUE, TRUE)
  result <- s_proportion(
    df = rsp,
    conf_level = 0.95,
    method = "jeffreys"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("`s_proportion` works with Agresti-Coull CI", {
  # "Mid" case.
  rsp <- c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)
  result <- s_proportion(
    df = rsp,
    conf_level = 0.9,
    method = "agresti-coull"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Edge case: Only responders.
  rsp <- c(TRUE, TRUE, TRUE, TRUE)
  result <- s_proportion(
    df = rsp,
    conf_level = 0.95,
    method = "agresti-coull"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("`estimate_proportion` is compatible with `rtables`", {
  # Data loading and processing
  anl <- tern_ex_adrs %>%
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
      .formats = c(n_prop = "xx.xx (xx.xx%)", prop_ci = "(xx.xxxx, xx.xxxx)")
    ) %>%
    build_table(anl)
  result <- get_formatted_cells(result)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("`estimate_proportion` and strat_wilson is compatible with `rtables`", {
  set.seed(1)
  # Data loading and processing
  anl <- tern_ex_adrs %>%
    dplyr::filter(PARAMCD == "BESRSPI") %>%
    dplyr::mutate(DTHFL = DTHFL == "Y") # Death flag yes

  suppressWarnings(result <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    add_colcounts() %>%
    add_overall_col(label = "All") %>%
    estimate_proportion(
      vars = "DTHFL",
      conf_level = 0.95,
      method = "strat_wilson",
      variables = list(strata = c("SEX", "REGION1")),
      .formats = c(n_prop = "xx.xx (xx.xx%)", prop_ci = "(xx.xxxx, xx.xxxx)")
    ) %>%
    build_table(anl))

  result <- get_formatted_cells(result)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that(
  "`estimate_proportion` and strat_wilson with equal weights and specific number of interactions works with `rtables`",
  {
    set.seed(1)

    # Data loading and processing
    anl <- tern_ex_adrs %>%
      dplyr::filter(PARAMCD == "BESRSPI") %>%
      dplyr::mutate(DTHFL = DTHFL == "Y") # Death flag yes

    # Changing other variables (weights and max_nt)
    n_ws <- length(unique(anl$SEX)) * length(unique(anl$STRATA1))
    result <- basic_table() %>%
      split_cols_by(var = "ARM") %>%
      add_colcounts() %>%
      add_overall_col(label = "All") %>%
      estimate_proportion(
        vars = "DTHFL",
        conf_level = 0.95,
        method = "strat_wilson",
        variables = list(strata = c("SEX", "STRATA1")),
        weights = rep(1 / n_ws, n_ws),
        max_iterations = 1,
        .formats = c(n_prop = "xx.xx (xx.xx%)", prop_ci = "(xx.xxxx, xx.xxxx)")
      ) %>%
      build_table(anl) %>%
      get_formatted_cells()

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)
testthat::test_that("`estimate_proportion` works with different denominators", {
  set.seed(1)

  # Data loading and processing
  anl <- tern_ex_adrs %>%
    dplyr::filter(PARAMCD == "BESRSPI") %>%
    dplyr::mutate(DTHFL = DTHFL == "Y") # Death flag yes

  # Changing other variables (weights and max_nt)
  n_ws <- length(unique(anl$SEX)) * length(unique(anl$STRATA1))
  expect_error(
    {
      result <- basic_table() %>%
        estimate_proportion(
          vars = "DTHFL",
          method = "strat_wilson",
          variables = list(strata = c("SEX", "STRATA1")),
          weights = rep(1 / n_ws, n_ws),
          denom = "N_cols"
        ) %>%
        build_table(anl)
    },
    "Stratified methods only support"
  )

  result <- basic_table() %>%
    estimate_proportion(
      vars = "DTHFL",
      denom = "N_col"
    ) %>%
    build_table(anl, col_counts = c(200))
  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  result <- basic_table() %>%
    estimate_proportion(
      vars = "DTHFL",
      denom = "n"
    ) %>%
    build_table(anl)
  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
