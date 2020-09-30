
test_that("prop_chisq returns right result", {

  set.seed(1, kind = "Wichmann-Hill")
  rsp <- c(
    sample(c(TRUE, FALSE), size = 20, prob = c(3 / 4, 1 / 4), replace = TRUE),
    sample(c(TRUE, FALSE), size = 20, prob = c(1 / 2, 1 / 2), replace = TRUE)
  )
  grp <- c(rep("A", 20), rep("B", 20))
  tbl <- table(grp, rsp)

  result <- prop_chisq(tbl)
  expected <- 0.0015
  expect_equal(result, expected, tolerance = 1e-4, check.attributes = FALSE)

})

test_that("prop_cmh returns right result", {

  set.seed(1, kind = "Wichmann-Hill")
  rsp <- sample(c(TRUE, FALSE), 100, TRUE)
  grp <- factor(rep(c("A", "B"), each = 50))
  strata <- factor(rep(c("V", "W", "X", "Y", "Z"), each = 20))
  tbl <- table(grp, rsp, strata)

  result <- prop_cmh(tbl)
  expected <- 0.6613
  expect_equal(result, expected, tolerance = 1e-4, check.attributes = FALSE)

})

test_that("prop_fisher returns right result", {

  set.seed(1, kind = "Wichmann-Hill")
  rsp <- c(
    sample(c(TRUE, FALSE), size = 20, prob = c(3 / 4, 1 / 4), replace = TRUE),
    sample(c(TRUE, FALSE), size = 20, prob = c(1 / 2, 1 / 2), replace = TRUE)
  )
  grp <- c(rep("A", 20), rep("B", 20))
  tbl <- table(grp, rsp)

  result <- prop_fisher(tbl)
  expected <- 0.0036
  expect_equal(result, expected, tolerance = 1e-4, check.attributes = FALSE)

})

test_that("prop_schouten returns right result", {
  result <- sapply(
    1:100,
    function(x) {

      set.seed(x, kind = "Wichmann-Hill")
      n <- runif(2)
      N <- runif(2, min = 5, max = 100) # nolint

      rsp <- c(
        sample(c(TRUE, FALSE), size = N[1], prob = c(n[1], 1 - n[1]), replace = TRUE),
        sample(c(TRUE, FALSE), size = N[2], prob = c(n[2], 1 - n[2]), replace = TRUE)
      )
      grp <- c(rep("A", N[1]), rep("B", N[2]))

      tbl <- table(grp, rsp)
      if (ncol(tbl) < 2 | nrow(tbl) < 2) return(NA_real_)
      prop_schouten(tbl)
    }
  )

  # Results obtained with initial version for 100 random samples.
  expected <- c(
    0, 0, 2e-05, 0.54964, 0.70345, 0.01529, 1e-05, 0.03273, 0.00051,
    0, 0.74303, 0.03571, 0, 0, 0.55293, 0.05028, 0, 0, 0.14816, 0.72437,
    0, 0, 0.00012, 0.1371, 0.06622, 0.10328, 0.00025, 0, 0, 0.38382,
    0.00021, 0, 0.33918, 8e-05, 0.07427, 1e-05, 0, 0, 0.67378, 0.75593,
    0, 0, 9e-05, 0, 0.19572, 0.19949, 0.00577, 0.05601, 0.29104,
    0.32235, 0.04809, 0, 0.06569, 0.54562, 0.6423, 0.0237, 2e-05,
    0, 0.01161, 0.97357, 0.33165, 0.09247, NA, 0.26629, 0.04719,
    4e-05, 0, 0, 0.00916, 0.17724, 0.0031, 0.00094, 0.00131, 0.32759,
    0.16916, 0.36903, 2e-05, 0, 0.90684, 0.0138, 0.00032, 0, 0.01318,
    0, 0.9758, 0, 6e-05, 0, 0.10609, 0.01319, 0.44322, 1e-05, 0.00041,
    0.05099, 0.00112, 0, 0, 0.56411, 0.00532, 0.00029
  )
  expect_equal(result, expected, tolerance = 1e-5)
})

test_that("s_test_proportion_diff and d_test_proportion_diff return right result", {

  set.seed(194, kind = "Wichmann-Hill")
  dta <- data.frame(
    rsp = sample(c(TRUE, FALSE), 100, TRUE),
    grp = factor(rep(c("A", "B"), each = 50)),
    strat = factor(rep(c("V", "W", "X", "Y", "Z"), each = 20))
  )

  method <- "cmh"
  result <- list(
    d = d_test_proportion_diff(method),
    s = s_test_proportion_diff(
      df = subset(dta, grp == "A"),
      .var = "rsp",
      .ref_group = subset(dta, grp == "B"),
      .in_ref_col = FALSE,
      variables = list(strata = "strat"),
      method = "cmh"
    )
  )

  expected <- list(
    d = "p-value (Cochran-Mantel-Haenszel Test)",
    s = list(pval = with_label(0.18876, result$d))
  )
  expect_equal(result, expected, tolerance = 1e-4)
})

test_that("test_proportion_diff returns right result", {
  set.seed(376, kind = "Wichmann-Hill")
  dta <- data.frame(
    rsp = sample(c(TRUE, FALSE), 100, TRUE),
    grp = factor(rep(c("A", "B"), each = 50)),
    strat = factor(rep(c("V", "W", "X", "Y", "Z"), each = 20))
  )

  result <- split_cols_by(lyt = NULL, var = "grp", ref_group = "B") %>%
    test_proportion_diff(
      vars = "rsp",
      method = "cmh", variables = list(strata = "strat")
    ) %>% build_table(df = dta)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "  p-value (Cochran-Mantel-Haenszel Test)", "B",
      "", "A", "0.3415"
    ), .Dim = 2:3
  )
  expect_identical(result_matrix, expected_matrix)
})
