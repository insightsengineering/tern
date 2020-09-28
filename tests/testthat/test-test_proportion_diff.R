
test_that("prop_chisq returns right result", {

  set.seed(1)
  rsp <- c(
    sample(c(TRUE, FALSE), size = 20, prob = c(3 / 4, 1 / 4), replace = TRUE),
    sample(c(TRUE, FALSE), size = 20, prob = c(1 / 2, 1 / 2), replace = TRUE)
  )
  grp <- c(rep("A", 20), rep("B", 20))
  tbl <- table(grp, rsp)

  result <- prop_chisq(tbl)
  expected <- 0.0565
  expect_equal(result, expected, tolerance = 1e-4, check.attributes = FALSE)

})

test_that("prop_cmh returns right result", {

  set.seed(1)
  rsp <- sample(c(TRUE, FALSE), 100, TRUE)
  grp <- factor(rep(c("A", "B"), each = 50))
  strata <- factor(rep(c("V", "W", "X", "Y", "Z"), each = 20))
  tbl <- table(grp, rsp, strata)

  result <- prop_cmh(tbl)
  expected <- 0.6477
  expect_equal(result, expected, tolerance = 1e-4, check.attributes = FALSE)

})

test_that("prop_fisher returns right result", {

  set.seed(1)
  rsp <- c(
    sample(c(TRUE, FALSE), size = 20, prob = c(3 / 4, 1 / 4), replace = TRUE),
    sample(c(TRUE, FALSE), size = 20, prob = c(1 / 2, 1 / 2), replace = TRUE)
  )
  grp <- c(rep("A", 20), rep("B", 20))
  tbl <- table(grp, rsp)

  result <- prop_fisher(tbl)
  expected <- 0.1110
  expect_equal(result, expected, tolerance = 1e-4, check.attributes = FALSE)

})

test_that("prop_schouten returns right result", {

  set.seed(1)
  rsp <- c(
    sample(c(TRUE, FALSE), size = 20, prob = c(3 / 4, 1 / 4), replace = TRUE),
    sample(c(TRUE, FALSE), size = 20, prob = c(1 / 2, 1 / 2), replace = TRUE)
  )
  grp <- c(rep("A", 20), rep("B", 20))
  tbl <- table(grp, rsp)

  result <- prop_schouten(tbl)
  expected <- 0.0843
  expect_equal(result, expected, tolerance = 1e-4, check.attributes = FALSE)

})

test_that("s_test_proportion_diff and d_test_proportion_diff return right result", {

  set.seed(1984)
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
    s = list(pval = with_label(0.6477, result$d))
  )
  expect_equal(result, expected, tolerance = 1e-4)
})

test_that("test_proportion_diff returns right result", {
  set.seed(1984)
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
      "", "p-value (Cochran-Mantel-Haenszel Test)", "B",
      "", "A", "0.6477"
    ), .Dim = 2:3
  )
  expect_identical(result_matrix, expected_matrix)
})
