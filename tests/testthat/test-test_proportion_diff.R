testthat::test_that("prop_chisq returns right result", {
  set.seed(1, kind = "Mersenne-Twister")
  rsp <- c(
    sample(c(TRUE, FALSE), size = 20, prob = c(3 / 4, 1 / 4), replace = TRUE),
    sample(c(TRUE, FALSE), size = 20, prob = c(1 / 2, 1 / 2), replace = TRUE)
  )
  grp <- c(rep("A", 20), rep("B", 20))
  tbl <- table(grp, rsp)
  result <- prop_chisq(tbl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("prop_cmh returns right result", {
  set.seed(1, kind = "Mersenne-Twister")
  rsp <- sample(c(TRUE, FALSE), 100, TRUE)
  grp <- factor(rep(c("A", "B"), each = 50))
  strata <- factor(rep(c("V", "W", "X", "Y", "Z"), each = 20))
  tbl <- table(grp, rsp, strata)
  result <- prop_cmh(tbl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("prop_cmh also works when there are strata with just one observation", {
  tbl <- structure(
    c(
      20L, 17L, 7L, 10L, 0L, 0L, 2L, 3L, 21L, 14L, 3L,
      0L, 0L, 0L, 1L, 0L, 21L, 18L, 3L, 4L, 79L, 16L, 30L, 9L, 1L,
      0L, 13L, 4L
    ),
    .Dim = c(2L, 2L, 7L),
    .Dimnames = list(
      grp = c("Placebo", "Treatment"),
      x = c("no", "yes"),
      strat = c("A", "B", "C", "D", "E", "F", "G")
    ),
    class = "table"
  )

  testthat::expect_warning(
    result <- prop_cmh(tbl),
    "<5 data points in some strata. CMH test may be incorrect."
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("prop_fisher returns right result", {
  set.seed(1, kind = "Mersenne-Twister")
  rsp <- c(
    sample(c(TRUE, FALSE), size = 20, prob = c(3 / 4, 1 / 4), replace = TRUE),
    sample(c(TRUE, FALSE), size = 20, prob = c(1 / 2, 1 / 2), replace = TRUE)
  )
  grp <- c(rep("A", 20), rep("B", 20))
  tbl <- table(grp, rsp)
  result <- prop_fisher(tbl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("prop_schouten returns right result", {
  result <- sapply(
    1:100,
    function(x) {
      set.seed(x, kind = "Wichmann-Hill")
      n <- stats::runif(2)
      N <- stats::runif(2, min = 5, max = 100) # nolint

      rsp <- c(
        sample(c(TRUE, FALSE), size = N[1], prob = c(n[1], 1 - n[1]), replace = TRUE),
        sample(c(TRUE, FALSE), size = N[2], prob = c(n[2], 1 - n[2]), replace = TRUE)
      )
      grp <- c(rep("A", N[1]), rep("B", N[2]))

      tbl <- table(grp, rsp)
      if (ncol(tbl) < 2 | nrow(tbl) < 2) {
        return(NA_real_)
      }
      prop_schouten(tbl)
    }
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_test_proportion_diff and d_test_proportion_diff return right result", {
  set.seed(1984, kind = "Mersenne-Twister")
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("test_proportion_diff returns right result", {
  set.seed(1984, kind = "Mersenne-Twister")
  dta <- data.frame(
    rsp = sample(c(TRUE, FALSE), 100, TRUE),
    grp = factor(rep(c("A", "B"), each = 50)),
    strat = factor(rep(c("V", "W", "X", "Y", "Z"), each = 20))
  )

  result <- basic_table() %>%
    split_cols_by(var = "grp", ref_group = "B", split_fun = ref_group_position("first")) %>%
    test_proportion_diff(
      vars = "rsp",
      method = "cmh", variables = list(strata = "strat")
    ) %>%
    build_table(df = dta)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("test_proportion_diff edge case: all responder by chisq", {
  dta <- data.frame(
    rsp = rep(TRUE, each = 100),
    grp = factor(rep(c("A", "B"), each = 50))
  )

  result <- basic_table() %>%
    split_cols_by(var = "grp", ref_group = "B", split_fun = ref_group_position("first")) %>%
    test_proportion_diff(
      vars = "rsp",
      method = c("chisq", "schouten", "fisher", "cmh")[1]
    ) %>%
    build_table(df = dta)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("test_proportion_diff edge case: all responder by schouten", {
  dta <- data.frame(
    rsp = rep(TRUE, each = 100),
    grp = factor(rep(c("A", "B"), each = 50))
  )

  result <- basic_table() %>%
    split_cols_by(var = "grp", ref_group = "B", split_fun = ref_group_position("first")) %>%
    test_proportion_diff(
      vars = "rsp",
      method = c("chisq", "schouten", "fisher", "cmh")[2]
    ) %>%
    build_table(df = dta)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("test_proportion_diff edge case: all responder by fisher", {
  dta <- data.frame(
    rsp = rep(TRUE, each = 100),
    grp = factor(rep(c("A", "B"), each = 50))
  )

  result <- basic_table() %>%
    split_cols_by(var = "grp", ref_group = "B", split_fun = ref_group_position("first")) %>%
    test_proportion_diff(
      vars = "rsp",
      var_labels = "Variable Label",
      show_labels = "visible",
      method = c("chisq", "schouten", "fisher", "cmh")[3]
    ) %>%
    build_table(df = dta)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("test_proportion_diff edge case: all responder by CMH", {
  dta <- data.frame(
    rsp = rep(TRUE, each = 100),
    grp = factor(rep(c("A", "B"), each = 50)),
    strat = factor(rep(c("V", "W", "X", "Y", "Z"), each = 20))
  )

  result <- basic_table() %>%
    split_cols_by(var = "grp", ref_group = "B", split_fun = ref_group_position("first")) %>%
    test_proportion_diff(
      vars = "rsp",
      var_labels = "Variable Label",
      show_labels = "visible",
      method = c("chisq", "schouten", "fisher", "cmh")[4],
      variables = list(strata = "strat")
    ) %>%
    build_table(df = dta)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
