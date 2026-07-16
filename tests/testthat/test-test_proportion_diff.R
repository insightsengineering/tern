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

  result_less <- prop_chisq(tbl, alternative = "less")
  res_less <- testthat::expect_silent(result_less)
  testthat::expect_snapshot(res_less)

  result_greater <- prop_chisq(tbl, alternative = "greater")
  res_greater <- testthat::expect_silent(result_greater)
  testthat::expect_snapshot(res_greater)
})

testthat::test_that("prop_cmh returns right result for odds ratio < 1", {
  set.seed(1, kind = "Mersenne-Twister")
  rsp <- factor(sample(c(TRUE, FALSE), 100, TRUE), levels = c("TRUE", "FALSE"))
  grp <- factor(rep(c("A", "B"), each = 50))
  strata <- factor(rep(c("V", "W", "X", "Y", "Z"), each = 20))
  tbl <- table(grp, rsp, strata)
  checkmate::assert_true(stats::mantelhaen.test(tbl, correct = FALSE)$estimate < 1)

  result <- prop_cmh(tbl)
  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
  mh_result <- stats::mantelhaen.test(tbl, correct = FALSE)
  expected_z_stat <- sqrt(unname(mh_result$statistic))
  expected <- structure(mh_result$p.value, z_stat = expected_z_stat)
  testthat::expect_equal(result, expected, tolerance = 1e-3)

  result_less <- prop_cmh(tbl, alternative = "less")
  res_less <- testthat::expect_silent(result_less)
  testthat::expect_snapshot(res_less)
  expected_less <- structure(
    stats::mantelhaen.test(tbl, correct = FALSE, alternative = "less")$p.value,
    z_stat = expected_z_stat
  )
  testthat::expect_equal(result_less, expected_less, tolerance = 1e-3)

  result_greater <- prop_cmh(tbl, alternative = "greater")
  res_greater <- testthat::expect_silent(result_greater)
  testthat::expect_snapshot(res_greater)
  expected_greater <- structure(
    stats::mantelhaen.test(tbl, correct = FALSE, alternative = "greater")$p.value,
    z_stat = expected_z_stat
  )
  testthat::expect_equal(result_greater, expected_greater, tolerance = 1e-3)
})

testthat::test_that("prop_cmh returns right result for odds ratio > 1", {
  set.seed(847, kind = "Mersenne-Twister")
  rsp <- factor(sample(c(TRUE, FALSE), 100, TRUE), levels = c("TRUE", "FALSE"))
  grp <- factor(rep(c("A", "B"), each = 50))
  strata <- factor(rep(c("V", "W", "X", "Y", "Z"), each = 20))
  tbl <- table(grp, rsp, strata)
  checkmate::assert_true(stats::mantelhaen.test(tbl, correct = FALSE)$estimate > 1)

  result <- prop_cmh(tbl)
  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
  mh_result <- stats::mantelhaen.test(tbl, correct = FALSE)
  expected_z_stat <- -sqrt(unname(mh_result$statistic))
  expected <- structure(mh_result$p.value, z_stat = expected_z_stat)
  testthat::expect_equal(result, expected, tolerance = 1e-3)

  result_less <- prop_cmh(tbl, alternative = "less")
  res_less <- testthat::expect_silent(result_less)
  testthat::expect_snapshot(res_less)
  expected_less <- structure(
    stats::mantelhaen.test(tbl, correct = FALSE, alternative = "less")$p.value,
    z_stat = expected_z_stat
  )
  testthat::expect_equal(result_less, expected_less, tolerance = 1e-3)

  result_greater <- prop_cmh(tbl, alternative = "greater")
  res_greater <- testthat::expect_silent(result_greater)
  testthat::expect_snapshot(res_greater)
  expected_greater <- structure(
    stats::mantelhaen.test(tbl, correct = FALSE, alternative = "greater")$p.value,
    z_stat = expected_z_stat
  )
  testthat::expect_equal(result_greater, expected_greater, tolerance = 1e-3)
})

testthat::test_that("prop_cmh also works when there are strata with just one observation", {
  tbl <- structure(
    c(
      20L, 17L, 7L, 10L, 0L, 0L, 2L, 3L, 21L, 14L, 3L,
      0L, 0L, 0L, 1L, 0L, 21L, 18L, 3L, 4L, 79L, 16L, 30L, 9L, 1L,
      0L, 13L, 4L
    ),
    dim = c(2L, 2L, 7L),
    dimnames = list(
      grp = c("Placebo", "Treatment"),
      x = c("no", "yes"),
      strata = c("A", "B", "C", "D", "E", "F", "G")
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

  result_less <- prop_fisher(tbl, alternative = "less")
  res_less <- testthat::expect_silent(result_less)
  testthat::expect_snapshot(res_less)

  result_greater <- prop_fisher(tbl, alternative = "greater")
  res_greater <- testthat::expect_silent(result_greater)
  testthat::expect_snapshot(res_greater)
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
  testthat::expect_snapshot_value(res, style = "deparse", tolerance = 1e-3)
})

testthat::test_that("prop_schouten returns right result for less or greater alternative", {
  set.seed(1, kind = "Mersenne-Twister")
  rsp <- c(
    sample(c(TRUE, FALSE), size = 20, prob = c(3 / 4, 1 / 4), replace = TRUE),
    sample(c(TRUE, FALSE), size = 20, prob = c(1 / 2, 1 / 2), replace = TRUE)
  )
  grp <- c(rep("A", 20), rep("B", 20))
  tbl <- table(grp, rsp)

  result_less <- prop_schouten(tbl, alternative = "less")
  res_less <- testthat::expect_silent(result_less)
  testthat::expect_snapshot_value(res_less, style = "deparse", tolerance = 1e-3)

  result_greater <- prop_schouten(tbl, alternative = "greater")
  res_greater <- testthat::expect_silent(result_greater)
  testthat::expect_snapshot_value(res_greater, style = "deparse", tolerance = 1e-3)

  # And these results are in line with the standard Chi-Squared test.
  result_chisq_less <- prop_chisq(tbl, alternative = "less")
  result_chisq_greater <- prop_chisq(tbl, alternative = "greater")

  expect_equal(result_less, result_chisq_less, tolerance = 1e-1)
  expect_equal(result_greater, result_chisq_greater, tolerance = 1e-1)
})

testthat::test_that("prop_cmh with Wilson-Hilferty transformation works", {
  set.seed(1, kind = "Mersenne-Twister")
  rsp <- sample(c(TRUE, FALSE), 100, TRUE)
  grp <- factor(rep(c("A", "B"), each = 50))
  strata <- factor(rep(c("V", "W", "X", "Y", "Z"), each = 20))
  tbl <- table(grp, rsp, strata)

  result_less <- prop_cmh(tbl, alternative = "less", transform = "wilson_hilferty")
  res_less <- testthat::expect_silent(result_less)
  testthat::expect_snapshot(res_less)

  result_greater <- prop_cmh(tbl, alternative = "greater", transform = "wilson_hilferty")
  res_greater <- testthat::expect_silent(result_greater)
  testthat::expect_snapshot(res_greater)

  # And these results are in line with the standard CMH test.
  result_cmh_less <- prop_cmh(tbl, alternative = "less")
  result_cmh_greater <- prop_cmh(tbl, alternative = "greater")

  expect_equal(as.numeric(result_less), as.numeric(result_cmh_less), tolerance = 1e-1)
  expect_equal(as.numeric(result_greater), as.numeric(result_cmh_greater), tolerance = 1e-1)
  expected_z_stat <- {
    z_stat <- attr(result_cmh_less, "z_stat")
    ((1 - 2 / 9) - (z_stat^2)^(1 / 3)) / sqrt(2 / 9) * sign(z_stat)
  }
  expect_equal(attr(result_less, "z_stat"), expected_z_stat)
  expect_equal(attr(result_greater, "z_stat"), expected_z_stat)
})

testthat::test_that("prop_cmh with Sato variance estimator works", {
  tbl1 <- structure(
    c(
      2L, 5L, 14L, 11L,
      3L, 4L, 8L, 6L,
      2L, 7L, 17L, 13L,
      4L, 5L, 13L, 12L
    ),
    dim = c(2L, 2L, 4L),
    dimnames = list(
      grp = c("B", "A"),
      rsp = c("TRUE", "FALSE"),
      strata = c("S1", "S2", "S3", "S4")
    ),
    class = "table"
  )
  result1 <- prop_cmh(tbl1, diff_se = "sato")
  testthat::expect_equal(as.numeric(result1), 0.035, tolerance = 1e-3)
  testthat::expect_equal(attr(result1, "z_stat"), 2.109, tolerance = 1e-3)

  tbl2 <- structure(
    c(
      2L, 7L, 14L, 8L,
      3L, 3L, 8L, 7L,
      2L, 10L, 17L, 10L,
      4L, 5L, 13L, 12L
    ),
    dim = c(2L, 2L, 4L),
    dimnames = list(
      grp = c("B", "A"),
      rsp = c("TRUE", "FALSE"),
      strata = c("S1", "S2", "S3", "S4")
    ),
    class = "table"
  )
  result2 <- prop_cmh(tbl2, diff_se = "sato")
  testthat::expect_equal(as.numeric(result2), 0.004, tolerance = 1e-2)
  testthat::expect_equal(attr(result2, "z_stat"), 2.864, tolerance = 1e-3)
})

testthat::test_that("prop_cmh with Sato variance estimator and Wilson-Hilferty transformation works", {
  tbl <- structure(
    c(
      2L, 5L, 14L, 11L,
      3L, 4L, 8L, 6L,
      2L, 7L, 17L, 13L,
      4L, 5L, 13L, 12L
    ),
    dim = c(2L, 2L, 4L),
    dimnames = list(
      grp = c("B", "A"),
      rsp = c("TRUE", "FALSE"),
      strata = c("S1", "S2", "S3", "S4")
    ),
    class = "table"
  )
  testthat::expect_warning(
    result <- prop_cmh(tbl, diff_se = "sato", transform = "wilson_hilferty"),
    "not designed for use with the Sato variance estimator"
  )
  testthat::expect_snapshot_value(result, style = "deparse", tolerance = 1e-3)
})

testthat::test_that("s_test_proportion_diff and d_test_proportion_diff return right result", {
  set.seed(1984, kind = "Mersenne-Twister")
  dta <- data.frame(
    rsp = sample(c(TRUE, FALSE), 100, TRUE),
    grp = factor(rep(c("A", "B"), each = 50)),
    strata = factor(rep(c("V", "W", "X", "Y", "Z"), each = 20))
  )
  method <- "cmh"
  result <- list(
    d = d_test_proportion_diff(method),
    s = s_test_proportion_diff(
      df = subset(dta, grp == "A"),
      .var = "rsp",
      .ref_group = subset(dta, grp == "B"),
      .in_ref_col = FALSE,
      variables = list(strata = "strata"),
      method = "cmh"
    )
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_test_proportion_diff and d_test_proportion_diff work with less and greater alternatives", {
  set.seed(1984, kind = "Mersenne-Twister")
  dta <- data.frame(
    rsp = sample(c(TRUE, FALSE), 100, TRUE),
    grp = factor(rep(c("A", "B"), each = 50)),
    strata = factor(rep(c("V", "W", "X", "Y", "Z"), each = 20))
  )
  method <- "cmh"
  for (alternative in c("greater", "less")) {
    result <- list(
      d = d_test_proportion_diff(method, alternative = alternative),
      s = s_test_proportion_diff(
        df = subset(dta, grp == "A"),
        .var = "rsp",
        .ref_group = subset(dta, grp == "B"),
        .in_ref_col = FALSE,
        variables = list(strata = "strata"),
        method = "cmh",
        alternative = alternative
      )
    )

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
})

testthat::test_that("test_proportion_diff returns right result", {
  set.seed(1984, kind = "Mersenne-Twister")
  dta <- data.frame(
    rsp = sample(c(TRUE, FALSE), 100, TRUE),
    grp = factor(rep(c("A", "B"), each = 50)),
    strata = factor(rep(c("V", "W", "X", "Y", "Z"), each = 20))
  )

  result <- basic_table() |>
    split_cols_by(var = "grp", ref_group = "B", split_fun = ref_group_position("first")) |>
    test_proportion_diff(
      vars = "rsp",
      method = "cmh", variables = list(strata = "strata")
    ) |>
    build_table(df = dta)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("test_proportion_diff uses alternative argument", {
  set.seed(1984, kind = "Mersenne-Twister")
  dta <- data.frame(
    rsp = sample(c(TRUE, FALSE), 100, TRUE),
    grp = factor(rep(c("A", "B"), each = 50)),
    strata = factor(rep(c("V", "W", "X", "Y", "Z"), each = 20))
  )

  result <- basic_table() |>
    split_cols_by(var = "grp", ref_group = "B", split_fun = ref_group_position("first")) |>
    test_proportion_diff(
      vars = "rsp",
      method = "cmh",
      alternative = "greater",
      variables = list(strata = "strata")
    ) |>
    build_table(df = dta)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("test_proportion_diff edge case: all responder by chisq", {
  dta <- data.frame(
    rsp = rep(TRUE, each = 100),
    grp = factor(rep(c("A", "B"), each = 50))
  )

  result <- basic_table() |>
    split_cols_by(var = "grp", ref_group = "B", split_fun = ref_group_position("first")) |>
    test_proportion_diff(
      vars = "rsp",
      method = c("chisq", "schouten", "fisher", "cmh", "cmh_wh")[1]
    ) |>
    build_table(df = dta)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("test_proportion_diff edge case: all responder by schouten", {
  dta <- data.frame(
    rsp = rep(TRUE, each = 100),
    grp = factor(rep(c("A", "B"), each = 50))
  )

  result <- basic_table() |>
    split_cols_by(var = "grp", ref_group = "B", split_fun = ref_group_position("first")) |>
    test_proportion_diff(
      vars = "rsp",
      method = c("chisq", "schouten", "fisher", "cmh", "cmh_wh")[2]
    ) |>
    build_table(df = dta)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("test_proportion_diff edge case: all responder by fisher", {
  dta <- data.frame(
    rsp = rep(TRUE, each = 100),
    grp = factor(rep(c("A", "B"), each = 50))
  )

  result <- basic_table() |>
    split_cols_by(var = "grp", ref_group = "B", split_fun = ref_group_position("first")) |>
    test_proportion_diff(
      vars = "rsp",
      var_labels = "Variable Label",
      show_labels = "visible",
      method = c("chisq", "schouten", "fisher", "cmh", "cmh_wh")[3]
    ) |>
    build_table(df = dta)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("test_proportion_diff edge case: all responder by CMH", {
  dta <- data.frame(
    rsp = rep(TRUE, each = 100),
    grp = factor(rep(c("A", "B"), each = 50)),
    strata = factor(rep(c("V", "W", "X", "Y", "Z"), each = 20))
  )

  result <- basic_table() |>
    split_cols_by(var = "grp", ref_group = "B", split_fun = ref_group_position("first")) |>
    test_proportion_diff(
      vars = "rsp",
      var_labels = "Variable Label",
      show_labels = "visible",
      method = c("chisq", "schouten", "fisher", "cmh", "cmh_wh")[4],
      variables = list(strata = "strata")
    ) |>
    build_table(df = dta)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("test_proportion_diff edge case: all responder by CMH with Wilson-Hilferty transformation", {
  dta <- data.frame(
    rsp = rep(TRUE, each = 100),
    grp = factor(rep(c("A", "B"), each = 50)),
    strata = factor(rep(c("V", "W", "X", "Y", "Z"), each = 20))
  )

  result <- basic_table() |>
    split_cols_by(var = "grp", ref_group = "B", split_fun = ref_group_position("first")) |>
    test_proportion_diff(
      vars = "rsp",
      var_labels = "Variable Label",
      show_labels = "visible",
      method = c("chisq", "schouten", "fisher", "cmh", "cmh_wh")[5],
      variables = list(strata = "strata")
    ) |>
    build_table(df = dta)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("test_proportion_diff edge case: all responder by CMH with Sato variance estimator", {
  dta <- data.frame(
    rsp = rep(TRUE, each = 100),
    grp = factor(rep(c("A", "B"), each = 50)),
    strata = factor(rep(c("V", "W", "X", "Y", "Z"), each = 20))
  )

  result <- basic_table() |>
    split_cols_by(var = "grp", ref_group = "B", split_fun = ref_group_position("first")) |>
    test_proportion_diff(
      vars = "rsp",
      var_labels = "Variable Label",
      show_labels = "visible",
      method = "cmh_sato",
      variables = list(strata = "strata")
    ) |>
    build_table(df = dta)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
