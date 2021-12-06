testthat::test_that("or_glm estimates right OR and CI", {
  data <- data.frame(
    rsp = as.logical(c(1, 1, 0, 1, 0, 0, 1, 1, 0, 0)),
    grp = letters[c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3)],
    stringsAsFactors = TRUE
  )

  data_ab <- subset(data, grp %in% c("a", "b"))
  data_ab$grp <- droplevels(data_ab$grp)

  result <- or_glm(data_ab, conf_level = 0.95)[[1]]
  expected <- c(est = 1 / 2 / 2 / 1, lcl = 0.0083, ucl = 7.4518)
  testthat::expect_equal(result, expected, tolerance = 1e-4)

  # Because `rtables` works by column (compared to the reference), we verified
  # that the model fitted on the complete dataset (grp: a, b, c) provides equal
  # estimations to the model fitted to the subset group and reference (grp: a, b).
  model_fit <- glm(rsp ~ grp, data, family = stats::binomial(link = "logit"))
  expected <- c(
    exp(stats::coef(model_fit)[-1])["grpb"],
    exp(stats::confint.default(model_fit, level = 0.95)["grpb", ])
  )
  testthat::expect_equal(result, expected, tolerance = 1e-4, check.attributes = FALSE)
})

testthat::test_that("or_clogit estimates right OR and CI", {
  data <- data.frame(
    rsp = as.logical(c(1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0)),
    grp =    letters[c(1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 1, 1, 1, 2, 2, 2, 3, 3, 2, 2)],
    strata = LETTERS[c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)],
    stringsAsFactors = TRUE
  )

  result <- or_clogit(data, conf_level = 0.95)
  expected <- list(
    or_ci = list(# from SAS
      b = c(est = 0.288, lcl = 0.036, ucl = 2.272),
      c = c(est = 0.780, lcl = 0.075, ucl = 8.146)
    ),
    n_tot = setNames(20, "n_tot")
  )
  testthat::expect_equal(result, expected, tolerance = 1e-3)
})

testthat::test_that("s_odds_ratio estimates right OR and CI (unstratified analysis)", {

  data <- data.frame(
    rsp = as.logical(c(1, 1, 0, 1, 0, 0, 1, 1, 0, 0)),
    grp = letters[c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3)]
  )

  result <- s_odds_ratio(
    df = subset(data, grp == "b"),
    .var = "rsp",
    .ref_group = subset(data, grp == "a"),
    .in_ref_col = FALSE
  )

  expected <- list(
    or_ci = with_label(
      c(est = 1 / 2 / 2 / 1, lcl = 0.0083, ucl = 7.4518),
      "Odds Ratio (95% CI)"
    ),
    n_tot = with_label(setNames(6, "n_tot"), "Total n")
  )
  testthat::expect_equal(result, expected, tolerance = 1e-4)
})

testthat::test_that("s_odds_ratio estimates right OR and CI (stratified analysis)", {

  set.seed(12)
  dta <- data.frame(
    rsp = sample(c(TRUE, FALSE), 100, TRUE),
    grp = factor(rep(c("A", "B"), each = 50), levels = c("B", "A")),
    strata = factor(sample(c("C", "D"), 100, TRUE))
  )

  result <- s_odds_ratio(
    df = subset(dta, grp == "A"),
    .var = "rsp",
    .ref_group = subset(dta, grp == "B"),
    .in_ref_col = FALSE,
    .df_row = dta,
    variables = list(arm = "grp", strata = "strata")
  )

  expected <- list(
    or_ci = with_label(
      c(est = 0.76898, lcl = 0.34242, ucl = 1.72692),
      "Odds Ratio (95% CI)"
    ),
    n_tot = with_label(setNames(100, "n_tot"), "Total n")
  )
  testthat::expect_equal(result, expected, tolerance = 1e-4)
})

testthat::test_that("estimate_odds_ratio estimates right OR and CI (unstratified analysis)", {
  data <- data.frame(
    rsp = as.logical(c(1, 1, 0, 1, 0, 0, 1, 1, 0, 0)),
    grp = letters[c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3)]
  )

  result <- basic_table() %>%
    split_cols_by(var = "grp", ref_group = "a") %>%
    estimate_odds_ratio(vars = "rsp") %>%
    build_table(df = data)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "",
      "Odds Ratio (95% CI)",
      "a",
      "",
      "b",
      "0.25 (0.01 - 7.45)",
      "c",
      "0.5 (0.02 - 11.09)"
    ),
    .Dim = c(2L, 4L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

test_that("estimate_odds_ratio estimates right OR and CI (stratified analysis)", {
  utils.nest::skip_if_too_deep(5)
  set.seed(12)
  data <- data.frame(
    rsp = sample(c(TRUE, FALSE), 100, TRUE),
    grp = factor(rep(c("A", "B"), each = 50), levels = c("B", "A")),
    strata = factor(sample(c("C", "D"), 100, TRUE))
  )

  result <- basic_table() %>%
    split_cols_by(var = "grp", ref_group = "A") %>%
    estimate_odds_ratio(vars = "rsp", variables = list(arm = "grp", strata = "strata")) %>%
    build_table(df = data)

  result_matrix <- to_string_matrix(result)
  expected_matrix <-
    structure(
      c(
        "",
        "Odds Ratio (95% CI)",
        "A",
        "",
        "B",
        "1.3 (0.58 - 2.92)"
      ),
      .Dim = 2:3
    )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("estimate_odds_ratio works with strata and combined groups", {
  set.seed(1, kind = "Mersenne-Twister")
  anl <- data.frame(
    rsp = sample(x = c(TRUE, FALSE), size = 100, replace = TRUE),
    ARM = factor(
      sample(x =  c("C: Combination", "A: Drug X", "B: Placebo"), size = 100, replace = TRUE),
      levels = c("C: Combination", "A: Drug X", "B: Placebo")
    ),
    SEX = factor(sample(x = c("D", "E"), size = 100, replace = TRUE))
  )
  groups <- combine_groups(fct = anl[["ARM"]])
  lyt <- basic_table() %>%
    split_cols_by_groups(
      var = "ARM",
      groups_list = groups,
      ref_group = names(groups)[1]
    ) %>%
    estimate_odds_ratio(
      vars = "rsp",
      variables = list(arm = "ARM", strata = "SEX"),
      conf_level = 0.95,
      table_names = "s_est_or",
      groups_list = groups
    )

  result <- build_table(lyt = lyt, df = anl)
  result_matrix <- to_string_matrix(result)
  expected_matrix <-
    structure(
      c(
        "",
        "Odds Ratio (95% CI)",
        "C: Combination",
        "",
        "A: Drug X/B: Placebo",
        "1.24 (0.54 - 2.89)"
      ),
      .Dim = 2:3
    )
  testthat::expect_identical(result_matrix, expected_matrix)
})
