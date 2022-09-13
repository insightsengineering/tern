testthat::test_that("h_ancova works with healthy input", {
  result <- h_ancova(
    .var = "Sepal.Length",
    .df_row = iris,
    variables = list(arm = "Species", covariates = c("Petal.Length * Petal.Width", "Sepal.Width"))
  ) %>%
    as.data.frame()

  # Fixing class differences between versions
  exp_class <- "data.frame"
  if (inherits(result, "summary_emm")) exp_class <- c("summary_emm", exp_class)

  expected <- structure(
    data.frame(
      Species = as.factor(c("setosa", "versicolor", "virginica")),
      emmean = c(6.154823, 5.717409, 5.406377),
      SE = c(0.33709131, 0.06680849, 0.14879463),
      df = c(143, 143, 143),
      lower.CL = c(5.488497, 5.585349, 5.112255),
      upper.CL = c(6.821149, 5.849469, 5.700498)
    ),
    class = exp_class,
    estName = "emmean",
    clNames = c("lower.CL", "upper.CL"),
    pri.vars = "Species",
    adjust = "none",
    side = 0,
    delta = 0,
    type = "link",
    mesg = "Confidence level used: 0.95"
  )

  testthat::expect_equal(result, expected, tolerance = 0.0000001)
})

testthat::test_that("h_ancova fails wrong inputs", {
  testthat::expect_error(
    h_ancova(
      .var = "Wrong.Var",
      .df_row = iris,
      variables = list(arm = "Species", covariates = c("Petal.Length * Petal.Width", "Sepal.Width"))
    )
  )

  testthat::expect_error(
    h_ancova(
      .var = "Sepal.Length",
      .df_row = iris,
      variables = list(arm = "Species", covariates = c("Wrong.Var", "Sepal.Width"))
    )
  )
})

testthat::test_that("s_ancova works with healthy input", {
  df_col <- iris %>% dplyr::filter(Species == "versicolor")
  df_ref <- iris %>% dplyr::filter(Species == "setosa")
  result <- s_ancova(
    df = df_col,
    .var = "Sepal.Length",
    .df_row = iris,
    variables = list(arm = "Species", covariates = c("Petal.Length * Petal.Width", "Sepal.Width")),
    .ref_group = df_ref,
    .in_ref_col = FALSE,
    conf_level = 0.99
  )

  expected <- list(
    n = 50L,
    lsmean = formatters::with_label(5.717409, "Adjusted Mean"),
    lsmean_diff = formatters::with_label(-0.4374138, "Difference in Adjusted Means"),
    lsmean_diff_ci = formatters::with_label(c(-1.1865544, 0.3117267), "99% CI"),
    pval = formatters::with_label(0.2503574, "p-value")
  )

  testthat::expect_equal(result, expected, tolerance = 0.0000001)
})

testthat::test_that("s_ancova fails wrong inputs", {
  testthat::expect_error(
    s_ancova(
      df = iris,
      .var = "Sepal.Length",
      variables = list(arm = "Species", covariates = c("Petal.Length * Petal.Width", "Sepal.Width")),
      .df_row = iris,
      .in_ref_col = FALSE,
      conf_level = 0.99
    )
  )
})

testthat::test_that("summarize_ancova works with healthy inputs", {
  result <- basic_table() %>%
    split_cols_by("Species", ref_group = "setosa") %>%
    add_colcounts() %>%
    summarize_ancova(
      vars = "Sepal.Length",
      variables = list(arm = "Species", covariates = NULL),
      conf_level = 0.95, var_labels = "Unadjusted comparison",
      .labels = c(lsmean = "Mean", lsmean_diff = "Difference in Means"),
      table_names = "unadjusted"
    ) %>%
    summarize_ancova(
      vars = "Sepal.Length",
      variables = list(arm = "Species", covariates = "Petal.Length"),
      conf_level = 0.95, var_labels = "Adjusted comparison (covariates Petal.Length)",
      table_names = "adjusted"
    ) %>%
    build_table(iris)
  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Unadjusted comparison", "n", "Mean", "Difference in Means", "95% CI", "p-value",
      "Adjusted comparison (covariates Petal.Length)", "n", "Adjusted Mean", "Difference in Adjusted Means",
      "95% CI", "p-value",
      "setosa", "(N=50)", "", "50", "5.01", "", "", "", "", "50", "7.08", "", "", "",
      "versicolor", "(N=50)", "", "50", "5.94", "0.93", "(0.73, 1.13)", "<0.0001",
      "", "50", "5.48", "-1.60", "(-1.98, -1.22)", "<0.0001",
      "virginica", "(N=50)", "", "50", "6.59", "1.58", "(1.38, 1.79)", "<0.0001",
      "", "50", "4.97", "-2.12", "(-2.66, -1.58)", "<0.0001"
    ),
    .Dim = c(14L, 4L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})


testthat::test_that("summarize_ancova works with interaction", {
  iris_new <- iris %>%
    mutate(p_group = case_when(
      substr(Petal.Width, 3, 3) < 3 ~ "A",
      substr(Petal.Width, 3, 3) < 5 & substr(Petal.Width, 3, 3) > 2 ~ "B",
      TRUE ~ "C"
    )) %>%
    mutate(p_group = as.factor(p_group))

  result <- basic_table() %>%
    split_cols_by("Species", ref_group = "setosa") %>%
    add_colcounts() %>%
    summarize_ancova(
      vars = "Petal.Length",
      variables = list(arm = "Species", covariates = c("Sepal.Width", "p_group", "Species*p_group")),
      conf_level = 0.95,
      var_labels = "Petal_B",
      table_names = "Petal_B",
      interaction_y = "B",
      interaction_item = "p_group"
    ) %>%
    build_table(iris_new)
  result_matrix <- to_string_matrix(result)

  lm_fit <- stats::lm(formula = "Petal.Length ~ Sepal.Width + p_group + Species*p_group + Species", data = iris_new)
  emmeans_fit <- emmeans::emmeans(lm_fit, specs = c("Species", "p_group"), data = iris_new)
  emmean <- emmeans_fit %>%
    as.data.frame() %>%
    filter(p_group == "B") %>%
    select(emmean) %>%
    unlist() %>%
    round(., 2)
  testthat::expect_equal(as.numeric(emmean), as.numeric(result_matrix[5, 2:4]), tolerance = 0.0000001)

  emmeans_contrasts <- emmeans::contrast(emmeans_fit, method = "trt.vs.ctrl", ref = 4)
  sum_contrasts <- summary(emmeans_contrasts, infer = TRUE, adjust = "none") %>%
    as.data.frame() %>%
    filter(contrast %in% c("versicolor B - setosa B", "virginica B - setosa B")) %>%
    select(estimate, lower.CL, upper.CL) %>%
    round(., 2)
  ci_a <- paste0("(", as.numeric(sum_contrasts[1, 2]), ", ", as.numeric(sum_contrasts[1, 3]), ")")
  ci_b <- paste0("(", as.numeric(sum_contrasts[2, 2]), ", ", as.numeric(sum_contrasts[2, 3]), ")")
  testthat::expect_identical(result_matrix[7, 3], ci_a)
  testthat::expect_identical(result_matrix[7, 4], ci_b)
})
