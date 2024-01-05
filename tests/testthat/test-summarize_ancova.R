testthat::test_that("h_ancova works with healthy input", {
  result <- h_ancova(
    .var = "Sepal.Length",
    .df_row = iris,
    variables = list(arm = "Species", covariates = c("Petal.Length * Petal.Width", "Sepal.Width"))
  ) %>%
    as.data.frame()

  res <- testthat::expect_silent(broom::tidy(result))
  testthat::expect_snapshot(res)
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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

testthat::test_that("s_ancova works with interaction and .in_ref_col = TRUE", {
  iris_new <- iris %>%
    dplyr::mutate(p_group = dplyr::case_when(
      substr(Petal.Width, 3, 3) < 3 ~ "A",
      substr(Petal.Width, 3, 3) < 5 & substr(Petal.Width, 3, 3) > 2 ~ "B",
      TRUE ~ "C"
    )) %>%
    mutate(p_group = as.factor(p_group))
  df_col <- iris_new %>% dplyr::filter(Species == "versicolor")
  df_ref <- iris_new %>% dplyr::filter(Species == "setosa")

  result <- s_ancova(
    df_col,
    .var = "Petal.Length",
    variables = list(arm = "Species", covariates = c("Sepal.Width", "p_group", "Species*p_group")),
    .in_ref_col = TRUE,
    .df_row = iris_new,
    .ref_group = df_ref,
    conf_level = 0.95,
    interaction_y = "B",
    interaction_item = "p_group"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("summarize_ancova works with interaction", {
  iris_new <- iris %>%
    dplyr::mutate(p_group = dplyr::case_when(
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
  result_matrix <- to_string_matrix(result, with_spaces = FALSE, print_txt_to_copy = FALSE)

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

testthat::test_that("summarize_ancova works with irregular arm levels", {
  adsl <- tern_ex_adsl
  adrs <- tern_ex_adrs
  adsl$ARMCD2 <- factor(adsl$ARMCD,
    levels = c("ARM A", "ARM B", "ARM C"),
    labels = c("ARM A", "ARM A Subgroup", "ARM C")
  )
  adrs$ARMCD2 <- factor(adrs$ARMCD,
    levels = c("ARM A", "ARM B", "ARM C"),
    labels = c("ARM A", "ARM A Subgroup", "ARM C")
  )
  adsl$ARMCD3 <- factor(adsl$ARMCD,
    levels = c("ARM A", "ARM B", "ARM C"),
    labels = c("ARM A", "ARM B (x)", "ARM C")
  )
  adrs$ARMCD3 <- factor(adrs$ARMCD,
    levels = c("ARM A", "ARM B", "ARM C"),
    labels = c("ARM A", "ARM B (x)", "ARM C")
  )

  set.seed(1)
  adrs_single <- adrs %>% mutate(CHG = rnorm(nrow(.)))

  result1 <- basic_table() %>%
    split_cols_by("ARMCD2", ref_group = "ARM C") %>%
    add_colcounts() %>%
    summarize_ancova(
      vars = "CHG",
      variables = list(arm = "ARMCD2", covariates = NULL),
      table_names = "unadj",
      conf_level = 0.95, var_labels = "Unadjusted comparison",
      .labels = c(lsmean = "Mean", lsmean_diff = "Difference in Means")
    ) %>%
    build_table(adrs_single, alt_counts_df = adsl)

  res <- testthat::expect_silent(result1)
  testthat::expect_snapshot(res)

  result2 <- basic_table() %>%
    split_cols_by("ARMCD3", ref_group = "ARM C") %>%
    add_colcounts() %>%
    summarize_ancova(
      vars = "CHG",
      variables = list(arm = "ARMCD3", covariates = NULL),
      table_names = "unadj",
      conf_level = 0.95, var_labels = "Unadjusted comparison",
      .labels = c(lsmean = "Mean", lsmean_diff = "Difference in Means")
    ) %>%
    build_table(adrs_single, alt_counts_df = adsl)

  res <- testthat::expect_silent(result2)
  testthat::expect_snapshot(res)
})
