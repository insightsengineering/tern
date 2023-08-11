testthat::test_that("s_count_patients_with_flags handles NA", {
  test_data <- data.frame(
    SUBJID = c("1001", "1001", "1001", "1002", "1002", "1002"),
    TRTEMFL = c(TRUE, FALSE, FALSE, NA, FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  result <- s_count_patients_with_flags(
    test_data,
    .var = "SUBJID",
    flag_variables = "TRTEMFL"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_count_patients_with_flags handles multiple columns", {
  test_data <- data.frame(
    SUBJID = c("1001", "1001", "1001", "1002", "1002", "1002", "1003", "1003", "1003"),
    TRTEMFL = c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE),
    AEOUTFL = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  result <- s_count_patients_with_flags(
    test_data,
    .var = "SUBJID",
    flag_variables = c("TRTEMFL", "AEOUTFL")
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_count_patients_with_flags custom variable label behaviour works", {
  adae_local <- tern_ex_adae %>%
    dplyr::mutate(
      SER = AESER == "Y",
      REL = AEREL == "Y",
      CTC35 = AETOXGR %in% c("3", "4", "5"),
      CTC45 = AETOXGR %in% c("4", "5")
    )
  aesi_vars <- c("SER", "REL", "CTC35", "CTC45")

  # No variable labels (variable names used)
  result <- s_count_patients_with_flags(
    adae_local,
    .var = "USUBJID",
    flag_variables = aesi_vars
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  labels <- c("Serious AE", "Related AE", "Grade 3-5 AE", "Grade 4/5 AE")
  for (i in seq_along(aesi_vars)) {
    attr(adae_local[[aesi_vars[i]]], "label") <- labels[i]
  }

  # Variable labels from df
  result <- s_count_patients_with_flags(
    adae_local,
    .var = "USUBJID",
    flag_variables = aesi_vars
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Custom labels via flag_labels argument
  result <- s_count_patients_with_flags(
    adae_local,
    .var = "USUBJID",
    flag_variables = aesi_vars,
    flag_labels = c("Category 1", "Category 2", "Category 3", "Category 4")
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Labels supplied within flag_variables argument
  result <- s_count_patients_with_flags(
    adae_local,
    .var = "USUBJID",
    flag_variables = formatters::var_labels(adae_local[, aesi_vars])
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("count_patients_with_flags works as expected", {
  test_data <- tibble::tibble(
    SUBJID = c("1001", "1001", "1001", "1002", "1002", "1002", "1003", "1003", "1003"),
    ARM = factor(c("A", "A", "A", "A", "A", "A", "B", "B", "B"), levels = c("A", "B")),
    TRTEMFL = c("Y", "", "", "NA", "", "", "Y", "", ""),
    AEOUT = c("", "", "", "", "", "", "FATAL", "", "FATAL")
  )
  test_data <- test_data %>%
    dplyr::mutate(
      flag1 = TRTEMFL == "Y",
      flag2 = TRTEMFL == "Y" & AEOUT == "FATAL",
    )
  labels <- c(
    "A",
    "B",
    "C",
    "D",
    "Total number of patients with at least one adverse event",
    "Total number of patients with fatal AEs"
  )
  formatters::var_labels(test_data) <- labels

  test_adsl_like <- tibble::tibble(
    SUBJID = as.character(1001:1010),
    ARM = factor(c("A", "A", "B", "B", "A", "A", "A", "B", "B", "A"), levels = c("A", "B")),
    stringsAsFactors = FALSE
  )

  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    count_patients_with_flags(
      "SUBJID",
      flag_variables = formatters::var_labels(test_data[, c("flag1", "flag2")]),
      denom = "N_col"
    )
  result <- build_table(lyt, df = test_data, alt_counts_df = test_adsl_like)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("count_patients_with_flags works as expected when specifying table_names", {
  test_data <- tibble::tibble(
    USUBJID = c("1001", "1001", "1001", "1002", "1002", "1002", "1003", "1003", "1003"),
    SUBJID = c("1001", "1001", "1001", "1002", "1002", "1002", "1003", "1003", "1003"),
    ARM = factor(c("A", "A", "A", "A", "A", "A", "B", "B", "B"), levels = c("A", "B")),
    TRTEMFL = c("Y", "", "", "NA", "", "", "Y", "", ""),
    AEOUT = c("", "", "", "", "", "", "FATAL", "", "FATAL")
  )
  test_data <- test_data %>%
    dplyr::mutate(
      flag1 = TRTEMFL == "Y",
      flag2 = TRTEMFL == "Y" & AEOUT == "FATAL",
    )
  columns <- c("flag1", "flag2")
  labels <- c(
    "Total number of patients with at least one adverse event",
    "Total number of patients with fatal AEs"
  )
  formatters::var_labels(test_data)[columns] <- labels

  test_adsl_like <- tibble::tibble(
    USUBJID = as.character(1001:1010),
    SUBJID = as.character(1001:1010),
    ARM = factor(c("A", "A", "B", "B", "A", "A", "A", "B", "B", "A"), levels = c("A", "B")),
    stringsAsFactors = FALSE
  )

  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    count_patients_with_flags(
      "SUBJID",
      flag_variables = formatters::var_labels(test_data[, c("flag1", "flag2")]),
      table_names = "SUBJID",
      denom = "N_col"
    ) %>%
    count_patients_with_flags(
      "USUBJID",
      flag_variables = formatters::var_labels(test_data[, c("flag1", "flag2")]),
      table_names = "USUBJID",
      denom = "N_col"
    )
  result <- build_table(lyt, df = test_data, alt_counts_df = test_adsl_like)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("count_patients_with_flags works with label row specified", {
  # Create custom flags:
  adae_local <- tern_ex_adae %>%
    dplyr::mutate(
      SER = AESER == "Y",
      REL = AEREL == "Y",
      CTC35 = AETOXGR %in% c("3", "4", "5"),
      CTC45 = AETOXGR %in% c("4", "5")
    )
  columns <- c("SER", "REL", "CTC35", "CTC45")
  labels <- c("Serious AE", "Related AE", "Grade 3-5 AE", "Grade 4/5 AE")
  for (i in seq_along(columns)) {
    attr(adae_local[[columns[i]]], "label") <- labels[i]
  }
  aesi_vars <- c("SER", "REL", "CTC35", "CTC45")

  # Create layout
  lyt <- basic_table() %>%
    split_cols_by("ACTARM") %>%
    add_colcounts() %>%
    count_patients_with_event(
      vars = "USUBJID",
      filters = c("STUDYID" = as.character(unique(adae_local$STUDYID))),
      denom = "N_col",
      .labels = c(count_fraction = "Total number of patients with at least one adverse event")
    ) %>%
    count_patients_with_flags(
      "USUBJID",
      flag_variables = formatters::var_labels(adae_local[, aesi_vars]),
      denom = "N_col",
      var_labels = "Total number of patients with at least one",
      show_labels = "visible"
    )
  result <- build_table(lyt, df = adae_local, alt_counts_df = tern_ex_adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("count_patients_with_flags custom variable label behaviour works with var_labels specified", {
  adae_local <- tern_ex_adae %>%
    dplyr::mutate(
      SER = AESER == "Y",
      REL = AEREL == "Y",
      CTC35 = AETOXGR %in% c("3", "4", "5"),
      CTC45 = AETOXGR %in% c("4", "5")
    )
  aesi_vars <- c("SER", "REL", "CTC35", "CTC45")

  # No variable labels (variable names used)
  lyt <- basic_table() %>%
    split_cols_by("ACTARM") %>%
    count_patients_with_flags(
      "USUBJID",
      flag_variables = aesi_vars,
      denom = "N_col",
      var_labels = "Total number of patients with at least one",
      show_labels = "visible"
    )
  result <- build_table(lyt, df = adae_local, alt_counts_df = tern_ex_adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  labels <- c("Serious AE", "Related AE", "Grade 3-5 AE", "Grade 4/5 AE")
  for (i in seq_along(aesi_vars)) {
    attr(adae_local[[aesi_vars[i]]], "label") <- labels[i]
  }

  # Variable labels from df
  lyt <- basic_table() %>%
    split_cols_by("ACTARM") %>%
    count_patients_with_flags(
      "USUBJID",
      flag_variables = aesi_vars,
      denom = "N_col",
      var_labels = "Total number of patients with at least one",
      show_labels = "visible"
    )
  result <- build_table(lyt, df = adae_local, alt_counts_df = tern_ex_adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Custom labels via flag_labels argument
  lyt <- basic_table() %>%
    split_cols_by("ACTARM") %>%
    count_patients_with_flags(
      "USUBJID",
      flag_variables = aesi_vars,
      flag_labels = c("Category 1", "Category 2", "Category 3", "Category 4"),
      denom = "N_col",
      var_labels = "Total number of patients with at least one",
      show_labels = "visible"
    )
  result <- build_table(lyt, df = adae_local, alt_counts_df = tern_ex_adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Labels supplied within flag_variables argument
  lyt <- basic_table() %>%
    split_cols_by("ACTARM") %>%
    count_patients_with_flags(
      "USUBJID",
      flag_variables = formatters::var_labels(adae_local[, aesi_vars]),
      denom = "N_col",
      var_labels = "Total number of patients with at least one",
      show_labels = "visible"
    )
  result <- build_table(lyt, df = adae_local, alt_counts_df = tern_ex_adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("count_patients_with_flags works as expected with risk difference column", {
  set.seed(1)
  adae <- tern_ex_adae %>%
    mutate(
      SER = sample(c(TRUE, FALSE), nrow(.), replace = TRUE),
      SERFATAL = sample(c(TRUE, FALSE), nrow(.), replace = TRUE)
    ) %>%
    var_relabel(
      SER = "SAE",
      SERFATAL = "SAE with fatal outcome"
    )

  # One statistic
  result <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM", split_fun = add_riskdiff("A: Drug X", "B: Placebo")) %>%
    count_patients_with_flags(
      var = "USUBJID",
      flag_variables = c("SER", "SERFATAL"),
      riskdiff = TRUE
    ) %>%
    build_table(adae, alt_counts_df = tern_ex_adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Multiple statistics
  result <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM", split_fun = add_riskdiff("A: Drug X", "B: Placebo")) %>%
    count_patients_with_flags(
      var = "USUBJID",
      flag_variables = c("SER", "SERFATAL"),
      .stats = c("count", "count_fraction"),
      riskdiff = TRUE
    ) %>%
    build_table(adae)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
