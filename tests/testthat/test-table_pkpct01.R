# Preparation of the test case.
adpc <- adpc_raw

testthat::test_that("PKCT01 is produced correctly", {
  l <- basic_table() %>%
    split_rows_by(
      var = "ARM",
      split_fun = keep_split_levels(c("A: Drug X", "C: Combination"))
    ) %>%
    split_rows_by(var = "PARAM") %>%
    summarize_vars_in_cols(var = "AVAL", col_split = TRUE)

  result <- build_table(l, df = adpc)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "A: Drug X", "Plasma Drug X", "Plasma Drug Y", "Urine Drug X", "Urine Drug Y",
      "C: Combination", "Plasma Drug X", "Plasma Drug Y", "Urine Drug X", "Urine Drug Y",
      "n", "", "1474", "0", "804", "804", "", "1452", "1452", "792", "792",
      "Mean", "", "6.5", "NA", "0.9", "0.9", "", "6.5", "13.1", "0.8", "0.8",
      "SD", "", "6.7", "NA", "1.8", "1.8", "", "6.7", "13.5", "1.8", "1.8",
      "SE", "", "0.2", "NA", "0.1", "0.1", "", "0.2", "0.4", "0.1", "0.1",
      "CV (%)", "", "102.4", "NA", "210.7", "210.7", "", "103.3", "103.3", "212.4", "212.4",
      "CV % Geometric Mean", "", "NA", "NA", "NA", "NA", "", "NA", "NA", "NA", "NA"
    ),
    .Dim = c(11L, 7L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)

  l2 <- basic_table() %>%
    split_rows_by(
      var = "ARM",
      split_fun = keep_split_levels(c("A: Drug X", "C: Combination"))
    ) %>%
    split_rows_by(var = "PARAM") %>%
    summarize_vars_in_cols(
      var = "AVALC", var_type = "character", .stats = c("n_blq"),
      .labels = c(n_blq = "n_blq"), col_split = TRUE
    )

  adpc <- adpc %>% mutate(AVALC = as.factor(AVALC))
  result <- build_table(l2, df = adpc)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "A: Drug X", "Plasma Drug X", "Plasma Drug Y", "Urine Drug X", "Urine Drug Y",
      "C: Combination", "Plasma Drug X", "Plasma Drug Y", "Urine Drug X", "Urine Drug Y",
      "n_blq", "", "402", "0", "402", "402", "", "396", "396", "396", "396"
    ),
    .Dim = c(11L, 2L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("Specific PKCT01 features are present", {
  # Helper function
  threesigfmt <- function(x, ...) {
    as.character(signif(x, 3))
  }

  # Setting up the data
  adpc_1 <- adpc %>%
    mutate(
      NRELTM1 = as.factor(NRELTM1),
      AVALC = as.factor(AVALC)
    ) %>%
    filter(ACTARM %in% c("A: Drug X")) %>%
    mutate(ACTARM = factor(ACTARM, levels = c("A: Drug X"))) %>%
    select(NRELTM1, ACTARM, VISIT, AVAL, PARAM, AVALC)
  adpc_2 <- adpc_1 %>% mutate(AVAL = AVALC)

  # Row structure
  l_rows <- basic_table() %>%
    split_rows_by(
      var = "ACTARM",
      split_label = "Cohort/Treatment",
      label_pos = "topleft"
    ) %>%
    split_rows_by(
      var = "VISIT",
      split_label = "Visit",
      label_pos = "topleft"
    ) %>%
    split_rows_by(
      var = "NRELTM1",
      split_label = "Norminal Time from First Dose",
      label_pos = "topleft"
    )

  # Column results for numeric values
  l1 <- l_rows %>%
    summarize_vars_in_cols(
      var = "AVAL",
      col_split = TRUE,
      .stats = c(
        "n", "mean", "sd", "cv",
        "geom_mean", "geom_cv", # "geom_mean_ci",
        "median", "min", "max"
      ),
      .formats = c(
        n = "xx.",
        mean = threesigfmt,
        sd = threesigfmt,
        cv = "xx.x",
        median = threesigfmt,
        geom_mean = threesigfmt,
        geom_cv = "xx.x",
        min = threesigfmt,
        max = threesigfmt
      ),
      .labels = c(
        n = "n",
        mean = "Mean",
        sd = "SD",
        cv = "CV (%) Mean",
        geom_mean = "Geometric Mean",
        geom_cv = "CV % Geometric Mean",
        median = "Median",
        min = "Minimum",
        max = "Maximum"
      ),
      na_str = "NE"
    )
  result1 <- build_table(l1, df = adpc_1)

  # Column results for character counts
  l2 <- l_rows %>%
    summarize_vars_in_cols(
      var = "AVAL",
      var_type = "character",
      col_split = TRUE,
      .stats = c("n_blq"),
      .labels = c(
        n_blq = "Number\nof\n<LTR/BLQ>s"
      )
    )
  result2 <- build_table(l2, df = adpc_2)

  # Merging and trimming
  result <- cbind_rtables(result1[, 1], result2, result1[, 2:ncol(result1)]) %>%
    trim_rows()

  # Decorating
  main_title(result) <- "Summary of PK Concentrations by Nominal Time and Treatment: PK Evaluable\n Protocol: xxxxx"
  subtitles(result) <- paste("Analyte: ", unique(unique(adpc$PARAM)), "Treatment:", unique(unique(adpc$ACTARM)))
  main_footer(result) <- "NE: Not Estimable"
  paginate_table(result, verbose = TRUE, lpp = 20)

  table_structure(result)

  # Comparing
  string_res <- strsplit(toString(matrix_form(result, TRUE)), "\n")

  # Checking NAs are NEs
  testthat::expect_false(any(sapply(string_res, grepl, pattern = "NA")))
  testthat::expect_equal(sum(sapply(string_res, grepl, pattern = "NE")), 4L)

  # Checking significative digits (DISCLAIMER: this is an hack and is NOT well supported)
  mean_vals <- rtables::cell_values(result,
    rowpath = NULL,
    colpath = c("multivars", "AVAL._[[2]]_.")
  ) %>%
    unlist()
  names(mean_vals) <- NULL
  tmp_result <- sapply(seq_len(nrow(result)), function(x) matrix_form(result[x, 3])$strings[2, 2])
  tmp_result <- tmp_result[tmp_result != ""]
  testthat::expect_equal(tmp_result, as.character(signif(mean_vals, 3)))
})
