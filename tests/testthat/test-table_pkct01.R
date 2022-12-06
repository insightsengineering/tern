# Preparation of the test case.
adpc <- adpc_raw

testthat::test_that("PKCT01 is produced correctly", {
  l <- basic_table() %>%
    split_rows_by(
      var = "ARM",
      split_fun = keep_split_levels(c(
        "A: Drug X",
        "C: Combination"
      ))
    ) %>%
    split_rows_by(var = "PARAM") %>%
    summarize_vars_in_cols(vars = "AVAL")

  result <- build_table(l, df = adpc)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "A: Drug X", "Plasma Drug X", " ", "Plasma Drug Y", " ",
      "Urine Drug X", " ", "Urine Drug Y", " ", "C: Combination",
      "Plasma Drug X", " ", "Plasma Drug Y", " ", "Urine Drug X", " ",
      "Urine Drug Y", " ", "n", "", "", "1474", "", "0", "", "804", "",
      "804", "", "", "1452", "", "1452", "", "792", "", "792", "Mean", "",
      "", "6.5", "", "NA", "", "0.9", "", "0.9", "", "", "6.5", "", "13.1",
      "", "0.8", "", "0.8", "SD", "", "", "6.7", "", "NA", "", "1.8", "",
      "1.8", "", "", "6.7", "", "13.5", "", "1.8", "", "1.8", "SE", "", "",
      "0.2", "", "NA", "", "0.1", "", "0.1", "", "", "0.2", "", "0.4", "",
      "0.1", "", "0.1", "CV (%)", "", "", "102.4", "", "NA", "", "210.7",
      "", "210.7", "", "", "103.3", "", "103.3", "", "212.4", "", "212.4",
      "CV % Geometric Mean", "", "", "NA", "", "NA", "", "NA", "", "NA", "",
      "", "NA", "", "NA", "", "NA", "", "NA"
    ),
    .Dim = c(19L, 7L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)

  l2 <- basic_table() %>%
    split_rows_by(
      var = "ARM",
      split_fun = keep_split_levels(c("A: Drug X", "C: Combination"))
    ) %>%
    split_rows_by(var = "PARAM") %>%
    summarize_vars_in_cols(
      vars = "AVALC", var_type = "character", .stats = c("n_blq"),
      .labels = c(n_blq = "n_blq")
    )

  adpc <- adpc %>% mutate(AVALC = as.factor(AVALC))
  result <- build_table(l2, df = adpc)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "A: Drug X", "Plasma Drug X", " ", "Plasma Drug Y", " ",
      "Urine Drug X", " ", "Urine Drug Y", " ", "C: Combination",
      "Plasma Drug X", " ", "Plasma Drug Y", " ", "Urine Drug X",
      " ", "Urine Drug Y", " ", "n_blq", "", "", "402", "", "0", "",
      "402", "", "402", "", "", "396", "", "396", "", "396", "", "396"
    ),
    .Dim = c(19L, 2L)
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
  lyt <- l_rows %>%
    summarize_vars_in_cols(
      vars = c("AVAL", "AVALC", rep("AVAL", 8)),
      .stats = c(
        "n", "n_blq", "mean", "sd", "cv",
        "geom_mean", "geom_cv", # "geom_mean_ci",
        "median", "min", "max"
      ),
      .formats = c(
        n = "xx.",
        n_blq = "xx.",
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
        n_blq = "Number\nof\n<LTR/BLQ>s",
        mean = "Mean",
        sd = "SD",
        cv = "CV (%) Mean",
        geom_mean = "Geometric Mean",
        geom_cv = "CV % Geometric Mean",
        median = "Median",
        min = "Minimum",
        max = "Maximum"
      ),
      na_level = "NE"
    )
  result <- build_table(lyt, df = adpc_1) %>% prune_table()

  # Decorating
  main_title(result) <- "Summary of PK Concentrations by Nominal Time and Treatment: PK Evaluable\n Protocol: xxxxx"
  subtitles(result) <- paste("Analyte: ", unique(unique(adpc$PARAM)), "Treatment:", unique(unique(adpc$ACTARM)))
  main_footer(result) <- "NE: Not Estimable"

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

  # Pagination works roughly
  pag_works <- paginate_table(result, verbose = FALSE, lpp = 20)
  testthat::expect_equal(length(pag_works), 11L)

  # Values are correct
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "Cohort/Treatment", "  Visit", "    Norminal Time from First Dose", "",
      "", "A: Drug X", "Day 1", "0", " ", "0.5", " ", "1", " ", "1.5", " ",
      "2", " ", "3", " ", "4", " ", "8", " ", "12", " ", "24", " ", "Day 2",
      "48", " ", "", "", "", "", "n", "", "", "", "402", "", "134", "", "134",
      "", "134", "", "134", "", "134", "", "402", "", "402", "", "402", "",
      "402", "", "", "402", "", "", "Number", "of", "<LTR/BLQ>s", "", "", "",
      "402", "", "0", "", "0", "", "0", "", "0", "", "0", "", "0", "", "0", "",
      "0", "", "402", "", "", "402", "", "", "", "", "Mean", "", "", "", "0",
      "", "12.6", "", "16.2", "", "15.6", "", "13.4", "", "8.47", "", "4.79",
      "", "0.348", "", "0.0224", "", "0", "", "", "0", "", "", "", "", "SD",
      "", "", "", "0", "", "1.51", "", "1.63", "", "1.46", "", "1.35", "",
      "1.25", "", "1.01", "", "0.179", "", "0.0189", "", "0", "", "", "0",
      "", "", "", "", "CV (%) Mean", "", "", "", "NE", "", "12.0", "",
      "10.0", "", "9.3", "", "10.1", "", "14.7", "", "21.2", "", "51.6",
      "", "84.4", "", "NE", "", "", "NE", "", "", "", "", "Geometric Mean",
      "", "", "", "NE", "", "12.5", "", "16.1", "", "15.6", "", "13.4", "",
      "8.38", "", "4.69", "", "0.303", "", "0.0156", "", "NE", "", "", "NE",
      "", "", "", "", "CV % Geometric Mean", "", "", "", "NE", "", "12.2", "",
      "10.1", "", "9.3", "", "10.0", "", "15.0", "", "21.9", "", "58.2", "",
      "111.2", "", "NE", "", "", "NE", "", "", "", "", "Median", "", "", "",
      "0", "", "12.6", "", "16.2", "", "15.5", "", "13.3", "", "8.4", "",
      "4.79", "", "0.318", "", "0.017", "", "0", "", "", "0", "", "", "",
      "", "Minimum", "", "", "", "0", "", "9.72", "", "12.6", "", "12.3",
      "", "10.8", "", "5.88", "", "2.7", "", "0.076", "", "0.002", "", "0",
      "", "", "0", "", "", "", "", "Maximum", "", "", "", "0", "", "15.6",
      "", "19.9", "", "19", "", "16.5", "", "10.9", "", "7.09", "", "0.866",
      "", "0.083", "", "0", "", "", "0"
    ),
    .Dim = c(30L, 11L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})
