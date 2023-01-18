# Preparation of the test case

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
    analyze_vars_in_cols(vars = "AVAL")

  result <- build_table(l, df = adpc)

  res <- expect_silent(result)
  expect_snapshot(res)

  l2 <- basic_table() %>%
    split_rows_by(
      var = "ARM",
      split_fun = keep_split_levels(c("A: Drug X", "C: Combination"))
    ) %>%
    split_rows_by(var = "PARAM") %>%
    analyze_vars_in_cols(
      vars = "AVALC", var_type = "character", .stats = c("n_blq"),
      .labels = c(n_blq = "n_blq")
    )

  adpc <- adpc %>% mutate(AVALC = as.factor(AVALC))
  result <- build_table(l2, df = adpc)

  res <- expect_silent(result)
  expect_snapshot(res)
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
    analyze_vars_in_cols(
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
  res <- expect_silent(result)
  expect_snapshot(res)
})
