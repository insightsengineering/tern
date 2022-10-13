# Test variants of DTH01

adsl <- adsl_raw

testthat::test_that("DTH01 variant 1 is produced correctly", {
  adsl <- adsl %>%
    df_explicit_na()

  tbl1 <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    count_values(
      "DTHFL",
      values = "Y",
      .labels =  c(count_fraction = "Total number of deaths"),
      .formats = c(count_fraction = "xx (xx.x%)")
    ) %>%
    summarize_vars(vars = c("DTHCAT"), var_labels = c("Primary cause of death")) %>%
    build_table(df = adsl)

  result <- to_string_matrix(tbl1)
  expected <- structure(
    c(
      "", "", "Total number of deaths", "Primary cause of death",
      "n", "ADVERSE EVENT", "OTHER", "PROGRESSIVE DISEASE", "A: Drug X",
      "(N=134)", "25 (18.7%)", "", "25", "9 (36%)", "8 (32%)",
      "8 (32%)", "B: Placebo", "(N=134)", "23 (17.2%)", "", "23",
      "7 (30.4%)", "10 (43.5%)", "6 (26.1%)", "C: Combination", "(N=132)",
      "22 (16.7%)", "", "22", "10 (45.5%)", "6 (27.3%)", "6 (27.3%)",
      "All Patients", "(N=400)", "70 (17.5%)", "", "70", "26 (37.1%)",
      "24 (34.3%)", "20 (28.6%)"
    ),
    .Dim = c(8L, 5L)
  )
  testthat::expect_identical(result, expected)
})

testthat::test_that("DTH01 variant 2 is produced correctly", {
  adsl <- adsl %>%
    df_explicit_na()

  part1 <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    count_values(
      "DTHFL",
      values = "Y",
      .labels =  c(count_fraction = "Total number of deaths"),
      .formats = c(count_fraction = "xx (xx.x%)")
    ) %>%
    summarize_vars(vars = c("DTHCAT"), var_labels = c("Primary cause of death")) %>%
    build_table(df = adsl)

  part2 <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    split_rows_by("DTHCAT", split_fun = keep_split_levels("OTHER"), child_labels = "hidden") %>%
    summarize_vars(
      "DTHCAUS",
      nested = TRUE,
      .stats = "count_fraction",
      .indent_mods = c("count_fraction" = 4L)
    ) %>%
    build_table(df = adsl) %>%
    prune_table()
  part3 <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    summarize_vars(
      vars = "LDDTHGR1",
      var_labels = "Days from last drug administration",
      show_labels = "visible"
    ) %>%
    build_table(df = adsl)
  part4 <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    split_rows_by(
      "LDDTHGR1",
      split_fun = remove_split_levels("<Missing>"),
      split_label = "Primary cause by days from last study drug administration",
      label_pos = "visible"
    ) %>%
    summarize_vars("DTHCAT") %>%
    build_table(df = adsl)
  col_info(part2) <- col_info(part1)
  col_info(part3) <- col_info(part2)
  col_info(part4) <- col_info(part3)
  tbl2 <- rbind(part1, part2)
  tbl2 <- rbind(tbl2, part3)
  tbl2 <- rbind(tbl2, part4)

  result <- to_string_matrix(tbl2)
  expected <- structure(
    c(
      "", "", "Total number of deaths", "Primary cause of death",
      "n", "ADVERSE EVENT", "OTHER", "PROGRESSIVE DISEASE", "LOST TO FOLLOW UP", "MISSING",
      "Post-study reporting of death", "SUICIDE", "UNKNOWN", "Days from last drug administration",
      "n", "<=30", ">30", "Primary cause by days from last study drug administration",
      "<=30", "n", "ADVERSE EVENT", "OTHER", "PROGRESSIVE DISEASE",
      ">30", "n", "ADVERSE EVENT", "OTHER", "PROGRESSIVE DISEASE",
      "A: Drug X", "(N=134)", "25 (18.7%)", "", "25", "9 (36%)",
      "8 (32%)", "8 (32%)", "2 (25%)", "2 (25%)", "1 (12.5%)",
      "2 (25%)", "1 (12.5%)", "", "25", "14 (56%)", "11 (44%)", "", "", "14",
      "4 (28.6%)", "4 (28.6%)", "6 (42.9%)", "", "11", "5 (45.5%)", "4 (36.4%)",
      "2 (18.2%)", "B: Placebo", "(N=134)", "23 (17.2%)", "", "23", "7 (30.4%)",
      "10 (43.5%)", "6 (26.1%)", "2 (20%)", "3 (30%)", "2 (20%)", "2 (20%)", "1 (10%)",
      "", "23", "11 (47.8%)", "12 (52.2%)", "", "", "11", "2 (18.2%)",
      "6 (54.5%)", "3 (27.3%)", "", "12", "5 (41.7%)", "4 (33.3%)", "3 (25%)",
      "C: Combination", "(N=132)", "22 (16.7%)", "", "22", "10 (45.5%)",
      "6 (27.3%)", "6 (27.3%)", "2 (33.3%)", "2 (33.3%)", "1 (16.7%)", "1 (16.7%)", "0",
      "", "22", "14 (63.6%)", "8 (36.4%)", "", "", "14", "6 (42.9%)", "4 (28.6%)", "4 (28.6%)", "", "8",
      "4 (50%)", "2 (25%)", "2 (25%)", "All Patients", "(N=400)", "70 (17.5%)", "", "70", "26 (37.1%)",
      "24 (34.3%)", "20 (28.6%)", "6 (25%)", "7 (29.2%)", "4 (16.7%)", "5 (20.8%)",
      "2 (8.3%)", "", "70", "39 (55.7%)", "31 (44.3%)", "", "", "39",
      "12 (30.8%)", "14 (35.9%)", "13 (33.3%)", "", "31", "14 (45.2%)", "10 (32.3%)", "7 (22.6%)"
    ),
    .Dim = c(28L, 5L)
  )
  testthat::expect_identical(result, expected)
})

testthat::test_that("DTH01 variant 3 is produced correctly", {
  adsl <- adsl %>%
    df_explicit_na()

  # Reorder the levels in "DTHCAT" to put Other category at the end.
  adsl$DTHCAT <- factor(adsl$DTHCAT, levels = c("ADVERSE EVENT", "PROGRESSIVE DISEASE", "OTHER", "<Missing>")) # nolint

  dthcaus_levels <- levels(adsl[adsl$DTHCAT == "OTHER", ]$DTHCAUS)

  lyt1 <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    count_values(
      "DTHFL",
      values = "Y",
      .labels =  c(count_fraction = "Total number of deaths"),
      .formats = c(count_fraction = "xx (xx.x%)")
    ) %>%
    summarize_vars(
      vars = c("DTHCAT"),
      var_labels = c("Primary cause of death"),
      table_names = "primary_cause"
    )
  part1 <- build_table(lyt1, df = adsl)

  lyt2 <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    split_rows_by(
      "DTHCAT",
      split_fun = keep_split_levels("OTHER"),
      child_labels = "hidden"
    ) %>%
    count_values(
      "DTHCAUS",
      values = dthcaus_levels[5],
      .labels = c(count_fraction = "Post study reporting of deaths"),
      .formats = c(count_fraction = "xx (xx.x%)"),
      .indent_mods = c(count_fraction = 2L),
      table_names = "post_study_deaths"
    ) %>%
    count_values(
      "DTHCAUS",
      values = dthcaus_levels[-5],
      .labels = c(count_fraction = "All other causes"),
      .formats = c(count_fraction = "xx (xx.x%)"),
      .indent_mods = c(count_fraction = 2L),
      table_names = "all_other_causes"
    )
  part2 <- build_table(lyt2, df = adsl)

  # now combine tables
  col_info(part2) <- col_info(part1)
  tbl3 <- rbind(part1, part2)

  result <- to_string_matrix(tbl3)
  expected <- structure(
    c(
      "", "", "Total number of deaths", "Primary cause of death",
      "n", "ADVERSE EVENT", "PROGRESSIVE DISEASE", "OTHER", "Post study reporting of deaths",
      "All other causes", "A: Drug X", "(N=134)", "25 (18.7%)", "",
      "25", "9 (36%)", "8 (32%)", "8 (32%)", "1 (12.5%)", "7 (87.5%)",
      "B: Placebo", "(N=134)", "23 (17.2%)", "", "23", "7 (30.4%)",
      "6 (26.1%)", "10 (43.5%)", "2 (20.0%)", "8 (80.0%)", "C: Combination",
      "(N=132)", "22 (16.7%)", "", "22", "10 (45.5%)", "6 (27.3%)",
      "6 (27.3%)", "1 (16.7%)", "5 (83.3%)", "All Patients", "(N=400)",
      "70 (17.5%)", "", "70", "26 (37.1%)", "20 (28.6%)", "24 (34.3%)",
      "4 (16.7%)", "20 (83.3%)"
    ),
    .Dim = c(10L, 5L)
  )
  testthat::expect_identical(result, expected)
})

testthat::test_that("DTH01 variant 4 is produced correctly", {
  adsl <- adsl %>%
    df_explicit_na()

  # Reorder the levels in "DTHCAT" to put Other category at the end.
  adsl$DTHCAT <- factor(adsl$DTHCAT, levels = c("ADVERSE EVENT", "PROGRESSIVE DISEASE", "OTHER", "<Missing>")) # nolint

  dthcaus_levels <- levels(adsl[adsl$DTHCAT == "OTHER", ]$DTHCAUS)

  # create a helper variable DTHCAUS_other for part3
  adsl <- adsl %>%
    dplyr::mutate(
      DTHCAUS_other = ifelse(
        DTHCAT == "OTHER" & DTHCAUS != "Post-study reporting of death", as.character(DTHCAUS), NA
      )
    )
  adsl$DTHCAUS_other <- factor( # nolint
    adsl$DTHCAUS_other,
    levels = c("LOST TO FOLLOW UP", "SUICIDE", "UNKNOWN", "MISSING")
  )
  adsl$DTHCAUS_other <- explicit_na(adsl$DTHCAUS_other) # nolint

  lyt1 <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    count_values(
      "DTHFL",
      values = "Y",
      .labels =  c(count_fraction = "Total number of deaths"),
      .formats = c(count_fraction = "xx (xx.x%)")
    ) %>%
    summarize_vars(vars = c("DTHCAT"), var_labels = c("Primary cause of death"))
  part1 <- build_table(lyt1, df = adsl)

  lyt2 <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    split_rows_by("DTHCAT", split_fun = keep_split_levels("OTHER"), child_labels = "hidden") %>%
    count_values(
      "DTHCAUS",
      values = dthcaus_levels[5],
      .labels = c(count_fraction = "Post study reporting of deaths"),
      .formats = c(count_fraction = "xx (xx.x%)"),
      .indent_mods = c(count_fraction = 2L),
      table_names = "post_study_deaths"
    ) %>%
    count_values(
      "DTHCAUS",
      values = dthcaus_levels[-5],
      .labels = c(count_fraction = "All other causes"),
      .formats = c(count_fraction = "xx (xx.x%)"),
      .indent_mods = c(count_fraction = 2L),
      table_names = "all_other_causes"
    )
  part2 <- build_table(lyt2, df = adsl)

  lyt3 <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    split_rows_by("DTHCAT", split_fun = keep_split_levels("OTHER"), child_labels = "hidden") %>%
    summarize_vars(
      "DTHCAUS_other",
      nested = TRUE,
      .stats = "count_fraction",
      .indent_mods = c("count_fraction" = 3L)
    )
  part3 <- build_table(lyt3, df = adsl)

  # now combine the tables
  col_info(part2) <- col_info(part1)
  col_info(part3) <- col_info(part2)

  tbl4 <- rbind(part1, part2, part3)
  tbl4

  result <- to_string_matrix(tbl4)
  expected <- structure(
    c(
      "", "", "Total number of deaths", "Primary cause of death",
      "n", "ADVERSE EVENT", "PROGRESSIVE DISEASE", "OTHER", "Post study reporting of deaths",
      "All other causes", "LOST TO FOLLOW UP", "SUICIDE", "UNKNOWN",
      "MISSING", "A: Drug X", "(N=134)", "25 (18.7%)", "", "25", "9 (36%)",
      "8 (32%)", "8 (32%)", "1 (12.5%)", "7 (87.5%)", "2 (28.6%)",
      "2 (28.6%)", "1 (14.3%)", "2 (28.6%)", "B: Placebo", "(N=134)",
      "23 (17.2%)", "", "23", "7 (30.4%)", "6 (26.1%)", "10 (43.5%)",
      "2 (20.0%)", "8 (80.0%)", "2 (25%)", "2 (25%)", "1 (12.5%)", "3 (37.5%)",
      "C: Combination", "(N=132)", "22 (16.7%)", "", "22", "10 (45.5%)",
      "6 (27.3%)", "6 (27.3%)", "1 (16.7%)", "5 (83.3%)", "2 (40%)", "1 (20%)",
      "0", "2 (40%)", "All Patients", "(N=400)", "70 (17.5%)", "",
      "70", "26 (37.1%)", "20 (28.6%)", "24 (34.3%)", "4 (16.7%)",
      "20 (83.3%)", "6 (30%)", "5 (25%)", "2 (10%)", "7 (35%)"
    ),
    .Dim = c(14L, 5L)
  )
  testthat::expect_identical(result, expected)
})
