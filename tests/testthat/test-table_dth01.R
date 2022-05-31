# Test variants of DTH01

library(scda)
library(dplyr)

adsl <- synthetic_cdisc_data("rcd_2022_02_28")$adsl

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
      "(N=134)", "22 (16.4%)", "", "22", "6 (27.3%)", "9 (40.9%)",
      "7 (31.8%)", "B: Placebo", "(N=134)", "26 (19.4%)", "", "26",
      "12 (46.2%)", "5 (19.2%)", "9 (34.6%)", "C: Combination", "(N=132)",
      "19 (14.4%)", "", "19", "7 (36.8%)", "4 (21.1%)", "8 (42.1%)",
      "All Patients", "(N=400)", "67 (16.8%)", "", "67", "25 (37.3%)",
      "18 (26.9%)", "24 (35.8%)"
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
      "n", "ADVERSE EVENT", "OTHER", "PROGRESSIVE DISEASE", "LOST TO FOLLOW UP",
      "MISSING", "SUICIDE", "UNKNOWN", "Days from last drug administration",
      "n", "<=30", ">30", "Primary cause by days from last study drug administration",
      "<=30", "n", "ADVERSE EVENT", "OTHER", "PROGRESSIVE DISEASE",
      ">30", "n", "ADVERSE EVENT", "OTHER", "PROGRESSIVE DISEASE",
      "A: Drug X", "(N=134)", "22 (16.4%)", "", "22", "6 (27.3%)",
      "9 (40.9%)", "7 (31.8%)", "2 (22.2%)", "4 (44.4%)", "2 (22.2%)",
      "1 (11.1%)", "", "22", "12 (54.5%)", "10 (45.5%)", "", "", "12",
      "3 (25%)", "6 (50%)", "3 (25%)", "", "10", "3 (30%)", "3 (30%)",
      "4 (40%)", "B: Placebo", "(N=134)", "26 (19.4%)", "", "26", "12 (46.2%)",
      "5 (19.2%)", "9 (34.6%)", "2 (40%)", "1 (20%)", "1 (20%)", "1 (20%)",
      "", "26", "16 (61.5%)", "10 (38.5%)", "", "", "16", "9 (56.2%)",
      "3 (18.8%)", "4 (25%)", "", "10", "3 (30%)", "2 (20%)", "5 (50%)",
      "C: Combination", "(N=132)", "19 (14.4%)", "", "19", "7 (36.8%)",
      "4 (21.1%)", "8 (42.1%)", "1 (25%)", "2 (50%)", "1 (25%)", "0",
      "", "19", "10 (52.6%)", "9 (47.4%)", "", "", "10", "4 (40%)",
      "3 (30%)", "3 (30%)", "", "9", "3 (33.3%)", "1 (11.1%)", "5 (55.6%)",
      "All Patients", "(N=400)", "67 (16.8%)", "", "67", "25 (37.3%)",
      "18 (26.9%)", "24 (35.8%)", "5 (27.8%)", "7 (38.9%)", "4 (22.2%)",
      "2 (11.1%)", "", "67", "38 (56.7%)", "29 (43.3%)", "", "", "38",
      "16 (42.1%)", "12 (31.6%)", "10 (26.3%)", "", "29", "9 (31%)",
      "6 (20.7%)", "14 (48.3%)"
    ),
    .Dim = c(27L, 5L)
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
      "All other causes", "A: Drug X", "(N=134)", "22 (16.4%)", "",
      "22", "6 (27.3%)", "7 (31.8%)", "9 (40.9%)", "2 (22.2%)", "7 (77.8%)",
      "B: Placebo", "(N=134)", "26 (19.4%)", "", "26", "12 (46.2%)",
      "9 (34.6%)", "5 (19.2%)", "1 (20.0%)", "4 (80.0%)", "C: Combination",
      "(N=132)", "19 (14.4%)", "", "19", "7 (36.8%)", "8 (42.1%)",
      "4 (21.1%)", "1 (25.0%)", "3 (75.0%)", "All Patients", "(N=400)",
      "67 (16.8%)", "", "67", "25 (37.3%)", "24 (35.8%)", "18 (26.9%)",
      "4 (22.2%)", "14 (77.8%)"
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
      "MISSING", "A: Drug X", "(N=134)", "22 (16.4%)", "", "22", "6 (27.3%)",
      "7 (31.8%)", "9 (40.9%)", "2 (22.2%)", "7 (77.8%)", "2 (22.2%)",
      "2 (22.2%)", "1 (11.1%)", "4 (44.4%)", "B: Placebo", "(N=134)",
      "26 (19.4%)", "", "26", "12 (46.2%)", "9 (34.6%)", "5 (19.2%)",
      "1 (20.0%)", "4 (80.0%)", "2 (40%)", "1 (20%)", "1 (20%)", "1 (20%)",
      "C: Combination", "(N=132)", "19 (14.4%)", "", "19", "7 (36.8%)",
      "8 (42.1%)", "4 (21.1%)", "1 (25.0%)", "3 (75.0%)", "1 (25%)", "1 (25%)",
      "0", "2 (50%)", "All Patients", "(N=400)", "67 (16.8%)", "",
      "67", "25 (37.3%)", "24 (35.8%)", "18 (26.9%)", "4 (22.2%)",
      "14 (77.8%)", "5 (27.8%)", "4 (22.2%)", "2 (11.1%)", "7 (38.9%)"
    ),
    .Dim = c(14L, 5L)
  )
  testthat::expect_identical(result, expected)
})
