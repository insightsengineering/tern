# Test variants of DTH01

adsl <- adsl_raw

testthat::test_that("DTH01 variant 1 is produced correctly", {
  adsl <- adsl %>%
    df_explicit_na()

  result <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    count_values(
      "DTHFL",
      values = "Y",
      .labels = c(count_fraction = "Total number of deaths"),
      .formats = c(count_fraction = "xx (xx.x%)")
    ) %>%
    summarize_vars(vars = c("DTHCAT"), var_labels = c("Primary cause of death")) %>%
    build_table(df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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
      .labels = c(count_fraction = "Total number of deaths"),
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
  result <- rbind(tbl2, part4)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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
      .labels = c(count_fraction = "Total number of deaths"),
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
  result <- rbind(part1, part2)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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
      .labels = c(count_fraction = "Total number of deaths"),
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
  result <- rbind(part1, part2, part3)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
