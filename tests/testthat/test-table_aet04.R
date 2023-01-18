# Data pre-processing
adsl <- adsl_raw
adae <- adae_raw %>%
  dplyr::mutate(
    AEDECOD = as.character(AEDECOD),
    AEBODSYS = as.character(AEBODSYS),
  )
gr_grp <- list(
  "- Any Grade -" = c("1", "2", "3", "4", "5"),
  "Grade 1-2" = c("1", "2"),
  "Grade 3-4" = c("3", "4"),
  "Grade 5" = "5"
)

# Raw result for future pruning
raw_result <- basic_table() %>%
  split_cols_by("ACTARM") %>%
  add_colcounts() %>%
  count_occurrences_by_grade(
    var = "AETOXGR",
    grade_groups = gr_grp
  ) %>%
  split_rows_by("AEBODSYS",
                split_fun = trim_levels_in_group("AETOXGR"),
                child_labels = "visible", nested = TRUE
  ) %>%
  summarize_occurrences_by_grade(
    var = "AETOXGR",
    grade_groups = gr_grp
  ) %>%
  split_rows_by("AEDECOD",
                split_fun = trim_levels_in_group("AETOXGR"),
                child_labels = "visible", nested = TRUE
  ) %>%
  summarize_num_patients(
    var = "USUBJID",
    .stats = "unique",
    .labels = "- Any Grade -"
  ) %>%
  count_occurrences_by_grade(
    var = "AETOXGR",
    grade_groups = gr_grp[-1],
    .indent_mods = -1L
  ) %>%
  build_table(adae, alt_counts_df = adsl) %>%
  sort_at_path(
    path = "AEBODSYS",
    scorefun = cont_n_allcols,
    decreasing = TRUE
  ) %>%
  sort_at_path(
    path = c("AEBODSYS", "*", "AEDECOD"),
    scorefun = cont_n_allcols,
    decreasing = TRUE
  )

testthat::test_that("AET04 variant 1 is produced correctly", {
  lyt <- basic_table() %>%
    split_cols_by("ACTARM") %>%
    add_colcounts() %>%
    count_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp
    ) %>%
    split_rows_by("AEBODSYS",
      split_fun = trim_levels_in_group("AETOXGR"),
      child_labels = "visible", nested = TRUE
    ) %>%
    summarize_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp
    ) %>%
    split_rows_by("AEDECOD",
      split_fun = trim_levels_in_group("AETOXGR"),
      child_labels = "visible", nested = TRUE
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = "- Any Grade -"
    ) %>%
    count_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp[-1],
      .indent_mods = -1L
    )

  result <- lyt %>%
    build_table(adae, alt_counts_df = adsl) %>%
    sort_at_path(
      path = "AEBODSYS",
      scorefun = cont_n_allcols,
      decreasing = TRUE
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEDECOD"),
      scorefun = cont_n_allcols,
      decreasing = TRUE
    )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Pagination also works (and sorting)
  testthat::expect_silent(
    pag_result <- paginate_table(result, lpp = 15)
  )

  testthat::expect_identical(
    to_string_matrix(pag_result[[3]])[3, 1],
    "cl B.2"
  )
  testthat::expect_identical(
    to_string_matrix(pag_result[[1]])[3:4, 1],
    c("- Any Grade -", "Grade 1-2")
  )
})

testthat::test_that("AET04 variant 2 is produced correctly (Fill in of Treatment Groups)", {
  adae <- adae %>%
    dplyr::filter(ACTARM == "A: Drug X")

  lyt <- basic_table() %>%
    split_cols_by("ACTARM") %>%
    add_colcounts() %>%
    count_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp
    ) %>%
    split_rows_by("AEBODSYS",
      split_fun = trim_levels_in_group("AETOXGR"),
      child_labels = "visible", nested = TRUE
    ) %>%
    summarize_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp
    ) %>%
    split_rows_by("AEDECOD",
      split_fun = trim_levels_in_group("AETOXGR"),
      child_labels = "visible", nested = TRUE
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = "- Any Grade -"
    ) %>%
    count_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp[-1],
      .indent_mods = -1L
    )

  result <- lyt %>%
    build_table(adae, alt_counts_df = adsl) %>%
    sort_at_path(
      path = "AEBODSYS",
      scorefun = cont_n_allcols,
      decreasing = TRUE
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEDECOD"),
      scorefun = cont_n_allcols,
      decreasing = TRUE
    )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Pagination also works (and sorting)
  testthat::expect_silent(
    pag_result <- paginate_table(result, lpp = 15)
  )

  testthat::expect_identical(
    to_string_matrix(pag_result[[3]])[3, 1],
    "cl B.2"
  )
  testthat::expect_identical(
    to_string_matrix(pag_result[[1]])[3:4, 2],
    c("122 (91.0%)", "13 (9.7%)")
  )
})

testthat::test_that("AET04 variant 3 is produced correctly (Fill in of Grades)", {
  lyt <- basic_table() %>%
    split_cols_by("ACTARM") %>%
    add_colcounts() %>%
    count_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp
    ) %>%
    split_rows_by("AEBODSYS", child_labels = "visible", nested = TRUE) %>%
    summarize_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp
    ) %>%
    split_rows_by("AEDECOD", child_labels = "visible", nested = TRUE) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = "- Any Grade -"
    ) %>%
    count_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp[-1],
      .indent_mods = -1L
    )

  result <- lyt %>%
    build_table(adae, alt_counts_df = adsl) %>%
    sort_at_path(
      path = "AEBODSYS",
      scorefun = cont_n_allcols,
      decreasing = TRUE
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEDECOD"),
      scorefun = cont_n_allcols,
      decreasing = TRUE
    ) %>%
    trim_rows()

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Pagination also works (and sorting)
  testthat::expect_silent(
    pag_result <- paginate_table(result, lpp = 15)
  )

  testthat::expect_identical(
    to_string_matrix(pag_result[[3]])[3, 1],
    "cl B.2"
  )
  testthat::expect_identical(
    to_string_matrix(pag_result[[1]])[3:4, 2],
    c("122 (91.0%)", "13 (9.7%)")
  )
})

testthat::test_that("AET04 variant 4 is produced correctly (Collapsing of Grades: grades 1&2, grades 3&4&5)", {
  gr_grp_tmp <- list(
    "- Any Grade -" = c("1", "2", "3", "4", "5"),
    "Grade 1-2" = c("1", "2"),
    "Grade 3-5" = c("3", "4", "5")
  )

  lyt <- basic_table() %>%
    split_cols_by("ACTARM") %>%
    add_colcounts() %>%
    count_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp_tmp
    ) %>%
    split_rows_by("AEBODSYS",
      split_fun = trim_levels_in_group("AETOXGR"),
      child_labels = "visible", nested = TRUE
    ) %>%
    summarize_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp_tmp
    ) %>%
    split_rows_by("AEDECOD",
      split_fun = trim_levels_in_group("AETOXGR"),
      child_labels = "visible", nested = TRUE
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = "- Any Grade -"
    ) %>%
    count_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp_tmp[-1],
      .indent_mods = -1L
    )

  result <- lyt %>%
    build_table(adae, alt_counts_df = adsl) %>%
    sort_at_path(
      path = "AEBODSYS",
      scorefun = cont_n_allcols,
      decreasing = TRUE
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEDECOD"),
      scorefun = cont_n_allcols,
      decreasing = TRUE
    )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Pagination works
  testthat::expect_silent(
    pag_result <- paginate_table(result, lpp = 15)
  )
})

# No test done for variant 5 (Using Worst Grade Flags from ADAE) since it's similar to
# variant 1 (just pre-processing the data).

testthat::test_that("AET04 variant 6 is produced correctly (with an
                    Incidence Rate of at Least 5%, totals restricted)", {
  # Simple wrapper to return subset ADAE to a threshold of xx%.
  get_adae_trimmed <- function(adsl, adae, cutoff_rate) {
    n_per_arm <- adsl %>%
      dplyr::count(ACTARM)

    anl_terms <- adae %>%
      dplyr::group_by(ACTARM, AEBODSYS, AEDECOD) %>%
      dplyr::summarise(
        unique_terms = dplyr::n_distinct(USUBJID)
      ) %>%
      dplyr::ungroup()

    anl_terms <- dplyr::left_join(
      anl_terms,
      n_per_arm,
      by = "ACTARM"
    ) %>%
      dplyr::mutate(
        ae_rate = unique_terms / n
      ) %>%
      dplyr::filter(ae_rate >= cutoff_rate) %>%
      dplyr::select(AEDECOD) %>%
      unique()

    anl <- dplyr::left_join(
      anl_terms,
      adae,
      by = "AEDECOD"
    )
    anl
  }

  adae <- get_adae_trimmed(adsl, adae, cutoff_rate = 0.4) %>%
    dplyr::mutate(AETOXGR = droplevels(AETOXGR))

  lyt <- basic_table() %>%
    split_cols_by("ACTARM") %>%
    add_colcounts() %>%
    count_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp
    ) %>%
    split_rows_by("AEBODSYS",
      split_fun = trim_levels_in_group("AETOXGR"),
      child_labels = "visible", nested = TRUE
    ) %>%
    summarize_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp
    ) %>%
    split_rows_by("AEDECOD",
      split_fun = trim_levels_in_group("AETOXGR"),
      child_labels = "visible", nested = TRUE
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = "- Any Grade -"
    ) %>%
    count_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp[-1],
      .indent_mods = -1L
    )

  result <- lyt %>%
    build_table(adae, alt_counts_df = adsl) %>%
    sort_at_path(
      path = "AEBODSYS",
      scorefun = cont_n_allcols,
      decreasing = TRUE
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEDECOD"),
      scorefun = cont_n_allcols,
      decreasing = TRUE
    )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Pagination works
  testthat::expect_silent(
    pag_result <- paginate_table(result, lpp = 15)
  )
})

# No test done for variant 7, Adverse Events by Highest NCI CTCAE Grade
# (with an Incidence Rate of at Least X Patients, totals unrestriced).
# With this variant, the SOC level is not trimmed (even if there are no terms left).

# This function is needed to check only the inner loops as the first piece should
# not be checked and filtered out by prune_table.
my_row_condition <- function(row_fnc_condition) {
  function(table_row) {
    if (indent_mod(table_row) == 0) {
      return(TRUE)
    } else {
      row_fnc_condition(table_row)
    }
  }
}

# NOTE: STREAM logic will only trim at term level
testthat::test_that("AET04 variant 8 is produced correctly (with an Incidence Rate of at Least X Patients)", {
  cutoff <- 58L
  row_condition <- has_count_in_any_col(atleast = cutoff, col_names = levels(adsl$ACTARM))

  result <- prune_table(raw_result, keep_rows(my_row_condition(row_condition)))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Pagination works
  testthat::expect_silent(
    pag_result <- paginate_table(result, lpp = 15)
  )
})

# NOTE: STREAM logic will only stream at term level
testthat::test_that("AET04 variant 9 is produced correctlyb(with a Difference in Incidence Rate of at Least X%)", {
  cutoff <- 0.1
  row_condition <- has_fractions_difference(atleast = cutoff, col_names = names(raw_result))

  result <- prune_table(raw_result, keep_rows(my_row_condition(row_condition)))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Pagination works
  testthat::expect_silent(
    pag_result <- paginate_table(result, lpp = 15)
  )
})

# No test done for variant 10, Adverse Events by Highest NCI CTCAE Grade
# (with an Incidence Rate of at Least X%, SOCs below X% removed).
# With this variant, SOC levels above the threshold are still in the table even if
# there are no terms left.

testthat::test_that(
  "AET04 variant 11 is produced correctly
  (with an Incidence Rate of at Least X%, all SOCs w/o preferred terms removed)",
  {
    cutoff <- 0.4
    row_condition <- has_fraction_in_any_col(atleast = cutoff, col_names = levels(adsl$ACTARM))

    result <- prune_table(raw_result, keep_rows(my_row_condition(row_condition)))

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)

    # Pagination works
    testthat::expect_silent(
      pag_result <- paginate_table(result, lpp = 15)
    )
  }
)
