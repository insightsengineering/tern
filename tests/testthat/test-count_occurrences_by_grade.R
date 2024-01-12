raw_data <- data.frame(
  USUBJID = as.character(c(1:6, 1)),
  USUBJID2 = as.character(c(1:6, 1) * 10),
  AETOXGR = factor(c(1, 2, 3, 1, 1, 2, 3), levels = c(1:5)),
  AESEV = factor(
    c("MILD", "MODERATE", "SEVERE", "MILD", "MILD", "MODERATE", "SEVERE"),
    levels = c("MILD", "MODERATE", "SEVERE")
  ),
  ARM = factor(c("A", "A", "A", "B", "B", "B", "A"), levels = c("A", "B")),
  ARM_EMPTY = factor(c("A", "A", "A", "B", "B", "B", "A"), levels = c("A", "B", "D")),
  BMRKR = factor(rep(c("HIGH", "LOW"), 4)[-1], levels = c("LOW", "HIGH")),
  stringsAsFactors = FALSE
)

testthat::test_that("h_append_grade_groups works with valid input", {
  result <- h_append_grade_groups(
    list(
      "Any Grade" = as.character(1:5),
      "Grade 1-2" = c("1", "2"),
      "Grade 3-4" = c("3", "4")
    ),
    list("1" = 10, "2" = 20, "3" = 30, "4" = 40, "5" = 50),
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_append_grade_groups works with valid input with revers order and one-element grade groups", {
  result <- h_append_grade_groups(
    list(
      "Any Grade" = as.character(5:1),
      "Grade A" = "5",
      "Grade B" = c("4", "3")
    ),
    list("1" = 10, "2" = 20, "3" = 30, "4" = 40, "5" = 50),
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_count_occurrences_by_grade works with valid input and default arguments for grade", {
  df <- raw_data
  result <- s_count_occurrences_by_grade(df = df, .var = "AETOXGR", .N_col = 10)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Test with empty input.
  df_empty <- raw_data %>%
    dplyr::filter(ARM == "D")

  result <- s_count_occurrences_by_grade(df = df_empty, .var = "AETOXGR", .N_col = 10)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_count_occurrences_by_grade sorts grade levels so that 'missing' level appears last", {
  df <- raw_data
  df$AETOXGR <- factor(c("Missing", 2, 3, 1, 1, 2, 3), levels = c("Missing", 1:5))

  result <- s_count_occurrences_by_grade(df = df, .var = "AETOXGR", .N_col = 10)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_count_occurrences_by_grade works with valid input for grade grouping", {
  df <- raw_data
  result <- s_count_occurrences_by_grade(
    df = df,
    .var = "AETOXGR",
    .N_col = 10,
    grade_groups = list(
      "Any Grade" = as.character(1:5),
      "Grade 1-2" = c("1", "2"),
      "Grade 3-4" = c("3", "4")
    )
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Test with empyt input.
  df_empty <- raw_data %>%
    dplyr::filter(ARM == "D")

  result <- s_count_occurrences_by_grade(
    df = df_empty,
    .var = "AETOXGR",
    .N_col = 10,
    grade_groups = list(
      "Any Grade" = as.character(1:5),
      "Grade 1-2" = c("1", "2"),
      "Grade 3-4" = c("3", "4")
    )
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # keep only grade groups
  result <- s_count_occurrences_by_grade(
    df = df,
    .var = "AETOXGR",
    .N_col = 10,
    grade_groups = list(
      "Any Grade" = as.character(1:5),
      "Grade 1-2" = c("1", "2"),
      "Grade 3-4" = c("3", "4")
    ),
    only_grade_groups = TRUE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_count_occurrences_by_grade works with valid input for intensity and custom arguments", {
  df <- raw_data

  result <- s_count_occurrences_by_grade(
    df = df,
    .var = "AESEV",
    .N_col = 10L,
    id = "USUBJID2",
    grade_groups = list(
      "Any Intensity" = c("MILD", "MODERATE", "SEVERE")
    )
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("count_occurrences_by_grade works with default arguments for intensity", {
  df <- raw_data
  df_adsl <- unique(df[c("ARM", "ARM_EMPTY", "USUBJID")])

  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    count_occurrences_by_grade(var = "AESEV") %>%
    build_table(df, alt_counts_df = df_adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Test with empty column.
  result <- basic_table() %>%
    split_cols_by("ARM_EMPTY") %>%
    add_colcounts() %>%
    count_occurrences_by_grade(var = "AESEV") %>%
    build_table(df, alt_counts_df = df_adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("count_occurrences_by_grade label works when more than one variables are analyzed", {
  df <- raw_data
  df_adsl <- unique(df[c("ARM", "ARM_EMPTY", "USUBJID")])

  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    count_occurrences_by_grade(var = "AESEV") %>%
    count_occurrences_by_grade(var = "AETOXGR", var_labels = "Toxicity Grade") %>%
    build_table(df, alt_counts_df = df_adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})


testthat::test_that("count_occurrences_by_grade works with custom arguments for grade", {
  df <- raw_data
  df_adsl <- unique(df[c("ARM", "ARM_EMPTY", "USUBJID")])

  # Define additional grade groupings
  grade_groups <- list(
    "-Any-" = c("1", "2", "3", "4", "5"),
    "Grade 1-2" = c("1", "2"),
    "Grade 3-5" = c("3", "4", "5")
  )

  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    count_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = grade_groups,
      .formats = "xx.xx (xx.xx%)"
    ) %>%
    build_table(df, alt_counts_df = df_adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # keep only grade groups
  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    count_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = grade_groups,
      only_grade_groups = TRUE,
      .formats = "xx.xx (xx.xx%)"
    ) %>%
    build_table(df, alt_counts_df = df_adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("summarize_occurrences_by_grade works with default arguments for intensity", {
  df <- raw_data
  df_adsl <- data.frame(
    USUBJID = 1:9,
    ARM = rep(c("A", "B"), c(4, 5))
  )

  result <- basic_table() %>%
    add_colcounts() %>%
    split_rows_by("ARM", child_labels = "visible", nested = TRUE) %>%
    summarize_occurrences_by_grade(
      var = "AESEV",
      .formats = c("count_fraction" = "xx.xx (xx.xx%)")
    ) %>%
    build_table(df, alt_counts_df = df_adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Test with empty input.
  df <- raw_data
  df_adsl <- data.frame(
    USUBJID = 1:20,
    ARM_EMPTY = rep(c("A", "B"), each = 10)
  )

  result <- basic_table() %>%
    split_cols_by("ARM_EMPTY") %>%
    add_colcounts() %>%
    split_rows_by("BMRKR", child_labels = "visible", nested = TRUE) %>%
    summarize_occurrences_by_grade(
      var = "AESEV",
      .formats = c("count_fraction" = "xx.xx (xx.xx%)")
    ) %>%
    build_table(df, alt_counts_df = df_adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("summarize_occurrences_by_grade works with custom arguments for grade", {
  df <- raw_data
  df_adsl <- data.frame(
    USUBJID = 1:10,
    ARM_EMPTY = rep(c("A", "B"), each = 5)
  )

  # Define additional grade groupings
  grade_groups <- list(
    "-Any-" = c("1", "2", "3", "4", "5"),
    "Grade 1-2" = c("1", "2"),
    "Grade 3-5" = c("3", "4", "5")
  )

  result <- basic_table() %>%
    add_colcounts() %>%
    split_rows_by("ARM", child_labels = "visible", nested = TRUE) %>%
    summarize_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = grade_groups
    ) %>%
    build_table(df, alt_counts_df = df_adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("count_occurrences_by_grade works with trim_levels_in_group split function", {
  df <- data.frame(
    USUBJID = as.character(1:30),
    ARM = factor(c(rep("ARM A", 15), rep("ARM B", 15)), levels = c("ARM A", "ARM B")),
    SOC = as.factor(rep("SOC1", 30)),
    AETOXGR = factor(c(rep(1, 10), rep(2, 20)), levels = c(1:5))
  )
  df_adsl <- df %>%
    dplyr::select(USUBJID, ARM) %>%
    unique()

  # Define additional grade groupings
  grade_groups <- list(
    "-Any-" = as.character(1:5),
    "Grade 1-2" = c("1", "2"),
    "Grade 3-4" = c("3", "4")
  )

  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    split_rows_by("SOC", split_fun = trim_levels_in_group("AETOXGR")) %>%
    count_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = grade_groups
    ) %>%
    build_table(df, alt_counts_df = df_adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("summarize_ and count_occurrences_by_grade works with pagination and sorting", {
  df <- raw_data
  df_adsl <- data.frame(
    USUBJID = 1:10,
    ARM_EMPTY = rep(c("A", "B"), each = 5)
  )
  df <- rbind(df, df[4, ])
  df$USUBJID[nrow(df)] <- max(as.numeric(df$USUBJID)) + 1

  # Define additional grade groupings
  grade_groups <- list(
    "-Any-" = c("1", "2", "3", "4", "5"),
    "Grade 1-2" = c("1", "2"),
    "Grade 3-5" = c("3", "4", "5")
  )

  # Scoring function to extract specific values from path
  score_occurrences_from_path <- function(path) {
    function(table_row) {
      value_at(table_row, path)[1]
    }
  }

  result <- basic_table() %>%
    add_colcounts() %>%
    count_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = grade_groups
    ) %>%
    split_rows_by("ARM",
      child_labels = "visible",
      nested = TRUE,
      indent_mod = 1L
    ) %>%
    count_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = grade_groups
    ) %>%
    build_table(df, alt_counts_df = df_adsl) %>%
    sort_at_path(
      path = c("AETOXGR"),
      scorefun = score_occurrences,
      decreasing = TRUE
    ) %>%
    sort_at_path(
      path = c("ARM", "*", "AETOXGR"),
      scorefun = score_occurrences,
      decreasing = TRUE
    ) %>%
    sort_at_path(
      path = c("ARM"),
      scorefun = score_occurrences_from_path(c("AETOXGR", "-Any-")),
      decreasing = TRUE
    )

  pag_result <- paginate_table(result, lpp = 20)

  testthat::expect_identical(
    to_string_matrix(pag_result[[1]], with_spaces = FALSE, print_txt_to_copy = FALSE)[3:4, 1],
    c("-Any-", "Grade 1-2")
  )
  testthat::expect_identical(
    to_string_matrix(pag_result[[2]], with_spaces = FALSE, print_txt_to_copy = FALSE)[3, 1],
    "  A"
  )
})

testthat::test_that("count_occurrences_by_grade works as expected with risk difference column", {
  tern_ex_adae$AESEV <- factor(tern_ex_adae$AESEV)

  # Default parameters
  result <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM", split_fun = add_riskdiff("A: Drug X", "B: Placebo")) %>%
    count_occurrences_by_grade(
      var = "AESEV",
      riskdiff = TRUE
    ) %>%
    build_table(tern_ex_adae)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Grade groups, custom id var
  grade_groups <- list("-Any-" = levels(tern_ex_adae$AESEV))

  result <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM", split_fun = add_riskdiff("A: Drug X", "B: Placebo")) %>%
    count_occurrences_by_grade(
      var = "AESEV",
      riskdiff = TRUE,
      show_labels = "hidden",
      .indent_mods = 1L,
      grade_groups = grade_groups,
      id = "SITEID"
    ) %>%
    build_table(tern_ex_adae)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
