library(dplyr)

get_df_ae <- function() {
  set.seed(1)
  dfsl <- data.frame(
    USUBJID = as.character(c(1, 2, 3, 4, 5)),
    ARM = sample(c("A", "B", "C"), 5, replace = TRUE)
  )

  dfae <- data.frame(
    USUBJID = as.character(c(1, 2, 3, 4)),
    AEBODSYS = sample(c("AEBS1", "AEBS2"), 20, replace = TRUE),
    AEDECOD = sample(c("AEPT1", "AEPT2", "AEPT3"), 20, replace = TRUE)
  )
  dfae <- dfae %>% arrange(USUBJID, AEBODSYS, AEDECOD) #nolint
  dfae <- left_join(dfae, dfsl, by = "USUBJID")
  structure(
    dfae,
    n_per_arm = table(dfsl$ARM)
  )
}

get_full_table <- function() {

  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one event",
        nonunique = "Total number of events"
      )) %>%
    split_rows_by("AEBODSYS", child_labels = "visible", nested = FALSE)  %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one event",
        nonunique = "Total number of events"
      )) %>%
    count_occurrences(vars = "AEDECOD")

  dfae <- get_df_ae()  #nolint

  build_table(lyt, dfae, col_counts = attr(dfae, "n_per_arm")) %>%
    prune_table()
}

test_that("score_occurrences functions as expected", {
  full_table <- get_full_table()

  sorted_table <- full_table %>%
    sort_at_path(path =  c("AEBODSYS", "*", "AEDECOD"), scorefun = score_occurrences)

  result <- to_string_matrix(sorted_table)

  expected <- rbind(
    c("",    "A",         "B",        "C"),
    c("",    "(N=3)",     "(N=1)",    "(N=1)"),
    c("Total number of patients with at least one event", "2 (66.7%)", "1 (100%)", "1 (100%)"),
    c("Total number of events", "10", "5", "5"),
    c("AEBS1", "", "", ""),
    c("Total number of patients with at least one event", "2 (66.7%)", "1 (100%)", "1 (100%)"),
    c("Total number of events", "7", "3", "4"),
    c("AEPT1", "2 (66.7%)", "1 (100%)", "1 (100%)"),
    c("AEPT2", "2 (66.7%)", "1 (100%)", "1 (100%)"),
    c("AEPT3", "1 (33.3%)", "1 (100%)", "1 (100%)"),
    c("AEBS2", "", "", ""),
    c("Total number of patients with at least one event", "2 (66.7%)", "1 (100%)", "1 (100%)"),
    c("Total number of events", "3", "2", "1"),
    c("AEPT2", "2 (66.7%)", "1 (100%)", "0"),
    c("AEPT1", "1 (33.3%)", "0", "0"),
    c("AEPT3", "0", "0", "1 (100%)")
  )

  expect_equal(result, expected)
})

test_that("score_occurrences_cols functions as expected", {
  full_table <- get_full_table()

  score_col_c <- score_occurrences_cols(col_names = "C")
  expect_is(score_col_c, "function")

  sorted_table <- full_table %>%
    sort_at_path(path =  c("AEBODSYS", "*", "AEDECOD"), scorefun = score_col_c)

  result <- to_string_matrix(sorted_table)

  expected <- structure(
    c("", "", "Total number of patients with at least one event",
      "Total number of events", "AEBS1", "Total number of patients with at least one event",
      "Total number of events", "AEPT1", "AEPT2", "AEPT3", "AEBS2",
      "Total number of patients with at least one event", "Total number of events",
      "AEPT3", "AEPT1", "AEPT2", "A", "(N=3)", "2 (66.7%)", "10", "",
      "2 (66.7%)", "7", "2 (66.7%)", "2 (66.7%)", "1 (33.3%)", "",
      "2 (66.7%)", "3", "0", "1 (33.3%)", "2 (66.7%)", "B", "(N=1)",
      "1 (100%)", "5", "", "1 (100%)", "3", "1 (100%)", "1 (100%)",
      "1 (100%)", "", "1 (100%)", "2", "0", "0", "1 (100%)", "C", "(N=1)",
      "1 (100%)", "5", "", "1 (100%)", "4", "1 (100%)", "1 (100%)",
      "1 (100%)", "", "1 (100%)", "1", "1 (100%)", "0", "0"),
    .Dim = c(16L, 4L)
  )
  expect_identical(result, expected)
})

test_that("score_occurrences_subtable functions as expected", {
  dfae <- get_df_ae()

  full_table <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    split_rows_by("AEBODSYS", child_labels = "visible", nested = FALSE)  %>%
    count_occurrences(vars = "AEDECOD") %>%
    build_table(dfae, col_counts = attr(dfae, "n_per_arm")) %>%
    prune_table()

  score_subtable_all <- score_occurrences_subtable(col_names = names(full_table))
  expect_is(score_subtable_all, "function")

  sorted_table <- full_table %>%
    sort_at_path(path =  c("AEBODSYS"), scorefun = score_subtable_all, decreasing = FALSE)

  result <- to_string_matrix(sorted_table)

  expected <- structure(
    c("", "", "AEBS2", "AEPT1", "AEPT2", "AEPT3", "AEBS1",
      "AEPT1", "AEPT2", "AEPT3", "A", "(N=3)", "", "1 (33.3%)", "2 (66.7%)",
      "0", "", "2 (66.7%)", "2 (66.7%)", "1 (33.3%)", "B", "(N=1)",
      "", "0", "1 (100%)", "0", "", "1 (100%)", "1 (100%)", "1 (100%)",
      "C", "(N=1)", "", "0", "0", "1 (100%)", "", "1 (100%)", "1 (100%)",
      "1 (100%)"),
    .Dim = c(10L, 4L)
  )
  expect_identical(result, expected)
})
