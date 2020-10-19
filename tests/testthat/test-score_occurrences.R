test_that("score_occurrences functions as expected with valid input and default arguments", {
  library(dplyr)
  set.seed(1)
  dfsl <- data.frame(
    USUBJID = as.character(c(1, 2, 3, 4, 5)),
    ARM = sample(c("A", "B", "C"), 5, replace = TRUE)
  )
  N_per_arm <- table(dfsl$ARM) # nolint

  dfae <- data.frame(
    USUBJID = as.character(c(1, 2, 3, 4)),
    AEBODSYS = sample(c("AEBS1", "AEBS2"), 20, replace = TRUE),
    AEDECOD = sample(c("AEPT1", "AEPT2", "AEPT3"), 20, replace = TRUE)
  )
  dfae <- dfae %>% arrange(USUBJID, AEBODSYS, AEDECOD)
  dfae <- left_join(dfae, dfsl, by = "USUBJID")

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

  rtable_object <- build_table(lyt, dfae, col_counts = N_per_arm) %>%
    prune_table()

  rtable_object_sorted <- rtable_object %>%
    sort_at_path(path =  c("AEBODSYS", "*", "AEDECOD"), scorefun = score_occurrences)

  result <- to_string_matrix(rtable_object_sorted)

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
