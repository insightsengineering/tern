# Tests all variants of DST01.
get_adsl0 <- function() {
  # This specific ADSL is currently not part of RCD.
  set.seed(1, kind = "Mersenne-Twister")

  # nolint start
  adsl0 <- adsl_raw %>%
    dplyr::mutate(
      COMPSTUD = sample(
        c("Y", "N"),
        size = nrow(adsl_raw),
        replace = TRUE
      ) %>% as.factor(),
      STUDONS = sample(
        c("Alive: On Treatment", "Alive: In Follow-up", NA),
        size = nrow(adsl_raw),
        replace = TRUE
      ) %>% as.factor(),
      STDDRS = sample(
        c(
          "Death", "Lost To Follow-Up",
          "Protocol Violation", "Withdrawal By Subject",
          "Other"
        ),
        size = nrow(adsl_raw),
        replace = TRUE
      ) %>% as.factor(),
      GOTTRT = ifelse(!is.na(ACTARMCD), "Y", "N") %>%
        as.factor(),
      DISTRTFL = sample(
        c("Y", "N"),
        size = nrow(adsl_raw),
        replace = TRUE
      ) %>% as.factor(),
      TRTDRS = sample(
        c(
          "ADVERSE EVENT", "PROGRESSIVE DISEASE",
          "PHYSICIAN DECISION", "LACK OF EFFICACY",
          "OTHER"
        ),
        size = nrow(adsl_raw),
        replace = TRUE
      ) %>% as.factor(),
      STUDONS = dplyr::case_when(COMPSTUD == "N" ~ STUDONS),
      STDDRS = dplyr::case_when(COMPSTUD == "N" & is.na(STUDONS) ~ STDDRS),
      DISSTDFL = dplyr::case_when(!is.na(STDDRS) ~ "Y"),
      DISTRTFL = dplyr::case_when(GOTTRT == "Y" ~ DISTRTFL),
      TRTDRS = dplyr::case_when(DISTRTFL == "Y" ~ TRTDRS),
      DRSCAT = dplyr::case_when(
        TRTDRS %in% c("ADVERSE EVENT", "PHYSICIAN DECISION") ~ "Safety",
        !is.na(TRTDRS) ~ "Other"
      )
    )
  columns <- c("COMPSTUD", "STUDONS", "DISSTDFL", "STDDRS", "GOTTRT", "DISTRTFL", "TRTDRS", "DRSCAT")
  labels <- c(
    "Complete Study",
    "On-study Status",
    "Discontinued Study",
    "Reason for Study \r\nDiscontinuation",
    "Received Treatment",
    "Discontinued Treatment",
    "Reason for Treatment \r\nDiscontinuation",
    "Subcategory for Treatment Discontinuation"
  )
  formatters::var_labels(adsl0)[columns] <- labels
  # nolint end
  adsl0
}

testthat::test_that("DST01 default variant is produced correctly", {
  adsl0 <- get_adsl0()

  result <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    count_values("COMPSTUD", values = "Y", .labels = c(count_fraction = "Completed Study")) %>%
    split_rows_by("DISSTDFL", split_fun = keep_split_levels("Y")) %>%
    summarize_row_groups(label_fstr = "Discontinued Study") %>%
    summarize_vars("STDDRS", .stats = "count_fraction") %>%
    build_table(adsl0)

  res <- expect_silent(result)
  expect_snapshot(res)
})

testthat::test_that("DST01 variant with grouping of reasons is produced correctly", {
  adsl0 <- get_adsl0()

  result <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    count_values("GOTTRT", values = "Y", .labels = c(count_fraction = "Received treatment")) %>%
    split_rows_by("DISTRTFL", split_fun = keep_split_levels("Y")) %>%
    summarize_row_groups(label_fstr = "Discontinued treatment") %>%
    split_rows_by("DRSCAT", split_fun = reorder_split_levels(c("Safety", "Other"))) %>%
    summarize_row_groups() %>%
    summarize_vars("TRTDRS", .stats = "count_fraction") %>%
    build_table(adsl0) %>%
    prune_table()

  res <- expect_silent(result)
  expect_snapshot(res)
})

testthat::test_that("DST01 variant with adding other optional rows is produced correctly", {
  adsl0 <- get_adsl0()

  result <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    count_values("COMPSTUD", values = "Y", .labels = c(count_fraction = "Completed Study")) %>%
    count_values("STUDONS", values = "Alive: In Follow-up", .labels = c(count_fraction = "Alive: In Follow-up")) %>%
    split_rows_by("DISSTDFL", split_fun = keep_split_levels("Y")) %>%
    summarize_row_groups(label_fstr = "Discontinued Study") %>%
    summarize_vars("STDDRS", .stats = "count_fraction") %>%
    build_table(adsl0)

  res <- expect_silent(result)
  expect_snapshot(res)
})
