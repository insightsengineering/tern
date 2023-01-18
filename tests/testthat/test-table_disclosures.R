adae <- adae_raw

adsl_local <- local({
  set.seed(1)
  # nolint start
  adsl_f <- adsl_raw %>%
    dplyr::filter(SAFFL == "Y") %>% # Safety Evaluable Population
    dplyr::mutate(
      STSTFL = dplyr::case_when(
        is.na(RANDDT) ~ "N", # derive flag for "Started Study",
        TRUE ~ "Y"
      ),
      COMPSTUD = dplyr::case_when(
        EOSSTT == "COMPLETED" ~ "Y", # derive flag for "Completed Study"
        TRUE ~ "N"
      ),
      DISCSTUD = dplyr::case_when(
        EOSSTT == "DISCONTINUED" ~ "Y", # derive flag for "Discontinued study"
        TRUE ~ "N"
      ),
      AGEGRP = dplyr::case_when(
        AGE < 65 ~ "< 65 yrs",
        AGE >= 65 ~ ">= 65 yrs"
      ),
      ETHNIC = sample(
        c("Ethnicity 1", "Ethnicity 2", "Unknown"),
        nrow(.),
        replace = TRUE
      )
    )
  columns <- c("STSTFL", "COMPSTUD", "DISCSTUD", "AGEGRP", "ETHNIC")
  labels <- c("Start Study", "Study Completion Flag", "Study Discontinuation Flag", "Age Group", "Ethnicity")
  formatters::var_labels(adsl_f)[columns] <- labels

  adsl_f$AGEGRP <- factor(adsl_f$AGEGRP, levels = c("< 65 yrs", ">= 65 yrs"))
  adsl_f$ETHNIC <- factor(adsl_f$ETHNIC, levels = c("Ethnicity 1", "Ethnicity 2", "Unknown"))
  # nolint end
  adsl_f <- df_explicit_na(adsl_f)
  adsl_f
})

get_adae_trimmed <- function(adsl, adae, cutoff_rate) {
  n_per_arm <- adsl %>%
    dplyr::count(ARM)

  anl_terms <- adae %>%
    dplyr::group_by(ARM, AEBODSYS, AEDECOD) %>%
    dplyr::summarise(
      unique_terms = dplyr::n_distinct(USUBJID)
    ) %>%
    dplyr::ungroup()

  anl_terms <- dplyr::left_join(
    anl_terms,
    n_per_arm,
    by = "ARM"
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

testthat::test_that("Patient Disposition table is produced correctly", {
  adsl <- adsl_local

  result <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    count_values(
      "STSTFL",
      values = "Y",
      .labels = c(count_fraction = "Started Study"),
      .formats = c(count_fraction = "xx (xx.xx%)")
    ) %>%
    count_values(
      "COMPSTUD",
      values = "Y",
      .labels = c(count_fraction = "Completed Study"),
      .formats = c(count_fraction = "xx (xx.xx%)")
    ) %>%
    count_values(
      "DISCSTUD",
      values = "Y",
      .labels = c(count_fraction = "Discontinued Study"),
      .formats = "xx (xx.xx%)"
    ) %>%
    summarize_vars(
      "DCSREAS",
      .stats = "count_fraction",
      show_labels = "hidden",
      .indent_mods = c(count_fraction = 2L),
      .formats = c(count_fraction = "xx (xx.xx%)"),
      denom = "N_col"
    ) %>%
    build_table(adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("Demographic table is produced correctly", {
  adsl <- adsl_local
  vars <- c("AGE", "AGEGRP", "SEX", "RACE", "ETHNIC")
  var_labels <- c(
    "Age (yr)",
    "Age group (yr)",
    "Sex",
    "Race",
    "Ethnicity"
  )
  result <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    summarize_vars(vars = vars, var_labels = var_labels) %>%
    build_table(adsl) %>%
    prune_table()

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("Enrollment by Country Table is produced correctly", {
  adsl <- adsl_local
  result <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    summarize_vars("COUNTRY", .formats = c(count_fraction = "xx (xx.xx%)")) %>%
    build_table(adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("Death table is produced correctly", {
  adsl <- adsl_local

  result <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    count_patients_with_event(
      "USUBJID",
      filters = c("AESDTH" = "Y"),
      .labels = c(count_fraction = "Total Number of Deaths"),
      .formats = c(count_fraction = "xx (xx.xx%)")
    ) %>%
    build_table(adae, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("Table of Serious Adverse Events is produced correctly (for one specific treatment arm)", {
  adae_serious <- adae %>% dplyr::filter(AESER == "Y", SAFFL == "Y")
  adae_serious_arm <- adae_serious %>% dplyr::filter(ARM == "A: Drug X")

  filters_list <- list(
    related = formatters::with_label(c(AEREL = "Y"), "Events (Related)"),
    fatal = formatters::with_label(c(AESDTH = "Y"), "Events (Fatal)"),
    fatal_related = formatters::with_label(c(AEREL = "Y", AESDTH = "Y"), "Events (Fatal & Related)")
  )

  result <- basic_table() %>%
    summarize_patients_events_in_cols(
      filters_list = filters_list,
      empty_stats = c("all", "related", "fatal", "fatal_related"),
      custom_label = "Total number of patients with at least one serious adverse event"
    ) %>%
    split_rows_by("AEBODSYS", nested = FALSE, split_fun = drop_split_levels, indent_mod = -1L) %>%
    split_rows_by("AEDECOD", split_fun = drop_split_levels) %>%
    summarize_patients_events_in_cols(
      filters_list = filters_list,
      col_split = FALSE
    ) %>%
    build_table(adae_serious_arm)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("Table of Non-Serious Adverse Events is produced correctly", {
  adsl <- adsl_local
  adae_nonser <- adae %>% dplyr::filter(AESER != "Y", SAFFL == "Y")
  adae_trim <- get_adae_trimmed(adsl, adae_nonser, cutoff_rate = 0.05)

  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    summarize_patients_events_in_cols(
      custom_label = "Total number of patients with at least one non-SAE and number of events"
    ) %>%
    split_rows_by("AEBODSYS", nested = FALSE, split_fun = drop_split_levels, indent_mod = -1L) %>%
    split_rows_by("AEDECOD", split_fun = drop_split_levels) %>%
    summarize_patients_events_in_cols(
      col_split = FALSE
    ) %>%
    build_table(adae_trim, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
