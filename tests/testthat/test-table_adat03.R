adpc <- adpc_raw %>% select(USUBJID, NRELTM1, AVAL, AVALU, AVALC)
anl <- adab_raw %>%
  filter(., PARAM == "ADA interpreted per sample result") %>%
  select(-AVAL, AVALC, AVALU)

anl <- merge(anl, adpc, by = c("USUBJID", "NRELTM1")) %>%
  mutate(AVAL_LT = ifelse(AVAL <= 15, TRUE, FALSE))

# parameters in columns
adat03_stats <- c("n", "mean", "sd", "median", "min", "max", "cv", "geom_mean", "count_fraction")
adat03_lbls <- c(
  n = "Total Number\nof Measurable\n Samples",
  mean = "Mean",
  sd = "SD",
  median = "Median",
  min = "Minimum",
  max = "Maximum",
  cv = "CV (%)",
  geom_mean = "Geometric Mean",
  count_fraction = paste0("Samples with\nConcentration\n≤ 15μg/mL")
)
adat03_fmts <- c(
  n = "xx.",
  mean = sprintf_format("%.3e"),
  sd = sprintf_format("%.3e"),
  median = sprintf_format("%.3e"),
  min = sprintf_format("%.3e"),
  max = sprintf_format("%.3e"),
  cv = "xx.x",
  geom_mean = sprintf_format("%.3e"),
  count_fraction = format_count_fraction
)

afun_list <- lapply(
  1:9,
  function(i) make_afun(s_summary, .stats = adat03_stats[i], .formats = adat03_fmts[i], .labels = "Overall")
)

testthat::test_that("ADAT03 is produced correctly", {
  result <- basic_table() %>%
    split_rows_by(
      var = "ARM",
      label_pos = "topleft",
      split_label = "Treatment Group",
      split_fun = drop_split_levels,
      section_div = ""
    ) %>%
    add_rowcounts() %>%
    split_rows_by(
      var = "VISIT",
      label_pos = "topleft",
      split_label = "Visit"
    ) %>%
    analyze_vars_in_cols(
      vars = c(rep("AVAL", 8), "AVAL_LT"),
      .stats = adat03_stats,
      .labels = adat03_lbls,
      .formats = adat03_fmts
    ) %>%
    analyze_colvars(
      afun_list,
      nested = FALSE,
      extra_args = list(".labels" = "Overall")
    ) %>%
    build_table(anl, alt_counts_df = adsl_raw)

  fnotes_at_path(result, rowpath = NULL, colpath = c("multivars", "AVAL")) <- "footnote1" # nolint
  fnotes_at_path(result, rowpath = NULL, colpath = c("multivars", "AVAL_LT")) <- "footnote2" # nolint

  res <- expect_silent(result)
  expect_snapshot(res)
})
