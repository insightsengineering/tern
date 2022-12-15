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

afun_list <- lapply(1:9,
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

  fnotes_at_path(result, rowpath = NULL, colpath = c("multivars", "AVAL")) <- "Refers to the total no. of measurable ADA samples that have a corresponding measurable [Drug] concentration sample (i.e. results with
valid numeric values and LTRs). LTR results on post-dose samples are replaced by aaa µg/mL i.e. half of MQC value."
  fnotes_at_path(result, rowpath = NULL, colpath = c("multivars", "AVAL_LT")) <- "In validation, the assay was able to detect yyy ng/mL of surrogate ADA in the presence of zzz µg/mL of [Drug]. BLQ = Below Limit of
Quantitation, LTR = Lower than Reportable, MQC = Minimum Quantifiable Concentration, ADA = Anti-Drug Antibodies (is also referred to as ATA,
or Anti-Therapeutic Antibodies) ROXXXXXXX is also known as [drug]"

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "Treatment Group", "  Visit", "", "", "A: Drug X (N=1206)", "Day 1", " ", "Day 2", " ", "C: Combination (N=2112)", "Day 1", " ", "Day 2", " ", "Overall",
      "", "Total Number", "of Measurable", " Samples {1}", "", "", "938", "", "268", "", "", "1584", "", "528", "3318",
      "", "", "", "Mean", "", "", "8.227e+00", "", "1.344e+01", "", "", "1.469e+01", "", "2.018e+01", "1.363e+01",
      "", "", "", "SD", "", "", "7.351e+00", "", "1.351e+00", "", "", "1.237e+01", "", "7.129e+00", "1.059e+01",
      "", "", "", "Median", "", "", "1.131e+01", "", "1.330e+01", "", "", "1.451e+01", "", "1.888e+01", "1.377e+01",
      "", "", "", "Minimum", "", "", "0.000e+00", "", "1.075e+01", "", "", "0.000e+00", "", "1.073e+01", "0.000e+00",
      "", "", "", "Maximum", "", "", "1.986e+01", "", "1.646e+01", "", "", "3.947e+01", "", "3.259e+01", "3.947e+01",
      "", "", "", "CV (%)", "", "", "89.4", "", "10.0", "", "", "84.3", "", "35.3", "77.7",
      "", "", "", "Geometric Mean", "", "", "NA", "", "1.338e+01", "", "", "NA", "", "1.891e+01", "NA",
      "", "Samples with",  "Concentration", "≤ 15μg/mL {2}", "", "", "738 (78.7%)", "", "228 (85.1%)", "", "", "836 (52.8%)", "", "210 (39.8%)", "2012 (60.6%)"
    ),
    .Dim = c(15L, 10L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
