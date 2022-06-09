## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- message=FALSE-----------------------------------------------------------
library(rtables)
library(tern)
library(dplyr)

## ---- message=FALSE-----------------------------------------------------------
adsl <- ex_adsl
adae <- ex_adae
adrs <- ex_adrs

## -----------------------------------------------------------------------------
# Select variables to include in table.
vars <- c("AGE", "SEX")
var_labels <- c("Age (yr)", "Sex")

basic_table() %>%
  split_cols_by(var = "ARM") %>%
  add_overall_col("All Patients") %>%
  add_colcounts() %>%
  summarize_vars(
    vars = vars,
    var_labels = var_labels
  ) %>%
  build_table(adsl)

## -----------------------------------------------------------------------------
# Reorder the levels in the ARM variable.
adsl$ARM <- factor(adsl$ARM, levels = c("B: Placebo", "A: Drug X", "C: Combination")) # nolint

# Reorder the levels in the SEX variable.
adsl$SEX <- factor(adsl$SEX, levels = c("M", "F", "U", "UNDIFFERENTIATED")) # nolint

basic_table() %>%
  split_cols_by(var = "ARM") %>%
  add_overall_col("All Patients") %>%
  add_colcounts() %>%
  summarize_vars(
    vars = vars,
    var_labels = var_labels
  ) %>%
  build_table(adsl)

## -----------------------------------------------------------------------------
# Select statistics and modify default formats.
basic_table() %>%
  split_cols_by(var = "ARM") %>%
  add_overall_col("All Patients") %>%
  add_colcounts() %>%
  summarize_vars(
    vars = vars,
    var_labels = var_labels,
    .stats = c("n", "mean_sd", "count"),
    .formats = c(mean_sd = "xx.xx (xx.xx)")
  ) %>%
  build_table(adsl)

## -----------------------------------------------------------------------------

lyt <- basic_table() %>%
  split_cols_by(var = "ARM") %>%
  add_overall_col("All Patients") %>%
  add_colcounts() %>%
  summarize_vars(
    vars = vars,
    var_labels = var_labels
  )

build_table(lyt, df = adsl %>% dplyr::filter(COUNTRY == "BRA"))

build_table(lyt, df = adsl %>% dplyr::filter(COUNTRY == "CHN"))

## -----------------------------------------------------------------------------
basic_table() %>%
  split_cols_by(var = "ACTARM") %>%
  add_colcounts() %>%
  add_overall_col(label = "All Patients") %>%
  summarize_num_patients(
    var = "USUBJID",
    .stats = c("unique", "nonunique"),
    .labels = c(
      unique = "Total number of patients with at least one AE",
      nonunique = "Overall total number of events"
    )
  ) %>%
  build_table(
    df = adae,
    alt_counts_df = adsl
  )

## -----------------------------------------------------------------------------
s_num_patients(x = adae$USUBJID, labelstr = "", .N_col = nrow(adae))

## -----------------------------------------------------------------------------
basic_table() %>%
  split_cols_by(var = "ACTARM") %>%
  add_colcounts() %>%
  add_overall_col(label = "All Patients") %>%
  summarize_num_patients(
    var = "USUBJID",
    .stats = "unique_count",
    .labels = c(unique_count = "Total number of patients with at least one AE")
  ) %>%
  build_table(
    df = adae,
    alt_counts_df = adsl
  )

## -----------------------------------------------------------------------------
basic_table() %>%
  split_cols_by(var = "ACTARM") %>%
  add_colcounts() %>%
  add_overall_col(label = "All Patients") %>%
  summarize_num_patients(
    var = "USUBJID",
    .stats = c("unique", "nonunique"),
    .labels = c(
      unique = "Total number of patients with at least one AE",
      nonunique = "Overall total number of events"
    )
  ) %>%
  split_rows_by(
    "AEBODSYS",
    child_labels = "visible",
    nested = FALSE,
    indent_mod = -1L,
    split_fun = drop_split_levels
  ) %>%
  summarize_num_patients(
    var = "USUBJID",
    .stats = c("unique", "nonunique"),
    .labels = c(
      unique = "Total number of patients with at least one AE",
      nonunique = "Overall total number of events"
    )
  ) %>%
  build_table(
    df = adae,
    alt_counts_df = adsl
  )

## -----------------------------------------------------------------------------
basic_table() %>%
  split_cols_by(var = "ACTARM") %>%
  add_colcounts() %>%
  add_overall_col(label = "All Patients") %>%
  count_occurrences(vars = "AEDECOD") %>%
  build_table(
    df = adae,
    alt_counts_df = adsl
  )

## -----------------------------------------------------------------------------
basic_table() %>%
  split_cols_by(var = "ACTARM") %>%
  add_colcounts() %>%
  add_overall_col(label = "All Patients") %>%
  summarize_num_patients(
    var = "USUBJID",
    .stats = c("unique", "nonunique"),
    .labels = c(
      unique = "Total number of patients with at least one AE",
      nonunique = "Overall total number of events"
    )
  ) %>%
  split_rows_by(
    "AEBODSYS",
    child_labels = "visible",
    nested = FALSE,
    indent_mod = -1L,
    split_fun = drop_split_levels
  ) %>%
  summarize_num_patients(
    var = "USUBJID",
    .stats = c("unique", "nonunique"),
    .labels = c(
      unique = "Total number of patients with at least one AE",
      nonunique = "Overall total number of events"
    )
  ) %>%
  count_occurrences(vars = "AEDECOD") %>%
  build_table(
    df = adae,
    alt_counts_df = adsl
  )

## -----------------------------------------------------------------------------
# Preprocessing to select an analysis endpoint.
anl <- adrs %>%
  dplyr::filter(PARAMCD == "BESRSPI") %>%
  dplyr::mutate(is_rsp = AVALC %in% c("CR", "PR"))

## -----------------------------------------------------------------------------
basic_table() %>%
  split_cols_by(var = "ARM") %>%
  add_colcounts() %>%
  estimate_proportion(
    vars = "is_rsp",
    table_names = "est_prop"
  ) %>%
  build_table(anl)

## -----------------------------------------------------------------------------
basic_table() %>%
  split_cols_by(var = "ARM", ref_group = "B: Placebo") %>%
  add_colcounts() %>%
  estimate_proportion(
    vars = "is_rsp"
  ) %>%
  build_table(anl)

## -----------------------------------------------------------------------------
basic_table() %>%
  split_cols_by(var = "ARM", ref_group = "B: Placebo") %>%
  add_colcounts() %>%
  estimate_proportion(
    vars = "is_rsp",
    method = "clopper-pearson",
    conf_level = 0.9
  ) %>%
  build_table(anl)

## -----------------------------------------------------------------------------
basic_table() %>%
  split_cols_by(var = "ARM", ref_group = "B: Placebo") %>%
  add_colcounts() %>%
  estimate_proportion_diff(
    vars = "is_rsp",
    show_labels = "visible",
    var_labels = "Unstratified Analysis"
  ) %>%
  build_table(anl)

## -----------------------------------------------------------------------------
basic_table() %>%
  split_cols_by(var = "ARM", ref_group = "B: Placebo") %>%
  add_colcounts() %>%
  test_proportion_diff(vars = "is_rsp") %>%
  build_table(anl)

## -----------------------------------------------------------------------------
basic_table() %>%
  split_cols_by(var = "ARM", ref_group = "B: Placebo") %>%
  add_colcounts() %>%
  test_proportion_diff(
    vars = "is_rsp",
    method = "schouten"
  ) %>%
  build_table(anl)

## -----------------------------------------------------------------------------
basic_table() %>%
  split_cols_by(var = "ARM", ref_group = "B: Placebo") %>%
  add_colcounts() %>%
  estimate_proportion(
    vars = "is_rsp",
    method = "clopper-pearson",
    conf_level = 0.9,
    table_names = "est_prop"
  ) %>%
  estimate_proportion_diff(
    vars = "is_rsp",
    show_labels = "visible",
    var_labels = "Unstratified Analysis",
    table_names = "est_prop_diff"
  ) %>%
  test_proportion_diff(
    vars = "is_rsp",
    method = "schouten",
    table_names = "test_prop_diff"
  ) %>%
  build_table(anl)

