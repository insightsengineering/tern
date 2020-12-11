library(random.cdisc.data)
library(dplyr)

# 1. Preprocess ADAE so that deaths do not occur in arm "A: Drug X".
# 2. Concatenate AEBODSYS and AEDECOD per GDSR output standard AET07.
preprocess_adae <- function(adae) {

  set.seed(1, kind = "Mersenne-Twister")
  adae %>%
    mutate(
      # Convert AESDTH to character for next step.
      AESDTH = as.character(AESDTH),
      # For demonstration purpose only,
      # make "A: Drug X" as the arm without AE leading to death.
      AESDTH = case_when(
        ARM == "A: Drug X" ~ NA_character_,
        TRUE ~ AESDTH
      ),
      AESDTH = as.factor(AESDTH),
      SOC_PT = factor(paste(AEBODSYS, "/", AEDECOD))
    ) %>%
    filter(AESDTH == "Y")
}

test_that("AET07 variant 1 is produced correctly", {

  adsl <- radsl(cached = TRUE)
  adae <- radae(cached = TRUE) %>%
    preprocess_adae()

  # Not all arms have events so need to subset adsl counts.
  n_per_arm <- count(adsl, ACTARM) %>%
    filter(ACTARM %in% unique(adae$ACTARM)) %>%
    pull(n)

  lyt <- basic_table() %>%
    split_cols_by("ACTARM", split_fun = drop_split_levels) %>%
    add_colcounts() %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = c(unique = "Total number of deaths")
    ) %>%
    count_occurrences(
      vars = "SOC_PT",
      .indent_mods = -1L
    )

  result <- build_table(lyt, adae, col_counts = n_per_arm) %>%
    sort_at_path(
      path = "SOC_PT",
      scorefun = score_occurrences,
      decreasing = TRUE
    )

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Total number of deaths", "cl D.1 / dcd D.1.1.1.1",
      "cl B.1 / dcd B.1.1.1.1", "B: Placebo", "(N=134)", "70 (52.2%)",
      "42 (31.3%)", "49 (36.6%)", "C: Combination", "(N=132)", "75 (56.8%)",
      "51 (38.6%)", "43 (32.6%)"
    ),
  .Dim = c(5L, 3L)
  )

  expect_identical(result_matrix, expected_matrix)

})

test_that("AET07 variant 2 is produced correctly", {

  adsl <- radsl(cached = TRUE)
  adae <- radae(cached = TRUE) %>%
    preprocess_adae()

  n_per_arm <- table(adsl$ACTARM)

  lyt <- basic_table() %>%
    split_cols_by("ACTARM") %>%
    add_colcounts() %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = c(unique = "Total number of deaths")
    ) %>%
    count_occurrences(
      vars = "SOC_PT",
      .indent_mods = -1L
    )

  result <- build_table(lyt, adae, col_counts = n_per_arm) %>%
    prune_table() %>%
    sort_at_path(
      path = "SOC_PT",
      scorefun = score_occurrences,
      decreasing = TRUE
    )

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
    "", "", "Total number of deaths", "cl D.1 / dcd D.1.1.1.1",
    "cl B.1 / dcd B.1.1.1.1", "A: Drug X", "(N=134)", "0", "0", "0",
    "B: Placebo", "(N=134)", "70 (52.2%)", "42 (31.3%)", "49 (36.6%)",
    "C: Combination", "(N=132)", "75 (56.8%)", "51 (38.6%)", "43 (32.6%)"
  ),
  .Dim = 5:4
  )

  expect_identical(result_matrix, expected_matrix)

})
