library(random.cdisc.data)

preprocess_adtte <- function(adtte) {

  # Save variable labels before data processing steps.
  adtte_labels <- var_labels(adtte)

  adtte <- adtte %>%
    dplyr::filter(
      PARAMCD == "OS",
      ARM %in% c("B: Placebo", "A: Drug X"),
      SEX %in% c("M", "F")
    ) %>%
    dplyr::mutate(
      # Reorder levels of ARM to display reference arm before treatment arm.
      ARM = droplevels(forcats::fct_relevel(ARM, "B: Placebo")),
      SEX = droplevels(SEX),
      is_event = CNSR == 0
    ) %>%
    var_relabel(
      ARM = adtte_labels["ARM"],
      SEX = adtte_labels["SEX"],
      is_event = "Event Flag"
    )

  adtte
}

test_that("h_survtime_df functions as expected with valid input and default arguments", {

  adtte <- radtte(cached = TRUE) %>%
    preprocess_adtte()

  result <- h_survtime_df(
    tte = adtte$AVAL,
    is_event = adtte$is_event,
    arm = adtte$ARM
  )

  expected <- data.frame(
    arm = factor(c("B: Placebo", "A: Drug X"), levels = c("B: Placebo", "A: Drug X")),
    n = c(134, 134),
    n_events = c(92, 81),
    median = c(813.5768, 1010.2328),
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected, tolerance = 0.000001)
})

test_that("h_survtime_df functions as expected when median is NA", {

  adtte <- radtte(cached = TRUE) %>%
    preprocess_adtte()

  # Edge case: median cannot be estimated.
  df_na <- data.frame(
    tte = c(1:3, 1:3),
    is_event = c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE),
    arm = factor(c("A", "A", "A", "B", "B", "B"), levels = c("A", "B"))
  )

  result <- expect_warning(h_survtime_df(
    tte = df_na$tte,
    is_event = df_na$is_event,
    arm = df_na$arm
  ))

  expected <- data.frame(
    arm = factor(c("A", "B"), levels = c("A", "B")),
    n = c(3, 3),
    n_events = c(1, 3),
    median = c(NA, 2),
    stringsAsFactors = FALSE
  )

})

test_that("h_survtime_df functions as expected when 0 records in one group", {

  adtte <- radtte(cached = TRUE) %>%
    preprocess_adtte()

  # Edge case: 0 records in one group.
  df_na <- data.frame(
    tte = c(1:6),
    is_event = c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE),
    arm = factor(rep("B", 6), levels = c("A", "B"))
  )

  result <- h_survtime_df(
    tte = df_na$tte,
    is_event = df_na$is_event,
    arm = df_na$arm
  )

  expected <- data.frame(
    arm = factor(c("A", "B"), levels = c("A", "B")),
    n = c(0, 6),
    n_events = c(0, 4),
    median = c(NA, 5),
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected, tolerance = 0.000001)

})

test_that("h_survtime_df fails with wrong input", {

  expect_error(h_survtime_df(
    tte = c(1, 2, NA),
    is_event = c(TRUE, FALSE, TRUE),
    arm = factor(c("A", "B", "A"), levels = c("B", "A"))
  ))

})

test_that("h_split_by_subgroups functions as expected with valid input and default arguments", {

  lvls_y <- c("B", "A", "C")

  df_test <- data.frame(
    x = c(1:2),
    y = factor(c("A", "B"), levels = lvls_y),
    z = factor(c("D", "D"), levels = "D")
  )
  var_labels(df_test) <- paste("label for", names(df_test))

  result <- h_split_by_subgroups(
    data  = df_test,
    subgroups = c("y", "z")
  )

  df_yb <- data.frame(
    x = 2,
    y = factor("B", levels = lvls_y),
    z = factor("D", levels = "D")
  )
  var_labels(df_yb) <- c("label for x", "label for y", "label for z")

  df_ya <- data.frame(
    x = 1,
    y = factor("A", levels = lvls_y),
    z = factor("D", levels = "D")
  )
  var_labels(df_ya) <- c("label for x", "label for y", "label for z")

  expected <- list(
    `y.B` = list(
      df = df_yb,
      df_labels = data.frame(
        subgroup = "B",
        var = "y",
        var_label = "label for y",
        stringsAsFactors = FALSE
      )
    ),
    `y.A` = list(
      df = df_ya,
      df_labels = data.frame(
        subgroup = "A",
        var = "y",
        var_label = "label for y",
        stringsAsFactors = FALSE
      )
    ),
    `z.D` = list(
      df = df_test,
      df_labels = data.frame(
        subgroup = "D",
        var = "z",
        var_label = "label for z",
        stringsAsFactors = FALSE
      )
    )
  )

  expect_equal(result, expected)

})

test_that("h_survtime_subgroups_df functions as expected with valid input and default arguments", {

  adtte <- radtte(cached = TRUE) %>%
    preprocess_adtte()

  result <- h_survtime_subgroups_df(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM", subgroups = c("SEX", "BMRKR2")),
    data = adtte
  )

  expected <- data.frame(
    arm = factor(rep(c("B: Placebo", "A: Drug X"), 6), levels = c("B: Placebo", "A: Drug X")),
    n = c(134, 134, 82, 79, 52, 55, 45, 50, 56, 37, 33, 47),
    n_events = c(92, 81, 59, 44, 33, 37, 28, 34, 37, 19, 27, 28),
    median = c(
      813.5769, 1010.2328, 676.0553, 987.2498, 1180.4056, 1062.6402,
      818.6373, 965.8316, 971.3165, 1474.6575, 476.9332, 956.0405
    ),
    subgroup = c("All Patients", "All Patients", "F", "F", "M", "M", "LOW", "LOW", "MEDIUM", "MEDIUM", "HIGH", "HIGH"),
    var = c(rep("ALL", 2), rep("SEX", 4), rep("BMRKR2", 6)),
    var_label = c(rep("All Patients", 2), rep("Sex", 4), rep("Categorical Level Biomarker 2", 6)),
    row_type = c(rep("content", 2), rep("analysis", 10)),
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected, tol = 0.000001)

})

test_that("h_survtime_subgroups_df functions as expected when subgroups is NULL.", {

  adtte <- radtte(cached = TRUE) %>%
    preprocess_adtte()

  result <- h_survtime_subgroups_df(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM"),
    data = adtte
  )

  expected <- data.frame(
    arm = factor(c("B: Placebo", "A: Drug X"), levels = c("B: Placebo", "A: Drug X")),
    n = c(134, 134),
    n_events = c(92, 81),
    median = c(813.5769, 1010.2328),
    subgroup = c("All Patients", "All Patients"),
    var = c(rep("ALL", 2)),
    var_label = c(rep("All Patients", 2)),
    row_type = c(rep("content", 2)),
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected, tol = 0.000001)

})

test_that("h_coxph_df functions as expected with valid input and default arguments", {

  adtte <- radtte(cached = TRUE) %>%
    preprocess_adtte()

  result <- h_coxph_df(
    tte = adtte$AVAL,
    is_event = adtte$is_event,
    arm = adtte$ARM
  )

  expected <- data.frame(
    arm = " ",
    n_tot = 268,
    hr = 0.8412573,
    lcl = 0.6231147,
    ucl = 1.135768,
    conf_level = 0.95,
    pval = 0.2584456,
    pval_label = "p-value (log-rank)",
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected, tol = 0.000001)

})

test_that("h_coxph_df functions as expected with one stratification factor", {

  adtte <- radtte(cached = TRUE) %>%
    preprocess_adtte()

  # Test with one stratification factor.
  result <- h_coxph_df(
    tte = adtte$AVAL,
    is_event = adtte$is_event,
    arm = adtte$ARM,
    strata_data = adtte$STRATA1
  )

  expected <- data.frame(
    arm = " ",
    n_tot = 268,
    hr =  0.8419229,
    lcl = 0.6223851,
    ucl = 1.1389,
    conf_level = 0.95,
    pval = 0.2637548,
    pval_label = "p-value (log-rank)",
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected, tol = 0.000001)
})

test_that("h_coxph_df functions as expected with multiple stratification factors", {

  adtte <- radtte(cached = TRUE) %>%
    preprocess_adtte()

  result <- h_coxph_df(
    tte = adtte$AVAL,
    is_event = adtte$is_event,
    arm = adtte$ARM,
    strata_data = adtte[, c("STRATA1", "STRATA2")]
  )

  expected <- data.frame(
    arm = " ",
    n_tot = 268,
    hr = 0.7979136,
    lcl = 0.5840112,
    ucl = 1.090161,
    conf_level = 0.95,
    pval = 0.1554903,
    pval_label = "p-value (log-rank)",
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected, tol = 0.000001)
})

test_that("h_coxph_df fails with wrong input", {

  expect_error(h_coxph_df(
    tte = 1,
    is_event = TRUE,
    arm = factor("A", levels = c("B", "A"))
  ))

})

test_that("h_coxph_subgroups_df functions as expected with valid input and default arguments", {

  adtte <- radtte(cached = TRUE) %>%
    preprocess_adtte()

  result <- h_coxph_subgroups_df(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM", subgroups = c("SEX", "BMRKR2")),
    data = adtte
  )

  expected <- data.frame(
    arm = rep(" ", 6),
    n_tot = c(268, 161, 107, 95, 93, 80),
    hr = c(0.8412573, 0.6531635, 1.2312049, 1.0592234, 0.6068906, 0.7683790),
    lcl = c(0.6231147, 0.4390645, 0.7644891, 0.6383431, 0.3432599, 0.4489251),
    ucl = c(1.1357683, 0.9716624, 1.9828478, 1.7576037, 1.0729952, 1.3151552),
    conf_level = 0.95,
    pval = c(0.25844564, 0.03429498, 0.39147827, 0.82376106, 0.08300454, 0.33527427),
    pval_label = rep("p-value (log-rank)", 6),
    subgroup = c("All Patients", "F", "M", "LOW", "MEDIUM", "HIGH"),
    var = c("ALL", "SEX", "SEX", "BMRKR2", "BMRKR2", "BMRKR2"),
    var_label = c("All Patients", "Sex", "Sex", rep("Categorical Level Biomarker 2", 3)),
    row_type = c("content", rep("analysis", 5)),
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected, tol = 0.000001)

  # Test edge case where HR is (0, Inf)
  adtte <- radtte(cached = TRUE) %>%
    preprocess_adtte() %>%
    filter(COUNTRY %in% c("CAN", "GBR"))

  result <- expect_warning(h_coxph_subgroups_df(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM", subgroups = "COUNTRY"),
    data = adtte
  ))

  expected <- data.frame(
    arm = rep(" ", 3),
    n_tot = c(12, 7, 5),
    hr = c(7.327311e-01, 8.756061e-10, 1.405150),
    lcl = c(0.116615850209759, 0, 0.124664516285412),
    ucl = c(4.603962, Inf, 15.838070),
    conf_level = 0.95,
    pval = c(0.7392974, 0.4142162, 0.7821768),
    pval_label = rep("p-value (log-rank)", 3),
    subgroup = c("All Patients", "GBR", "CAN"),
    var = c("ALL", "COUNTRY", "COUNTRY"),
    var_label = c("All Patients", "Country", "Country"),
    row_type = c("content", rep("analysis", 2)),
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected, tol = 0.000001)

})

test_that("h_coxph_subgroups_df functions as expected with with stratification factors", {

  adtte <- radtte(cached = TRUE) %>%
    preprocess_adtte()

  result <- h_coxph_subgroups_df(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM", subgroups = "SEX", strat = "STRATA1"),
    data = adtte
  )

  expected <- data.frame(
    arm = rep(" ", 3),
    n_tot = c(268, 161, 107),
    hr = c(0.8419229, 0.6768647, 1.2255609),
    lcl = c(0.6223851, 0.4507315, 0.7538494),
    ucl = c(1.138900, 1.016449, 1.992440),
    conf_level = 0.95,
    pval = c(0.26375482, 0.05843529, 0.41126770),
    pval_label = rep("p-value (log-rank)", 3),
    subgroup = c("All Patients", "F", "M"),
    var = c("ALL", "SEX", "SEX"),
    var_label = c("All Patients", "Sex", "Sex"),
    row_type = c("content", rep("analysis", 2)),
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected, tol = 0.000001)

})

test_that("h_coxph_subgroups_df functions as expected when subgroups is NULL.", {

  adtte <- radtte(cached = TRUE) %>%
    preprocess_adtte()

  result <- h_coxph_subgroups_df(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM"),
    data = adtte
  )

  expected <- data.frame(
    arm = " ",
    n_tot = 268,
    hr = 0.8412573,
    lcl = 0.6231147,
    ucl = 1.1357683,
    conf_level = 0.95,
    pval = 0.25844564,
    pval_label = "p-value (log-rank)",
    subgroup = "All Patients",
    var = "ALL",
    var_label = "All Patients",
    row_type = "content",
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected, tol = 0.000001)
  })
