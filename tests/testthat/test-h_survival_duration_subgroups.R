library(random.cdisc.data)

preprocess_adtte <- function(adtte) {

  # Save variable labels before data processing steps.
  adtte_labels <- var_labels(adtte)

  adtte_mod <- adtte %>%
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
    )

  reapply_varlabels(adtte_mod, adtte_labels, is_event = "Event Flag")
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
    n_events = c(87, 79),
    median = c(837.42801327648, 1260.49053370248),
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

  result <- h_survtime_df(
    tte = df_na$tte,
    is_event = df_na$is_event,
    arm = df_na$arm
  )

  expected <- data.frame(
    arm = factor(c("A", "B"), levels = c("A", "B")),
    n = c(3, 3),
    n_events = c(1, 3),
    median = c(NA, 2),
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected)

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
    n_events = c(NA, 4),
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
    n_events = c(87L, 79L, 50L, 45L, 37L, 34L, 30L, 31L, 36L, 19L, 21L, 29L),
    median = c(
      837.42801327648, 1260.49053370248, 850.920785514258, 1274.80474338372,
      527.665885794264, 849.297617184933, 751.431436610118, 1160.64578110184,
      722.792588842567, 1269.40388857211, 848.239273340441, 1070.80218764022
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
    n_events = c(87, 79),
    median = c(837.42801327648, 1260.49053370248),
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
    hr = 0.71736505115489,
    lcl = 0.527523110746632,
    ucl = 0.975526201857014,
    conf_level = 0.95,
    pval = 0.0334029294775113,
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
    hr =  0.7343822,
    lcl = 0.5376802,
    ucl = 1.003045,
    conf_level = 0.95,
    pval = 0.05142933,
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
    hr = 0.7412854,
    lcl = 0.5390265,
    ucl = 1.019438,
    conf_level = 0.95,
    pval = 0.06468801,
    pval_label = "p-value (log-rank)",
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected, tol = 0.000001)
})

test_that("h_coxph_df functions as expected when 0 records in one group", {

  adtte <- radtte(cached = TRUE) %>%
    preprocess_adtte() %>%
    filter(ARM == "A: Drug X")

  result <- h_coxph_df(
    tte = adtte$AVAL,
    is_event = adtte$is_event,
    arm = adtte$ARM
  )

  expected <- data.frame(
    arm = " ",
    n_tot = 134,
    hr = NA,
    lcl = NA,
    ucl = NA,
    conf_level = 0.95,
    pval = NA,
    pval_label = NA,
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected, tol = 0.000001)

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
    hr = c(
      0.71736505115489, 0.697969331159471, 0.783616674201674, 0.705072968604656, 0.572806884078014, 0.976900177598777
    ),
    lcl = c(
      0.527523110746632, 0.464781196048063, 0.487344418692843, 0.424365474268753, 0.324419621563317, 0.555200234313668
    ),
    ucl = c(
      0.975526201857014, 1.04815167089682, 1.26000230747263, 1.17146167914251, 1.01136831633695, 1.71890049393127
    ),
    conf_level = 0.95,
    pval = c(
      0.0334029294775113, 0.0814817359933963, 0.313183467032326, 0.17526198076925,
      0.0517494169527888, 0.935389266684535
    ),
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
    filter(COUNTRY %in% c("CAN", "GBR")) %>%
    reapply_varlabels(var_labels(radtte(cached = TRUE)))

  result <- expect_warning(h_coxph_subgroups_df(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM", subgroups = "COUNTRY"),
    data = adtte
  ))

  expected <- data.frame(
    arm = rep(" ", 3),
    n_tot = c(12, 7, 5),
    hr = c(1.2230641194542, 0.679005471337507, 1142066054.5383),
    lcl = c(0.270922018090357, 0.112310823072285, 0),
    ucl = c(5.52146278416316, 4.10511131068404, Inf),
    conf_level = 0.95,
    pval = c(0.793122593044781, 0.671398806014098, 0.414216178242525),
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
    hr = c(0.734382192317288, 0.759888515051216, 0.72253798908495),
    lcl = c(0.537680185840195, 0.501798302845741, 0.428328174772149),
    ucl = c(1.00304459527366, 1.15072241582342, 1.21883447417073),
    conf_level = 0.95,
    pval = c(0.0514293311402528, 0.193419942197434, 0.22139944778939),
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
    hr = 0.71736505115489,
    lcl = 0.5275231,
    ucl = 0.9755262,
    conf_level = 0.95,
    pval = 0.03340293,
    pval_label = "p-value (log-rank)",
    subgroup = "All Patients",
    var = "ALL",
    var_label = "All Patients",
    row_type = "content",
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected, tol = 0.000001)
})
