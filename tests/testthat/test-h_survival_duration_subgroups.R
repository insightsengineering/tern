# Local data pre-processing
preprocess_adtte <- function(adtte) {
  # Save variable labels before data processing steps.
  adtte_labels <- formatters::var_labels(adtte)

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

adtte_local <- tern_ex_adtte %>%
  preprocess_adtte()

testthat::test_that("h_survtime_df functions as expected with valid input and default arguments", {
  adtte <- adtte_local

  result <- h_survtime_df(
    tte = adtte$AVAL,
    is_event = adtte$is_event,
    arm = adtte$ARM
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_survtime_df functions as expected when median is NA", {
  adtte <- adtte_local

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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_survtime_df functions as expected when 0 records in one group", {
  adtte <- adtte_local

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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_survtime_df fails with wrong input", {
  testthat::expect_error(h_survtime_df(
    tte = c(1, 2, "hello"),
    is_event = c(TRUE, FALSE, TRUE),
    arm = factor(c("A", "B", "A"), levels = c("B", "A"))
  ))
})

testthat::test_that("h_split_by_subgroups functions as expected with valid input and default arguments", {
  lvls_y <- c("B", "A", "C")

  df_test <- data.frame(
    x = c(1:2),
    y = factor(c("A", "B"), levels = lvls_y),
    z = factor(c("D", "D"), levels = "D")
  )
  formatters::var_labels(df_test) <- paste("label for", names(df_test))

  result <- h_split_by_subgroups(
    data = df_test,
    subgroups = c("y", "z")
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_split_by_subgroups works as expected with groups_lists", {
  lvls_y <- c("B", "A", "C")

  df_test <- data.frame(
    x = c(1:2),
    y = factor(c("A", "B"), levels = lvls_y),
    z = factor(c("D", "D"), levels = "D")
  )
  formatters::var_labels(df_test) <- paste("label for", names(df_test))

  result <- h_split_by_subgroups(
    data = df_test,
    subgroups = c("y", "z"),
    groups_lists = list(
      y = list("AB" = c("A", "B"), "E" = "C")
    )
  )

  res <- testthat::expect_silent(names(result))
  testthat::expect_snapshot(res)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_survtime_subgroups_df functions as expected with valid input and default arguments", {
  adtte <- adtte_local

  result <- h_survtime_subgroups_df(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM", subgroups = c("SEX", "BMRKR2")),
    data = adtte
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_survtime_subgroups_df functions as expected when subgroups is NULL.", {
  adtte <- adtte_local

  result <- h_survtime_subgroups_df(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM"),
    data = adtte
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_survtime_subgroups_df works as expected with groups_lists", {
  adtte <- adtte_local

  result <- h_survtime_subgroups_df(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM", subgroups = c("SEX", "BMRKR2")),
    data = adtte,
    groups_lists = list(
      BMRKR2 = list(
        "low" = "LOW",
        "low/medium" = c("LOW", "MEDIUM"),
        "low/medium/high" = c("LOW", "MEDIUM", "HIGH")
      )
    )
  )

  res <- testthat::expect_silent(result[result$var == "BMRKR2", "subgroup"])
  testthat::expect_snapshot(res)
})

testthat::test_that("h_coxph_df functions as expected with valid input and default arguments", {
  adtte <- adtte_local

  result <- h_coxph_df(
    tte = adtte$AVAL,
    is_event = adtte$is_event,
    arm = adtte$ARM
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_coxph_df functions as expected with one stratification factor", {
  adtte <- adtte_local

  # Test with one stratification factor.
  result <- h_coxph_df(
    tte = adtte$AVAL,
    is_event = adtte$is_event,
    arm = adtte$ARM,
    strata_data = adtte$STRATA1
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_coxph_df functions as expected with multiple stratification factors", {
  adtte <- adtte_local

  result <- h_coxph_df(
    tte = adtte$AVAL,
    is_event = adtte$is_event,
    arm = adtte$ARM,
    strata_data = adtte[, c("STRATA1", "STRATA2")]
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_coxph_df functions as expected when 0 records in one group", {
  adtte <- adtte_local %>%
    dplyr::filter(ARM == "A: Drug X")

  result <- h_coxph_df(
    tte = adtte$AVAL,
    is_event = adtte$is_event,
    arm = adtte$ARM
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_coxph_subgroups_df functions as expected with valid input and default arguments", {
  adtte <- adtte_local

  result <- h_coxph_subgroups_df(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM", subgroups = c("SEX", "BMRKR2")),
    data = adtte
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Test edge case where HR is (0, Inf)
  adtte <- adtte %>%
    dplyr::filter(COUNTRY %in% c("CAN", "GBR")) %>%
    reapply_varlabels(formatters::var_labels(adtte))

  testthat::expect_warning(result <- h_coxph_subgroups_df(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM", subgroups = "COUNTRY"),
    data = adtte
  ))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_coxph_subgroups_df functions as expected with stratification factors", {
  adtte <- adtte_local

  result <- h_coxph_subgroups_df(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM", subgroups = "SEX", strata = "STRATA1"),
    data = adtte
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_coxph_subgroups_df functions as expected when subgroups is NULL.", {
  adtte <- adtte_local

  result <- h_coxph_subgroups_df(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM"),
    data = adtte
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_coxph_subgroups_df works as expected with groups_lists", {
  adtte <- adtte_local

  result <- h_coxph_subgroups_df(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM", subgroups = c("SEX", "BMRKR2")),
    data = adtte,
    groups_lists = list(
      BMRKR2 = list(
        "low" = "LOW",
        "low/medium" = c("LOW", "MEDIUM"),
        "low/medium/high" = c("LOW", "MEDIUM", "HIGH")
      )
    )
  )

  res <- testthat::expect_silent(result[result$var == "BMRKR2", "subgroup"])
  testthat::expect_snapshot(res)
})
