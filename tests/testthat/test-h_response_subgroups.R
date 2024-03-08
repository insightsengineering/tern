# Local data pre-processing
preprocess_adrs <- function(adrs, n_records = 20) {
  adrs_labels <- formatters::var_labels(adrs)
  adrs <- adrs %>%
    dplyr::filter(PARAMCD == "BESRSPI") %>%
    dplyr::filter(ARM %in% c("A: Drug X", "B: Placebo")) %>%
    dplyr::slice(1:n_records) %>%
    droplevels() %>%
    dplyr::mutate(
      # Reorder levels of factor to make the placebo group the reference arm.
      ARM = forcats::fct_relevel(ARM, "B: Placebo"),
      rsp = AVALC == "CR"
    )
  formatters::var_labels(adrs) <- c(adrs_labels, "Response")
  adrs
}

adrs_20 <- preprocess_adrs(tern_ex_adrs, 20)
adrs_100 <- preprocess_adrs(tern_ex_adrs, 100)

testthat::test_that("h_proportion_df functions as expected with valid input and default arguments", {
  rsp <- c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE)
  arm <- factor(c("A", "B", "A", "B", "A", "A"), levels = c("B", "A"))

  result <- h_proportion_df(rsp = rsp, arm = arm)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_proportion_df functions as expected when 0 responses in one group", {
  rsp <- c(TRUE, FALSE, FALSE, FALSE)
  arm <- factor(c("A", "A", "B", "B"), levels = c("A", "B"))

  result <- h_proportion_df(rsp = rsp, arm = arm)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_proportion_df fails with wrong input", {
  testthat::expect_error(h_proportion_df(
    rsp = c(TRUE, FALSE, Inf),
    arm = factor(c("A", "B", "A"), levels = c("B", "A"))
  ))
})

testthat::test_that("h_proportion_df functions when 0 obs in one arm", {
  rsp <- c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE)
  arm <- factor(rep("A", 6), levels = c("B", "A"))

  result <- h_proportion_df(rsp = rsp, arm = arm)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_proportion_subgroups_df functions as expected with valid input and default arguments", {
  adrs <- adrs_20

  result <- h_proportion_subgroups_df(
    variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "STRATA2")),
    data = adrs
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_proportion_subgroups_df functions as expected when subgroups is NULL.", {
  adrs <- adrs_20

  result <- h_proportion_subgroups_df(
    variables = list(rsp = "rsp", arm = "ARM"),
    data = adrs
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_proportion_subgroups_df works as expected with groups_lists", {
  adrs <- adrs_20

  result <- h_proportion_subgroups_df(
    variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "BMRKR2")),
    data = adrs,
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

testthat::test_that("h_odds_ratio_df functions as expected with valid input and default arguments", {
  result <- h_odds_ratio_df(
    c(TRUE, FALSE, FALSE, TRUE),
    arm = factor(c("A", "A", "B", "B"), levels = c("A", "B"))
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_odds_ratio_df functions as expected with valid input and non-default arguments", {
  adrs <- adrs_100

  result <- h_odds_ratio_df(
    rsp = adrs$rsp,
    arm = adrs$ARM,
    conf_level = 0.9,
    method = "chisq"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_odds_ratio_df functions as expected with strata", {
  adrs <- adrs_100

  # https://github.com/therneau/survival/issues/240
  withr::with_options(
    opts_partial_match_old,
    result <- h_odds_ratio_df(
      rsp = adrs$rsp,
      arm = adrs$ARM,
      strata_data = adrs[, c("STRATA1", "STRATA2")],
      method = "cmh",
      conf_level = 0.9
    )
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_odds_ratio_df functions when 0 obs in one arm", {
  rsp <- c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE)
  arm <- factor(rep("A", 6), levels = c("B", "A"))

  result <- h_odds_ratio_df(
    c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE),
    arm = factor(rep("A", 6), levels = c("B", "A")),
    method = "chisq"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_odds_ratio_subgroups_df functions as expected with valid input and default arguments", {
  adrs <- adrs_100

  result <- h_odds_ratio_subgroups_df(
    variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "STRATA2")),
    data = adrs
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_odds_ratio_subgroups_df functions as expected when subgroups is NULL.", {
  adrs <- adrs_100

  result <- h_odds_ratio_subgroups_df(
    variables = list(rsp = "rsp", arm = "ARM"),
    data = adrs
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_odds_ratio_subgroups_df functions as expected with strata", {
  adrs <- adrs_100

  # https://github.com/therneau/survival/issues/240
  withr::with_options(
    opts_partial_match_old,
    result <- h_odds_ratio_subgroups_df(
      variables = list(
        rsp = "rsp",
        arm = "ARM",
        subgroups = c("SEX", "STRATA2"),
        strata = "STRATA1"
      ),
      data = adrs,
      method = "cmh"
    )
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_odds_ratio_subgroups_df works as expected with groups_lists", {
  adrs <- adrs_20

  result <- h_odds_ratio_subgroups_df(
    variables = list(
      rsp = "rsp",
      arm = "ARM",
      subgroups = c("SEX", "BMRKR2")
    ),
    data = adrs,
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
