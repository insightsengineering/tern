library(random.cdisc.data)

preprocess_adrs <- function(adrs) {

  adrs_labels <- var_labels(adrs)
  adrs <- adrs %>%
    dplyr::filter(PARAMCD == "BESRSPI") %>%
    dplyr::filter(ARM %in% c("A: Drug X", "B: Placebo")) %>%
    dplyr::slice(1:20) %>%
    droplevels() %>%
    dplyr::mutate(
      # Reorder levels of factor to make the placebo group the reference arm.
      ARM = forcats::fct_relevel(ARM, "B: Placebo"),
      rsp = AVALC == "CR"
    )
  var_labels(adrs) <- c(adrs_labels, "Response")

  adrs
}

test_that("h_proportion_df functions as expected with valid input and default arguments", {

  # Typical case.
  rsp <- c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE)
  arm <- factor(c("A", "B", "A", "B", "A", "A"), levels = c("B", "A"))

  result <- h_proportion_df(rsp = rsp, arm = arm)

  expected <- data.frame(
    arm = factor(c("B", "A"), levels = c("B", "A")),
    n = c(2, 4),
    n_rsp = c(1, 1),
    prop = c(0.5, 0.25),
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected)

  # Edge case: 0 responses in one group.
  rsp <- c(TRUE, FALSE, FALSE, FALSE)
  arm <- factor(c("A", "A", "B", "B"), levels = c("A", "B"))

  result <- h_proportion_df(rsp = rsp, arm = arm)

  expected <- data.frame(
    arm = factor(c("A", "B"), levels = c("A", "B")),
    n = c(2, 2),
    n_rsp = c(1, 0),
    prop = c(0.5, 0),
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected)

})

test_that("h_proportion_df fails with wrong input", {

  expect_error(h_proportion_df(
    rsp = c(TRUE, FALSE, NA),
    arm = factor(c("A", "B", "A"), levels = c("B", "A"))
  ))

})

test_that("h_proportion_subgroups_df functions as expected with valid input and default arguments", {

  adrs <- radrs(cached = TRUE) %>%
    preprocess_adrs()

  result <- h_proportion_subgroups_df(
    variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "STRATA2")),
    data = adrs
  )

  expected <- data.frame(
    arm = factor(rep(c("B: Placebo", "A: Drug X"), 4), levels = c("B: Placebo", "A: Drug X")),
    n = c(3, 5, 4, 8, 2, 9, 5, 4),
    n_rsp = c(1, 5, 3, 6, 2, 8, 2, 3),
    prop = c(0.3333333, 1, 0.75, 0.75, 1, 0.8888889, 0.4, 0.75),
    subgroup = c("F", "F", "M", "M", "S1", "S1", "S2", "S2"),
    var = c(rep("SEX", 4), rep("STRATA2", 4)),
    var_label = c(rep("Sex", 4), rep("Stratification Factor 2", 4)),
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected, tol = 0.000001)

})
