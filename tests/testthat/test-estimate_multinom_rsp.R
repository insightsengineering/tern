
test_that("d_onco_rsp_label provide right response labels", {
  rsp <- c("CR", "NE", "PR")
  result <- d_onco_rsp_label(rsp)
  expected <- c(
    CR = "Complete Response (CR)", NE = "Not Evaluable (NE)", PR = "Partial Response (PR)"
  )

  expect_identical(result, expected)
})

# Preparation of a common test case for unit tests.
dta_test <- data.frame(
  USUBJID = paste0("S", 1:12),
  ARM = rep(LETTERS[1:3], each = 4),
  AVAL = c(A = c(1, 1, 1, 1), B = c(0, 0, 1, 1), C = c(0, 0, 0, 0))
)

dta_test$AVALC <- as.factor(c( # nolint
  "Complete Response (CR)", "Partial Response (PR)"
)[dta_test$AVAL + 1])

test_that("h_prop_ci returns right result", {
  result <- h_prop_ci(
    rsp = as.logical(subset(dta_test, ARM == "A")$AVAL), conf_level = 0.95
  )

  expected <- list(
    n_prop = structure(c(4, 1), label = "Responders"),
    prop_ci = structure(
      c(0.875,  1), label = "95% CI for Response Rates (Wald, with correction)"
    )
  )

  expect_identical(result, expected)
})

test_that("s_multinomial_response returns right result", {
  result <- s_multinomial_response(dta_test$AVALC[dta_test$ARM == "A"])

  expected <- list(
    n_prop = structure(c(0, 0), label = "Complete Response (CR)"),
    prop_ci = structure(c(0, 0.125), label = "95% CI (Wald, with correction)"),
    n_prop = structure(c(4, 1), label = "Partial Response (PR)"),
    prop_ci = structure(c(0.875, 1), label = "95% CI (Wald, with correction)")
  )

  expect_identical(result, expected)
})

test_that("estimate_multinomial_response returns right result", {

  lyt <- split_cols_by(lyt = NULL, var = "ARM") %>%
    estimate_multinomial_response(var = "AVALC")
  result <- build_table(lyt = lyt, df = dta_test)
  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "Complete Response (CR)", "95% CI (Wald, with correction)",
      "Partial Response (PR)", "95% CI (Wald, with correction)", "A",
      "0 (0%)", "(0, 0.12)", "4 (100%)", "(0.88, 1)", "B", "2 (50%)",
      "(0, 1)", "2 (50%)", "(0, 1)", "C", "4 (100%)", "(0.88, 1)",
      "0 (0%)", "(0, 0.12)"
    ),
    .Dim = 5:4
  )

  expect_identical(result_matrix, expected_matrix)
})
