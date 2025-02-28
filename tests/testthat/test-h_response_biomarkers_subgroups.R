# Local data pre-processing
preprocess_adrs <- function(adrs) {
  # Save variable labels before data processing steps.
  adrs_labels <- formatters::var_labels(adrs)
  adrs %>%
    dplyr::filter(PARAMCD == "BESRSPI") %>%
    dplyr::mutate(rsp = with_label(AVALC == "CR", "Response"))
}

adrs_local <- tern_ex_adrs %>%
  preprocess_adrs()

# h_rsp_to_logistic_variables ----

testthat::test_that("h_rsp_to_logistic_variables works as expected", {
  testthat::expect_silent(
    res <- h_rsp_to_logistic_variables(
      variables = list(
        rsp = "RSP",
        covariates = c("A", "B"),
        strata = "D"
      ),
      biomarker = "AGE"
    )
  )

  testthat::expect_snapshot(res)
})

# h_logistic_mult_cont_df ----

testthat::test_that("h_logistic_mult_cont_df works as expected", {
  adrs_f <- adrs_local

  # https://github.com/therneau/survival/issues/240
  withr::with_options(
    opts_partial_match_old,
    testthat::expect_silent(
      res <- h_logistic_mult_cont_df(
        variables = list(
          rsp = "rsp",
          biomarkers = c("BMRKR1", "AGE"),
          covariates = "SEX",
          strata = "STRATA2"
        ),
        data = adrs_f
      )
    )
  )

  testthat::expect_snapshot(res)
})

testthat::test_that("h_logistic_mult_cont_df returns missing values if data is empty (0 rows)", {
  adrs_f <- adrs_local

  testthat::expect_silent(
    res <- h_logistic_mult_cont_df(
      variables = list(
        rsp = "rsp",
        biomarkers = c("BMRKR1", "AGE"),
        covariates = "SEX",
        strata = "STRATA2"
      ),
      data = adrs_f[NULL, ]
    )
  )

  testthat::expect_snapshot(res)
})

testthat::test_that("h_logistic_mult_cont_df also works with response not being called rsp", {
  adrs_f <- adrs_local %>%
    dplyr::rename(RESP = rsp)

  # https://github.com/therneau/survival/issues/240
  withr::with_options(
    opts_partial_match_old,
    testthat::expect_silent(
      res <- h_logistic_mult_cont_df(
        variables = list(
          rsp = "RESP",
          biomarkers = c("BMRKR1", "AGE"),
          covariates = "SEX",
          strata = "STRATA2"
        ),
        data = adrs_f
      )
    )
  )

  testthat::expect_snapshot(res)
})
