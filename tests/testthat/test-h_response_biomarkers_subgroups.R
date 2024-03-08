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

# h_tab_rsp_one_biomarker ----

testthat::test_that("h_tab_rsp_one_biomarker works as expected", {
  df <- data.frame(
    n_tot = c(48L, 48L),
    n_rsp = c(24L, 24L),
    prop = c(0.5, 0.5),
    or = c(0.992727618706316, 1.00485769099575),
    lcl = c(0.859391304891713, 0.950491104268725),
    ucl = c(1.14675133356916, 1.06233396043214),
    conf_level = c(0.95, 0.95),
    pval = c(0.920991170690111, 0.864415775291559),
    pval_label = c("p-value (Wald)", "p-value (Wald)"),
    subgroup = c("All patients", "All patients"),
    row_type = c("content", "content"),
    var = c("ALL", "ALL"),
    var_label = c("All patients", "All patients")
  )
  testthat::expect_silent(
    res <- h_tab_rsp_one_biomarker(
      df = df,
      vars = c("n_tot", "or", "ci")
    )
  )

  testthat::expect_snapshot(res)
})
