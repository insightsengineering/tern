# Local data pre-processing
adae_local <- tern_ex_adae[1:20, ] %>% df_explicit_na()
adae_local[1, ] <- NA

testthat::test_that("h_stack_by_baskets returns the correct dataframe", {
  result <- h_stack_by_baskets(df = adae_local) %>% data.frame()

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that(
  "h_stack_by_baskets fails when selecting Standardized/Customized query names that do not start with 'SMQ' or 'CQ' ",
  code = {
    testthat::expect_error(
      h_stack_by_baskets(
        df = adae_local,
        baskets = c("NOT_SMQ01NAM")
      )
    )
  }
)

testthat::test_that(
  "h_stack_by_baskets fails when selecting Standardized/Customized query names that do not end with 'NAM' ",
  code = {
    testthat::expect_error(
      h_stack_by_baskets(
        df = adae_local,
        baskets = c("SMQ01NAM_NOT")
      )
    )
  }
)

testthat::test_that(
  paste(
    "h_stack_by_baskets returns an empty dataframe with desired variables and labels when there are no",
    "adverse events falling within any of the baskets selected"
  ),
  code = {
    adae <- adae_local
    baskets <- grep("^(SMQ|CQ).*(NAM)$", names(adae), value = TRUE)
    adae[, baskets] <- "<Missing>"
    result <- h_stack_by_baskets(df = adae)

    res <- testthat::expect_silent(nrow(result))
    testthat::expect_snapshot(res)

    res <- testthat::expect_silent(names(result))
    testthat::expect_snapshot(res)

    res <- testthat::expect_silent(formatters::var_labels(result))
    testthat::expect_snapshot(res)

    res <- testthat::expect_silent(levels(result$SMQ))
    testthat::expect_snapshot(res)
  }
)

testthat::test_that(
  "The levels of the SMQ column does also include the options from aag_summary not observed in ADAE",
  code = {
    baskets <- grep("^(SMQ|CQ).*(NAM)$", names(adae_local), value = TRUE)

    aag_summary <- data.frame(
      basket = c("CQ01NAM", "CQ02NAM", "SMQ01NAM", "SMQ02NAM"),
      basket_name = c(
        "D.2.1.5.3/A.1.1.1.1 AESI", "level1_not_in_adae",
        "C.1.1.1.3/B.2.2.3.1 AESI(BROAD)", "level2_not_in_adae"
      )
    )
    suppressWarnings(result <- h_stack_by_baskets(df = adae_local, aag_summary = aag_summary))

    res <- testthat::expect_silent(levels(result$SMQ))
    testthat::expect_snapshot(res)
  }
)
