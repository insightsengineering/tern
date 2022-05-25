library(scda)
testthat::test_that("h_stack_by_baskets returns the correct dataframe", {
  adae <- synthetic_cdisc_data("rcd_2022_02_28")$adae[1:20, ] %>% df_explicit_na()
  result <- h_stack_by_baskets(df = adae)

  expected <-
    structure(
      list(
        STUDYID = structure(
          c(1L, 1L, 1L, 1L),
          .Label = "AB12345",
          class = "factor",
          label = c(STUDYID = "Study Identifier")
        ),
        USUBJID = structure(
          c(2L, 2L, 2L, 4L),
          .Label = c(
            "AB12345-BRA-1-id-134",
            "AB12345-BRA-1-id-141",
            "AB12345-BRA-1-id-236",
            "AB12345-BRA-1-id-265",
            "AB12345-BRA-1-id-42"
          ),
          class = "factor",
          label = c(USUBJID = "Unique Subject Identifier")
        ),
        ASTDTM = structure(
          c(1621814400, 1638230400, 1650499200, 1605312000),
          tzone = "",
          label = c(ASTDTM = "Analysis Start Datetime"),
          class = c("POSIXct", "POSIXt")
        ),
        AEDECOD = structure(
          c(10L, 1L, 1L, 6L),
          .Label = c(
            "dcd A.1.1.1.1",
            "dcd A.1.1.1.2",
            "dcd B.1.1.1.1",
            "dcd B.2.1.2.1",
            "dcd B.2.2.3.1",
            "dcd C.1.1.1.3",
            "dcd C.2.1.2.1",
            "dcd D.1.1.1.1",
            "dcd D.1.1.4.2",
            "dcd D.2.1.5.3"
          ),
          class = "factor",
          label = c(AEDECOD = "Dictionary-Derived Term")
        ),
        AESEQ = structure(c(2L, 3L, 5L, 4L), label = c(AESEQ = "Sponsor-Defined Identifier")),
        SMQ = structure(
          c(2L, 2L, 2L, 1L),
          .Label = c("C.1.1.1.3/B.2.2.3.1 AESI(BROAD)", "D.2.1.5.3/A.1.1.1.1 AESI", "SMQ02NAM"),
          class = "factor",
          label = structure("Standardized MedDRA Query", .Names = "")
        )
      ),
      row.names = c(NA, -4L),
      class = c("tbl_df", "tbl", "data.frame")
    )
  testthat::expect_identical(result, expected)
})

testthat::test_that(
  "h_stack_by_baskets fails when selecting Standardized/Customized query names
  that do not start with 'SMQ' or 'CQ' ",
  code = {
    adae <- synthetic_cdisc_data("rcd_2022_02_28")$adae[1:20, ] %>% df_explicit_na()

    testthat::expect_error(
      h_stack_by_baskets(
        df = adae,
        baskets = c("NOT_SMQ01NAM")
      )
    )
  }
)

testthat::test_that(
  "h_stack_by_baskets fails when selecting Standardized/Customized
  query names that do not end with 'NAM' ",
  code = {
    adae <- synthetic_cdisc_data("rcd_2022_02_28")$adae[1:20, ] %>% df_explicit_na()

    testthat::expect_error(
      h_stack_by_baskets(
        df = adae,
        baskets = c("SMQ01NAM_NOT")
      )
    )
  }
)

testthat::test_that(
  "h_stack_by_baskets returns an empty dataframe with desired variables and labels when there are no
  adverse events falling within any of the baskets selected",
  code = {
    adae <- synthetic_cdisc_data("rcd_2022_02_28")$adae[1:20, ] %>% df_explicit_na()
    baskets <- grep("^(SMQ|CQ).*(NAM)$", names(adae), value = TRUE)

    adae[, baskets] <- "<Missing>"
    result <- h_stack_by_baskets(
      df = adae
    )
    result_nrow <- nrow(result)
    expected_nrow <- 0L

    testthat::expect_identical(result_nrow, expected_nrow)

    result_names <- names(result)
    expected_names <- c("STUDYID", "USUBJID", "ASTDTM", "AEDECOD", "AESEQ", "SMQ")

    testthat::expect_identical(result_names, expected_names)

    result_var_labels <- formatters::var_labels(result)
    expected_var_labels <- c(
      STUDYID = "Study Identifier", USUBJID = "Unique Subject Identifier", ASTDTM = "Analysis Start Datetime",
      AEDECOD = "Dictionary-Derived Term", AESEQ = "Sponsor-Defined Identifier",
      SMQ = "Standardized MedDRA Query"
    )
    testthat::expect_identical(result_var_labels, expected_var_labels)

    result_smq_levels <- levels(result$SMQ)
    expected_smq_levels <- c("CQ01NAM", "SMQ01NAM", "SMQ02NAM")
    testthat::expect_identical(result_smq_levels, expected_smq_levels)
  }
)

testthat::test_that(
  "The levels of the SMQ column does also
  include the options from aag_summary not observed in ADAE",
  code = {
    adae <- synthetic_cdisc_data("rcd_2022_02_28")$adae[1:20, ] %>% df_explicit_na()
    baskets <- grep("^(SMQ|CQ).*(NAM)$", names(adae), value = TRUE)

    aag_summary <- data.frame(
      basket = c("CQ01NAM", "CQ02NAM", "SMQ01NAM", "SMQ02NAM"),
      basket_name = c(
        "D.2.1.5.3/A.1.1.1.1 AESI", "level1_not_in_adae",
        "C.1.1.1.3/B.2.2.3.1 AESI(BROAD)", "level2_not_in_adae"
      )
    )

    result <- h_stack_by_baskets(df = adae, aag_summary = aag_summary)

    result_levels <- levels(result$SMQ)
    expected_levels <- c(
      "C.1.1.1.3/B.2.2.3.1 AESI(BROAD)", "D.2.1.5.3/A.1.1.1.1 AESI",
      "level1_not_in_adae", "level2_not_in_adae"
    )
    testthat::expect_identical(result_levels, expected_levels)
  }
)
