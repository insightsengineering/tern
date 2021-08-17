library(scda)
test_that("h_stack_by_baskets returns the correct dataframe", {

  adae <- synthetic_cdisc_data("rcd_2021_05_05")$adae[1:20, ]

  result <- h_stack_by_baskets(df = adae)
  attributes(result)$.internal.selfref <- NULL #nolintr

  expected <- structure(list(
    STUDYID = structure(
      c("AB12345", "AB12345", "AB12345", "AB12345"),
      label = "Study Identifier"),
    USUBJID = structure(
      c("AB12345-BRA-1-id-141", "AB12345-BRA-1-id-141", "AB12345-BRA-1-id-141", "AB12345-BRA-1-id-265"),
      label = "Unique Subject Identifier"),
    ASTDTM = structure(
      c(1621814400, 1638230400, 1650499200, 1605312000),
      tzone = "",
      label = "Analysis Start Datetime",
      class = c("POSIXct", "POSIXt")),
    AESEQ = structure(
      c(2L, 3L, 5L, 4L),
      label = "Sponsor-Defined Identifier"),
    AETERM = structure(
      c(10L, 1L, 1L, 6L),
      .Label = c(
        "trm A.1.1.1.1", "trm A.1.1.1.2", "trm B.1.1.1.1",
        "trm B.2.1.2.1", "trm B.2.2.3.1", "trm C.1.1.1.3",
        "trm C.2.1.2.1", "trm D.1.1.1.1", "trm D.1.1.4.2", "trm D.2.1.5.3"),
      label = c("Reported Term for the Adverse Event"), class = "factor"),
    SMQ = structure(
      c("D.2.1.5.3/A.1.1.1.1 AESI", "D.2.1.5.3/A.1.1.1.1 AESI",
        "D.2.1.5.3/A.1.1.1.1 AESI", "C.1.1.1.3/B.2.2.3.1 AESI(BROAD)"),
      label = "Standardized MedDRA Query")),
    row.names = 1:4,
    class = c("tbl_df", "tbl", "data.frame"))
  expect_identical(result, expected)
})

test_that(
  "h_stack_by_baskets fails when selecting Standardized/Customized query names
  that do not start with 'SMQ' or 'CQ' ", {

    adae <- synthetic_cdisc_data("rcd_2021_05_05")$adae[1:20, ]

    expect_error(
      h_stack_by_baskets(
        df = adae,
        baskets = c("NOT_SMQ01NAM")
      )
    )
  })

test_that(
  "h_stack_by_baskets fails when selecting Standardized/Customized
  query names that do not end with 'NAM' ", {

    adae <- synthetic_cdisc_data("rcd_2021_05_05")$adae[1:20, ]

    expect_error(
      h_stack_by_baskets(
        df = adae,
        baskets = c("SMQ01NAM_NOT")
      )
    )

  })

test_that(
  "h_stack_by_baskets returns an empty dataframe with desired variables when there are no
  adverse events falling within any of the baskets selected", {

    adae <- synthetic_cdisc_data("rcd_2021_05_05")$adae[1:20, ]
    baskets <- grep("^(SMQ|CQ).*(NAM|SC)$", names(adae), value = TRUE)

    # data switching to logical after this NA assignment.
    adae[, baskets] <- NA
    # so will reconvert them to character
    adae[baskets] <- lapply(adae[baskets], as.character)
    result <- h_stack_by_baskets(
      df = adae
    )
    result_nrow <- nrow(result)
    expected_nrow <- 0L

    expect_identical(result_nrow, expected_nrow)

    result_names <- names(result)
    expected_names <- c("STUDYID", "USUBJID", "ASTDTM", "AESEQ", "AETERM", "SMQ")

    expect_identical(result_names, expected_names)

    result_var_labels <- var_labels(result)
    expected_var_labels <- c(
      STUDYID = "Study Identifier", USUBJID = "Unique Subject Identifier", ASTDTM = "Analysis Start Datetime",
      AESEQ = "Sponsor-Defined Identifier", AETERM = "Reported Term for the Adverse Event",
      SMQ  = "Standardized MedDRA Query")
    expect_identical(result_var_labels, expected_var_labels)
  })
