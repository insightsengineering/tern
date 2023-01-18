# Test the single variant for EGT05_QTCAT

adsl <- adsl_raw

get_adeg <- function() {
  adeg <- adeg_raw
  adeg_labels <- formatters::var_labels(adeg)

  adeg_f <- adeg %>%
    dplyr::filter(PARAMCD == "QT" & ANL01FL == "Y") %>%
    # Categorize AVAL and CHG
    dplyr::mutate(
      AVALCAT1 = dplyr::case_when(
        AVAL <= 450 ~ "<=450 msec",
        AVAL <= 480 ~ ">450 to <=480 msec",
        AVAL <= 500 ~ ">480 to <= 500 msec",
        AVAL > 500 ~ ">500 msec"
      ),
      CHGCAT1 = dplyr::case_when(
        CHG <= 30 ~ "<=30 msec",
        CHG <= 60 ~ ">30 to <=60 msec",
        CHG > 60 ~ ">60 msec"
      )
    )

  adeg_f$AVALCAT1 <- factor( # nolint snake_case
    adeg_f$AVALCAT1,
    levels = c("<=450 msec", ">450 to <=480 msec", ">480 to <= 500 msec", ">500 msec")
  )

  adeg_f$CHGCAT1 <- factor( # nolint snake_case
    adeg_f$CHGCAT1,
    levels = c("<=30 msec", ">30 to <=60 msec", ">60 msec")
  )

  formatters::var_labels(adeg_f) <- c(adeg_labels, "AVALCAT1" = "Value at Visit", "CHGCAT1" = "Change from Baseline")

  adeg_f <- df_explicit_na(adeg_f)
  adeg_f
}

testthat::test_that("EGT05_QTCAT default variant is produced correctly", {
  adeg <- get_adeg()

  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    split_rows_by("AVISIT", split_label = "Visit", child_labels = "default", label_pos = "visible") %>%
    summarize_vars(
      vars = c("AVALCAT1", "CHGCAT1"),
      var_labels = c("Value at Visit", "Change from Baseline")
    ) %>%
    build_table(df = adeg, alt_counts_df = adsl) %>%
    prune_table()

  res <- expect_silent(result)
  expect_snapshot(res)
})
