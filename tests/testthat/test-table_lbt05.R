adsl <- adsl_raw

get_adlb <- function() {
  adlb <- adlb_raw # nolint
  # Modify ANRIND and create AVALCAT1/PARCAT2
  # PARCAT2 is just used for filtering, but in order to be the
  # filtering as realistic as possible, will create the variable.
  qntls <- adlb %>%
    dplyr::group_by(.data$PARAMCD) %>%
    dplyr::summarise(
      q1 = stats::quantile(.data$AVAL, probs = c(0.1)),
      q2 = stats::quantile(.data$AVAL, probs = c(0.9))
    )

  adlb <- adlb %>%
    dplyr::left_join(qntls, by = "PARAMCD")

  adlb_f <- adlb %>%
    dplyr::group_by(.data$USUBJID, .data$PARAMCD, .data$BASETYPE) %>%
    dplyr::mutate(
      ANRIND = factor(
        dplyr::case_when(
          .data$ANRIND == "LOW" & .data$AVAL <= .data$q1 ~ "LOW LOW",
          .data$ANRIND == "HIGH" & .data$AVAL >= .data$q2 ~ "HIGH HIGH",
          TRUE ~ as.character(ANRIND)
        ),
        levels = c("", "HIGH", "HIGH HIGH", "LOW", "LOW LOW", "NORMAL")
      )
    )
  adlb_f
}

testthat::test_that("LBT05 variant 1 is produced correctly", {
  adlb <- get_adlb()

  avalcat1 <- c("LAST", "REPLICATED", "SINGLE")

  set.seed(1, kind = "Mersenne-Twister")

  adlb <- adlb %>%
    dplyr::mutate(
      AVALCAT1 = factor(
        dplyr::case_when(
          .data$ANRIND %in% c("HIGH HIGH", "LOW LOW") ~
            sample(
              x = avalcat1,
              size = dplyr::n(),
              replace = TRUE,
              prob = c(0.3, 0.6, 0.1)
            ),
          TRUE ~ ""
        ),
        levels = c("", avalcat1)
      ),
      PARCAT2 = factor("LS")
    ) %>%
    dplyr::select(-"q1", -"q2")

  # Let's remove all marked abrnormalities for ALT so that it can be demonstrated that
  # just the `Any Abnormality` row is shown when there is no marked abonormality.
  adlb$ANRIND[adlb$PARAMCD == "ALT"] <- "NORMAL"

  # Preprocessing steps
  adlb <- adlb %>%
    dplyr::filter(.data$ONTRTFL == "Y" & .data$PARCAT2 == "LS" & .data$SAFFL == "Y" & !is.na(.data$AVAL)) %>%
    dplyr::mutate(abn_dir = factor(
      dplyr::case_when(
        ANRIND == "LOW LOW" ~ "Low",
        ANRIND == "HIGH HIGH" ~ "High",
        TRUE ~ ""
      ),
      levels = c("Low", "High")
    ))

  map <- unique(
    adlb[adlb$abn_dir %in% c("Low", "High") & adlb$AVALCAT1 != "", c("PARAMCD", "abn_dir")]
  ) %>%
    lapply(as.character) %>%
    as.data.frame() %>%
    dplyr::arrange(PARAMCD, dplyr::desc(abn_dir)) %>%
    tibble::add_row(PARAMCD = "ALT", abn_dir = "Low")

  lyt <- basic_table() %>%
    split_cols_by("ACTARMCD") %>%
    add_colcounts() %>%
    split_rows_by("PARAMCD", label_pos = "topleft", split_label = "Laboratory Test") %>%
    summarize_num_patients(var = "USUBJID", .stats = "unique_count") %>%
    append_topleft("  Direction of abnormality") %>%
    split_rows_by("abn_dir", split_fun = trim_levels_to_map(map)) %>%
    count_abnormal_by_marked(
      var = "AVALCAT1",
      variables = list(id = "USUBJID", param = "PARAMCD", direction = "abn_dir")
    )

  result <- build_table(lyt, df = adlb, alt_counts_df = adsl)

  all_zero_or_na_not_any <- function(tr) {
    if (!inherits(tr, "TableRow") || inherits(tr, "LabelRow") || obj_label(tr) == "Any Abnormality") {
      return(FALSE)
    }
    rvs <- unlist(unname(row_values(tr)))
    all(is.na(rvs) | rvs == 0 | !is.finite(rvs))
  }

  result <- trim_rows(result, criteria = all_zero_or_na_not_any)

  res <- expect_silent(result)
  expect_snapshot(res)
})

testthat::test_that("LBT05 variant 2 is produced correctly", {
  adlb <- get_adlb()

  avalcat1 <- c("LAST", "REPLICATED", "SINGLE")

  set.seed(1, kind = "Mersenne-Twister")

  adlb <- adlb %>%
    dplyr::mutate(
      AVALCAT1 = factor(
        dplyr::case_when(
          .data$ANRIND %in% c("HIGH HIGH", "LOW LOW") ~
            sample(
              x = avalcat1,
              size = dplyr::n(),
              replace = TRUE,
              prob = c(0.3, 0.6, 0.1)
            ),
          TRUE ~ ""
        ),
        levels = c("", avalcat1)
      ),
      PARCAT2 = factor("LS")
    ) %>%
    dplyr::select(-"q1", -"q2")

  # Let's remove all marked abrnormalities for ALT so that it can be demonstrated that
  # just the `Any Abnormality` row is shown when there is no marked abonormality.
  adlb$ANRIND[adlb$PARAMCD == "ALT"] <- "NORMAL"

  # Preprocessing steps
  adlb <- adlb %>%
    dplyr::filter(.data$ONTRTFL == "Y" & .data$PARCAT2 == "LS" & .data$SAFFL == "Y" & !is.na(.data$AVAL)) %>%
    dplyr::mutate(abn_dir = factor(
      dplyr::case_when(
        ANRIND == "LOW LOW" ~ "Low",
        ANRIND == "HIGH HIGH" ~ "High",
        TRUE ~ ""
      ),
      levels = c("Low", "High")
    ))

  map <- unique(
    adlb[adlb$abn_dir %in% c("Low", "High") & adlb$AVALCAT1 != "", c("PARAMCD", "abn_dir")]
  ) %>%
    lapply(as.character) %>%
    as.data.frame() %>%
    dplyr::arrange(PARAMCD, dplyr::desc(abn_dir)) %>%
    tibble::add_row(PARAMCD = "ALT", abn_dir = "Low")

  lyt <- basic_table() %>%
    split_cols_by("ACTARMCD") %>%
    add_colcounts() %>%
    split_rows_by("PARAMCD", label_pos = "topleft", split_label = "Laboratory Test") %>%
    summarize_num_patients(var = "USUBJID", .stats = "unique_count") %>%
    append_topleft("  Direction of abnormality") %>%
    split_rows_by("abn_dir", split_fun = trim_levels_to_map(map)) %>%
    count_abnormal_by_marked(
      var = "AVALCAT1",
      variables = list(id = "USUBJID", param = "PARAMCD", direction = "abn_dir")
    )

  result <- build_table(lyt, df = adlb, alt_counts_df = adsl)

  res <- expect_silent(result)
  expect_snapshot(res)
})

testthat::test_that("LBT05 variant 4 is produced correctly", {
  adlb <- get_adlb()

  avalcat1 <- c("LAST", "REPLICATED", "SINGLE")

  set.seed(1, kind = "Mersenne-Twister")

  adlb <- adlb %>%
    dplyr::mutate(
      AVALCAT1 = factor(
        dplyr::case_when(
          .data$ANRIND %in% c("HIGH HIGH", "LOW LOW") ~
            sample(
              x = avalcat1,
              size = dplyr::n(),
              replace = TRUE,
              prob = c(0.3, 0.6, 0.1)
            ),
          TRUE ~ ""
        ),
        levels = c("", avalcat1)
      ),
      PARCAT2 = factor("LS")
    ) %>%
    dplyr::select(-"q1", -"q2")

  # Let's remove all marked abrnormalities for ALT so that it can be demonstrated that
  # ALT rows are removed
  adlb$ANRIND[adlb$PARAMCD == "ALT"] <- "NORMAL"

  # Preprocessing steps
  adlb <- adlb %>%
    dplyr::filter(.data$ONTRTFL == "Y" & .data$PARCAT2 == "LS" & .data$SAFFL == "Y" & !is.na(.data$AVAL)) %>%
    dplyr::mutate(abn_dir = factor(
      dplyr::case_when(
        ANRIND == "LOW LOW" ~ "Low",
        ANRIND == "HIGH HIGH" ~ "High",
        TRUE ~ ""
      ),
      levels = c("Low", "High")
    ))

  map <- unique(
    adlb[adlb$abn_dir %in% c("Low", "High") & adlb$AVALCAT1 != "", c("PARAMCD", "abn_dir")]
  ) %>%
    lapply(as.character) %>%
    as.data.frame() %>%
    dplyr::arrange(PARAMCD, dplyr::desc(abn_dir))

  lyt <- basic_table() %>%
    split_cols_by("ACTARMCD") %>%
    add_colcounts() %>%
    split_rows_by(
      "PARAMCD",
      split_fun = trim_levels_in_group("abn_dir", drop_outlevs = TRUE),
      label_pos = "topleft",
      split_label = "Laboratory Test"
    ) %>%
    summarize_num_patients(var = "USUBJID", .stats = "unique_count") %>%
    append_topleft("  Direction of abnormality") %>%
    split_rows_by("abn_dir") %>%
    count_abnormal_by_marked(
      var = "AVALCAT1",
      variables = list(id = "USUBJID", param = "PARAMCD", direction = "abn_dir")
    )

  result <- build_table(lyt, df = adlb, alt_counts_df = adsl) %>%
    prune_table()

  res <- expect_silent(result)
  expect_snapshot(res)
})
