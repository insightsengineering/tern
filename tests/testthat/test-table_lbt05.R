library(scda)
library(dplyr)

get_adlb <- function() {
  adlb <- synthetic_cdisc_data("rcd_2022_02_28")$adlb # nolintr
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
  adsl <- synthetic_cdisc_data("rcd_2022_02_28")$adsl

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
    dplyr::select(-.data$q1, -.data$q2)

  # Let's remove all marked abrnormalities for ALT so that it can be demonstrated that
  # just the `Any Abnormality` row is shown when there is no marked abonormality.
  adlb$ANRIND[adlb$PARAMCD == "ALT"] <- "NORMAL"

  # Preprocessing steps
  adlb <- adlb %>%
    dplyr::filter(.data$ONTRTFL == "Y" & .data$PARCAT2 == "LS" & .data$SAFFL == "Y" & !is.na(.data$AVAL)) %>%
    dplyr::mutate(abn_dir = factor(dplyr::case_when(
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

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "Laboratory Test", "  Direction of abnormality",
      "ALT (n)", "Low", "Any Abnormality", "CRP (n)", "Low", "Single, not last",
      "Last or replicated", "Any Abnormality", "High", "Single, not last",
      "Last or replicated", "Any Abnormality", "IGA (n)", "Low", "Single, not last",
      "Last or replicated", "Any Abnormality", "High", "Single, not last",
      "Last or replicated", "Any Abnormality", "ARM A", "(N=134)",
      "134", "", "0", "134", "", "2 (1.5%)", "10 (7.5%)", "12 (9%)",
      "", "1 (0.7%)", "10 (7.5%)", "11 (8.2%)", "134", "", "2 (1.5%)",
      "5 (3.7%)", "7 (5.2%)", "", "0", "4 (3%)", "4 (3%)", "ARM B",
      "(N=134)", "134", "", "0", "134", "", "0", "7 (5.2%)", "7 (5.2%)",
      "", "2 (1.5%)", "9 (6.7%)", "11 (8.2%)", "134", "", "1 (0.7%)",
      "8 (6%)", "9 (6.7%)", "", "0", "9 (6.7%)", "9 (6.7%)", "ARM C",
      "(N=132)", "132", "", "0", "132", "", "0", "7 (5.3%)", "7 (5.3%)",
      "", "1 (0.8%)", "12 (9.1%)", "13 (9.8%)", "132", "", "1 (0.8%)",
      "10 (7.6%)", "11 (8.3%)", "", "1 (0.8%)", "5 (3.8%)", "6 (4.5%)"
    ),
    .Dim = c(23L, 4L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("LBT05 variant 2 is produced correctly", {
  adlb <- get_adlb()
  adsl <- synthetic_cdisc_data("rcd_2022_02_28")$adsl

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
    dplyr::select(-.data$q1, -.data$q2)

  # Let's remove all marked abrnormalities for ALT so that it can be demonstrated that
  # just the `Any Abnormality` row is shown when there is no marked abonormality.
  adlb$ANRIND[adlb$PARAMCD == "ALT"] <- "NORMAL"

  # Preprocessing steps
  adlb <- adlb %>%
    dplyr::filter(.data$ONTRTFL == "Y" & .data$PARCAT2 == "LS" & .data$SAFFL == "Y" & !is.na(.data$AVAL)) %>%
    dplyr::mutate(abn_dir = factor(dplyr::case_when(
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
  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "Laboratory Test", "  Direction of abnormality",
      "ALT (n)", "Low", "Single, not last", "Last or replicated", "Any Abnormality",
      "CRP (n)", "Low", "Single, not last", "Last or replicated", "Any Abnormality",
      "High", "Single, not last", "Last or replicated", "Any Abnormality",
      "IGA (n)", "Low", "Single, not last", "Last or replicated", "Any Abnormality",
      "High", "Single, not last", "Last or replicated", "Any Abnormality",
      "ARM A", "(N=134)", "134", "", "0", "0", "0", "134", "", "2 (1.5%)",
      "10 (7.5%)", "12 (9%)", "", "1 (0.7%)", "10 (7.5%)", "11 (8.2%)",
      "134", "", "2 (1.5%)", "5 (3.7%)", "7 (5.2%)", "", "0", "4 (3%)",
      "4 (3%)", "ARM B", "(N=134)", "134", "", "0", "0", "0", "134",
      "", "0", "7 (5.2%)", "7 (5.2%)", "", "2 (1.5%)", "9 (6.7%)",
      "11 (8.2%)", "134", "", "1 (0.7%)", "8 (6%)", "9 (6.7%)", "",
      "0", "9 (6.7%)", "9 (6.7%)", "ARM C", "(N=132)", "132", "", "0",
      "0", "0", "132", "", "0", "7 (5.3%)", "7 (5.3%)", "", "1 (0.8%)",
      "12 (9.1%)", "13 (9.8%)", "132", "", "1 (0.8%)", "10 (7.6%)",
      "11 (8.3%)", "", "1 (0.8%)", "5 (3.8%)", "6 (4.5%)"
    ),
    .Dim = c(25L, 4L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("LBT05 variant 4 is produced correctly", {
  adlb <- get_adlb()
  adsl <- synthetic_cdisc_data("rcd_2022_02_28")$adsl

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
    dplyr::select(-.data$q1, -.data$q2)

  # Let's remove all marked abrnormalities for ALT so that it can be demonstrated that
  # ALT rows are removed
  adlb$ANRIND[adlb$PARAMCD == "ALT"] <- "NORMAL"

  # Preprocessing steps
  adlb <- adlb %>%
    dplyr::filter(.data$ONTRTFL == "Y" & .data$PARCAT2 == "LS" & .data$SAFFL == "Y" & !is.na(.data$AVAL)) %>%
    dplyr::mutate(abn_dir = factor(dplyr::case_when(
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

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "Laboratory Test", "  Direction of abnormality",
      "CRP (n)", "Low", "Single, not last", "Last or replicated", "Any Abnormality",
      "High", "Single, not last", "Last or replicated", "Any Abnormality",
      "IGA (n)", "Low", "Single, not last", "Last or replicated", "Any Abnormality",
      "High", "Single, not last", "Last or replicated", "Any Abnormality",
      "ARM A", "(N=134)", "134", "", "2 (1.5%)", "10 (7.5%)", "12 (9%)",
      "", "1 (0.7%)", "10 (7.5%)", "11 (8.2%)", "134", "", "2 (1.5%)",
      "5 (3.7%)", "7 (5.2%)", "", "0", "4 (3%)", "4 (3%)", "ARM B",
      "(N=134)", "134", "", "0", "7 (5.2%)", "7 (5.2%)", "", "2 (1.5%)",
      "9 (6.7%)", "11 (8.2%)", "134", "", "1 (0.7%)", "8 (6%)", "9 (6.7%)",
      "", "0", "9 (6.7%)", "9 (6.7%)", "ARM C", "(N=132)", "132", "",
      "0", "7 (5.3%)", "7 (5.3%)", "", "1 (0.8%)", "12 (9.1%)", "13 (9.8%)",
      "132", "", "1 (0.8%)", "10 (7.6%)", "11 (8.3%)", "", "1 (0.8%)",
      "5 (3.8%)", "6 (4.5%)"
    ),
    .Dim = c(20L, 4L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
