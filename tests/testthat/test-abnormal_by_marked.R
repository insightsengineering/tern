library(scda)
library(rtables)
library(dplyr)

adlb_raw <- local({
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
})


testthat::test_that("s_count_abnormal_by_marked works as expected", {
  adlb <- adlb_raw
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
  # Preprocessing steps
  adlb_f <- adlb %>%
    dplyr::filter(.data$ONTRTFL == "Y" & .data$PARCAT2 == "LS" & .data$SAFFL == "Y" & !is.na(.data$AVAL)) %>%
    dplyr::mutate(abn_dir = factor(dplyr::case_when(
      ANRIND == "LOW LOW" ~ "Low",
      ANRIND == "HIGH HIGH" ~ "High",
      TRUE ~ ""
    )))

  adlb_crp <- adlb_f %>%
    dplyr::filter(PARAMCD == "CRP") %>%
    droplevels()
  full_parent_df <- list(adlb_crp, "not_needed")
  cur_col_subset <- list(adlb_crp$ARMCD == "ARM A", "not_needed")

  spl_context <- data.frame(
    split = c("PARAMCD", "abn_dir"),
    full_parent_df = I(full_parent_df),
    cur_col_subset = I(cur_col_subset)
  )

  result <- s_count_abnormal_by_marked(
    df = adlb_crp %>% dplyr::filter(ARMCD == "ARM A" & abn_dir == "Low") %>% droplevels(),
    .spl_context = spl_context,
    .var = "AVALCAT1",
    variables = list(id = "USUBJID", param = "PARAMCD", direction = "abn_dir")
  )

  expected <- list(count_fraction = list(
    `Single, not last` = c(2.00000000, 0.01492537),
    `Last or replicated` = c(10.00000000, 0.07462687),
    `Any Abnormality` = c(12.00000000, 0.08955224)
  ))
  testthat::expect_equal(result, expected, tolerance = 0.000001)
})


testthat::test_that("s_count_abnormal_by_marked works as expected", {
  adlb <- adlb_raw
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
  # Preprocessing steps
  adlb_f <- adlb %>%
    dplyr::filter(.data$ONTRTFL == "Y" & .data$PARCAT2 == "LS" & .data$SAFFL == "Y" & !is.na(.data$AVAL)) %>%
    dplyr::mutate(abn_dir = factor(dplyr::case_when(
      ANRIND == "LOW LOW" ~ "Low",
      ANRIND == "HIGH HIGH" ~ "High",
      TRUE ~ ""
    )))

  adlb_crp <- adlb_f %>%
    dplyr::filter(PARAMCD == "CRP") %>%
    droplevels()
  full_parent_df <- list(adlb_crp, "not_needed")
  cur_col_subset <- list(adlb_crp$ARMCD == "ARM A", "not_needed")

  spl_context <- data.frame(
    split = c("PARAMCD", "abn_dir"),
    full_parent_df = I(full_parent_df),
    cur_col_subset = I(cur_col_subset)
  )

  result <- s_count_abnormal_by_marked(
    df = adlb_crp %>% dplyr::filter(ARMCD == "ARM A" & abn_dir == "High") %>% droplevels(),
    .spl_context = spl_context,
    .var = "AVALCAT1",
    variables = list(id = "USUBJID", param = "PARAMCD", direction = "abn_dir")
  )

  expected <- list(count_fraction = list(
    `Single, not last` = c(1.000000000, 0.007462687),
    `Last or replicated` = c(10.00000000, 0.07462687),
    `Any Abnormality` = c(11.00000000, 0.08208955)
  ))
  testthat::expect_equal(result, expected, tolerance = 0.000001)
})


testthat::test_that("s_count_abnormal_by_marked returns an error when `abn_dir` contains
          two direction values", {
  adlb <- adlb_raw
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
  # Preprocessing steps
  adlb_f <- adlb %>%
    dplyr::filter(.data$ONTRTFL == "Y" & .data$PARCAT2 == "LS" & .data$SAFFL == "Y" & !is.na(.data$AVAL)) %>%
    dplyr::mutate(abn_dir = factor(dplyr::case_when(
      ANRIND == "LOW LOW" ~ "Low",
      ANRIND == "HIGH HIGH" ~ "High",
      TRUE ~ ""
    )))

  adlb_crp <- adlb_f %>%
    dplyr::filter(PARAMCD == "CRP") %>%
    droplevels()
  full_parent_df <- list(adlb_crp, "not_needed")
  cur_col_subset <- list(adlb_crp$ARMCD == "ARM A", "not_needed")

  spl_context <- data.frame(
    split = c("PARAMCD", "abn_dir"),
    full_parent_df = I(full_parent_df),
    cur_col_subset = I(cur_col_subset)
  )

  testthat::expect_error(s_count_abnormal_by_marked(
    df = adlb_crp,
    .spl_context = spl_context,
    .var = "AVALCAT1",
    variables = list(id = "USUBJID", param = "PARAMCD", direction = "abn_dir")
  ))
})


testthat::test_that("count_abnormal_by_marked works as expected", {
  adlb <- adlb_raw

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
  # Preprocessing steps
  adlb_f <- adlb %>%
    dplyr::filter(.data$ONTRTFL == "Y" & .data$PARCAT2 == "LS" & .data$SAFFL == "Y" & !is.na(.data$AVAL)) %>%
    dplyr::mutate(abn_dir = factor(dplyr::case_when(
      ANRIND == "LOW LOW" ~ "Low",
      ANRIND == "HIGH HIGH" ~ "High",
      TRUE ~ ""
    )))

  adlb_f <- adlb_f %>%
    dplyr::filter(PARAMCD == "CRP") %>%
    droplevels()

  map <- unique(
    adlb_f[
      adlb_f$abn_dir %in% c("Low", "High") & adlb_f$AVALCAT1 != "" & adlb_f$PARAMCD == "CRP",
      c("PARAMCD", "abn_dir")
    ]
  ) %>%
    lapply(as.character) %>%
    as.data.frame() %>%
    dplyr::arrange(PARAMCD, !dplyr::desc(abn_dir))


  result <- basic_table() %>%
    split_cols_by("ARMCD") %>%
    split_rows_by("PARAMCD") %>%
    summarize_num_patients(var = "USUBJID", .stats = "unique_count") %>%
    split_rows_by("abn_dir", split_fun = trim_levels_to_map(map)) %>%
    count_abnormal_by_marked(
      var = "AVALCAT1",
      variables = list(id = "USUBJID", param = "PARAMCD", direction = "abn_dir")
    ) %>%
    build_table(df = adlb_f)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "CRP (n)", "Low", "Single, not last", "Last or replicated",
      "Any Abnormality", "High", "Single, not last", "Last or replicated",
      "Any Abnormality", "ARM A", "134", "", "2 (1.5%)", "10 (7.5%)",
      "12 (9%)", "", "1 (0.7%)", "10 (7.5%)", "11 (8.2%)", "ARM B",
      "134", "", "0", "7 (5.2%)", "7 (5.2%)", "", "2 (1.5%)", "9 (6.7%)",
      "11 (8.2%)", "ARM C", "132", "", "0", "7 (5.3%)", "7 (5.3%)",
      "", "1 (0.8%)", "12 (9.1%)", "13 (9.8%)"
    ),
    .Dim = c(10L, 4L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
