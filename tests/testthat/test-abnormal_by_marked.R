# Local data pre-processing
adlb_local <- local({
  # Modify ANRIND and create AVALCAT1/PARCAT2
  # PARCAT2 is just used for filtering, but in order to be the
  # filtering as realistic as possible, will create the variable.
  qntls <- tern_ex_adlb %>%
    dplyr::group_by(.data$PARAMCD) %>%
    dplyr::summarise(
      q1 = stats::quantile(.data$AVAL, probs = c(0.1)),
      q2 = stats::quantile(.data$AVAL, probs = c(0.9))
    )

  tern_ex_adlb %>%
    dplyr::left_join(qntls, by = "PARAMCD") %>%
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
})

testthat::test_that("s_count_abnormal_by_marked works as expected", {
  avalcat1 <- c("LAST", "REPLICATED", "SINGLE")

  set.seed(1, kind = "Mersenne-Twister")

  adlb_local <- adlb_local %>%
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
  # Preprocessing steps
  adlb_f <- adlb_local %>%
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_count_abnormal_by_marked works as expected", {
  avalcat1 <- c("LAST", "REPLICATED", "SINGLE")

  set.seed(1, kind = "Mersenne-Twister")

  adlb_local <- adlb_local %>%
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
  # Preprocessing steps
  adlb_f <- adlb_local %>%
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_count_abnormal_by_marked returns an error when `abn_dir` contains two direction values", {
  avalcat1 <- c("LAST", "REPLICATED", "SINGLE")

  set.seed(1, kind = "Mersenne-Twister")

  adlb_local <- adlb_local %>%
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
  # Preprocessing steps
  adlb_f <- adlb_local %>%
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

  testthat::expect_error(
    s_count_abnormal_by_marked(
      df = adlb_crp,
      .spl_context = spl_context,
      .var = "AVALCAT1",
      variables = list(id = "USUBJID", param = "PARAMCD", direction = "abn_dir")
    )
  )
})

testthat::test_that("count_abnormal_by_marked works as expected", {
  avalcat1 <- c("LAST", "REPLICATED", "SINGLE")

  set.seed(1, kind = "Mersenne-Twister")

  adlb_local <- adlb_local %>%
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
  # Preprocessing steps
  adlb_f <- adlb_local %>%
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

  # fix for update in Rtables tern#593 (NA alternative -> " ")
  lev_v <- levels(adlb_f$abn_dir)
  levels(adlb_f$abn_dir)[sapply(lev_v, nchar) == 0] <- "NA"

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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
