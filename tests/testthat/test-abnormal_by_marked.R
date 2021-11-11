library(scda)
library(rtables)
library(dplyr)

adlb_raw <- local({
  adlb <- synthetic_cdisc_data("rcd_2021_05_05")$adlb # nolintr
  # Modify ANRIND and create AVALCAT1/PARCAT2
  # PARCAT2 is just used for filtering, but in order to be the
  # filtering as realistic as possible, will create the variable.
  qntls <- adlb %>%
    group_by(.data$PARAMCD) %>%
    summarise(
      q1 = quantile(.data$AVAL, probs = c(0.1)),
      q2 = quantile(.data$AVAL, probs = c(0.9))
    )

  adlb <- adlb %>%
    left_join(qntls, by = "PARAMCD")

  adlb_f <- adlb %>%
    group_by(.data$USUBJID, .data$PARAMCD, .data$BASETYPE) %>%
    mutate(
      ANRIND = factor(
        case_when(
          .data$ANRIND == "LOW" & .data$AVAL <= .data$q1 ~ "LOW LOW",
          .data$ANRIND == "HIGH" & .data$AVAL >= .data$q2 ~ "HIGH HIGH",
          TRUE ~ as.character(ANRIND)
        ),
        levels = c("", "HIGH", "HIGH HIGH", "LOW", "LOW LOW", "NORMAL")
      ))
  adlb_f
})


test_that("s_count_abnormal_by_marked works as expected", {
  adlb <- adlb_raw
  avalcat1 <- c("LAST", "REPLICATED", "SINGLE")

  set.seed(1, kind = "Mersenne-Twister")

  adlb <- adlb %>% mutate(
    AVALCAT1 = factor(
      case_when(
        .data$ANRIND %in% c("HIGH HIGH", "LOW LOW") ~
          sample(
            x = avalcat1,
            size = n(),
            replace = TRUE,
            prob = c(0.3, 0.6, 0.1)
          ),
        TRUE ~ ""
      ),
      levels = c("", avalcat1)
    ),
    PARCAT2 = factor("LS")
  ) %>%
    select(-.data$q1, -.data$q2)
  #Preprocessing steps
  adlb_f <- adlb %>%
    filter(.data$ONTRTFL == "Y" & .data$PARCAT2 == "LS" & .data$SAFFL == "Y" & !is.na(.data$AVAL)) %>%
    mutate(abn_dir = factor(case_when(
      ANRIND == "LOW LOW" ~ "Low",
      ANRIND == "HIGH HIGH" ~ "High",
      TRUE ~ ""
      )
      )
      )

  adlb_crp <- adlb_f %>% filter(PARAMCD == "CRP") %>% droplevels()
  full_parent_df <- list(adlb_crp, "not_needed")
  cur_col_subset <- list(adlb_crp$ARMCD == "ARM A", "not_needed")

  spl_context <- data.frame(
    split = c("PARAMCD", "abn_dir"),
    full_parent_df = I(full_parent_df),
    cur_col_subset = I(cur_col_subset)
  )

  result <- s_count_abnormal_by_marked(
    df = adlb_crp %>% filter(ARMCD == "ARM A") %>% droplevels(),
    .spl_context = spl_context,
    .var = "AVALCAT1",
    abnormal = c(Low = "Low"),
    variables = list(id = "USUBJID", param = "PARAMCD", direction = "abn_dir")
    )

  expected <- list(count_fraction = list(
    `Single, not last` = c(2.00000000, 0.01492537),
    `Last or replicated` = c(10.00000000, 0.07462687),
    `Any Abnormality` = c(12.00000000, 0.08955224)
  ))
  expect_equal(result, expected, tolerance = 0.000001)
})


test_that("s_count_abnormal_by_marked works as expected", {
  adlb <- adlb_raw
  avalcat1 <- c("LAST", "REPLICATED", "SINGLE")

  set.seed(1, kind = "Mersenne-Twister")

  adlb <- adlb %>% mutate(
    AVALCAT1 = factor(
      case_when(
        .data$ANRIND %in% c("HIGH HIGH", "LOW LOW") ~
          sample(
            x = avalcat1,
            size = n(),
            replace = TRUE,
            prob = c(0.3, 0.6, 0.1)
          ),
        TRUE ~ ""
      ),
      levels = c("", avalcat1)
    ),
    PARCAT2 = factor("LS")
  ) %>%
    select(-.data$q1, -.data$q2)
  #Preprocessing steps
  adlb_f <- adlb %>%
    filter(.data$ONTRTFL == "Y" & .data$PARCAT2 == "LS" & .data$SAFFL == "Y" & !is.na(.data$AVAL)) %>%
    mutate(abn_dir = factor(case_when(
      ANRIND == "LOW LOW" ~ "Low",
      ANRIND == "HIGH HIGH" ~ "High",
      TRUE ~ ""
    )
    )
    )

  adlb_crp <- adlb_f %>% filter(PARAMCD == "CRP") %>% droplevels()
  full_parent_df <- list(adlb_crp, "not_needed")
  cur_col_subset <- list(adlb_crp$ARMCD == "ARM A", "not_needed")

  spl_context <- data.frame(
    split = c("PARAMCD", "abn_dir"),
    full_parent_df = I(full_parent_df),
    cur_col_subset = I(cur_col_subset)
  )

  result <- s_count_abnormal_by_marked(
    df = adlb_crp %>% filter(ARMCD == "ARM A") %>% droplevels(),
    .spl_context = spl_context,
    .var = "AVALCAT1",
    abnormal = c(High = "High"),
    variables = list(id = "USUBJID", param = "PARAMCD", direction = "abn_dir")
  )

  expected <- list(count_fraction = list(
    `Single, not last` = c(1.000000000, 0.007462687),
    `Last or replicated` = c(10.00000000, 0.07462687),
    `Any Abnormality` = c(11.00000000, 0.08208955)
  ))
  expect_equal(result, expected, tolerance = 0.000001)
})



test_that("count_abnormal_by_marked works as expected", {
  adlb <- adlb_raw

  avalcat1 <- c("LAST", "REPLICATED", "SINGLE")

  set.seed(1, kind = "Mersenne-Twister")

  adlb <- adlb %>% mutate(
    AVALCAT1 = factor(
      case_when(
        .data$ANRIND %in% c("HIGH HIGH", "LOW LOW") ~
          sample(
            x = avalcat1,
            size = n(),
            replace = TRUE,
            prob = c(0.3, 0.6, 0.1)
          ),
        TRUE ~ ""
      ),
      levels = c("", avalcat1)
    ),
    PARCAT2 = factor("LS")
  ) %>%
    select(-.data$q1, -.data$q2)
  #Preprocessing steps
  adlb_f <- adlb %>%
    filter(.data$ONTRTFL == "Y" & .data$PARCAT2 == "LS" & .data$SAFFL == "Y" & !is.na(.data$AVAL)) %>%
    mutate(abn_dir = factor(case_when(
      ANRIND == "LOW LOW" ~ "Low",
      ANRIND == "HIGH HIGH" ~ "High",
      TRUE ~ ""
      )
      )
      )


  adlb_f <- adlb_f %>%
    filter(PARAMCD == "CRP") %>%
     droplevels()

  map <- unique(
    adlb_f[adlb_f$abn_dir %in% c("Low", "High") & adlb_f$AVALCAT1 != "" & adlb_f$PARAMCD == "CRP", c("PARAMCD", "abn_dir")]
    ) %>%
    lapply(as.character) %>%
    as.data.frame() %>%
    arrange(PARAMCD, abn_dir)

  basic_table() %>%
  split_cols_by("ARMCD") %>%
  split_rows_by("PARAMCD") %>%
  summarize_num_patients(var = "USUBJID", .stats = "unique_count") %>%
  split_rows_by("abn_dir", split_fun = trim_levels_to_map(map)) %>%
  count_abnormal_by_marked(
    var = "AVALCAT1",
    variables = list(id = "USUBJID", param = "PARAMCD", direction = "abn_dir")
  ) %>%
  build_table(df = adlb_f)

  expected_matrix <- structure(
    c("", "Low", "Single, not last", "Last or replicated",
      "Any Abnormality", "High", "Single, not last", "Last or replicated",
      "Any Abnormality", "ARM A", "", "2 (1.5%)", "10 (7.5%)", "12 (9%)",
      "", "1 (0.7%)", "10 (7.5%)", "11 (8.2%)", "ARM B", "", "0", "7 (5.2%)",
      "7 (5.2%)", "", "2 (1.5%)", "9 (6.7%)", "11 (8.2%)", "ARM C",
      "", "0", "7 (5.3%)", "7 (5.3%)", "", "1 (0.8%)", "12 (9.1%)",
      "13 (9.8%)"), .Dim = c(9L, 4L))
  expect_identical(result_matrix, expected_matrix)
})
