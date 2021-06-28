library(scda)
library(rtables)
library(dplyr)

get_adlb <- function() {

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
}

test_that("s_count_abnormal_by_marked works as expected", {
  adlb <- get_adlb()
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
    filter(.data$ONTRTFL == "Y" & .data$PARCAT2 == "LS" & .data$SAFFL == "Y" & !is.na(.data$AVAL))

  result <- s_count_abnormal_by_marked(
    df = adlb %>% dplyr::filter(ARMCD == "ARM A" & PARAMCD == "CRP"),
    .var = "AVALCAT1",
    abnormal = "LOW LOW",
    variables = list(id = "USUBJID", direction = "ANRIND")
  )
  expected <- list(count_fraction = list(
    `Single, not last` = c(2.00000000, 0.01492537),
    `Last or replicated` = c(10.00000000, 0.07462687),
    `Any Abnormality` = c(12.00000000, 0.08955224)
  ))
  expect_equal(result, expected, tolerance = 0.000001)
})


test_that("s_count_abnormal_by_marked works as expected", {
  adlb <- get_adlb()

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
    filter(.data$ONTRTFL == "Y" & .data$PARCAT2 == "LS" & .data$SAFFL == "Y" & !is.na(.data$AVAL))

  result <- s_count_abnormal_by_marked(
    df = adlb %>%
      dplyr::filter(ARMCD == "ARM A" & PARAMCD == "CRP"),
    .var = "AVALCAT1",
    abnormal = "HIGH HIGH",
    variables = list(id = "USUBJID", direction = "ANRIND")
  )
  expected <- list(count_fraction = list(
    `Single, not last` = c(1.000000000, 0.007462687),
    `Last or replicated` = c(10.00000000, 0.07462687),
    `Any Abnormality` = c(11.00000000, 0.08208955)
  ))
  expect_equal(result, expected, tolerance = 0.000001)
})


test_that("count_abnormal_by_marked works as expected", {
  adlb <- get_adlb()

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
    filter(.data$ONTRTFL == "Y" & .data$PARCAT2 == "LS" & .data$SAFFL == "Y" & !is.na(.data$AVAL))

  adlb_f <- adlb_f %>%
    filter(PARAMCD == "CRP") %>%
     droplevels()

  result <- basic_table() %>%
    split_cols_by("ARMCD") %>%
    count_abnormal_by_marked(
    var = "AVALCAT1",
    abnormal = c(Low = "LOW LOW", High = "HIGH HIGH"),
    variables = list(id = "USUBJID", direction = "ANRIND")) %>%
    build_table(df = adlb_f)
    result_matrix <- to_string_matrix(result)

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
