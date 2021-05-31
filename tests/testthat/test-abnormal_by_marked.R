library(scda)
library(rtables)
library(dplyr)

get_adlb <- function() {

  set.seed(123)

  adlb <- synthetic_cdisc_data("rcd_2021_05_05")$adlb # nolintr

  # Modify ANRIND and create AVALCAT1/PARCAT2
  # PARCAT2 is just used for filtering, but in order to be the
  # filtering as realistic as possible, will create the variable.
  qntls <- adlb %>%
    group_by(PARAMCD) %>%
    summarise(as_tibble(t(quantile(AVAL, probs = c(0.1, 0.9)))), .groups = "drop_last") %>%
    rename(q1 = 2, q2 = 3)

  adlb <- adlb %>%
    left_join(qntls, by = "PARAMCD")

  avalcat1 <- c("LAST", "REPLICATED", "SINGLE")

  adlb <- adlb %>%
    group_by(USUBJID, PARAMCD, BASETYPE) %>%
    mutate(
      ANRIND = factor(
        case_when(
          ANRIND == "LOW" & AVAL <= q1 ~ "LOW LOW",
          ANRIND == "HIGH" & AVAL >= q2 ~ "HIGH HIGH",
          TRUE ~ as.character(ANRIND)
        ),
        levels = c("", "HIGH", "HIGH HIGH", "LOW", "LOW LOW", "NORMAL")
      ),
      AVALCAT1 = factor(
        case_when(
          ANRIND %in% c("HIGH HIGH", "LOW LOW") ~
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
      PARCAT2 = factor(ifelse(
        ANRIND %in% c("HIGH HIGH", "LOW LOW"), "LS",
        sample(c("SI", "CV", "LS"), 1)
      ))
    ) %>%
    select(-q1, -q2)
  #Preprocessing steps
  adlb_f <- adlb %>%
    filter(ONTRTFL == "Y" & PARCAT2 == "LS" & SAFFL == "Y" & !is.na(AVAL))
  adlb_f
}

test_that("s_count_abnormal_by_marked works as expected", {
  adlb <- get_adlb()

  result <- s_count_abnormal_by_marked(
    df = adlb %>% dplyr::filter(ARMCD == "ARM A" & PARAMCD == "CRP"),
    .var = "AVALCAT1",
    abnormal = "LOW LOW",
    variables = list(id = "USUBJID", direction = "ANRIND")
  )
  expected <- list(count_fraction = list(
    `Single, not last` = c(1.00000000, 0.01785714),
    `Last or replicated` = c(11.0000000, 0.1964286),
    `Any Abnormality` = c(12.0000000, 0.2142857)
  ))
  expect_equal(result, expected, tolerance = 0.000001)
})

test_that("s_count_abnormal_by_marked works as expected", {
  adlb <- get_adlb()

  result <- s_count_abnormal_by_marked(
    df = adlb %>%
    dplyr::filter(ARMCD == "ARM A" & PARAMCD == "CRP"),
    .var = "AVALCAT1",
    abnormal = "HIGH HIGH",
    variables = list(id = "USUBJID", direction = "ANRIND")
  )
  expected <- list(count_fraction = list(
    `Single, not last` = c(0, 0),
    `Last or replicated` = c(11.0000000, 0.1964286),
    `Any Abnormality` = c(11.0000000, 0.1964286)
  ))
  expect_equal(result, expected, tolerance = 0.000001)
})

test_that("count_abnormal_by_marked works as expected", {
  adlb <- get_adlb()
  adlb_f <- adlb %>%
    dplyr::filter(PARAMCD == "CRP") %>%
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
      "Any Abnormality", "ARM A", "", "1 (1.8%)", "11 (19.6%)", "12 (21.4%)",
      "", "0", "11 (19.6%)", "11 (19.6%)", "ARM B", "", "0", "7 (10.4%)",
      "7 (10.4%)", "", "3 (4.5%)", "8 (11.9%)", "11 (16.4%)", "ARM C",
      "", "2 (3.5%)", "5 (8.8%)", "7 (12.3%)", "", "0", "13 (22.8%)",
      "13 (22.8%)"),
    .Dim = c(9L, 4L)
  )
  expect_identical(result_matrix, expected_matrix)
})
