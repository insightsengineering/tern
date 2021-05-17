library(random.cdisc.data)
library(dplyr)


get_adlb <- function() {

  set.seed(123)

  adlb <- radlb(cached = TRUE) # nolintr

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
    mutate(ANRIND = factor(case_when(
      ANRIND == "LOW" & AVAL <= q1 ~ "LOW LOW",
      ANRIND == "HIGH" & AVAL >= q2 ~ "HIGH HIGH",
      TRUE ~ as.character(ANRIND)),
      levels = c("", "HIGH", "HIGH HIGH", "LOW", "LOW LOW", "NORMAL")
    ),
    AVALCAT1 = factor(case_when(
      ANRIND %in% c("HIGH HIGH", "LOW LOW") ~
        sample(x = avalcat1, size = n(), replace = TRUE, prob = c(0.3, 0.6, 0.1)),
      TRUE ~ ""),
      levels = c("", avalcat1)
    ),
    PARCAT2 = factor(ifelse(ANRIND %in% c("HIGH HIGH", "LOW LOW"), "LS",
                            sample(c("SI", "CV", "LS"), 1)
    )
    )) %>%
    select(-q1, -q2)
  #Preprocessing steps

  adlb_f <- adlb %>% filter(ONTRTFL == "Y" & PARCAT2 == "LS" & SAFFL == "Y" & !is.na(AVAL)) # nolintr
  adlb_f
}

  test_that("LBT05 default variant is produced correctly", {
    adlb <- get_adlb()
    adsl <- radsl(cached = TRUE)

    split_fun <- drop_split_levels

    lyt <- basic_table() %>%
      split_cols_by("ACTARMCD") %>%
      add_colcounts() %>%
      split_rows_by("PARAM", split_fun = split_fun) %>%
      summarize_num_patients(var = "USUBJID", .stats = "unique_count") %>%
      count_abnormal_by_marked(
        var = "AVALCAT1",
        abnormal = c(Low = "LOW LOW", High = "HIGH HIGH"),
        variables = list(id = "USUBJID", direction = "ANRIND")
      )

    result <- build_table(lyt, df = adlb, alt_counts_df = adsl)

    result_matrix <- to_string_matrix(result)
    expected_matrix <- structure(
      c("", "", "Alanine Aminotransferase Measurement (n)",
        "Low", "Single, not last", "Last or replicated", "Any Abnormality",
        "High", "Single, not last", "Last or replicated", "Any Abnormality",
        "C-Reactive Protein Measurement (n)", "Low", "Single, not last",
        "Last or replicated", "Any Abnormality", "High", "Single, not last",
        "Last or replicated", "Any Abnormality", "Immunoglobulin A Measurement (n)",
        "Low", "Single, not last", "Last or replicated", "Any Abnormality",
        "High", "Single, not last", "Last or replicated", "Any Abnormality",
        "ARM A", "(N=134)", "54", "", "1 (1.9%)", "4 (7.4%)", "5 (9.3%)",
        "", "1 (1.9%)", "8 (14.8%)", "9 (16.7%)", "56", "", "1 (1.8%)",
        "11 (19.6%)", "12 (21.4%)", "", "0", "11 (19.6%)", "11 (19.6%)",
        "45", "", "0", "7 (15.6%)", "7 (15.6%)", "", "0", "4 (8.9%)",
        "4 (8.9%)", "ARM B", "(N=134)", "59", "", "1 (1.7%)", "12 (20.3%)",
        "13 (22%)", "", "2 (3.4%)", "10 (16.9%)", "12 (20.3%)", "67",
        "", "0", "7 (10.4%)", "7 (10.4%)", "", "3 (4.5%)", "8 (11.9%)",
        "11 (16.4%)", "57", "", "1 (1.8%)", "8 (14%)", "9 (15.8%)", "",
        "1 (1.8%)", "8 (14%)", "9 (15.8%)", "ARM C", "(N=132)", "61",
        "", "2 (3.3%)", "3 (4.9%)", "5 (8.2%)", "", "1 (1.6%)", "6 (9.8%)",
        "7 (11.5%)", "57", "", "2 (3.5%)", "5 (8.8%)", "7 (12.3%)", "",
        "0", "13 (22.8%)", "13 (22.8%)", "57", "", "1 (1.8%)", "10 (17.5%)",
        "11 (19.3%)", "", "1 (1.8%)", "5 (8.8%)", "6 (10.5%)"),
      .Dim = c(29L, 4L)
    )
    expect_identical(result_matrix, expected_matrix)
  })
