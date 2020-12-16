# Test single variant for CMT02_PT

library(random.cdisc.data)
library(dplyr)

test_that("CMT02_PT default variant is produced correctly", {
  adsl <- radsl(cached = TRUE)
  adcm <- radcm(cached = TRUE)

  # Update ADCM to follow structure with WHODrug treatment coding.
  adcm_path1 <- adcm %>%
    mutate(
      ATC1 = paste0("ATCCLAS1 ", substr(CMDECOD,  9, 9)),
      ATC2 = paste0("ATCCLAS2 ", substr(CMDECOD,  9, 9)),
      ATC3 = paste0("ATCCLAS3 ", substr(CMDECOD,  9, 9)),
      ATC4 = paste0("ATCCLAS4 ", substr(CMDECOD,  9, 9))
    )
  adcm_path2 <- adcm_path1 %>%
    filter(CMDECOD %in% c("medname A_1/3", "medname B_2/4", "medname B_3/4", "medname C_1/2")) %>%
    mutate(
      ATC1 = paste(ATC1, "p2"),
      ATC2 = paste(ATC2, "p2"),
      ATC3 = paste(ATC3, "p2"),
      ATC4 = paste(ATC4, "p2")
    )
  adcm_path3 <- adcm_path2 %>%
    filter(CMDECOD %in% c("medname B_2/4", "medname C_1/2")) %>%
    mutate(
      ATC1 = paste(ATC1, "p3"),
      ATC2 = paste(ATC2, "p3"),
      ATC3 = paste(ATC3, "p3"),
      ATC4 = paste(ATC4, "p3")
    )
  # Construct final VAD with multiple class paths per treatment.
  adcm <- rbind(
    adcm_path1,
    adcm_path2,
    adcm_path3
  )

  # Process VAD based on keys and keep only one treatment path per CMDECOD.
  adcm <- adcm %>%
    group_by(STUDYID, USUBJID, CMDECOD) %>%
    slice(1) %>%
    ungroup()

  result <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c("Total number of patients with at least one event", "Total number of events")
    ) %>%
    count_occurrences(var = "CMDECOD") %>%
    build_table(adcm, alt_counts_df = adsl) %>%
    sort_at_path(path =  c("CMDECOD"), scorefun = score_occurrences)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Total number of patients with at least one event",
      "Total number of events", "medname A_1/3", "medname C_2/2", "medname B_1/4",
      "medname B_2/4", "medname A_2/3", "medname C_1/2", "medname B_4/4",
      "medname A_3/3", "medname B_3/4", "A: Drug X", "(N=134)", "122 (91%)",
      "456", "54 (40.3%)", "52 (38.8%)", "52 (38.8%)", "52 (38.8%)",
      "53 (39.6%)", "51 (38.1%)", "50 (37.3%)", "45 (33.6%)", "47 (35.1%)",
      "B: Placebo", "(N=134)", "123 (91.8%)", "465", "49 (36.6%)",
      "58 (43.3%)", "57 (42.5%)", "55 (41%)", "50 (37.3%)", "50 (37.3%)",
      "45 (33.6%)", "54 (40.3%)", "47 (35.1%)", "C: Combination", "(N=132)",
      "120 (90.9%)", "511", "69 (52.3%)", "60 (45.5%)", "59 (44.7%)",
      "56 (42.4%)", "56 (42.4%)", "56 (42.4%)", "55 (41.7%)", "48 (36.4%)",
      "52 (39.4%)", "All Patients", "(N=400)", "365 (91.2%)", "1432",
      "172 (43%)", "170 (42.5%)", "168 (42%)", "163 (40.8%)", "159 (39.8%)",
      "157 (39.2%)", "150 (37.5%)", "147 (36.8%)", "146 (36.5%)"
    ),
    .Dim = c(13L, 5L)
  )

  expect_identical(result_matrix, expected_matrix)
})
