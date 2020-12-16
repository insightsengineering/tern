# Test all variants of AET09 SMQ, adverse events related to stuy drug by standardized MEDDRA query

library(dplyr)
library(tern)
library(random.cdisc.data)

stack_adae_by_smq <- function(adae, smq) {

  l_df <- lapply(smq, function(ae_grp) {

    keep <- !(is.na(adae[[ae_grp]]))
    df <- adae[keep, ]
    df[["SMQ"]] <- ae_grp
    df
  })

  do.call(rbind, l_df)
}

test_that("AET09 variant 1 (AEs related to study drug by SMQ) is produced correctly", {
  adsl <- radsl(cached = TRUE)
  adae <- radae(cached = TRUE)
  adsl_labels <- var_labels(adsl)
  adae_labels <- var_labels(adae)

  adae <- adae %>%
    mutate(
      SMQ1  = case_when(
        AEBODSYS %in% c("cl A.1", "cl B.1", "cl C.1", "cl D.1") ~ "SMQ 1 (broad)",
        TRUE ~ NA_character_
      ),
      SMQ2 = case_when(
        AEBODSYS %in% c("cl A.1",  "cl D.1") ~ "SMQ 1 (narrow)",
        TRUE ~ NA_character_
      ),
      SMQ3 = case_when(
        AEDECOD %in% c("dcd B.2.1.2.1", "dcd A.1.1.1.2", "dcd C.2.1.2.1", "dcd B.2.2.3.1") ~ "AESI",
        TRUE ~ NA_character_
      )
    )


  adae <- stack_adae_by_smq(adae, c("SMQ1"))
  adae_r <- adae[adae$AEREL == "Y", ]

  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts()  %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event related to study drug",
        nonunique = "Overall total number of events related to study drug"
      )) %>%
    split_rows_by(
      "SMQ",
      child_labels = "visible",
      nested = FALSE,
      indent_mod = -1L,
      split_fun = drop_split_levels
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event related to study drug",
        nonunique = "Total number of events related to study drug"
      )) %>%
    count_occurrences(vars = "AEDECOD", .indent_mods = -1L)

  result <- build_table(lyt, adae_r, alt_counts_df = adsl) %>%
    sort_at_path(path = c("SMQ"), scorefun = cont_n_allcols) %>%
    sort_at_path(path =  c("SMQ", "*", "AEDECOD"), scorefun = score_occurrences)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "",
      "",
      "Total number of patients with at least one adverse event related to study drug",
      "Overall total number of events related to study drug",
      "SMQ1",
      "Total number of patients with at least one adverse event related to study drug",
      "Total number of events related to study drug",
      "dcd D.1.1.1.1",
      "dcd B.1.1.1.1",
      "dcd C.1.1.1.3",
      "A: Drug X",
      "(N=134)",
      "91 (67.9%)",
      "172",
      "",
      "91 (67.9%)",
      "172",
      "50 (37.3%)",
      "47 (35.1%)",
      "43 (32.1%)",
      "B: Placebo",
      "(N=134)",
      "90 (67.2%)",
      "174",
      "",
      "90 (67.2%)",
      "174",
      "42 (31.3%)",
      "49 (36.6%)",
      "46 (34.3%)",
      "C: Combination",
      "(N=132)",
      "93 (70.5%)",
      "197",
      "",
      "93 (70.5%)",
      "197",
      "51 (38.6%)",
      "43 (32.6%)",
      "43 (32.6%)"
    ),
    .Dim = c(10L, 4L)
  )
  expect_identical(result_matrix, expected_matrix)
})

test_that("AET09 variant 2 (AEs related to study srug by SMQ <with customized queries>) is produced correctly", {

  adsl <- radsl(cached = TRUE)
  adae <- radae(cached = TRUE)
  adsl_labels <- var_labels(adsl)
  adae_labels <- var_labels(adae)

  adae <- adae %>%
    mutate(
      SMQ1  = case_when(
        AEBODSYS %in% c("cl A.1", "cl B.1", "cl C.1", "cl D.1") ~ "SMQ 1 (broad)",
        TRUE ~ NA_character_
      ),
      SMQ2 = case_when(
        AEBODSYS %in% c("cl A.1",  "cl D.1") ~ "SMQ 1 (narrow)",
        TRUE ~ NA_character_
      ),
      SMQ3 = case_when(
        AEDECOD %in% c("dcd B.2.1.2.1", "dcd A.1.1.1.2", "dcd C.2.1.2.1", "dcd B.2.2.3.1") ~ "AESI",
        TRUE ~ NA_character_
      )
    )


  adae <- stack_adae_by_smq(adae, c("SMQ1", "SMQ2", "SMQ3"))
  adae_r <- adae[adae$AEREL == "Y", ]

  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts()  %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event related to study drug",
        nonunique = "Overall total number of events related to study drug"
      )) %>%
    split_rows_by(
      "SMQ",
      child_labels = "visible",
      nested = FALSE,
      indent_mod = -1L,
      split_fun = drop_split_levels
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event related to study drug",
        nonunique = "Total number of events related to study drug"
      )) %>%
    count_occurrences(vars = "AEDECOD", .indent_mods = -1L)

  result <- build_table(lyt, adae_r, alt_counts_df = adsl) %>%
    sort_at_path(path = c("SMQ"), scorefun = cont_n_allcols) %>%
    sort_at_path(path =  c("SMQ", "*", "AEDECOD"), scorefun = score_occurrences)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "",
      "",
      "Total number of patients with at least one adverse event related to study drug",
      "Overall total number of events related to study drug",
      "SMQ1",
      "Total number of patients with at least one adverse event related to study drug",
      "Total number of events related to study drug",
      "dcd D.1.1.1.1",
      "dcd B.1.1.1.1",
      "dcd C.1.1.1.3",
      "SMQ2",
      "Total number of patients with at least one adverse event related to study drug",
      "Total number of events related to study drug",
      "dcd D.1.1.1.1",
      "SMQ3",
      "Total number of patients with at least one adverse event related to study drug",
      "Total number of events related to study drug",
      "dcd C.2.1.2.1",
      "A: Drug X",
      "(N=134)",
      "99 (73.9%)",
      "281",
      "",
      "91 (67.9%)",
      "172",
      "50 (37.3%)",
      "47 (35.1%)",
      "43 (32.1%)",
      "",
      "50 (37.3%)",
      "61",
      "50 (37.3%)",
      "",
      "35 (26.1%)",
      "48",
      "35 (26.1%)",
      "B: Placebo",
      "(N=134)",
      "98 (73.1%)",
      "278",
      "",
      "90 (67.2%)",
      "174",
      "42 (31.3%)",
      "49 (36.6%)",
      "46 (34.3%)",
      "",
      "42 (31.3%)",
      "51",
      "42 (31.3%)",
      "",
      "48 (35.8%)",
      "53",
      "48 (35.8%)",
      "C: Combination",
      "(N=132)",
      "102 (77.3%)",
      "333",
      "",
      "93 (70.5%)",
      "197",
      "51 (38.6%)",
      "43 (32.6%)",
      "43 (32.6%)",
      "",
      "51 (38.6%)",
      "71",
      "51 (38.6%)",
      "",
      "55 (41.7%)",
      "65",
      "55 (41.7%)"
    ),
    .Dim = c(18L, 4L)
  )
  expect_identical(result_matrix, expected_matrix)
})
