# Test variants of DTH01

library(random.cdisc.data)
library(dplyr)

## Function to generate adsl
gen_adsl <- function(){
  adsl <- radsl(cached = TRUE)
  set.seed(36857, kind = "Mersenne-Twister")
  # create DTHCAT
  adsl$DTHCAT <- NA   # nolint snake_case
  adsl$DTHCAT[!is.na(adsl$DTHDT)] <- sample(
    x = c("ADVERSE EVENT", "PROGRESSIVE DISEASE", "OTHER"),
    size = sum(!is.na(adsl$DTHDT)),
    replace = TRUE, prob = c(.5, .3, .2)
  )
  adsl$DTHCAT <- factor(adsl$DTHCAT, level = c("ADVERSE EVENT", "PROGRESSIVE DISEASE", "OTHER"))  # nolint snake_case
  #Create DTHCAUS with reasons for "Other" specified.
  #Death flag variable (Y or N)
  adsl <- adsl %>% mutate(
    DTHCAUS = ifelse(!is.na(DTHCAT) & DTHCAT == "OTHER",
                     sample(
                       x = c(
                         "Post-study reporting of death",
                         "LOST TO FOLLOW UP",
                         "MISSING",
                         "SUICIDE",
                         "UNKNOWN"),
                       size = sum((!is.na(adsl$DTHCAT) & adsl$DTHCAT == "OTHER")),
                       replace = TRUE, prob = c(.1, .3, .3, .2, .1)),
                     as.character(DTHCAT)
    ),
    DTHFL = ifelse(is.na(DTHDT), "N", "Y"))

  adsl$DTHCAUS <- as.factor(adsl$DTHCAUS) # nolint snake_case
  # Create dummy variables LDDTHELD and LDDTHGR1 as in ADSL GDSR structure
  adsl <- within(
    data = adsl,
    expr = {
      TRTEDTM <- as.Date(TRTEDTM) # nolint snake_case
      DTHDT <- as.Date(DTHDT) # nolint snake_case

      DTHDT[!is.na(DTHDT)] <- {
        DTHDT[!is.na(DTHDT)] +
          round(rnorm(n = sum(!is.na(DTHDT)), mean = 30, sd = 8))
      }
      # Dummy variable LDDTHELD: Elapsed Days from Last Dose to Death.
      LDDTHELD <- as.numeric(DTHDT - TRTEDTM)  # nolint snake_case

      # Dummy variable LDDTHGR1: Last Dose to Death - Days Elapsed Grp 1.
      LDDTHGR1 <- cut(    # nolint snake_case
        LDDTHELD,
        breaks = c(min(LDDTHELD, na.rm = TRUE), 30, max(LDDTHELD, na.rm = TRUE)
        ), include.lowest = TRUE
      )
    }
  )
  # LDDTHGR1: Categorical variable with 2 levels: "<=30" and ">30"
  levels(adsl$LDDTHGR1) <- c("<=30", ">30")  # nolint snake_case
  adsl
}

## test for variant 1
test_that("DTH01 variant 1  is produced correctly", {
  adsl <- gen_adsl()
  tbl1 <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    count_values(
      "DTHFL",
      values = "Y",
      .labels =  c(count_fraction = "Total number of deaths"),
      .formats = c(count_fraction = "xx (xx.x%)"))  %>%
    summarize_vars(vars = c("DTHCAT"), var_labels = c("Primary cause of death")) %>%
    build_table(df = adsl)

  result <- to_string_matrix(tbl1)
  expected <- structure(
    c(
      "", "", "Total number of deaths", "Primary cause of death",
      "n", "ADVERSE EVENT", "PROGRESSIVE DISEASE", "OTHER", "A: Drug X",
      "(N=134)", "107 (79.9%)", "", "107", "49 (45.8%)", "31 (29%)",
      "27 (25.2%)", "B: Placebo", "(N=134)", "112 (83.6%)", "", "112",
      "50 (44.6%)", "44 (39.3%)", "18 (16.1%)", "C: Combination", "(N=132)",
      "111 (84.1%)", "", "111", "53 (47.7%)", "37 (33.3%)", "21 (18.9%)",
      "All Patients", "(N=400)", "330 (82.5%)", "", "330", "152 (46.1%)",
      "112 (33.9%)", "66 (20%)"
      ),
    .Dim = c(8L, 5L),
    .Dimnames = list(NULL, c("", "A: Drug X", "B: Placebo", "C: Combination", "All Patients"))
  )
  expect_identical(result, expected)
})

## test for variant 2
test_that("DTH01 variant 2  is produced correctly", {
  adsl <- gen_adsl()

  part1 <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    count_values(
      "DTHFL",
      values = "Y",
      .labels =  c(count_fraction = "Total number of deaths"),
      .formats = c(count_fraction = "xx (xx.x%)"))  %>%
    summarize_vars(vars = c("DTHCAT"), var_labels = c("Primary cause of death"))  %>% build_table(df = adsl)

  part2 <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    split_rows_by("DTHCAT", split_fun = keep_split_levels("OTHER"), child_labels = "hidden") %>%
    summarize_vars(
      "DTHCAUS",
      nested = T,
      .stats = "count_fraction",
      .indent_mods = c("count_fraction" = 4L)) %>%
    build_table(df = adsl) %>% prune_table()
  part3 <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    summarize_vars(
      vars = "LDDTHGR1",
      var_labels = "Days from last drug administration",
      show_labels = "visible")  %>%
    build_table(df = adsl)
  part4 <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    split_rows_by(
      "LDDTHGR1",
      split_label = "Primary cause by days from last study drug administration",
      visible_label = TRUE) %>%
    summarize_vars("DTHCAT") %>%
    build_table(df = adsl)
  col_info(part2) <- col_info(part1)
  col_info(part3) <- col_info(part2)
  col_info(part4) <- col_info(part3)
  tbl2 <- rbind(part1, part2)
  tbl2 <- rbind(tbl2, part3)
  tbl2 <- rbind(tbl2, part4)

  result <- to_string_matrix(tbl2)
  expected <- structure(
    c("", "", "Total number of deaths", "Primary cause of death",
      "n", "ADVERSE EVENT", "PROGRESSIVE DISEASE", "OTHER", "    LOST TO FOLLOW UP",
      "    MISSING", "    Post-study reporting of death", "    SUICIDE",
      "    UNKNOWN", "Days from last drug administration", "n", "<=30",
      ">30", "Primary cause by days from last study drug administration",
      "<=30", "n", "ADVERSE EVENT", "PROGRESSIVE DISEASE", "OTHER",
      ">30", "n", "ADVERSE EVENT", "PROGRESSIVE DISEASE", "OTHER",
      "A: Drug X", "(N=134)", "107 (79.9%)", "", "107", "49 (45.8%)",
      "31 (29%)", "27 (25.2%)", "12 (44.4%)", "4 (14.8%)", "4 (14.8%)",
      "6 (22.2%)", "1 (3.7%)", "", "107", "62 (57.9%)", "45 (42.1%)",
      "", "", "62", "26 (41.9%)", "18 (29%)", "18 (29%)", "", "45",
      "23 (51.1%)", "13 (28.9%)", "9 (20%)", "B: Placebo", "(N=134)",
      "112 (83.6%)", "", "112", "50 (44.6%)", "44 (39.3%)", "18 (16.1%)",
      "5 (27.8%)", "6 (33.3%)", "0", "5 (27.8%)", "2 (11.1%)",
      "", "112", "56 (50%)", "56 (50%)", "", "", "56", "26 (46.4%)",
      "21 (37.5%)", "9 (16.1%)", "", "56", "24 (42.9%)", "23 (41.1%)",
      "9 (16.1%)", "C: Combination", "(N=132)", "111 (84.1%)", "",
      "111", "53 (47.7%)", "37 (33.3%)", "21 (18.9%)", "4 (19%)", "8 (38.1%)",
      "2 (9.5%)", "3 (14.3%)", "4 (19%)", "", "111", "56 (50.5%)",
      "55 (49.5%)", "", "", "56", "24 (42.9%)", "20 (35.7%)", "12 (21.4%)",
      "", "55", "29 (52.7%)", "17 (30.9%)", "9 (16.4%)", "All Patients",
      "(N=400)", "330 (82.5%)", "", "330", "152 (46.1%)", "112 (33.9%)",
      "66 (20%)", "21 (31.8%)", "18 (27.3%)", "6 (9.1%)", "14 (21.2%)",
      "7 (10.6%)", "", "330", "174 (52.7%)", "156 (47.3%)", "", "",
      "174", "76 (43.7%)", "59 (33.9%)", "39 (22.4%)", "", "156", "76 (48.7%)",
      "53 (34%)", "27 (17.3%)"), .Dim = c(28L, 5L), .Dimnames = list(
        NULL, c("", "A: Drug X", "B: Placebo", "C: Combination", "All Patients"))
    )
  expect_identical(result, expected)
})


#test for variant 3

test_that("DTH01 variant 3 is produced correctly", {
  adsl <- gen_adsl()
  # preprocessing steps
  l <- levels(adsl[adsl$DTHCAT == "OTHER", ]$DTHCAUS)
  #table
  part1 <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    count_values(
      "DTHFL",
      values = "Y",
      .labels =  c(count_fraction = "Total number of deaths"),
      .formats = c(count_fraction = "xx (xx.x%)"))  %>%
    summarize_vars(vars = c("DTHCAT"), var_labels = c("Primary cause of death")) %>%
    build_table(df = adsl)
  part2 <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    split_rows_by("DTHCAT", split_fun = keep_split_levels("OTHER"), child_labels = "hidden") %>%
    count_values(
      "DTHCAUS",
      values = l[4],
      .labels = c(count_fraction = "Post study reporting of deaths"),
      .formats = c(count_fraction = "xx (xx.x%)"),
      .indent_mods = c(count_fraction = 4L))  %>%
    count_values(
      "DTHCAUS",
      values = l[-4],
      .labels = c(count_fraction = "All other causes"),
      .formats = c(count_fraction = "xx (xx.x%)"),
      .indent_mods = c(count_fraction = 4L))  %>%
    build_table(adsl)

  col_info(part2) <- col_info(part1)
  tbl3 <- rbind(part1, part2)

  result <- to_string_matrix(tbl3)
  expected <- structure(
    c("", "", "Total number of deaths", "Primary cause of death",
      "n", "ADVERSE EVENT", "PROGRESSIVE DISEASE", "OTHER", "    Post study reporting of deaths",
      "    All other causes", "A: Drug X", "(N=134)", "107 (79.9%)",
      "", "107", "49 (45.8%)", "31 (29%)", "27 (25.2%)", "4 (14.8%)",
      "23 (85.2%)", "B: Placebo", "(N=134)", "112 (83.6%)", "", "112",
      "50 (44.6%)", "44 (39.3%)", "18 (16.1%)", "0 (0%)", "18 (100%)",
      "C: Combination", "(N=132)", "111 (84.1%)", "", "111", "53 (47.7%)",
      "37 (33.3%)", "21 (18.9%)", "2 (9.5%)", "19 (90.5%)", "All Patients",
      "(N=400)", "330 (82.5%)", "", "330", "152 (46.1%)", "112 (33.9%)",
      "66 (20%)", "6 (9.1%)", "60 (90.9%)"), .Dim = c(10L, 5L), .Dimnames = list(
        NULL, c("", "A: Drug X", "B: Placebo", "C: Combination", "All Patients"))
    )
  expect_identical(result, expected)

})

#test for variant 4
test_that("DTH01 variant 4 is produced correctly", {
  adsl <- gen_adsl()
  #preprocessing steps
  l <- levels(adsl[adsl$DTHCAT == "OTHER", ]$DTHCAUS)
  adsl <- adsl %>%
    mutate(
      DTHCAUS_other = ifelse(
        DTHCAT == "OTHER" & DTHCAUS != "Post-study reporting of death", as.character(DTHCAUS), NA)
    )
  adsl$DTHCAUS_other <- as.factor(adsl$DTHCAUS_other)  # nolint snake_case

  #table
  part1 <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    count_values(
      "DTHFL",
      values = "Y",
      .labels =  c(count_fraction = "Total number of deaths"),
      .formats = c(count_fraction = "xx (xx.x%)"))  %>%
    summarize_vars(vars = c("DTHCAT"), var_labels = c("Primary cause of death")) %>%
    build_table(df = adsl)
  part2 <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    split_rows_by("DTHCAT", split_fun = keep_split_levels("OTHER"), child_labels = "hidden") %>%
    count_values(
      "DTHCAUS",
      values = l[4],
      .labels = c(count_fraction = "Post study reporting of deaths"),
      .formats = c(count_fraction = "xx (xx.x%)"),
      .indent_mods = c(count_fraction = 4L))  %>%
    count_values(
      "DTHCAUS",
      values = l[-4],
      .labels = c(count_fraction = "All other causes"),
      .formats = c(count_fraction = "xx (xx.x%)"),
      .indent_mods = c(count_fraction = 4L))  %>%
    build_table(adsl)
  part3 <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    split_rows_by("DTHCAT", split_fun = keep_split_levels("OTHER"), child_labels = "hidden") %>%
    summarize_vars(
      "DTHCAUS_other",
      nested = T,
      .stats = "count_fraction",
      .indent_mods = c("count_fraction" = 6L)) %>%
    build_table(df = adsl)
  col_info(part2) <- col_info(part1)
  col_info(part3) <- col_info(part2)
  tbl4 <- rbind(part1, part2)
  tbl4 <- rbind(tbl4, part3)

  result <- to_string_matrix(tbl4)
  expected <- structure(
    c("", "", "Total number of deaths", "Primary cause of death",
      "n", "ADVERSE EVENT", "PROGRESSIVE DISEASE", "OTHER", "    Post study reporting of deaths",
      "    All other causes", "      LOST TO FOLLOW UP", "      MISSING",
      "      SUICIDE", "      UNKNOWN", "A: Drug X", "(N=134)", "107 (79.9%)",
      "", "107", "49 (45.8%)", "31 (29%)", "27 (25.2%)", "4 (14.8%)",
      "23 (85.2%)", "12 (52.2%)", "4 (17.4%)", "6 (26.1%)", "1 (4.3%)",
      "B: Placebo", "(N=134)", "112 (83.6%)", "", "112", "50 (44.6%)",
      "44 (39.3%)", "18 (16.1%)", "0 (0%)", "18 (100%)", "5 (27.8%)",
      "6 (33.3%)", "5 (27.8%)", "2 (11.1%)", "C: Combination", "(N=132)",
      "111 (84.1%)", "", "111", "53 (47.7%)", "37 (33.3%)", "21 (18.9%)",
      "2 (9.5%)", "19 (90.5%)", "4 (21.1%)", "8 (42.1%)", "3 (15.8%)",
      "4 (21.1%)", "All Patients", "(N=400)", "330 (82.5%)", "", "330",
      "152 (46.1%)", "112 (33.9%)", "66 (20%)", "6 (9.1%)", "60 (90.9%)",
      "21 (35%)", "18 (30%)", "14 (23.3%)", "7 (11.7%)"),
    .Dim = c(14L, 5L),
    .Dimnames = list(NULL, c("", "A: Drug X", "B: Placebo", "C: Combination", "All Patients"))
    )
  expect_identical(result, expected)

})
