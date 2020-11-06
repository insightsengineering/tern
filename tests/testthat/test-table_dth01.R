# Test variants of DTH01

library(random.cdisc.data)
library(dplyr)

## Function to generate adsl
gen_adsl <- function() {
  adsl <- radsl(cached = TRUE)
  set.seed(12, kind = "Mersenne-Twister")

  adsl$DTHCAT <- NA   # nolint snake_case
  adsl$DTHCAT[!is.na(adsl$DTHDT)] <- sample(
    x = c("ADVERSE EVENT", "PROGRESSIVE DISEASE", "OTHER"),
    size = sum(!is.na(adsl$DTHDT)),
    replace = TRUE, prob = c(.5, .3, .2)
  )
  adsl$DTHCAT <- factor(  # nolint snake_case
    adsl$DTHCAT,
    levels = c("ADVERSE EVENT", "PROGRESSIVE DISEASE", "OTHER")
  )

  adsl <- adsl %>%
    mutate(
      DTHCAUS = ifelse(
        !is.na(DTHCAT) & DTHCAT == "OTHER",
        sample(
          x = c(
            "Post-study reporting of death",
            "LOST TO FOLLOW UP",
            "MISSING",
            "SUICIDE",
            "UNKNOWN"
          ),
          size = sum((!is.na(adsl$DTHCAT) & adsl$DTHCAT == "OTHER")),
          replace = TRUE,
          prob = c(.1, .3, .3, .2, .1)
        ),
        as.character(DTHCAT)
      ),
      DTHFL = ifelse(is.na(DTHDT), "N", "Y")
    )

  adsl$DTHCAUS <- factor(  # nolint snake_case
    adsl$DTHCAUS,
    levels = c(
      "ADVERSE EVENT", "LOST TO FOLLOW UP", "MISSING", "Post-study reporting of death",
      "PROGRESSIVE DISEASE", "SUICIDE", "UNKNOWN"
    )
  )
  adsl$DTHFL <- factor(adsl$DTHFL, levels = c("Y", "N"))  #nolint snake_case

  # Create dummy variables LDDTHELD and LDDTHGR1 as in ADSL GDSR structure.
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

test_that("DTH01 variant 1 is produced correctly", {
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
      "(N=134)", "107 (79.9%)", "", "107", "57 (53.3%)", "32 (29.9%)",
      "18 (16.8%)", "B: Placebo", "(N=134)", "112 (83.6%)", "", "112",
      "54 (48.2%)", "36 (32.1%)", "22 (19.6%)", "C: Combination", "(N=132)",
      "111 (84.1%)", "", "111", "52 (46.8%)", "33 (29.7%)", "26 (23.4%)",
      "All Patients", "(N=400)", "330 (82.5%)", "", "330", "163 (49.4%)",
      "101 (30.6%)", "66 (20%)"
      ),
    .Dim = c(8L, 5L)
  )
  expect_identical(result, expected)
})

test_that("DTH01 variant 2 is produced correctly", {
  adsl <- gen_adsl()

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
    summarize_vars(
      "DTHCAUS",
      nested = T,
      .stats = "count_fraction",
      .indent_mods = c("count_fraction" = 4L)) %>%
    build_table(df = adsl) %>%
    prune_table()
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
      "n", "ADVERSE EVENT", "PROGRESSIVE DISEASE", "OTHER", "LOST TO FOLLOW UP",
      "MISSING", "Post-study reporting of death", "SUICIDE",
      "UNKNOWN", "Days from last drug administration", "n", "<=30",
      ">30", "Primary cause by days from last study drug administration",
      "<=30", "n", "ADVERSE EVENT", "PROGRESSIVE DISEASE", "OTHER",
      ">30", "n", "ADVERSE EVENT", "PROGRESSIVE DISEASE", "OTHER",
      "A: Drug X", "(N=134)", "107 (79.9%)", "", "107", "57 (53.3%)",
      "32 (29.9%)", "18 (16.8%)", "10 (55.6%)", "4 (22.2%)", "1 (5.6%)",
      "3 (16.7%)", "0", "", "107", "58 (54.2%)", "49 (45.8%)", "",
      "", "58", "32 (55.2%)", "17 (29.3%)", "9 (15.5%)", "", "49",
      "25 (51%)", "15 (30.6%)", "9 (18.4%)", "B: Placebo", "(N=134)",
      "112 (83.6%)", "", "112", "54 (48.2%)", "36 (32.1%)", "22 (19.6%)",
      "4 (18.2%)", "5 (22.7%)", "1 (4.5%)", "10 (45.5%)", "2 (9.1%)",
      "", "112", "66 (58.9%)", "46 (41.1%)", "", "", "66", "32 (48.5%)",
      "23 (34.8%)", "11 (16.7%)", "", "46", "22 (47.8%)", "13 (28.3%)",
      "11 (23.9%)", "C: Combination", "(N=132)", "111 (84.1%)", "",
      "111", "52 (46.8%)", "33 (29.7%)", "26 (23.4%)", "8 (30.8%)",
      "5 (19.2%)", "2 (7.7%)", "5 (19.2%)", "6 (23.1%)", "", "111",
      "55 (49.5%)", "56 (50.5%)", "", "", "55", "19 (34.5%)", "19 (34.5%)",
      "17 (30.9%)", "", "56", "33 (58.9%)", "14 (25%)", "9 (16.1%)",
      "All Patients", "(N=400)", "330 (82.5%)", "", "330", "163 (49.4%)",
      "101 (30.6%)", "66 (20%)", "22 (33.3%)", "14 (21.2%)", "4 (6.1%)",
      "18 (27.3%)", "8 (12.1%)", "", "330", "179 (54.2%)", "151 (45.8%)",
      "", "", "179", "83 (46.4%)", "59 (33%)", "37 (20.7%)", "", "151",
      "80 (53%)", "42 (27.8%)", "29 (19.2%)"),
    .Dim = c(28L, 5L)
  )
  expect_identical(result, expected)
})

test_that("DTH01 variant 3 is produced correctly", {
  adsl <- gen_adsl()

  dthcaus_levels <- levels(adsl[adsl$DTHCAT == "OTHER", ]$DTHCAUS)

  part1 <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    count_values(
      "DTHFL",
      values = "Y",
      .labels =  c(count_fraction = "Total number of deaths"),
      .formats = c(count_fraction = "xx (xx.x%)")
    )  %>%
    summarize_vars(vars = c("DTHCAT"), var_labels = c("Primary cause of death")) %>%
    build_table(df = adsl)
  part2 <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    split_rows_by("DTHCAT", split_fun = keep_split_levels("OTHER"), child_labels = "hidden") %>%
    count_values(
      "DTHCAUS",
      values = dthcaus_levels[4],
      .labels = c(count_fraction = "Post study reporting of deaths"),
      .formats = c(count_fraction = "xx (xx.x%)"),
      .indent_mods = c(count_fraction = 2L)
    ) %>%
    count_values(
      "DTHCAUS",
      values = dthcaus_levels[-4],
      .labels = c(count_fraction = "All other causes"),
      .formats = c(count_fraction = "xx (xx.x%)"),
      .indent_mods = c(count_fraction = 2L)
    ) %>%
    build_table(adsl)

  col_info(part2) <- col_info(part1)
  tbl3 <- rbind(part1, part2)

  result <- to_string_matrix(tbl3)
  expected <- structure(
    c("", "", "Total number of deaths", "Primary cause of death",
      "n", "ADVERSE EVENT", "PROGRESSIVE DISEASE", "OTHER", "Post study reporting of deaths",
      "All other causes", "A: Drug X", "(N=134)", "107 (79.9%)",
      "", "107", "57 (53.3%)", "32 (29.9%)", "18 (16.8%)", "1 (5.6%)",
      "17 (94.4%)", "B: Placebo", "(N=134)", "112 (83.6%)", "", "112",
      "54 (48.2%)", "36 (32.1%)", "22 (19.6%)", "1 (4.5%)", "21 (95.5%)",
      "C: Combination", "(N=132)", "111 (84.1%)", "", "111", "52 (46.8%)",
      "33 (29.7%)", "26 (23.4%)", "2 (7.7%)", "24 (92.3%)", "All Patients",
      "(N=400)", "330 (82.5%)", "", "330", "163 (49.4%)", "101 (30.6%)",
      "66 (20%)", "4 (6.1%)", "62 (93.9%)"),
    .Dim = c(10L, 5L)
  )
  expect_identical(result, expected)
})

test_that("DTH01 variant 4 is produced correctly", {
  adsl <- gen_adsl()

  dthcaus_levels <- levels(adsl[adsl$DTHCAT == "OTHER", ]$DTHCAUS)
  adsl <- adsl %>%
    mutate(
      DTHCAUS_other = ifelse(
        DTHCAT == "OTHER" & DTHCAUS != "Post-study reporting of death", as.character(DTHCAUS), NA)
    )
  adsl$DTHCAUS_other <- factor(  # nolint snake_case
    adsl$DTHCAUS_other,
    levels = c("LOST TO FOLLOW UP", "MISSING", "SUICIDE", "UNKNOWN")
  )

  part1 <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    count_values(
      "DTHFL",
      values = "Y",
      .labels =  c(count_fraction = "Total number of deaths"),
      .formats = c(count_fraction = "xx (xx.x%)")
    ) %>%
    summarize_vars(vars = c("DTHCAT"), var_labels = c("Primary cause of death")) %>%
    build_table(df = adsl)

  part2 <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    split_rows_by("DTHCAT", split_fun = keep_split_levels("OTHER"), child_labels = "hidden") %>%
    count_values(
      "DTHCAUS",
      values = dthcaus_levels[4],
      .labels = c(count_fraction = "Post study reporting of deaths"),
      .formats = c(count_fraction = "xx (xx.x%)"),
      .indent_mods = c(count_fraction = 2L)
    ) %>%
    count_values(
      "DTHCAUS",
      values = dthcaus_levels[-4],
      .labels = c(count_fraction = "All other causes"),
      .formats = c(count_fraction = "xx (xx.x%)"),
      .indent_mods = c(count_fraction = 2L)
    ) %>%
    build_table(adsl)

  part3 <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    split_rows_by("DTHCAT", split_fun = keep_split_levels("OTHER"), child_labels = "hidden") %>%
    summarize_vars(
      "DTHCAUS_other",
      nested = TRUE,
      .stats = "count_fraction",
      .indent_mods = c("count_fraction" = 3L)
    ) %>%
    build_table(df = adsl)

  col_info(part2) <- col_info(part1)
  col_info(part3) <- col_info(part2)
  tbl4 <- rbind(part1, part2)
  tbl4 <- rbind(tbl4, part3)

  result <- to_string_matrix(tbl4)
  expected <- structure(
    c("", "", "Total number of deaths", "Primary cause of death",
      "n", "ADVERSE EVENT", "PROGRESSIVE DISEASE", "OTHER", "Post study reporting of deaths",
      "All other causes", "LOST TO FOLLOW UP", "MISSING",
      "SUICIDE", "UNKNOWN", "A: Drug X", "(N=134)", "107 (79.9%)",
      "", "107", "57 (53.3%)", "32 (29.9%)", "18 (16.8%)", "1 (5.6%)",
      "17 (94.4%)", "10 (58.8%)", "4 (23.5%)", "3 (17.6%)", "0", "B: Placebo",
      "(N=134)", "112 (83.6%)", "", "112", "54 (48.2%)", "36 (32.1%)",
      "22 (19.6%)", "1 (4.5%)", "21 (95.5%)", "4 (19%)", "5 (23.8%)",
      "10 (47.6%)", "2 (9.5%)", "C: Combination", "(N=132)", "111 (84.1%)",
      "", "111", "52 (46.8%)", "33 (29.7%)", "26 (23.4%)", "2 (7.7%)",
      "24 (92.3%)", "8 (33.3%)", "5 (20.8%)", "5 (20.8%)", "6 (25%)",
      "All Patients", "(N=400)", "330 (82.5%)", "", "330", "163 (49.4%)",
      "101 (30.6%)", "66 (20%)", "4 (6.1%)", "62 (93.9%)", "22 (35.5%)",
      "14 (22.6%)", "18 (29%)", "8 (12.9%)"),
    .Dim = c(14L, 5L)
  )
  expect_identical(result, expected)
})
