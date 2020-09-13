context("test change from baseline table")

library(dplyr)

test_that("summary by visit table", {

  # nolint start
  tbl_stream <- rtable(
    rrow("", "Quebec-conc", "Quebec-uptake", "Mississippi-conc", "Mississippi-uptake"),
    rrow("Visit 1"),
    rrow("n", 7, 7, 7, 7, format = "xx", indent = 1),
    rrow("Mean (SD)", c(435, 317.73), c(29.97, 8.33), c(435, 317.73), c(18, 4.12), format = "xx.xx (xx.xx)", indent = 1),
    rrow("Median", 350, 32.5, 350, 18.9, format = "xx.x", indent = 1),
    rrow("IQR", 375, 7.8, 375, 4.2, format = "xx.xx", indent = 1),
    rrow("Min - Max", c(95, 1000), c(14.2, 38.7), c(95, 1000), c(10.5, 22.2), format = "xx.xx - xx.xx", indent = 1),
    rrow(),
    rrow("Visit 2"),
    rrow("n", 14, 14, 7, 7, format = "xx", indent = 1),
    rrow("Mean (SD)", c(435, 305.26), c(33.93, 10.81), c(435, 317.73), c(12.14, 2.19), format = "xx.xx (xx.xx)", indent = 1),
    rrow("Median", 350, 38.05, 350, 12.5, format = "xx.x", indent = 1),
    rrow("IQR", 437.5,  11.97, 375, 1.5, format = "xx.xx", indent = 1),
    rrow("Min - Max", c(95, 1000), c(9.3, 44.3), c(95, 1000), c(7.7, 14.4), format = "xx.xx - xx.xx", indent = 1),
    rrow(),
    rrow("Visit 3"),
    rrow("n", 14, 14, 7, 7, format = "xx", indent = 1),
    rrow("Mean (SD)", c(435, 305.26), c(35.1, 10.27), c(435, 317.73), c(17.3, 3.05), format = "xx.xx (xx.xx)", indent = 1),
    rrow("Median", 350, 39.25, 350, 17.9, format = "xx.x", indent = 1),
    rrow("IQR", 437.5, 9.13, 375, 0.55, format = "xx.xx", indent = 1),
    rrow("Min - Max", c(95, 1000), c(15.1, 45.5), c(95, 1000), c(10.6, 19.9), format = "xx.xx - xx.xx", indent = 1),
    rrow(),
    rrow("Visit last"),
    rrow("n", 7, 7, 21, 21, format = "xx", indent = 1),
    rrow("Mean (SD)", c(435, 317.73), c(33.23, 8.21), c(435, 301.42), c(25.95, 7.40), format = "xx.xx (xx.xx)", indent = 1),
    rrow("Median", 350, 35.3, 350, 28.1, format = "xx.x", indent = 1),
    rrow("IQR", 375, 5.6, 500, 9.1, format = "xx.xx", indent = 1),
    rrow("Min - Max", c(95, 1000), c(16, 39.7),  c(95, 1000), c(10.6, 35.5), format = "xx.xx - xx.xx", indent = 1)
  )
  # nolint end


  header(tbl_stream) <- rheader(
    rrow("", rcell("Quebec", colspan = 2), rcell("Mississippi", colspan = 2)),
    rrow("", rcell("(N=14)", colspan = 2), rcell("(N=21)", colspan = 2)),
    rrow("", "conc", "uptake", "conc", "uptake")
  )

  df <- CO2 %>%
    mutate(Visit = factor(ifelse(grepl("Mn|Qn1", Plant), "Visit last", gsub("Qc|Qn|Mc", "Visit ", Plant)))) %>%
    group_by(Visit) %>%
    mutate(ID = 1:n()) %>%
    ungroup()

  pop <- unique(df[, c("ID", "Type")])

  ANL <- tidyr::gather(df, param, val, conc, uptake) #nolintr

  tbls <- t_summary_by(
    x = ANL$val,
    row_by = ANL$Visit,
    col_by = interaction(ANL$param, ANL$Type),
    table_tree = TRUE
  )

  expect_true(TRUE, "t_summary_by does not provide the same results as stream")
})
