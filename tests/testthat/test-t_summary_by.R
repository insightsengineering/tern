context("test t_summary_by")

asl <- read.table(header = TRUE, sep = ";", stringsAsFactors = FALSE, text = '
                  "USUBJID";"AGE";"SEX";"ARMCD";"COUNTRY"
                  "AB12345-XYZ1-id-1";"33";"F";"ARM C";"AFG"
                  "AB12345-XYZ2-id-2";"20";"F";"ARM C";"BES"
                  "AB12345-XYZ1-id-3";"27";"F";"ARM C";"AFG"
                  "AB12345-XYZ1-id-4";"64";"M";"ARM A";"CUW"
                  "AB12345-XYZ2-id-5";"43";"M";"ARM B";"SXM"
                  "AB12345-XYZ2-id-6";"28";"F";"ARM B";"CUW"
                  "AB12345-XYZ1-id-7";"21";"F";"ARM B";"BES"
                  "AB12345-XYZ1-id-8";"35";"F";"ARM B";"BES"
                  "AB12345-XYZ2-id-9";"20";"F";"ARM C";"CUW"
                  "AB12345-XYZ2-id-10";"30";"M";"ARM A";"BES"
                  "AB12345-XYZ2-id-11";"25";"M";"ARM A";"AFG"
                  "AB12345-XYZ2-id-12";"63";"M";"ARM B";"SXM"
                  "AB12345-XYZ2-id-13";"60";"F";"ARM B";"CUW"
                  "AB12345-XYZ2-id-14";"38";"M";"ARM A";"AFG"
                  "AB12345-XYZ2-id-15";"20";"M";"ARM C";"AFG"
                  "AB12345-XYZ2-id-16";"58";"F";"ARM B";"AFG"
                  "AB12345-XYZ1-id-17";"57";"F";"ARM A";"AFG"
                  "AB12345-XYZ2-id-18";"54";"F";"ARM A";"BES"
                  "AB12345-XYZ2-id-19";"54";"F";"ARM B";"AFG"
                  "AB12345-XYZ1-id-20";"32";"F";"ARM B";"SXM"
                  "AB12345-XYZ1-id-21";"54";"F";"ARM B";"CUW"
                  "AB12345-XYZ1-id-22";"66";"F";"ARM A";"BES"
                  "AB12345-XYZ1-id-23";"40";"F";"ARM C";"SXM"
                  "AB12345-XYZ1-id-24";"20";"F";"ARM C";"AFG"
                  "AB12345-XYZ1-id-25";"55";"M";"ARM C";"BES"
                  "AB12345-XYZ2-id-26";"55";"M";"ARM A";"SXM"
                  "AB12345-XYZ2-id-27";"33";"M";"ARM B";"SXM"
                  "AB12345-XYZ2-id-28";"73";"M";"ARM C";"AFG"
                  "AB12345-XYZ2-id-29";"24";"F";"ARM B";"BES"
                  "AB12345-XYZ2-id-30";"46";"F";"ARM A";"SXM"'
)
# nolint start
asl$SEX <- factor(asl$SEX, levels = c("F", "M"))
asl$ARMCD <- factor(asl$ARMCD, levels = c("ARM A", "ARM B", "ARM C"))
asl$COUNTRY <- factor(asl$COUNTRY, levels = c("AFG", "BES", "CUW", "SXM"))
# nolint end

attributes(asl$USUBJID)$label <- "Subject ID"
attributes(asl$AGE)$label <- "Age"
attributes(asl$SEX)$label <- "Sex"
attributes(asl$ARMCD)$label <- "Arm Code"
attributes(asl$COUNTRY)$label <- "Country"

test_that("t_summary_by categorical results are as expected", {

  tbl_tern <- t_summary_by(
    x = asl$SEX,
    row_by = asl$COUNTRY,
    col_by = asl$ARMCD,
    total = "ALL",
    denominator = "n"
  )

  # nolint start
  tbl_test <- rtable(
    rrow(""),
    rrow("AFG"),
    rrow("n", 3, 2, 5, 10, format = "xx", indent = 1),
    rrow("F", c(1, 0.3333), c(2, 1), c(3, 0.6), c(6, 0.6), format = "xx (xx.xx%)", indent = 1),
    rrow("M", c(2, 0.6667), rcell(0, format = "xx"), c(2, 0.4), c(4, 0.4), format = "xx (xx.xx%)", indent = 1),
    rrow(""),
    rrow("BES"),
    rrow("n", 3, 3, 2, 8, format = "xx", indent = 1),
    rrow("F", c(2, 0.6667), c(3, 1), c(1, 0.5), c(6, 0.75), format = "xx (xx.xx%)", indent = 1),
    rrow("M", c(1, 0.3333), rcell(0, format = "xx"), c(1, 0.5), c(2, 0.25), format = "xx (xx.xx%)", indent = 1),
    rrow(""),
    rrow("CUW"),
    rrow("n", 1, 3, 1, 5, format = "xx", indent = 1),
    rrow("F", rcell(0, format = "xx"), c(3, 1), c(1, 1), c(4, 0.8), format = "xx (xx.xx%)", indent = 1),
    rrow("M", c(1, 1), rcell(0, format = "xx"), rcell(0, format = "xx"), c(1, 0.2), format = "xx (xx.xx%)", indent = 1),
    rrow(""),
    rrow("SXM"),
    rrow("n", 2, 4, 1, 7, format = "xx", indent = 1),
    rrow("F", c(1, 0.5), c(1, 0.25), c(1, 1), c(3, 0.4286), format = "xx (xx.xx%)", indent = 1),
    rrow("M", c(1, 0.5), c(3, 0.75), rcell(0, format = "xx"), c(4, 0.5714), format = "xx (xx.xx%)", indent = 1)
  )
  # nolint end

  header(tbl_test) <- rheader(
    rrow("Country", "ARM A", "ARM B", "ARM C", "ALL"),
    rrow("Sex", "(N=9)", "(N=12)", "(N=9)", "(N=30)", indent = 1)
  )

  comp <- compare_rtables(tbl_tern, tbl_test, comp.attr = FALSE)

  expect_true(all(comp == "."), "t_summary_by categorical does not provide the expected results")
})

test_that("t_summary_by categorical results are as expected", {

  tbl_tern <- t_summary_by(
    x = asl$AGE,
    row_by = asl$COUNTRY,
    col_by = asl$ARMCD,
    total = "ALL"
  )

  # nolint start
  tbl_test <- rtable(
    rrow(""),
    rrow("AFG"),
    rrow("n", 3, 2, 5, 10, format = "xx", indent = 1),
    rrow("Mean (SD)", c(40, 16.09), c(56, 2.83), c(34.6, 22.14), c(40.5, 18.69), format = "xx.xx (xx.xx)", indent = 1),
    rrow("Median", 38, 56, 27, 35.5, format = "xx.x", indent = 1),
    rrow("Min - Max", c(25, 57), c(54, 58), c(20, 73), c(20, 73), format = "xx.xx - xx.xx", indent = 1),
    rrow(""),
    rrow("BES"),
    rrow("n", 3, 3, 2, 8, format = "xx", indent = 1),
    rrow("Mean (SD)", c(50, 18.33), c(26.67, 7.37), c(37.5, 24.75), c(38.12, 17.77), format = "xx.xx (xx.xx)", indent = 1),
    rrow("Median", 54, 24, 37.5, 32.5, format = "xx.x", indent = 1),
    rrow("Min - Max", c(30, 66), c(21, 35), c(20, 55), c(20, 66), format = "xx.xx - xx.xx", indent = 1),
    rrow(""),
    rrow("CUW"),
    rrow("n", 1, 3, 1, 5, format = "xx", indent = 1),
    rrow("Mean (SD)", c(64, NA), c(47.33, 17.01), c(20, NA), c(45.2, 19.88), format = "xx.xx (xx.xx)", indent = 1),
    rrow("Median", 64, 54, 20, 54, format = "xx.x", indent = 1),
    rrow("Min - Max", c(64, 64), c(28, 60), c(20, 20), c(20, 64), format = "xx.xx - xx.xx", indent = 1),
    rrow(""),
    rrow("SXM"),
    rrow("n", 2, 4, 1, 7, format = "xx", indent = 1),
    rrow("Mean (SD)", c(50.5, 6.36), c(42.75, 14.38), c(40, NA), c(44.57, 11.3), format = "xx.xx (xx.xx)", indent = 1),
    rrow("Median", 50.5, 38, 40, 43, format = "xx.x", indent = 1),
    rrow("Min - Max", c(46, 55), c(32, 63), c(40, 40), c(32, 63), format = "xx.xx - xx.xx", indent = 1)
  )
  # nolint end

  header(tbl_test) <- rheader(
    rrow("Country", "ARM A", "ARM B", "ARM C", "ALL"),
    rrow("Age", "(N=9)", "(N=12)", "(N=9)", "(N=30)", indent = 1)
  )

  comp <- compare_rtables(tbl_tern, tbl_test, comp.attr = FALSE)

  expect_true(all(comp == "."), "t_summary_by numerical does not provide the expected results")

})
