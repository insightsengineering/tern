context("test t_summary")

test_that("t_summary results are as expected", {

  asl <- read.table(header = TRUE, sep = ";", stringsAsFactors = FALSE, text = '
                    "USUBJID";"AGE";"SEX";"ARMCD"
                    "AB12345-XYZ1-id-1";"39";"F";"ARM C"
                    "AB12345-XYZ1-id-2";"39";"F";"ARM B"
                    "AB12345-XYZ2-id-3";"58";"M";"ARM B"
                    "AB12345-XYZ2-id-4";"56";"M";"ARM A"
                    "AB12345-XYZ1-id-5";"51";"M";"ARM C"
                    "AB12345-XYZ2-id-6";"58";"M";"ARM B"
                    "AB12345-XYZ2-id-7";"55";"F";"ARM B"
                    "AB12345-XYZ2-id-8";"41";"F";"ARM A"
                    "AB12345-XYZ2-id-9";"20";"M";"ARM A"
                    "AB12345-XYZ1-id-10";"52";"M";"ARM B"
                    "AB12345-XYZ1-id-11";"38";"M";"ARM B"
                    "AB12345-XYZ1-id-12";"36";"F";"ARM A"
                    "AB12345-XYZ2-id-13";"20";"F";"ARM A"
                    "AB12345-XYZ1-id-14";"30";"U";"ARM B"
                    "AB12345-XYZ2-id-15";"48";"M";"ARM C"
                    "AB12345-XYZ1-id-16";"67";"F";"ARM B"
                    "AB12345-XYZ2-id-17";"37";"F";"ARM B"
                    "AB12345-XYZ2-id-18";"47";"F";"ARM B"
                    "AB12345-XYZ1-id-19";"38";"M";"ARM C"
                    "AB12345-XYZ2-id-20";"20";"M";"ARM B"
                    "AB12345-XYZ2-id-21";"31";"M";"ARM C"
                    "AB12345-XYZ1-id-22";"32";"M";"ARM B"
                    "AB12345-XYZ2-id-23";"38";"F";"ARM A"
                    "AB12345-XYZ1-id-24";"62";"F";"ARM A"
                    "AB12345-XYZ1-id-25";"55";"F";"ARM C"
                    "AB12345-XYZ1-id-26";"36";"F";"ARM B"
                    "AB12345-XYZ1-id-27";"34";"M";"ARM A"
                    "AB12345-XYZ1-id-28";"53";"F";"ARM C"
                    "AB12345-XYZ2-id-29";"51";"F";"ARM A"
                    "AB12345-XYZ1-id-30";"26";"M";"ARM C"'
                    )

  # nolint start
  asl$SEX <- factor(asl$SEX, levels = c("F", "M", "U"))
  asl$ARMCD <- factor(asl$ARMCD, levels = c("ARM A", "ARM B", "ARM C"))
  # nolint end

  tbl_tern <- t_summary(asl[, c("AGE", "SEX")], asl$ARMCD, drop_levels = TRUE, total = "All")

  # nolint start
  tbl_test <- rtable(
    rrow(""),
    rrow("AGE"),
    rrow("n", 9, 13, 8, 30, format = "xx", indent = 1),
    rrow("Mean (SD)", c(39.78, 14.65), c(43.77, 13.55), c(42.62, 10.73), c(42.27, 12.88), format = "xx.xx (xx.xx)", indent = 1),
    rrow("Median", 38, 39, 43.5, 39, format = "xx.x", indent = 1),
    rrow("Min - Max", c(20, 62), c(20, 67), c(26, 55), c(20, 67), format = "xx.xx - xx.xx", indent = 1),
    rrow(""),
    rrow("SEX"),
    rrow("n", 9, 13, 8, 30, format = "xx", indent = 1),
    rrow("F", c(6, 0.6667), c(6, 0.4615), c(3, 0.375), c(15, 0.5), format = "xx (xx.xx%)", indent = 1),
    rrow("M", c(3, 0.3333), c(6, 0.4615), c(5, 0.625), c(14, 0.4667), format = "xx (xx.xx%)", indent = 1),
    rrow("U", rcell("-", format = "xx"), c(1, 0.0769), rcell("-", format = "xx"), c(1, 0.0333), format = "xx (xx.xx%)", indent = 1)
  )
  # nolint end

  header(tbl_test) <- rheader(
    rrow("", "ARM A", "ARM B", "ARM C", "All"),
    rrow("", "(N=9)", "(N=13)", "(N=8)", "(N=30)")
  )

  comp <- compare_rtables(tbl_tern, tbl_test, comp.attr = FALSE)

  expect_true(all(comp == "."), "t_summary does not provide the expected results")

})
