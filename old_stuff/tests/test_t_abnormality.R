context("test t_abnormality")

advs <- read.table(
  header = TRUE,
  sep = ";",
  stringsAsFactors = FALSE,
  strip.white = TRUE,
  text = '
  "USUBJID";"ARM";"PARAMCD";"ANRIND";"BNRIND"
  "ID-1";"ARM A";"DIABP"; "L";"L"
  "ID-1";"ARM A";"DIABP"; "";"L"
  "ID-2";"ARM A";"DIABP"; "N";"L"
  "ID-2";"ARM A";"DIABP"; "";"L"
  "ID-3";"ARM A";"DIABP"; "H";"L"
  "ID-3";"ARM A";"DIABP"; "";"L"
  "ID-4";"ARM B";"DIABP"; "L";"N"
  "ID-5";"ARM B";"DIABP"; "N";"N"
  "ID-6";"ARM B";"DIABP"; "H";"N"
  "ID-7";"ARM C";"DIABP"; "L";"H"
  "ID-8";"ARM C";"DIABP"; "N";"H"
  "ID-9";"ARM C";"DIABP"; "H";"H"
  "ID-10";"ARM A";"DIABP"; "L";""
  "ID-11";"ARM B";"DIABP"; "N";""
  "ID-12";"ARM C";"DIABP"; "H";""
  "ID-13";"ARM A";"DIABP"; "";"L"
  "ID-14";"ARM B";"DIABP"; "";"N"
  "ID-15";"ARM C";"DIABP"; "";"H"
  "ID-1";"ARM A";"SYSBP"; "L";"L"
  "ID-1";"ARM A";"SYSBP"; "L";"L"
  "ID-1";"ARM A";"SYSBP"; "N";"L"
  "ID-1";"ARM A";"SYSBP"; "";"L"
  "ID-2";"ARM A";"SYSBP"; "N";"L"
  "ID-2";"ARM A";"SYSBP"; "L";"L"
  "ID-2";"ARM A";"SYSBP"; "";"L"
  "ID-4";"ARM B";"SYSBP"; "L";"N";
  "ID-4";"ARM B";"SYSBP"; "L";"N";
  "ID-4";"ARM B";"SYSBP"; "N";"N";
  "ID-5";"ARM B";"SYSBP"; "N";"N";
  "ID-5";"ARM B";"SYSBP"; "L";"N";
  "ID-5";"ARM B";"SYSBP"; "";"N";
  "ID-7";"ARM C";"SYSBP"; "L";"H"
  "ID-8";"ARM C";"SYSBP"; "N";"H"
  "ID-9";"ARM C";"SYSBP"; "L";"H"
  "ID-10";"ARM A";"SYSBP"; "L";""
  "ID-11";"ARM B";"SYSBP"; "N";""
  "ID-12";"ARM C";"SYSBP"; "N";"H"
  "ID-13";"ARM A";"SYSBP"; "";"L"
  "ID-14";"ARM B";"SYSBP"; "";"N"
  "ID-15";"ARM C";"SYSBP"; "";"H"'
)

# nolint start
advs$PARAMCD <- factor(advs$PARAMCD, levels = c("SYSBP", "DIABP"))
advs$ARM <- factor(advs$ARM, levels = c("ARM A", "ARM B", "ARM C"))

attributes(advs$USUBJID)$label <- "Subject ID"
attributes(advs$ARM)$label <- "Arm Code"
attributes(advs$PARAMCD)$label <- "Parameter Code"
attributes(advs$ANRIND)$label <- "Analysis Reference Range Indicator"
attributes(advs$BNRIND)$label <- "Baseline Reference Range Indicator"
# nolint end

test_format <- function(x, output) {
  if (x[1] > 0 & x[2] > 0) {
    paste0(x[1], "/", x[2], " (", round(x[3], 3) * 100, "%)")
  } else if (x[1] == 0) {
    paste0(x[1], "/", x[2])
  }

}

test_that(
  "t_abnormality variant with exclude_base_abn=FALSE results are as expected", {
    # nolint start
    tbl_tern <- t_abnormality(
      grade = sas_na(advs$ANRIND),
      # sas_na very important to convert "" to NA so those records are excluded from analysis
      abnormal = c("L", "H"),
      baseline = sas_na(advs$BNRIND),
      id = advs$USUBJID,
      exclude_base_abn = FALSE,
      row_by = advs[, c("PARAMCD")],
      col_by = advs$ARM,
      col_N = c(10, 12, 17)
    )
    # nolint end

    test_header <- rheader(
      rrow(row.name = "", "", "ARM A", "ARM B", "ARM C"),
      rrow(row.name = "", "Analysis Reference Range Indicator", "(N=10)", "(N=12)", "(N=17)")
    )

    # nolint start
    tbl_test <- rtable(
      header = test_header,
      rrow(
        row.name = "SYSBP",
        rcell("L"),
        rcell(c(3, 3, 3 / 3), format = test_format),
        rcell(c(2, 3, 2 / 3), format = test_format),
        rcell(c(2, 4, 2 / 4), format = test_format)
      ),
      rrow(
        row.name = "",
        rcell("H"),
        rcell(c(0, 3, 0), format = test_format),
        rcell(c(0, 3, 0), format = test_format),
        rcell(c(0, 4, 0), format = test_format)
      ),
      rrow(),
      rrow(
        row.name = "DIABP",
        rcell("L"),
        rcell(c(2, 4, 2 / 4), format = test_format),
        rcell(c(1, 4, 1 / 4), format = test_format),
        rcell(c(1, 4, 1 / 4), format = test_format)
      ),
      rrow(
        row.name = "",
        rcell("H"),
        rcell(c(1, 4, 1 / 4), format = test_format),
        rcell(c(1, 4, 1 / 4), format = test_format),
        rcell(c(2, 4, 2 / 4), format = test_format)
      )
    )
    # nolint end

    comp <- compare_rtables(tbl_tern, tbl_test, comp.attr = FALSE)

    expect_true(
      all(comp == "."),
      "t_abnormality with exclude_base_abn=FALSE does not provide the expected results"
    )
  })

test_that(
  "t_abnormality variant with exclude_base_abn=TRUE results are as expected", {
    # nolint start
    tbl_tern <- t_abnormality(
      grade = sas_na(advs$ANRIND),
      # sas_na very important to convert "" to NA so those records are excluded from analysis
      abnormal = c("L", "H"),
      baseline = sas_na(advs$BNRIND),
      id = advs$USUBJID,
      exclude_base_abn = TRUE,
      row_by = advs[, c("PARAMCD")],
      col_by = advs$ARM,
      col_N = c(10, 12, 17),
      total = "ALL"
    )
    # nolint end

    test_header <- rheader(
      rrow(row.name = "", "", "ARM A", "ARM B", "ARM C", "ALL"),
      rrow(row.name = "", "Analysis Reference Range Indicator", "(N=10)", "(N=12)", "(N=17)", "(N=39)")
    )

    # nolint start
    tbl_test <- rtable(
      header = test_header,
      rrow(
        row.name = "SYSBP",
        rcell("L"),
        rcell(c(1, 1, 1 / 1), format = test_format),
        rcell(c(2, 3, 2 / 3), format = test_format),
        rcell(c(2, 4, 2 / 4), format = test_format),
        rcell(c(5, 8, 5 / 8), format = test_format)
      ),
      rrow(
        row.name = "",
        rcell("H"),
        rcell(c(0, 3, 0 / 1), format = test_format),
        rcell(c(0, 3, 0 / 3), format = test_format),
        rcell(c(0, 0, 0 / 0), format = test_format),
        rcell(c(0, 6, 0 / 7), format = test_format)
      ),
      rrow(),
      rrow(
        row.name = "DIABP",
        rcell("L"),
        rcell(c(1, 1, 1 / 1), format = test_format),
        rcell(c(1, 4, 1 / 4), format = test_format),
        rcell(c(1, 4, 1 / 4), format = test_format),
        rcell(c(3, 9, 3 / 9), format = test_format)
      ),
      rrow(
        row.name = "",
        rcell("H"),
        rcell(c(1, 4, 1 / 4), format = test_format),
        rcell(c(1, 4, 1 / 4), format = test_format),
        rcell(c(1, 1, 1 / 1), format = test_format),
        rcell(c(3, 9, 3 / 9), format = test_format)
      )
    )
    # nolint end

    comp <- compare_rtables(tbl_tern, tbl_test, comp.attr = FALSE)

    expect_true(
      all(comp == "."),
      "t_abnormality with exclude_base_abn=TRUE does not provide the expected results"
    )
  })
