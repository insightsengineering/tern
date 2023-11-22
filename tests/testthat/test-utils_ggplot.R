testthat::test_that("rtables2gg works as expected", {
  dta <- data.frame(
    USUBJID = rep(1:6, each = 3),
    PARAMCD = rep("lab", 6 * 3),
    AVISIT  = rep(paste0("V", 1:3), 6),
    ARM     = rep(LETTERS[1:3], rep(6, 3)),
    AVAL    = c(9:1, rep(NA, 9))
  )

  lyt <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    split_rows_by(var = "AVISIT") %>%
    analyze_vars(vars = "AVAL")

  tbl <- build_table(lyt, df = dta)

  # defaults
  testthat::expect_snapshot_file(
    tbl %>%
      rtable2gg() %>%
      ggsave(filename = "rtable2gg_default.svg", path = "./_snaps/utils_ggplot", width = 5) %>%
      suppressMessages(),
    "rtable2gg_default.svg"
  )

  # custom fontsize
  testthat::expect_snapshot_file(
    tbl %>%
      rtable2gg(fontsize = 5) %>%
      ggsave(filename = "rtable2gg_fs.svg", path = "./_snaps/utils_ggplot", width = 5) %>%
      suppressMessages(),
    "rtable2gg_fs.svg"
  )

  # custom colwidths
  testthat::expect_snapshot_file(
    tbl %>%
      rtable2gg(colwidths = c(4, 2, 2, 3)) %>%
      ggsave(filename = "rtable2gg_cw.svg", path = "./_snaps/utils_ggplot", width = 5) %>%
      suppressMessages(),
    "rtable2gg_cw.svg"
  )

  # custom lbl_col_padding
  testthat::expect_snapshot_file(
    tbl %>%
      rtable2gg(lbl_col_padding = -5) %>%
      ggsave(filename = "rtable2gg_lblpad.svg", path = "./_snaps/utils_ggplot", width = 5) %>%
      suppressMessages(),
    "rtable2gg_lblpad.svg"
  )
})

testthat::test_that("rtables2gg works with multiple column splits", {
  dta2 <- data.frame(
    USUBJID = rep(1:6, each = 3),
    PARAMCD = rep("lab", 6 * 3),
    AVISIT  = rep(paste0("V", 1:2), 9),
    ARM     = rep(LETTERS[1:2], rep(9, 2)),
    SEX     = rep(c("M", "F", "M"), 6),
    AVAL    = c(1:15, rep(NA, 3))
  )

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by(var = "ARM") %>%
    split_cols_by(var = "AVISIT") %>%
    split_cols_by(var = "SEX") %>%
    analyze_vars(vars = "AVAL")

  tbl <- build_table(lyt, df = dta2)

  testthat::expect_snapshot_file(
    tbl %>%
      rtable2gg() %>%
      ggsave(filename = "rtable2gg_colsplits.svg", path = "./_snaps/utils_ggplot", height = 3, width = 10),
    "rtable2gg_colsplits.svg"
  )
})
