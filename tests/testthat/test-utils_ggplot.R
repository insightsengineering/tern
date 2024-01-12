testthat::test_that("rtable2gg works as expected", {
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
  rtable2gg_default <- tbl %>% rtable2gg()
  expect_snapshot_ggplot("rtable2gg_default", rtable2gg_default, width = 5)

  # custom fontsize
  rtable2gg_fs <- tbl %>% rtable2gg(fontsize = 5)
  expect_snapshot_ggplot("rtable2gg_fs", rtable2gg_fs, width = 5)

  # custom colwidths
  rtable2gg_cw <- tbl %>% rtable2gg(colwidths = c(4, 2, 2, 3))
  expect_snapshot_ggplot("rtable2gg_cw", rtable2gg_cw, width = 5)

  # custom lbl_col_padding
  rtable2gg_lblpad <- tbl %>% rtable2gg(lbl_col_padding = -5)
  expect_snapshot_ggplot("rtable2gg_lblpad", rtable2gg_lblpad, width = 5)
})

testthat::test_that("rtable2gg works with multiple column splits", {
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

  rtable2gg_colsplits <- tbl %>% rtable2gg()
  expect_snapshot_ggplot("rtable2gg_colsplits", rtable2gg_colsplits, width = 10, height = 3)
})
