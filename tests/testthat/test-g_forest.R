adrs <- tern_ex_adrs
n_records <- 20
adrs_labels <- formatters::var_labels(adrs, fill = TRUE)
adrs <- adrs %>%
  dplyr::filter(PARAMCD == "BESRSPI") %>%
  dplyr::filter(ARM %in% c("A: Drug X", "B: Placebo")) %>%
  dplyr::slice(seq_len(n_records)) %>%
  droplevels() %>%
  dplyr::mutate(rsp = AVALC == "CR")

formatters::var_labels(adrs) <- c(adrs_labels, "Response")
df <- extract_rsp_subgroups(
  variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "STRATA2")),
  data = adrs
)

testthat::test_that("g_forest default plot works", {
  tbl <- basic_table() %>%
    tabulate_rsp_subgroups(df)

  g_forest <- g_forest(tbl)

  expect_snapshot_ggplot("g_forest", g_forest, width = 15, height = 3)
})

testthat::test_that("g_forest works with custom arguments", {
  tbl <- rtable(
    header = rheader(
      rrow("", rcell("A", colspan = 2)),
      rrow("", "c1", "c2")
    ),
    rrow("row 1", 1, c(.8, 1.2)),
    rrow("row 2", 1.2, c(1.1, 1.4))
  )

  g_forest_custom <- g_forest(
    tbl = tbl,
    col_x = 1,
    col_ci = 2,
    xlim = c(0.5, 2),
    x_at = c(0.5, 1, 2),
    vline = 1,
    forest_header = c("Hello", "World")
  )

  expect_snapshot_ggplot("g_forest_custom", g_forest_custom, width = 4, height = 2)
})
