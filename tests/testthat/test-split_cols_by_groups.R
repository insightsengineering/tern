# groups_list_to_df ----
testthat::test_that("groups_list_to_df works as expected", {
  grade_groups <- list(
    "Any Grade (%)" = c("1", "2", "3", "4", "5"),
    "Grade 3-4 (%)" = c("3", "4"),
    "Grade 5 (%)" = "5"
  )
  result <- groups_list_to_df(grade_groups) %>% data.frame()

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

# combine_groups ----

testthat::test_that("combine_groups combines character vectors", {
  testthat::expect_warning(result <- combine_groups(fct = c("A", "B", "C")))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("combine_groups combines factors", {
  result <- testthat::expect_silent(combine_groups(fct = factor(c("A", "B", "C"))))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("combine_groups combines factors with given reference", {
  result <- combine_groups(fct = factor(c("A", "B", "C")), ref = "B")

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("combine_groups use good separator", {
  result <- combine_groups(fct = factor(c("A", "B", "C")), collapse = "||")

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("combine_groups can use multiple reference", {
  result <- combine_groups(
    fct = factor(c("A", "B", "C")),
    ref = c("C", "B"),
    collapse = "&"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

# split_cols_by_groups ----
testthat::test_that("split_cols_by_groups manages combinations of columns", {
  groups <- list(
    "Arms A+B" = c("A: Drug X", "B: Placebo"),
    "Arms A+C" = c("A: Drug X", "C: Combination")
  )
  result <- basic_table() %>%
    split_cols_by_groups("ARM", groups) %>%
    add_colcounts() %>%
    analyze("AGE") %>%
    build_table(DM)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("split_cols_by_groups manages combinations of columns with reference", {
  groups <- list(
    "Arms A+B" = c("A: Drug X", "B: Placebo"),
    "Arms A+C" = c("A: Drug X", "C: Combination")
  )
  result <- basic_table() %>%
    split_cols_by_groups("ARM", groups_list = groups, ref_group = "Arms A+B") %>%
    analyze(
      "AGE",
      afun = function(x, .ref_group, .in_ref_col) {
        if (.in_ref_col) {
          in_rows("Diff. of Averages" = rcell(NULL))
        } else {
          in_rows("Diff. of Averages" = rcell(mean(x) - mean(.ref_group), format = "xx.xx"))
        }
      }
    ) %>%
    build_table(DM)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("split_cols_by_groups equivalent to split_cols_by when no groups", {
  result <- basic_table() %>%
    split_cols_by_groups("ARM") %>%
    add_colcounts() %>%
    analyze("AGE") %>%
    build_table(DM)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("split_cols_by_groups equivalent to split_cols_by with ref_col but no groups", {
  afun <- function(x, .ref_group, .in_ref_col) {
    if (.in_ref_col) {
      in_rows("Diff. of Averages" = rcell(NULL))
    } else {
      in_rows("Diff. of Averages" = rcell(mean(x) - mean(.ref_group), format = "xx.xx"))
    }
  }
  result <- basic_table() %>%
    split_cols_by_groups("ARM", ref_group = "B: Placebo", split_fun = ref_group_position("first")) %>%
    add_colcounts() %>%
    analyze("AGE", afun = afun) %>%
    build_table(DM)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("split_cols_by_groups manages combinations of columns with reference and alt_counts_df", {
  groups <- list(
    "Arms A+B" = c("A: Drug X", "B: Placebo"),
    "Arms A+C" = c("A: Drug X", "C: Combination")
  )

  DM_ANL <- DM[1:100, ] # nolint

  result <- basic_table() %>%
    split_cols_by_groups("ARM", groups_list = groups, ref_group = "Arms A+B") %>%
    add_colcounts() %>%
    analyze(
      "AGE",
      afun = function(x, .ref_group, .in_ref_col) {
        if (.in_ref_col) {
          in_rows("Diff. of Averages" = rcell(NULL))
        } else {
          in_rows("Diff. of Averages" = rcell(mean(x) - mean(.ref_group), format = "xx.xx"))
        }
      }
    ) %>%
    build_table(DM_ANL, alt_counts_df = DM)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

# combine_counts ----
testthat::test_that("combine_counts combines character vectors", {
  fct <- c("A", "A", "A", "B", "B", "C")
  testthat::expect_warning(grp <- combine_groups(fct = fct))
  testthat::expect_warning(result <- combine_counts(fct, grp))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("combine_counts combines factors", {
  fct <- factor(c("A", "A", "A", "B", "B", "C"))
  grp <- combine_groups(fct = fct)
  result <- combine_counts(fct, grp)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("combine_counts combines factors", {
  fct <- factor(c("A", "A", "A", "B", "B", "C"))
  grp <- combine_groups(fct = fct, ref = c("A", "C"))
  result <- combine_counts(fct, grp)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("combine_counts with groups_list NULL", {
  fct <- factor(c("A", "A", "A", "B", "B", "C"))
  grp <- combine_groups(fct = fct, ref = c("A", "C"))
  result <- combine_counts(fct)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
