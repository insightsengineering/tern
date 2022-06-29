
# groups_list_to_df ----
testthat::test_that("groups_list_to_df works as expected", {
  grade_groups <- list(
    "Any Grade (%)" = c("1", "2", "3", "4", "5"),
    "Grade 3-4 (%)" = c("3", "4"),
    "Grade 5 (%)" = "5"
  )
  result <- groups_list_to_df(grade_groups)
  expected <- structure(
    list(
      valname = c("AnyGrade", "Grade34", "Grade5"),
      label = c("Any Grade (%)", "Grade 3-4 (%)", "Grade 5 (%)"),
      levelcombo = list(c("1", "2", "3", "4", "5"), c("3", "4"), "5"),
      exargs = list(list(), list(), list())
    ),
    row.names = c(NA, -3L),
    class = c("tbl_df", "tbl", "data.frame")
  )
  testthat::expect_identical(result, expected)
})

# combine_groups ----

testthat::test_that("combine_groups combines character vectors", {
  result <- testthat::expect_warning(combine_groups(fct = c("A", "B", "C")))
  expected <- list(A = "A", `B/C` = c("B", "C"))
  testthat::expect_identical(result, expected)
})

testthat::test_that("combine_groups combines factors", {
  result <- testthat::expect_silent(combine_groups(fct = factor(c("A", "B", "C"))))
  expected <- list(A = "A", `B/C` = c("B", "C"))
  testthat::expect_identical(result, expected)
})

testthat::test_that("combine_groups combines factors with given reference", {
  result <- combine_groups(
    fct = factor(c("A", "B", "C")),
    ref = "B"
  )
  expected <- list(B = "B", `A/C` = c("A", "C"))
  testthat::expect_identical(result, expected)
})

testthat::test_that("combine_groups use good separator", {
  result <- combine_groups(
    fct = factor(c("A", "B", "C")),
    collapse = "||"
  )
  expected <- list(A = "A", `B||C` = c("B", "C"))
  testthat::expect_identical(result, expected)
})

testthat::test_that("combine_groups can use multiple reference", {
  result <- combine_groups(
    fct = factor(c("A", "B", "C")),
    ref = c("C", "B"),
    collapse = "&"
  )
  expected <- list(`B&C` = c("B", "C"), A = "A")
  testthat::expect_identical(result, expected)
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
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "Mean", "Arms A+B", "(N=227)", "34.03", "Arms A+C",
      "(N=250)", "34.73"
    ),
    .Dim = c(3L, 3L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
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
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "Diff. of Averages", "Arms A+B", "", "Arms A+C",
      "0.71"
    ),
    .Dim = 2:3
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("split_cols_by_groups equivalent to split_cols_by when no groups", {
  result <- basic_table() %>%
    split_cols_by_groups("ARM") %>%
    add_colcounts() %>%
    analyze("AGE") %>%
    build_table(DM)

  expected <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    analyze("AGE") %>%
    build_table(DM)

  testthat::expect_identical(result, expected)
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
    split_cols_by_groups("ARM", ref_group = "B: Placebo") %>%
    add_colcounts() %>%
    analyze("AGE", afun = afun) %>%
    build_table(DM)

  expected <- basic_table() %>%
    split_cols_by("ARM", ref_group = "B: Placebo") %>%
    add_colcounts() %>%
    analyze("AGE", afun = afun) %>%
    build_table(DM)

  testthat::expect_identical(result, expected)
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

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "Diff. of Averages", "Arms A+B", "(N=227)",
      "", "Arms A+C", "(N=250)", "-0.75"
    ),
    .Dim = c(3L, 3L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

# combine_counts ----
testthat::test_that("combine_counts combines character vectors", {
  fct <- c("A", "A", "A", "B", "B", "C")
  grp <- testthat::expect_warning(combine_groups(fct = fct))
  result <- testthat::expect_warning(combine_counts(fct, grp))
  expected <- c(A = 3, `B/C` = 3)
  testthat::expect_identical(result, expected)
})

testthat::test_that("combine_counts combines factors", {
  fct <- factor(c("A", "A", "A", "B", "B", "C"))
  grp <- combine_groups(fct = fct)
  result <- combine_counts(fct, grp)
  expected <- c(A = 3, `B/C` = 3)
  testthat::expect_identical(result, expected)
})

testthat::test_that("combine_counts combines factors", {
  fct <- factor(c("A", "A", "A", "B", "B", "C"))
  grp <- combine_groups(fct = fct, ref = c("A", "C"))
  result <- combine_counts(fct, grp)
  expected <- c(`A/C` = 4, B = 2)
  testthat::expect_identical(result, expected)
})

testthat::test_that("combine_counts with groups_list NULL", {
  fct <- factor(c("A", "A", "A", "B", "B", "C"))
  grp <- combine_groups(fct = fct, ref = c("A", "C"))
  result <- combine_counts(fct)
  expected <- c(A = 3, B = 2, C = 1)
  testthat::expect_identical(result, expected)
})
