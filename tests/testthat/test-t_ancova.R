library(dplyr)
library(rtables)

get_example_data <- function() {
  read.table(
    header = TRUE,
    sep = ";",
    stringsAsFactors = FALSE,
    text = '
      "USUBJID";"ARM";"STRATA1";"BMRKR2";"AVISIT";"BASE";"AVAL"
      "AB12345-BRA-1-id-105";"A: Drug X";"B";"MEDIUM";"WEEK 1 DAY 8";"67.88985864";"50.43875924"
      "AB12345-BRA-1-id-105";"A: Drug X";"B";"MEDIUM";"WEEK 2 DAY 15";"67.88985864";"61.50291623"
      "AB12345-BRA-1-id-105";"A: Drug X";"B";"MEDIUM";"WEEK 3 DAY 22";"67.88985864";"50.68271806"
      "AB12345-BRA-1-id-105";"A: Drug X";"B";"MEDIUM";"WEEK 4 DAY 29";"67.88985864";"43.149968"
      "AB12345-BRA-1-id-105";"A: Drug X";"B";"MEDIUM";"WEEK 5 DAY 36";"67.88985864";"55.5516058"
      "AB12345-BRA-1-id-134";"A: Drug X";"B";"LOW";"WEEK 1 DAY 8";"60.02499749";"55.08326531"
      "AB12345-BRA-1-id-134";"A: Drug X";"B";"LOW";"WEEK 2 DAY 15";"60.02499749";"43.87093725"
      "AB12345-BRA-1-id-134";"A: Drug X";"B";"LOW";"WEEK 3 DAY 22";"60.02499749";"61.68408546"
      "AB12345-BRA-1-id-134";"A: Drug X";"B";"LOW";"WEEK 4 DAY 29";"60.02499749";"51.24487629"
      "AB12345-BRA-1-id-134";"A: Drug X";"B";"LOW";"WEEK 5 DAY 36";"60.02499749";"47.51623139"
      "AB12345-BRA-1-id-141";"C: Combination";"B";"HIGH";"WEEK 1 DAY 8";"43.29633267";"42.46491872"
      "AB12345-BRA-1-id-141";"C: Combination";"B";"HIGH";"WEEK 2 DAY 15";"43.29633267";"50.88502511"
      "AB12345-BRA-1-id-141";"C: Combination";"B";"HIGH";"WEEK 3 DAY 22";"43.29633267";"51.65439907"
      "AB12345-BRA-1-id-141";"C: Combination";"B";"HIGH";"WEEK 4 DAY 29";"43.29633267";"63.62737932"
      "AB12345-BRA-1-id-141";"C: Combination";"B";"HIGH";"WEEK 5 DAY 36";"43.29633267";"68.82593785"
      "AB12345-BRA-1-id-236";"B: Placebo";"A";"HIGH";"WEEK 1 DAY 8";"56.39948748";"58.12445932"
      "AB12345-BRA-1-id-236";"B: Placebo";"A";"HIGH";"WEEK 2 DAY 15";"56.39948748";"45.74268906"
      "AB12345-BRA-1-id-236";"B: Placebo";"A";"HIGH";"WEEK 3 DAY 22";"56.39948748";"62.35864011"
      "AB12345-BRA-1-id-236";"B: Placebo";"A";"HIGH";"WEEK 4 DAY 29";"56.39948748";"54.26595123"
      "AB12345-BRA-1-id-236";"B: Placebo";"A";"HIGH";"WEEK 5 DAY 36";"56.39948748";"49.47182985"
      "AB12345-BRA-1-id-265";"C: Combination";"A";"MEDIUM";"WEEK 1 DAY 8";"62.81857503";"42.59818754"
      "AB12345-BRA-1-id-265";"C: Combination";"A";"MEDIUM";"WEEK 2 DAY 15";"62.81857503";"53.01259956"
      "AB12345-BRA-1-id-265";"C: Combination";"A";"MEDIUM";"WEEK 3 DAY 22";"62.81857503";"59.02291642"
      "AB12345-BRA-1-id-265";"C: Combination";"A";"MEDIUM";"WEEK 4 DAY 29";"62.81857503";"45.08336743"
      "AB12345-BRA-1-id-265";"C: Combination";"A";"MEDIUM";"WEEK 5 DAY 36";"62.81857503";"59.64104106"
      "AB12345-BRA-1-id-42";"A: Drug X";"B";"MEDIUM";"WEEK 1 DAY 8";"48.2728554";"52.92784753"
      "AB12345-BRA-1-id-42";"A: Drug X";"B";"MEDIUM";"WEEK 2 DAY 15";"48.2728554";"49.84078804"
      "AB12345-BRA-1-id-42";"A: Drug X";"B";"MEDIUM";"WEEK 3 DAY 22";"48.2728554";"49.07336306"
      "AB12345-BRA-1-id-42";"A: Drug X";"B";"MEDIUM";"WEEK 4 DAY 29";"48.2728554";"70.31214801"
      "AB12345-BRA-1-id-42";"A: Drug X";"B";"MEDIUM";"WEEK 5 DAY 36";"48.2728554";"41.85491921"
      "AB12345-BRA-1-id-65";"B: Placebo";"A";"MEDIUM";"WEEK 1 DAY 8";"53.37767135";"55.48675537"
      "AB12345-BRA-1-id-65";"B: Placebo";"A";"MEDIUM";"WEEK 2 DAY 15";"53.37767135";"58.33272863"
      "AB12345-BRA-1-id-65";"B: Placebo";"A";"MEDIUM";"WEEK 3 DAY 22";"53.37767135";"35.04338482"
      "AB12345-BRA-1-id-65";"B: Placebo";"A";"MEDIUM";"WEEK 4 DAY 29";"53.37767135";"48.20473285"
      "AB12345-BRA-1-id-65";"B: Placebo";"A";"MEDIUM";"WEEK 5 DAY 36";"53.37767135";"48.32984847"
      "AB12345-BRA-1-id-93";"A: Drug X";"A";"LOW";"WEEK 1 DAY 8";"71.71464916";"47.70996987"
      "AB12345-BRA-1-id-93";"A: Drug X";"A";"LOW";"WEEK 2 DAY 15";"71.71464916";"53.07874642"
      "AB12345-BRA-1-id-93";"A: Drug X";"A";"LOW";"WEEK 3 DAY 22";"71.71464916";"51.2307956"
      "AB12345-BRA-1-id-93";"A: Drug X";"A";"LOW";"WEEK 4 DAY 29";"71.71464916";"51.83555366"
      "AB12345-BRA-1-id-93";"A: Drug X";"A";"LOW";"WEEK 5 DAY 36";"71.71464916";"60.62970709"'
  ) %>%
    dplyr::mutate(
      ARM = factor(
        ARM,
        levels = c("B: Placebo", "A: Drug X", "C: Combination")
      ),
      AVISIT = factor(
        AVISIT,
        levels = c("WEEK 1 DAY 8", "WEEK 2 DAY 15", "WEEK 3 DAY 22", "WEEK 4 DAY 29", "WEEK 5 DAY 36")
      )
    )
}

test_that("Elementary ANCOVA table works correctly", {
  anl <- get_example_data() %>%
    dplyr::filter(AVISIT == "WEEK 1 DAY 8")
  result <- t_el_ancova(
    formula = AVAL ~ arm(ARM) + BASE + BMRKR2 + STRATA1,
    data = anl
  )
  expected <- rtable(
    header = rheader(
      rrowl(NULL, "B: Placebo", "A: Drug X", "C: Combination"),
      rrowl(NULL, "(N=2)", "(N=4)", "(N=2)")
    ),
    rrow("n", 2, 4, 2),
    rrow("Adjusted Mean", 59.19, 50.23, 43.09),
    rrowl("Difference in Adjusted Means", list(NULL, -8.97, -16.1), format = "xx.xx"),
    rrowl("95% CI", list(NULL, c(-134.93, 117), c(-85.67, 53.46)), format = "xx.xx - xx.xx", indent = 1),
    rrowl("p-value", list(NULL, 0.5320, 0.2086), format = "x.xxxx | (<0.0001)", indent = 1)
  )
  comp <- compare_rtables(result, expected, comp.attr = FALSE)
  expect_true(all(comp == "."), "t_el_ancova does not produce same results any longer")
})

test_that("Compound ANCOVA table works correctly with a factor `row_by` variable", {
  anl <- get_example_data() %>%
    dplyr::filter(AVISIT %in% c("WEEK 1 DAY 8", "WEEK 2 DAY 15"))
  result <- t_ancova(
    formula = AVAL ~ arm(ARM) + BASE + BMRKR2 + STRATA1,
    data = anl,
    row_by = anl$AVISIT
  )
  expected <- rtable(
    header = rheader(
      rrowl(NULL, "B: Placebo", "A: Drug X", "C: Combination"),
      rrowl(NULL, "(N=2)", "(N=4)", "(N=2)")
    ),
    rrow("WEEK 1 DAY 8"),
    rrow("n", 2, 4, 2, indent = 1),
    rrow("Adjusted Mean", 59.19, 50.23, 43.09, indent = 1),
    rrowl("Difference in Adjusted Means", list(NULL, -8.97, -16.1), format = "xx.xx", indent = 1),
    rrowl("95% CI", list(NULL, c(-134.93, 117), c(-85.67, 53.46)), format = "xx.xx - xx.xx", indent = 2),
    rrowl("p-value", list(NULL, 0.5320, 0.2086), format = "x.xxxx | (<0.0001)", indent = 2),
    rrow(""),
    rrow("WEEK 2 DAY 15"),
    rrow("n", 2, 4, 2, indent = 1),
    rrow("Adjusted Mean", 52.81, 49.32, 51.89, indent = 1),
    rrowl("Difference in Adjusted Means", list(NULL, -3.5, -0.93), format = "xx.xx", indent = 1),
    rrowl("95% CI", list(NULL, c(-289.11, 282.12), c(-158.66, 156.81)), format = "xx.xx - xx.xx", indent = 2),
    rrowl("p-value", list(NULL, 0.9018, 0.9525), format = "x.xxxx | (<0.0001)", indent = 2)
  )
  comp <- compare_rtables(result, expected, comp.attr = FALSE)
  expect_true(all(comp == "."), "t_ancova does not produce same results any longer")
})
