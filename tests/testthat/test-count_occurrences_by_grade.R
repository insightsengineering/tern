get_test_data_simple <- function() {

  df <- data.frame(
    USUBJID = as.character(c(1:5, 1)),
    USUBJID2 = as.character(c(1:5, 1) * 10),
    AETOXGR = factor(c(1, 2, 3, 1, 2, 3), levels = c(1:5)),
    AESEV = factor(
      c("MILD", "MODERATE", "SEVERE", "MILD", "MODERATE", "SEVERE"),
      levels = c("MILD", "MODERATE", "SEVERE")
    ),
    ARM = factor(c("A", "A", "A", "B", "B", "A")),
    stringsAsFactors = FALSE
  )

  df
}

test_that("h_append_grade_groups works with valid input", {
  result <- h_append_grade_groups(
    list(
      "Any Grade" = as.character(1:5),
      "Grade 1-2" = c("1", "2"),
      "Grade 3-4" = c("3", "4")
      ),
    list("1" = 10L, "2" = 7L, "3" = 2L, "4" = 2L, "5" = 0L)
  )

  expected <- list(
    "Any Grade" = 21L,
    "Grade 1-2" = 17L,
    "1" = 10L,
    "2" = 7L,
    "Grade 3-4" = 4L,
    "3" = 2L,
    "4" = 2L,
    "5" = 0L
  )
  expect_equal(result, expected)
})

test_that("s_count_occurrences_by_grade works with valid input and default arguments for grade", {
  df <- get_test_data_simple()
  result <- s_count_occurrences_by_grade(df = df, .var = "AETOXGR", .N_col = 10)

  expected <- list(count_percent = c(
    "1" = list(c(1L, 0.1)),
    "2" = list(c(2L, 0.2)),
    "3" = list(c(2L, 0.2)),
    "4" = list(c(0, 0)),
    "5" = list(c(0, 0))

  ))
  expect_equal(result, expected)
})

test_that("s_count_occurrences_by_grade works with valid input for grade grouping", {
  df <- get_test_data_simple()
  result <- s_count_occurrences_by_grade(
    df = df,
    .var = "AETOXGR",
    .N_col = 10,
    grade_groups = list(
      "Any Grade" = as.character(1:5),
      "Grade 1-2" = c("1", "2"),
      "Grade 3-4" = c("3", "4")
    )
  )

  expected <- list(count_percent = c(
    "Any Grade" = list(c(5L, 0.5)),
    "Grade 1-2" = list(c(3L, 0.3)),
    "1" = list(c(1L, 0.1)),
    "2" = list(c(2L, 0.2)),
    "Grade 3-4" = list(c(2L, 0.2)),
    "3" = list(c(2L, 0.2)),
    "4" = list(c(0, 0)),
    "5" = list(c(0, 0))
  ))
  expect_equal(result, expected)
})

test_that("s_count_occurrences_by_grade works with valid input for intensity and custom arguments", {
  df <- get_test_data_simple()
  result <- s_count_occurrences_by_grade(
    df = df,
    .var = "AESEV",
    .N_col = 10,
    id = "USUBJID2",
    grade_groups = list(
      "Any Intensity" = c("MILD", "MODERATE", "SEVERE")
    ))

  expected <- list(count_percent = c(
    "Any Intensity" = list(c(5L, 0.5)),
    "MILD" = list(c(1L, 0.1)),
    "MODERATE" = list(c(2L, 0.2)),
    "SEVERE" = list(c(2L, 0.2))
  ))
  expect_equal(result, expected)
})
