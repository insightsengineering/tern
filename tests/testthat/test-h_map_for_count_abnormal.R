library(dplyr)
df <- data.frame(
  USUBJID = c(rep("1", 4), rep("2", 4), rep("3", 4)),
  AVISIT = c(
    rep("WEEK 1", 2), rep("WEEK 2", 2), rep("WEEK 1", 2), rep("WEEK 2", 2), rep("WEEK 1", 2), rep("WEEK 2", 2)
  ),
  PARAM = rep(c("ALT", "CPR"), 6),
  ANRIND = c("NORMAL", "NORMAL", "LOW", "HIGH", "LOW", "LOW", "HIGH", "HIGH", rep("NORMAL", 4))
)
df$ANRIND <- factor(df$ANRIND, levels = c("LOW", "HIGH", "NORMAL")) #nolint

test_that("h_map_for_count_abnormal returns the correct map for default method with healthy input", {

  result <- h_map_for_count_abnormal(
    df = df,
    variables = list(anl = "ANRIND", split_rows = "PARAM"),
    abnormal = list(low = "LOW", high = "HIGH"),
    method = "default"
  )

  # because the function doesn't require the order of anl variable, but for unit test stability, we arrange the
  # order of anl variable within the split_rows group because the order of split_rows is something we want to check
  result <- result %>% group_by(PARAM) %>% arrange(ANRIND, .by_group = TRUE)

  expected <- data.frame(
    PARAM = c(rep("ALT", 3), rep("CPR", 3)),
    ANRIND = rep(c("HIGH", "LOW", "NORMAL"), 2)
  )
  expect_identical(as.matrix(result), as.matrix(expected))
})

test_that("h_map_for_count_abnormal returns the correct map for range method with healthy input", {

  df$ANRLO <- 5
  df$ANRHI <- 20

  result <- h_map_for_count_abnormal(
    df = df,
    variables = list(anl = "ANRIND", split_rows = "PARAM", range_low = "ANRLO", range_high = "ANRHI"),
    abnormal = list(low = "LOW", high = "HIGH"),
    method = "range"
  )

  # because the function doesn't require the order of anl variable, but for unit test stability, we arrange the
  # order of anl variable within the split_rows group because the order of split_rows is something we want to check
  result <- result %>% group_by(PARAM) %>% arrange(ANRIND, .by_group = TRUE)

  expected <- data.frame(
    PARAM = c(rep("ALT", 3), rep("CPR", 3)),
    ANRIND = rep(c("HIGH", "LOW", "NORMAL"), 2)
  )
  expect_identical(as.matrix(result), as.matrix(expected))
})
