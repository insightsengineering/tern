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

test_that("h_map_for_count_abnormal returns the correct map for default method with healthy single input", {

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

test_that("h_map_for_count_abnormal returns the correct map for range method with healthy single input", {

  df$ANRLO <- 5 #nolint
  df$ANRHI <- 20 #nolint

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

# for default method, if LOW LOW and HIGH HIGH are not observed in the input dataset, they are dropped
test_that(
  "h_map_for_count_abnormal returns the correct map for default method with unused LOW LOW/HIGH HIGH input", {
    result <- h_map_for_count_abnormal(
      df = df,
      variables = list(anl = "ANRIND", split_rows = "PARAM"),
      abnormal = list(low = c("LOW", "LOW LOW"), high = c("HIGH", "HIGH")),
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
  }
)

# for range method, if LOW LOW and HIGH HIGH are not observed in the input dataset but specified in the abnormal,
# they are kept in the map.
test_that(
  "h_map_for_count_abnormal returns the correct map for range method with unused LOW LOW/HIGH HIGH input", {
    df$ANRLO <- 5 #nolint
    df$ANRHI <- 20 #nolint

    result <- h_map_for_count_abnormal(
      df = df,
      variables = list(anl = "ANRIND", split_rows = "PARAM", range_low = "ANRLO", range_high = "ANRHI"),
      abnormal = list(low = c("LOW", "LOW LOW"), high = c("HIGH", "HIGH HIGH")),
      method = "range"
    )

    # because the function doesn't require the order of anl variable, but for unit test stability, we arrange the
    # order of anl variable within the split_rows group because the order of split_rows is something we want to check
    result <- result %>% group_by(PARAM) %>% arrange(ANRIND, .by_group = TRUE)

    expected <- data.frame(
      PARAM = c(rep("ALT", 5), rep("CPR", 5)),
      ANRIND = rep(c("HIGH", "HIGH HIGH", "LOW", "LOW LOW", "NORMAL"), 2)
    )
    expect_identical(as.matrix(result), as.matrix(expected))
  }
)

# for default method, if only LOW LOW/HIGH HIGH is observed in the input dataset, the observed one will be kept.
test_that(
  "h_map_for_count_abnormal returns the correct map for default method with unused LOW LOW/HIGH HIGH input", {
    df <- df %>% mutate(
      ANRIND = ifelse(PARAM == "ALT" & ANRIND == "LOW" & USUBJID == "1", "LOW LOW", as.character(ANRIND))
    )
    df$ANRIND <- factor(df$ANRIND, levels = c("LOW", "HIGH", "NORMAL", "LOW LOW", "HIGH HIGH")) #nolint
    result <- h_map_for_count_abnormal(
      df = df,
      variables = list(anl = "ANRIND", split_rows = "PARAM"),
      abnormal = list(low = c("LOW", "LOW LOW"), high = c("HIGH", "HIGH")),
      method = "default"
    )

    # because the function doesn't require the order of anl variable, but for unit test stability, we arrange the
    # order of anl variable within the split_rows group because the order of split_rows is something we want to check
    result <- result %>% group_by(PARAM) %>% arrange(ANRIND, .by_group = TRUE)

    expected <- data.frame(
      PARAM = c(rep("ALT", 4), rep("CPR", 3)),
      ANRIND = c(c("HIGH", "LOW", "LOW LOW", "NORMAL"), c("HIGH", "LOW", "NORMAL"))
    )
    expect_identical(as.matrix(result), as.matrix(expected))
  }
)

# for range method, a theoretical map should be returned even that direction has zero counts, for the below example,
# every record is normal
test_that(
  "h_map_for_count_abnormal returns the correct map for range method with unused LOW LOW/HIGH HIGH input", {
    df$ANRLO <- 5 #nolint
    df$ANRHI <- 20 #nolint
    df$ANRIND = "NORMAL" #nolint
    df$ANRIND <- factor(df$ANRIND, levels = c("LOW", "HIGH", "NORMAL")) #nolint

    result <- h_map_for_count_abnormal(
      df = df,
      variables = list(anl = "ANRIND", split_rows = "PARAM", range_low = "ANRLO", range_high = "ANRHI"),
      abnormal = list(low = c("LOW"), high = c("HIGH")),
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
  }
)
