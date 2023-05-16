df <- data.frame(
  USUBJID = c(rep("1", 4), rep("2", 4), rep("3", 4)),
  AVISIT = c(
    rep("WEEK 1", 2), rep("WEEK 2", 2), rep("WEEK 1", 2), rep("WEEK 2", 2), rep("WEEK 1", 2), rep("WEEK 2", 2)
  ),
  PARAM = rep(c("ALT", "CPR"), 6),
  ANRIND = c("NORMAL", "NORMAL", "LOW", "HIGH", "LOW", "LOW", "HIGH", "HIGH", rep("NORMAL", 4))
)
df$ANRIND <- factor(df$ANRIND, levels = c("LOW", "HIGH", "NORMAL"))

testthat::test_that("h_map_for_count_abnormal returns the correct map for default method with healthy single input", {
  result <- h_map_for_count_abnormal(
    df = df,
    variables = list(anl = "ANRIND", split_rows = "PARAM"),
    abnormal = list(low = "LOW", high = "HIGH"),
    method = "default"
  )

  # because the function doesn't require the order of anl variable, but for unit test stability, we arrange the
  # order of anl variable within the split_rows group because the order of split_rows is something we want to check
  result <- result %>%
    dplyr::group_by(PARAM) %>%
    dplyr::arrange(ANRIND, .by_group = TRUE) %>%
    data.frame()

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_map_for_count_abnormal returns the correct map for range method with healthy single input", {
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
  result <- result %>%
    dplyr::group_by(PARAM) %>%
    dplyr::arrange(ANRIND, .by_group = TRUE) %>%
    data.frame()

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

# for default method, if LOW LOW and HIGH HIGH are not observed in the input dataset, they are dropped
testthat::test_that(
  "h_map_for_count_abnormal returns the correct map for default method with unused LOW LOW/HIGH HIGH input",
  code = {
    result <- h_map_for_count_abnormal(
      df = df,
      variables = list(anl = "ANRIND", split_rows = "PARAM"),
      abnormal = list(low = c("LOW", "LOW LOW"), high = c("HIGH", "HIGH")),
      method = "default"
    )

    # because the function doesn't require the order of anl variable, but for unit test stability, we arrange the
    # order of anl variable within the split_rows group because the order of split_rows is something we want to check
    result <- result %>%
      dplyr::group_by(PARAM) %>%
      dplyr::arrange(ANRIND, .by_group = TRUE) %>%
      data.frame()

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)

# for range method, if LOW LOW and HIGH HIGH are not observed in the input dataset but specified in the abnormal,
# they are kept in the map.
testthat::test_that(
  "h_map_for_count_abnormal returns the correct map for range method with unused LOW LOW/HIGH HIGH input",
  code = {
    df$ANRLO <- 5
    df$ANRHI <- 20

    result <- h_map_for_count_abnormal(
      df = df,
      variables = list(anl = "ANRIND", split_rows = "PARAM", range_low = "ANRLO", range_high = "ANRHI"),
      abnormal = list(low = c("LOW", "LOW LOW"), high = c("HIGH", "HIGH HIGH")),
      method = "range"
    )

    # because the function doesn't require the order of anl variable, but for unit test stability, we arrange the
    # order of anl variable within the split_rows group because the order of split_rows is something we want to check
    result <- result %>%
      dplyr::group_by(PARAM) %>%
      dplyr::arrange(ANRIND, .by_group = TRUE) %>%
      data.frame()

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)

# for default method, if only LOW LOW/HIGH HIGH is observed in the input dataset, the observed one will be kept.
testthat::test_that(
  "h_map_for_count_abnormal returns the correct map for default method with unused LOW LOW/HIGH HIGH input",
  code = {
    df <- df %>% dplyr::mutate(
      ANRIND = ifelse(PARAM == "ALT" & ANRIND == "LOW" & USUBJID == "1", "LOW LOW", as.character(ANRIND))
    )
    df$ANRIND <- factor(df$ANRIND, levels = c("LOW", "HIGH", "NORMAL", "LOW LOW", "HIGH HIGH"))
    result <- h_map_for_count_abnormal(
      df = df,
      variables = list(anl = "ANRIND", split_rows = "PARAM"),
      abnormal = list(low = c("LOW", "LOW LOW"), high = c("HIGH", "HIGH")),
      method = "default"
    )

    # because the function doesn't require the order of anl variable, but for unit test stability, we arrange the
    # order of anl variable within the split_rows group because the order of split_rows is something we want to check
    result <- result %>%
      dplyr::group_by(PARAM) %>%
      dplyr::arrange(ANRIND, .by_group = TRUE) %>%
      data.frame()

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)

# for range method, a theoretical map should be returned even that direction has zero counts, for the below example,
# every record is normal
testthat::test_that(
  "h_map_for_count_abnormal returns the correct map for range method with unused LOW LOW/HIGH HIGH input",
  code = {
    df$ANRLO <- 5
    df$ANRHI <- 20
    df$ANRIND <- "NORMAL"
    df$ANRIND <- factor(df$ANRIND, levels = c("LOW", "HIGH", "NORMAL"))

    result <- h_map_for_count_abnormal(
      df = df,
      variables = list(anl = "ANRIND", split_rows = "PARAM", range_low = "ANRLO", range_high = "ANRHI"),
      abnormal = list(low = c("LOW"), high = c("HIGH")),
      method = "range"
    )

    # because the function doesn't require the order of anl variable, but for unit test stability, we arrange the
    # order of anl variable within the split_rows group because the order of split_rows is something we want to check
    result <- result %>%
      dplyr::group_by(PARAM) %>%
      dplyr::arrange(ANRIND, .by_group = TRUE) %>%
      data.frame()

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)

# for range method, a theoretical map is built based on the rule at least one ANRLO >0 and one ANRHI not missing
testthat::test_that(
  "h_map_for_count_abnormal returns the correct map for range method with unused LOW LOW/HIGH HIGH input",
  code = {
    df$ANRLO <- 5
    df$ANRHI <- 20
    df$ANRLO <- ifelse(df$PARAM == "ALT", 0, df$ANRLO)
    df$ANRHI <- ifelse(df$PARAM == "CPR", NA, df$ANRHI)

    result <- h_map_for_count_abnormal(
      df = df,
      variables = list(anl = "ANRIND", split_rows = "PARAM", range_low = "ANRLO", range_high = "ANRHI"),
      abnormal = list(low = c("LOW"), high = c("HIGH")),
      method = "range"
    )

    # because the function doesn't require the order of anl variable, but for unit test stability, we arrange the
    # order of anl variable within the split_rows group because the order of split_rows is something we want to check
    result <- result %>%
      dplyr::group_by(PARAM) %>%
      dplyr::arrange(ANRIND, .by_group = TRUE) %>%
      data.frame()

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)
