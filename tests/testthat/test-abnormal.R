testthat::test_that("s_count_abnormal works with healthy input and default arguments", {
  abn_levels <- c("LOW", "NORMAL", "HIGH")

  df <- data.frame(
    USUBJID = as.character(c(1, 1, 2, 2)),
    ANRIND = factor(c("NORMAL", "LOW", "HIGH", "HIGH"), levels = abn_levels),
    BNRIND = factor(c("NORMAL", "NORMAL", "HIGH", "HIGH"), levels = abn_levels),
    ONTRTFL = c("", "Y", "", "Y"),
    stringsAsFactors = FALSE
  )

  df <- df %>% dplyr::filter(
    ONTRTFL == "Y"
  )

  # Check with LOW and HIGH abnormality.
  result <- s_count_abnormal(
    df = df,
    .var = "ANRIND",
    abnormal = list(high = "HIGH", low = "LOW")
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_count_abnormal works when excluding patients with abnormality at baseline", {
  abn_levels <- c("LOW", "NORMAL", "HIGH")

  df <- data.frame(
    USUBJID = as.character(c(1, 1, 2, 2, 3, 3)),
    ANRIND = factor(c("NORMAL", "LOW", "HIGH", "HIGH", "LOW", "HIGH"), levels = abn_levels),
    BNRIND = factor(c("NORMAL", "NORMAL", "HIGH", "HIGH", "LOW", "LOW")),
    ONTRTFL = c("", "Y", "", "Y", "", "Y"),
    stringsAsFactors = FALSE
  )

  df <- df %>% dplyr::filter(
    ONTRTFL == "Y"
  )

  # Check with LOW and HIGH abnormality.
  result <- s_count_abnormal(
    df = df,
    .var = "ANRIND",
    abnormal = list(high = "HIGH", low = "LOW"),
    exclude_base_abn = TRUE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_count_abnormal also works with tibble and custom arguments", {
  abn_levels <- c("LOW", "NORMAL", "HIGH")

  df <- tibble::as_tibble(
    data.frame(
      myid = as.character(c(1, 1, 1, 2, 2, 2)),
      myrange = factor(c("LOW", "NORMAL", "LOW", "NORMAL", "HIGH", "HIGH"), levels = abn_levels),
      myblrange = factor(c("LOW", "LOW", "LOW", "HIGH", "HIGH", "HIGH"), levels = abn_levels),
      mytrtfl = c("", "", "Y", "", "", "Y"),
      stringsAsFactors = FALSE
    )
  )
  df <- df %>% dplyr::filter(
    mytrtfl == "Y"
  )

  # Check with HIGH and LOW abnormality.
  result <- s_count_abnormal(
    df = df,
    .var = "myrange",
    abnormal = list(high = "HIGH", low = "LOW"),
    variables = list(id = "myid", baseline = "myblrange"),
    exclude_base_abn = TRUE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("count_abnormal works with default arguments", {
  abn_levels <- c("LOW", "NORMAL", "HIGH")

  df <- data.frame(
    USUBJID = as.character(c(1, 1, 2, 2)),
    ANRIND = factor(c("NORMAL", "LOW", "HIGH", "HIGH"), levels = abn_levels),
    BNRIND = factor(c("NORMAL", "NORMAL", "HIGH", "HIGH"), levels = abn_levels),
    ONTRTFL = c("", "Y", "", "Y")
  )

  df <- df %>%
    dplyr::filter(
      ONTRTFL == "Y"
    )

  result <- basic_table() %>%
    count_abnormal(var = "ANRIND", abnormal = list(low = "LOW", high = "HIGH"), exclude_base_abn = TRUE) %>%
    build_table(df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("count_abnormal works with custom arguments", {
  abn_levels <- c("LOW", "NORMAL", "HIGH")

  df2 <- data.frame(
    ID = as.character(c(1, 1, 2, 2)),
    RANGE = factor(c("NORMAL", "LOW", "HIGH", "HIGH"), levels = abn_levels),
    BL_RANGE = factor(c("NORMAL", "NORMAL", "HIGH", "HIGH"), levels = abn_levels),
    ONTRTFL = c("", "Y", "", "Y"),
    stringsAsFactors = FALSE
  )

  df2 <- df2 %>%
    dplyr::filter(
      ONTRTFL == "Y"
    )

  result <- basic_table() %>%
    count_abnormal(
      var = "RANGE",
      abnormal = list("< LLN" = "LOW", "> ULN" = "HIGH"),
      variables = list(id = "ID", baseline = "BL_RANGE"),
      .indent_mods = c(fraction = 1L),
      .formats = c(fraction = "xx / xx"),
      exclude_base_abn = TRUE
    ) %>%
    build_table(df2)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("count_abnormal works with default arguments and visit", {
  abn_levels <- c("LOW", "NORMAL", "HIGH")
  visit_levels <- c("BASELINE", "WEEK 1", "WEEK 2")

  df <- data.frame(
    USUBJID = as.character(c(1, 1, 1, 2, 2, 2)),
    AVISIT = factor(rep(c("BASELINE", "WEEK 1", "WEEK 2"), 2), levels = visit_levels),
    ANRIND = factor(c("NORMAL", "LOW", "LOW", "HIGH", "HIGH", "NORMAL"), levels = abn_levels),
    BNRIND = factor(c("NORMAL", "NORMAL", "NORMAL", "HIGH", "HIGH", "HIGH"), levels = abn_levels),
    ONTRTFL = c("", "Y", "Y", "", "Y", "Y")
  )

  df <- df %>%
    dplyr::filter(
      ONTRTFL == "Y"
    )

  result <- basic_table() %>%
    split_rows_by("AVISIT", split_fun = drop_split_levels) %>%
    count_abnormal(var = "ANRIND", abnormal = list(low = "LOW", high = "HIGH")) %>%
    build_table(df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_count_abnormal works with healthy input and grouped abnormal arguments", {
  abn_levels <- c("LOW", "NORMAL", "HIGH", "LOW LOW", "HIGH HIGH")

  df <- data.frame(
    USUBJID = as.character(c(1, 1, 2, 2, 3, 4)),
    ANRIND = factor(c("NORMAL", "LOW", "HIGH", "HIGH", "LOW LOW", "HIGH HIGH"), levels = abn_levels),
    BNRIND = factor(c("NORMAL", "NORMAL", "HIGH", "HIGH", "NORMAL", "NORMAL"), levels = abn_levels),
    ONTRTFL = c("", "Y", "", "Y", "Y", "Y"),
    stringsAsFactors = FALSE
  )

  df <- df %>% dplyr::filter(
    ONTRTFL == "Y"
  )

  # Check with LOW and HIGH abnormality.
  result <- s_count_abnormal(
    df = df,
    .var = "ANRIND",
    abnormal = list(high = c("HIGH", "HIGH HIGH"), low = c("LOW", "LOW LOW"))
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
