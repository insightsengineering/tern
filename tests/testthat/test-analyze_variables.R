testthat::test_that("s_summary return NA for x length 0L", {
  x <- numeric()

  result <- s_summary(x)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_summary handles NA", {
  x <- c(NA_real_, 1)

  # With `na.rm = TRUE`.
  result <- s_summary(x)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # With `na.rm = FALSE`.
  result <- s_summary(x, na.rm = FALSE)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_summary returns right results for n = 2", {
  x <- c(NA_real_, 1, 2)
  result <- s_summary(x)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_summary returns right results for n = 8", {
  x <- c(NA_real_, 1, 2, 5, 6, 7, 8, 9, 10)
  result <- s_summary(x)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_summary works with factors", {
  x <- factor(c("Female", "Male", "Female", "Male", "Male", "Unknown", "Unknown", "Unknown", "Unknown"))

  result <- s_summary(x)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_summary fails with factors that have no levels or have empty string levels", {
  x <- factor(c("Female", "Male", "Female", "Male", "Male", "Unknown", "Unknown", "Unknown", "Unknown", ""))
  testthat::expect_error(s_summary(x))
  testthat::expect_error(s_summary(factor()))
})

testthat::test_that("s_summary works when factors have NA levels", {
  x <- factor(c("Female", "Male", "Female", "Male", "Unknown", "Unknown", NA))
  result <- s_summary(x, na.rm = FALSE)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_summary works with factors with NA values handled and correctly removes them by default", {
  x <- factor(c("Female", "Male", "Female", "Male", "Male", "Unknown", "Unknown", "Unknown", "Unknown", NA))
  x <- explicit_na(x)

  result <- s_summary(x)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_summary works with length 0 factors that have levels", {
  x <- factor(levels = c("a", "b", "c"))

  result <- s_summary(x)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_summary works with factors and different denominator choices", {
  x <- factor(c("Female", "Male", "Female", "Male", "Male", "Unknown", "Unknown", "Unknown", "Unknown"))

  result <- s_summary(x, denom = "N_row", .N_row = 20)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  result <- s_summary(x, denom = "N_col", .N_col = 30)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_summary works with characters by converting to character", {
  x <- c("Female", "Male", "Female", "Male", "Male", "Unknown", "Unknown", "Unknown", "Unknown")

  testthat::expect_warning(result <- s_summary(x, denom = "N_row", .N_row = 20, .var = "SEX"))
  expected <- s_summary(factor(x), denom = "N_row", .N_row = 20)

  testthat::expect_identical(result, expected)
})

testthat::test_that("s_summary works with characters by converting to character and handling empty strings", {
  x <- c("Female", "Male", "Female", "Male", "Male", "", "Unknown", "Unknown", "Unknown", "Unknown")

  testthat::expect_warning(result <- s_summary(x, .var = "foo", na.rm = FALSE, denom = "N_row", .N_row = 10))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_summary does not work for length 0 character vectors", {
  x <- character()
  suppressWarnings(testthat::expect_error(s_summary(x, denom = "n", .var = "foo")))
})

testthat::test_that("s_summary works with logical vectors", {
  x <- c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE)

  result <- s_summary(x)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_summary works with length 0 logical vectors", {
  result <- s_summary(as.logical(c()))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_summary works with logical vectors and by default removes NA", {
  x <- c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, NA, NA)

  result <- s_summary(x)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_summary works with logical vectors and by if requested does not remove NA from n", {
  x <- c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, NA, NA)

  result <- s_summary(x, na.rm = FALSE)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("a_summary work with healthy input.", {
  options("width" = 100)

  # numeric input - a_summary
  set.seed(1)
  x <- rnorm(10)
  result <- a_summary(
    x = x, .N_col = 10, .N_row = 20, .var = "bla", .df_row = NULL, .ref_group = NULL, .in_ref_col = FALSE,
    compare = FALSE, .stats = get_stats("analyze_vars_numeric"), na.rm = TRUE, na_str = default_na_str()
  )
  res_out <- testthat::expect_silent(result)

  # numeric input - a_summary
  result <- a_summary(x = x, .N_col = 10, .N_row = 20, .var = "bla")
  res <- testthat::expect_silent(result)
  testthat::expect_identical(res_out, res)
  testthat::expect_snapshot(res)

  # factor input - a_summary
  x <- factor(c("a", "a", "b", "c", "a"))
  result <- a_summary(
    x = x, .N_col = 10, .N_row = 10, .var = "bla", .df_row = NULL, .ref_group = NULL, .in_ref_col = FALSE,
    compare = FALSE, .stats = get_stats("analyze_vars_counts"),
    na.rm = TRUE, na_str = default_na_str()
  )
  res_out <- testthat::expect_silent(result)

  # factor input - a_summary
  result <- a_summary(x = x, .N_row = 10, .N_col = 10)
  res <- testthat::expect_silent(result)
  testthat::expect_identical(res_out, res)
  testthat::expect_snapshot(res)

  # character input - a_summary
  x <- c("A", "B", "A", "C")
  result <- a_summary(
    x = x, .N_col = 10, .N_row = 10, .var = "x", .df_row = NULL, .ref_group = NULL, .in_ref_col = FALSE,
    compare = FALSE, .stats = get_stats("analyze_vars_counts"),
    na.rm = TRUE, na_str = default_na_str(),
    verbose = FALSE
  )
  res_out <- testthat::expect_silent(result)

  # character input - a_summary
  result <- a_summary(x = x, .var = "x", .N_col = 10, .N_row = 10, verbose = FALSE)
  res <- testthat::expect_silent(result)
  testthat::expect_identical(res_out, res)
  testthat::expect_snapshot(res)

  # logical input - a_summary
  x <- c(TRUE, FALSE, FALSE, TRUE, TRUE)
  result <- a_summary(
    x = x, .N_col = 10, .N_row = 10, .var = NULL, .df_row = NULL, .ref_group = NULL, .in_ref_col = FALSE,
    compare = FALSE, .stats = get_stats("analyze_vars_counts"),
    na.rm = TRUE, na_str = default_na_str()
  )
  res_out <- testthat::expect_silent(result)

  # logical input - a_summary
  result <- a_summary(x = x, .N_row = 10, .N_col = 10)
  res <- testthat::expect_silent(result)
  testthat::expect_identical(res_out, res)
  testthat::expect_snapshot(res)
})

testthat::test_that("a_summary works with custom input.", {
  options("width" = 100)
  result <- a_summary(
    rnorm(10),
    .N_col = 10, .N_row = 20, control_summarize_vars(conf_level = 0.90), .stats = c("sd", "median_ci"),
    .formats = c(sd = "xx.", median_ci = "xx.xx - xx.xx"), .labels = c(sd = "std. dev"), .indent_mods = 3L
  )
  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  result <- a_summary(
    factor(c("a", "a", "b", "c", NA)),
    .N_row = 10, .N_col = 10, .formats = c(n = "xx.xx"),
    .labels = c(n = "number of records"), .indent_mods = c(n = -1L, count = 5L), na.rm = FALSE
  )
  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("a_summary works with healthy input when compare = TRUE.", {
  options("width" = 100)
  # numeric input
  set.seed(1)
  result <- a_summary(rnorm(10, 5, 1), .ref_group = rnorm(20, -5, 1), .var = "bla", compare = TRUE)
  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # factor input
  result <- a_summary(
    factor(c("a", "a", "b", "c", "a")),
    .ref_group = factor(c("a", "a", "b", "c")), compare = TRUE
  )
  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # character input
  result <- a_summary(c("A", "B", "A", "C"), .ref_group = c("B", "A", "C"), .var = "x", compare = TRUE, verbose = FALSE)
  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # logical input
  result <- a_summary(c(TRUE, FALSE, FALSE, TRUE, TRUE), .ref_group = c(TRUE, FALSE), compare = TRUE)
  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("a_summary works with custom input when compare = TRUE.", {
  options("width" = 100)
  result <- a_summary(
    rnorm(10),
    .ref_group = rnorm(20, -5, 1), .N_col = 10, .N_row = 20, control_summarize_vars(conf_level = 0.90),
    .stats = c("pval", "median_ci"), .formats = c(median_ci = "xx.xx - xx.xx"), .labels = c(pval = "pvalue"),
    .indent_mods = 3L, compare = TRUE
  )
  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  result <- a_summary(
    factor(c("a", "a", "b", "c", NA)),
    .ref_group = factor(c("a", "a", "b", "c")), .N_row = 10, .N_col = 10,
    .formats = c(n = "xx.xx"), .labels = c(n = "number of records"), .indent_mods = c(n = -1L, count = 5L),
    na.rm = FALSE, compare = TRUE
  )
  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("`analyze_vars` works with healthy input, default `na.rm = TRUE`.", {
  dta_test <- data.frame(AVAL = c(1:4, NA, NA))

  l <- basic_table() %>%
    analyze_vars(vars = "AVAL")
  result <- build_table(l, df = dta_test)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("`analyze_vars` works with healthy input, and control function.", {
  dta_test <- data.frame(AVAL = c(1:9))

  l <- basic_table() %>%
    analyze_vars(
      vars = "AVAL",
      control = control_analyze_vars(quantiles = c(0.1, 0.9), conf_level = 0.9),
      .stats = c("n", "mean_sd", "mean_se", "mean_ci", "quantiles")
    )
  result <- build_table(l, df = dta_test)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("`analyze_vars` works with healthy input, alternative `na.rm = FALSE`", {
  dta_test <- data.frame(AVAL = c(1:4, NA, NA))

  l <- basic_table() %>%
    analyze_vars(vars = "AVAL", na.rm = FALSE)
  result <- build_table(l, df = dta_test)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("`analyze_vars` works with healthy factor input", {
  dta <- data.frame(foo = factor(c("a", "b", "a")))

  result <- basic_table() %>%
    analyze_vars(vars = "foo") %>%
    build_table(dta)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("`analyze_vars` works with healthy factor input, alternative `na.rm = FALSE`", {
  dta <- data.frame(foo = factor(c("a", NA, "b", "a", NA)))

  result <- basic_table() %>%
    analyze_vars(vars = "foo", na.rm = FALSE) %>%
    build_table(dta)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  dta <- df_explicit_na(dta)

  result <- basic_table() %>%
    analyze_vars(vars = "foo", na.rm = FALSE) %>%
    build_table(dta)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("`analyze_vars` works with factors and different denominators", {
  start <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    split_rows_by("SEX", split_fun = drop_split_levels) %>%
    add_rowcounts()

  result_n <- start %>%
    analyze_vars("RACE", denom = "n") %>%
    build_table(DM)

  result_ncol <- start %>%
    analyze_vars("RACE", denom = "N_col") %>%
    build_table(DM)

  result <- start %>%
    analyze_vars("RACE", denom = "N_row") %>%
    build_table(DM)

  testthat::expect_false(identical(result_n, result_ncol))
  testthat::expect_false(identical(result_n, result))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("analyze_vars works in demographic table example", {
  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("RACE") %>%
    analyze_vars("COUNTRY", .stats = "count_fraction") %>%
    build_table(DM)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("`analyze_vars` works with character input and gives the same result as with factor", {
  dta <- data.frame(
    foo = c("a", "b", "a"),
    stringsAsFactors = FALSE
  )

  l <- basic_table() %>%
    analyze_vars(vars = "foo")
  testthat::expect_warning(result <- build_table(l, dta))

  dta_factor <- dta %>%
    dplyr::mutate(foo = factor(foo))
  expected <- build_table(l, dta_factor)

  testthat::expect_identical(result, expected)
})

testthat::test_that("`analyze_vars` does not work with sparse character input due to missing statistics", {
  dta <- data.frame(
    foo = c("a", "b", "a"),
    boo = c("e", "e", "f"),
    stringsAsFactors = FALSE
  )

  l <- basic_table() %>%
    split_cols_by("boo") %>%
    analyze_vars(vars = "foo")
  suppressWarnings(testthat::expect_error(testthat::expect_warning(build_table(l, dta))))

  # But when converting to factor, it works because we keep the levels information across columns.
  dta_factor <- dta %>%
    dplyr::mutate(foo = factor(foo))
  testthat::expect_silent(build_table(l, dta_factor))
})

testthat::test_that("`analyze_vars` works with logical input", {
  dta <- data.frame(
    boo = c(TRUE, FALSE, FALSE, TRUE, TRUE)
  )

  result <- basic_table() %>%
    analyze_vars(vars = "boo") %>%
    build_table(dta)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("`analyze_vars` works with healthy logical input, alternative `na.rm = FALSE`", {
  dta <- data.frame(foo = factor(c(TRUE, NA, FALSE, TRUE, NA)))

  result <- basic_table() %>%
    analyze_vars(vars = "foo", na.rm = FALSE) %>%
    build_table(dta)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  dta <- df_explicit_na(dta)

  result <- basic_table() %>%
    analyze_vars(vars = "foo", na.rm = FALSE) %>%
    build_table(dta)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("`analyze_vars` works with empty named numeric variables", {
  dta <- tibble::tibble(
    foo = factor(c("a", "a", "b", "b", "c", "c"), levels = c("a", "b", "c")),
    boo = 1:6
  )
  dta <- dta %>% dplyr::filter(foo != "a")
  names(dta$boo) <- dta$foo

  result <- basic_table() %>%
    split_cols_by("foo") %>%
    analyze_vars(vars = "boo") %>%
    build_table(dta)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("analyze_vars 'na_str' argument works as expected", {
  dta <- data.frame(
    USUBJID = rep(1:6, each = 3),
    AVISIT  = rep(paste0("V", 1:3), 6),
    ARM     = rep(LETTERS[1:3], rep(6, 3)),
    AVAL    = c(9:1, rep(NA, 9))
  )

  result <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    split_rows_by(var = "AVISIT") %>%
    analyze_vars(vars = "AVAL", na_str = "-") %>%
    build_table(dta)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("control_analyze_vars works with customized parameters", {
  result <- control_analyze_vars(
    conf_level = 0.9,
    quantiles = c(0.1, 0.9)
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("control_analyze_vars fails wrong inputs", {
  testthat::expect_error(control_analyze_vars(quantiles = c(25, 75)))
  testthat::expect_error(control_analyze_vars(conf_level = 95))
})

# Deprecated functions ------
testthat::test_that("create_afun_summary creates an `afun` that works and throws a warning", {
  testthat::expect_warning(afun <- create_afun_summary(
    .stats = c("n", "count_fraction", "median", "range", "mean_ci"),
    .formats = c(median = "xx."),
    .labels = c(median = "My median"),
    .indent_mods = c(median = 1L)
  ))
  dta_test <- data.frame(
    USUBJID = rep(1:6, each = 3),
    PARAMCD = rep("lab", 6 * 3),
    AVISIT = rep(paste0("V", 1:3), 6),
    ARM = rep(LETTERS[1:3], rep(6, 3)),
    AVAL = c(9:1, rep(NA, 9)),
    stringsAsFactors = TRUE
  )

  l <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    split_rows_by(var = "AVISIT") %>%
    analyze(vars = c("AVAL", "ARM"), afun = afun)

  # From visual inspection it works as before, same output
  testthat::expect_silent(result <- build_table(l, df = dta_test))
})

testthat::test_that("analyze_vars works correctly with auto formats", {
  dt <- data.frame("VAR" = c(0.001, 0.2, 0.0011000, 3, 4))
  res <- basic_table() %>%
    analyze_vars(
      vars = "VAR",
      .stats = c("n", "mean", "mean_sd", "range"),
      .formats = c("mean_sd" = "auto", "range" = "auto")
    ) %>%
    build_table(dt)

  result <- testthat::expect_silent(res)
  testthat::expect_snapshot(res)
})
