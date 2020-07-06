context("test t_contingency")

dta <- c(rep("A", 5), rep("B", 2), rep("C", 3))

test_that(
  "`n_freq` provides good estimations of absolute and relative frequencies.",
  expect_true(all(n_freq(1:5, 10) == c(5, .5)))
)

test_that(
  "Absolute frequencies in `t_contingency` match `table` results.",
  code = {
    res <- t_contingency(row_grp = dta, col = dta, row = dta)

    expect_true(
      all(
        c(# Manual picking of values in `rtable`.
          res[[2]][[1]][1],
          res[[8]][[2]][1],
          res[[14]][[3]][1]
        ) ==  diag(table(dta, dta))
      )
    )
  }
)

test_that(
  "Relative frequencies in `t_contingency` match `table` results.",
  code = {
    res <- t_contingency(row_grp = rep("test", 10), col = dta, row = dta)
    res <- lapply(res[-1], function(x) c(x[[1]][2], x[[2]][2], x[[3]][2]))
    expect_true(
      all(
        matrix(unlist(res), ncol = 3) ==  table(dta, dta) / length(dta)
      )
    )
  }
)

test_that(
  "`add_NA`: all factor levels are interpreted and \"Missing\" is added when expected.",
  expect_true(
    all(
      c(unique(dta), "Missing") %in% levels(add_na(dta)),
      !("Missing" %in% levels(add_na(dta, ifany = TRUE))),
      "Missing" %in% levels(add_na(c(dta, NA)))
    )
  )
)

test_that(
  "`add_NA`: The character string for NA can be modified.",
  expect_true(
    "Not estimated" %in% levels(
      add_na(c(dta, NA), ifany = TRUE, na.strings = "Not estimated")
    )
  )
)

test_that(
  "`isnotnull_isatomic` returns NULL when not null and is atomic",
  expect_silent(isnotnull_isatomic(dta))
)

test_that(
  "`isnotnull_isatomic` detects NULL.",
  expect_error(isnotnull_isatomic(NULL), "`tern::t_contingency`:  is NULL.")
)

test_that(
  "`isnotnull_isatomic` detects non-atomic.",
  expect_error(
    isnotnull_isatomic(list()), "`tern::t_contingency`: list is not atomic."
  )
)
