
testthat::test_that("prop_wilson returns right result", {
  rsp <- c(
    TRUE, TRUE, TRUE, TRUE, TRUE,
    FALSE, FALSE, FALSE, FALSE, FALSE
  )

  expected <- c(0.2692718, 0.7307282)
  result <- prop_wilson(rsp, conf_level = 0.9)

  testthat::expect_equal(expected, result, tolerance = 1e-5)
})

testthat::test_that("prop_clopper_pearson returns right result", {
  rsp <- c(
    TRUE, TRUE, TRUE, TRUE, TRUE,
    FALSE, FALSE, FALSE, FALSE, FALSE
  )

  result <- prop_clopper_pearson(rsp, conf_level = .95)
  expected <- c(0.1871, 0.8129)
  testthat::expect_equal(expected, result, tolerance = 1e-4)
})

testthat::test_that("prop_wald returns right result", {
  rsp <- c(
    TRUE, TRUE, TRUE, TRUE, TRUE,
    FALSE, FALSE, FALSE, FALSE, FALSE
  )

  result <- prop_wald(rsp, conf_level = 0.95, correct = TRUE)
  expected <- c(0.1401, 0.8599)
  testthat::expect_equal(expected, result, tolerance = 1e-4)

  result <- prop_wald(rsp, conf_level = 0.95, correct = FALSE)
  expected <- c(0.1901, 0.8099)
  testthat::expect_equal(expected, result, tolerance = 1e-4)
})

testthat::test_that("prop_agresti_coull returns right result", {
  rsp <- c(
    TRUE, TRUE, TRUE, TRUE, TRUE,
    FALSE, FALSE, FALSE, FALSE, FALSE
  )

  result <- prop_agresti_coull(rsp, conf_level = 0.95)
  expected <- c(0.2366, 0.7634)
  testthat::expect_equal(expected, result, tolerance = 1e-4)
})


testthat::test_that("prop_jeffreys returns right result", {
  rsp <- c(
    TRUE, TRUE, TRUE, TRUE, TRUE,
    FALSE, FALSE, FALSE, FALSE, FALSE
  )

  result <- prop_jeffreys(rsp, conf_level = 0.95)
  expected <- c(0.2235, 0.7765)
  testthat::expect_equal(expected, result, tolerance = 1e-4)
})

testthat::test_that("s_proportion returns right result", {
  result <- s_proportion(c(1, 0, 1, 0))
  expected <- list(
    n_prop = c(2, .5),
    prop_ci = c(0, 100)
  )
  testthat::expect_equal(expected, result, tolerance = 1e-4, check.attributes = FALSE)
})

testthat::test_that("`s_proportion` works with Jeffreys CI", {

  # "Mid" case.
  rsp <- c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)
  result <- s_proportion(
    x = rsp,
    conf_level = 0.9,
    method = "jeffreys"
  )
  # according to SAS.
  expected <- list(
    n_prop = c(4, 4 / 6),
    prop_ci = formatters::with_label(
      c(34.0802, 89.5730),
      label = "90% CI for Response Rates (Jeffreys)"
    )
  )
  testthat::expect_equal(result, expected, tol = 0.0001, check.attributes = FALSE)

  # Corner case: Only responders.
  rsp <- c(TRUE, TRUE, TRUE, TRUE)
  result <- s_proportion(
    x = rsp,
    conf_level = 0.95,
    method = "jeffreys"
  )
  # according to SAS.
  expected <- list(
    n_prop = c(4, 1),
    prop_ci = formatters::with_label(
      x = c(55.5237, 100),
      label = "95% CI for Response Rates (Jeffreys)"
    )
  )
  testthat::expect_equal(result, expected, tol = 0.0001, check.attributes = FALSE)
})

testthat::test_that("`s_proportion` works with Agresti-Coull CI", {

  # "Mid" case.
  rsp <- c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)
  result <- s_proportion(
    x = rsp,
    conf_level = 0.9,
    method = "agresti-coull"
  )
  # according to SAS.
  expected <- list(
    n_prop = c(4, 4 / 6),
    prop_ci = formatters::with_label(
      c(34.3585, 88.6154),
      label = "90% CI for Response Rates (Agresti-Coull)"
    )
  )
  testthat::expect_equal(result, expected, tol = 0.0001, check.attributes = FALSE)

  # Edge case: Only responders.
  rsp <- c(TRUE, TRUE, TRUE, TRUE)
  result <- s_proportion(
    x = rsp,
    conf_level = 0.95,
    method = "agresti-coull"
  )
  # according to SAS.
  expected <- list(
    n_prop = c(4, 1),
    prop_ci = formatters::with_label(
      x = c(45.4050, 100),
      label = "95% CI for Response Rates (Agresti-Coull)"
    )
  )
  # Small additional difference acknowledged here.
  testthat::expect_equal(result, expected, tol = 0.00011, check.attributes = FALSE)
})
