test_that("proportion estimation by Wilson's score interval is right", {

  rsp <- c(
    TRUE, TRUE, TRUE, TRUE, TRUE,
    FALSE, FALSE, FALSE, FALSE, FALSE
  )

  expected <- c(0.2692718, 0.7307282)
  result <- prop_wilson(rsp, conf_level = 0.9)

  expect_equal(expected, result, tolerance = 1e-5)
}
)
