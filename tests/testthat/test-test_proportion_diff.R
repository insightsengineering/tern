
test_that("prop_chisq returns right result", {

  set.seed(1)
  rsp <- c(
    sample(c(TRUE, FALSE), size = 20, prob = c(3 / 4, 1 / 4), replace = TRUE),
    sample(c(TRUE, FALSE), size = 20, prob = c(1 / 2, 1 / 2), replace = TRUE)
  )
  grp <- c(rep("A", 20), rep("B", 20))
  tbl <- table(grp, rsp)

  result <- prop_chisq(tbl)
  expected <- 0.0565
  expect_equal(result, expected, tolerance = 1e-4, check.attributes = FALSE)

})

test_that("prop_cmh returns right result", {

  set.seed(1)
  rsp <- sample(c(TRUE, FALSE), 100, TRUE)
  grp <- factor(rep(c("A", "B"), each = 50))
  strata <- factor(rep(c("V", "W", "X", "Y", "Z"), each = 20))
  tbl <- table(grp, rsp, strata)

  result <- prop_cmh(tbl)
  expected <- 0.6477
  expect_equal(result, expected, tolerance = 1e-4, check.attributes = FALSE)

})

test_that("prop_fisher returns right result", {

  set.seed(1)
  rsp <- c(
    sample(c(TRUE, FALSE), size = 20, prob = c(3 / 4, 1 / 4), replace = TRUE),
    sample(c(TRUE, FALSE), size = 20, prob = c(1 / 2, 1 / 2), replace = TRUE)
  )
  grp <- c(rep("A", 20), rep("B", 20))
  tbl <- table(grp, rsp)

  result <- prop_fisher(tbl)
  expected <- 0.1110
  expect_equal(result, expected, tolerance = 1e-4, check.attributes = FALSE)

})


test_that("prop_schouten returns right result", {

  set.seed(1)
  rsp <- c(
    sample(c(TRUE, FALSE), size = 20, prob = c(3 / 4, 1 / 4), replace = TRUE),
    sample(c(TRUE, FALSE), size = 20, prob = c(1 / 2, 1 / 2), replace = TRUE)
  )
  grp <- c(rep("A", 20), rep("B", 20))
  tbl <- table(grp, rsp)

  result <- prop_schouten(tbl)
  expected <- 0.0843
  expect_equal(result, expected, tolerance = 1e-4, check.attributes = FALSE)

})
