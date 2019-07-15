context("keys")
library(random.cdisc.data)
asl <- cadsl
adrs <- cadrs
adte <- cadtte

test_that("check keys arguments", {
  expect_error(keys(asl, c("STUDYID", "SUBJID")), "unused argument")
  expect_error(keys(as.matrix(asl)) <- c("STUDYID", "SUBJID"), "is.data.frame")
  expect_error(keys(asl) <- factor(c("STUDYID", "SUBJID")), "is.character")
  expect_error(keys(asl) <- c("STUDYID", "unknown column"), "all\\(value %in% names\\(x\\)\\)")
})

test_that("keys assigned", {
  asl2 <- asl
  keys(asl) <- c("STUDYID", "SUBJID")
  attr(asl2, "keys") <- c("STUDYID", "SUBJID")

  expect_identical(asl, asl2)
  expect_identical(keys(asl), c("STUDYID", "SUBJID"))
  expect_identical(keys(asl), attr(asl, "keys"))
})

test_that("check duplicated on keys", {

  expect_silent(keys(asl) <- c("STUDYID", "SUBJID"))
  expect_silent(keys(asl) <- c("USUBJID"))
  expect_silent(keys(adte) <- c("STUDYID", "SUBJID", "PARAM"))
  expect_silent(keys(adrs) <- c("STUDYID", "SUBJID", "PARAM", "AVISIT"))

  expect_error(keys(asl) <- c("STUDYID"), "keys don't uniquely distinguish the rows")
  expect_error(keys(adte) <- c("STUDYID", "SUBJID"), "keys don't uniquely distinguish the rows")
  expect_error(keys(adrs) <- c("STUDYID", "SUBJID", "PARAM"), "keys don't uniquely distinguish the rows")
})
