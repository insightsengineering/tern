context("compare stream outputs for atezo data in the atezo.data R package")

# read about testthat (hadley wickham)
#   - unit test (read only a bit about the philosophy)
library(testthat)
library(tern)

library(atezo.data)
library(dplyr)

test_that("demographic table", {

  ASL <- asl(com.roche.cdt30019.go29436.re)
  tbl_stream <- get_demographic_table(com.roche.cdt30019.go29436.re)

  # order factor levels if they exist
  of <- function(x, ...) {
    lvls <- if (is.factor(x)) levels(x) else unique(x)
    olvls <- intersect(unlist(list(...)), lvls)
    factor(as.character(x), levels = c(olvls, setdiff(lvls, olvls)))
  }
  add_lbl <- function(lbl, x) {
    structure(x, label = lbl)
  }
  lbl <- function(x) {
    attr(x, "label")
  }
  
  ADSL <- ASL %>%
    filter(ITTWTFL == 'Y') %>%
    mutate(ARM = of(ARM, "DUMMY C", "DUMMY B", "DUMMY A")) %>%
    mutate(SEX = add_lbl(lbl(SEX), recode_factor(SEX, M = "MALE", F = "FEMALE")))
  
  ## change factor orders library(forcats)
  ## ?fct_relevel
  
  tbl <- t_summarize_variables(
    data = subset(ADSL, select = toupper(c("sex", "mliver", "tciclvl2", "bage", "age4cat",
                                           "ethnic", "race", "bwt", "tobhx", "hist", "EGFRMUT",
                                           "alkmut", "krasmut", "becog"))),
    col_by = factor(ADSL$ARM),
    total = "All Patients"
  )

  # Viewer(tbl, tbl_stream)
  # comp <- compare_rtables(tbl, tbl_stream)
  
  expect_identical(tbl, tbl_stream, "tables are not identical")
  
#  expect_identical(names(tbl), names(tbl_stream), "table headers missmatch")
#  expect_identical(dim(tbl), dim(tbl_stream), "dimension missmatch")


})




test_that("response table", {
  
  
})