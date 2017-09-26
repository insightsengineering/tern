context("test_time_to_event_table")

## read in your data
set.seed(1)
ATE <- data.frame(
  AVAL = rnorm(100)+50,
  PARAM = "OS",
  CNSR = sample(c('Y', 'N'), 100, replace = TRUE),
  stringsAsFactors = FALSE
)

X <- time_to_event_table(
  time_to_event = ATE$AVAL,
  is_event = ATE$CNSR == 'N'
)

test_that("Patients with Event is correct", {
  expect_equal(X[['Patients with Events (%)']][[1]], c(5, .2))
  
  expect_equal(X[['Patients with Events (%)']][[2]], c(4, .4))
})



test_that("Patients without Event is correct", {
  expect_equal(X[['Patients without Events (%)']][[1]], c(43, .6))
 # expect_equal(X[['Patients without Events (%)']][[2]], c(3, .5))
})
