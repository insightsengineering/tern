context("Test `rreplace_format`.")

format_01 <- "xx (xx%)"
format_02 <- "xx. (xx.%)"
format_03 <- "xx. (xx.%)"
format_04 <- function(x, output) "out"

tbl_01 <- rtable(
  header = c("Treatement\nN=100", "Comparison\nN=300"),
  format = format_01,
  rrow("A", c(104.867, .2874), c(100.571, .41489)),
  rrow("B", c(231.684, .4577), c(43.57, .5681))
)

tbl_02 <- rreplace_format(tbl_01, row = 1, new = format_02)
tbl_03 <- rreplace_format(tbl_02, col = 2, new = format_03)
tbl_04 <- rreplace_format(tbl_03, row = 2, old = format_01, new = format_04)

test_that("`rreplace_format()` changed format of a given row.", {
  expect_true(
    all(
      c(
        vapply(tbl_02[[1]], FUN = attr, which = "format", FUN.VALUE = "fvze") !=
          vapply(tbl_01[[1]], FUN = attr, which = "format", FUN.VALUE = "fvze"),
        vapply(
          tbl_02[[1]], FUN = attr, which = "format", FUN.VALUE = "fvze"
        ) == format_02
      )
    )
  )

})

test_that("`rreplace_format()` changed format of a given column.", {

  expect_true(
    all(
      c(
        vapply(
          tbl_03[1:2],
          FUN = function(x) attr(x[[2]], which = "format") == format_03,
          FUN.VALUE = TRUE
        ),

        vapply(
          tbl_03[1:2],
          FUN = function(x) {
            attr(x[[1]], which = "format") %in% c(format_01, format_02)
          },
          FUN.VALUE = TRUE
        )
      )
    )
  )

})

test_that("`rreplace_format()` changed a given old format by a new one.", {

  expect_true(
    all(
      attr(tbl_04[1, 1], "format") ==  format_02,
      attr(tbl_04[1, 2], "format") ==  format_03,
      attr(tbl_04[2, 2], "format") ==  format_03,
      is.function(attr(tbl_04[2, 1], "format"))
    )
  )

})
