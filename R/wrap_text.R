#' Split String
#'
#' Return splitted text that fits onto page width.
#'
#' @param txt string (or vector of strins) to be split in multiple lines, not that
#' \code{\\n} is also split into to lines
#' @param width max with of string, by default the width of the current viewport
#' @param gp graphical parameters for text
#'
#' @return e vector with the new strings
#'
#' @importFrom grid convertWidth gpar grobWidth textGrob
#'
#' @noRd
#'
#' @author Adrian Waddell
#'
#' @examples
#' text <- "This is a test with many words and more"
#' teal.modules.clinical:::wrap_text(txt = text, width = unit(4, "cm"), collapse = "\n")
#' teal.modules.clinical:::wrap_text(txt = text, width = unit(5, "cm"), collapse = "\n")
#'
wrap_text <- function(txt, # nousage # nolint
                      width = convertWidth(unit(1, "npc"), "inch", TRUE),
                      gp = gpar(),
                      collapse = NULL) {

  if (is.unit(width)) {
    width <- convertWidth(width, "inch", TRUE)
  }

  if (length(txt) == 0) {
    return(character(0))
  }

  g_string_width <- function(label) {
    vapply(label,
           function(lab) convertWidth(grobWidth(textGrob(lab, gp = gp)), "inch", TRUE),
           numeric(1)
    )
  }

  space_width <- g_string_width(" ")
  width_s <- width - space_width

  # splits a string into multiple strings that fit
  splitstr <- function(str) {
    if (g_string_width(str) > width_s) {
      strs <- unlist(strsplit(str, " ", fixed = TRUE))
      n <- length(strs)

      if (n <= 1) {
        str
      } else {
        strsw <- g_string_width(strs) + space_width
        part <- rep(NA_integer_, n)
        # string partition
        k <- 1
        part[1] <- k
        s <- strsw[1] # current width
        for (i in 2:n) {
          if (s + strsw[i] > width_s) {
            k <- k + 1
            s <- strsw[i]
          } else {
            s <- s + strsw[i]
          }
          part[i] <- k
        }
        as.vector(tapply(strs, part, function(x) paste(x, collapse = " "), simplify = TRUE))
      }
    } else {
      str
    }
  }
  strs_to_split <- unlist(
    Map(
      function(x) {
        if (length(x) == 0) {
          ""
        } else {
          x
        }
      },
      strsplit(txt, "\n", fixed = TRUE)
    )
  )

  strings <- unlist(
    Map(
      function(str) {
        splitstr(str)
      },
      strs_to_split
    ),
    use.names = FALSE
  )

  if (!is.null(collapse)) {
    paste(strings, collapse = collapse)
  } else {
    strings
  }
}
