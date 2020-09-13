#' Contingency table (stacked per group)
#'
#' This produces a stacked list of contingency tables, one table per group.
#' The tables report the absolute (n) and relative frequency (%) for
#' every combination of modalities between two factors. For each group, the
#' frequencies (n) add up to the group size (N) and percentages are calculated
#' using N as the denominator.
#'
#' @param row_grp,col,row The three factors used for row grouping,
#'   column modalities and row modalities, respectively. The three factors are
#'   expected to be of equal length.
#' @param row_grp_lab,row_lab,col_lab Optional labels for groups of rows,
#'   rows and columns annotation.
#' @param na.strings A character string for transformation of missing data
#'   into explicit factor level.
#'
#' @details
#' Every group is associated with a table composed of a headline where the
#' group name and size (N) is reported and a contingency table.
#' The table is annotated with the `label` attributes of `row_grp`, `col` and
#' `row`. If the `label` attribute is missing, the annotation is left blank,
#' unless labels are manually set via the corresponding `*_lab` arguments.
#'
#' The function is expected to be robust to the classes of the provided objects and
#' handling of missing values: non-factors are coerced to factors and
#' missing values are transformed into explicit factor levels.
#'
#' @template return_rtable
#'
#' @import rtables
#' @export
#'
#' @md
#'
#' @examples
#'
#' # Example 1:
#' # ==========
#'
#' n <- 50
#' g <- sample(LETTERS[1:3], size = n, replace = TRUE)
#' x <- sample(letters[1:3], size = n, replace = TRUE)
#' y <- sample(1:3, size = n, replace = TRUE)
#'
#' t_contingency(g, x, y)
#'
#' # Example 2:
#' # ==========
#'
#' g <- sample(c(rep(NA, n/5), x), size = n)
#' x <- sample(c(rep(NA, n/5), y), size = n)
#' y <- sample(c(rep(NA, n/5), g), size = n)
#'
#' t_contingency(
#'   row_grp = g,
#'   col = x,
#'   row = y,
#'   na.strings = "Missing",
#'   row_grp_lab = "Treatment",
#'   row_lab = "Levels of Factor A",
#'   col_lab = "Levels of factor B"
#' )
#'
#' # Example 3:
#' # ==========
#'
#' attr(g, "label") <- "Arm"
#' attr(x, "label") <- "Baseline"
#' attr(y, "label") <- "Post-Baseline"
#'
#' t_contingency(g, y, x)
#'
#' # Example 4:
#' # ==========
#'
#' g <- x <- y <- rep(NA, 50)
#'
#' t_contingency(g, y, x)
t_contingency <- function(row_grp,
                          col,
                          row,
                          row_grp_lab = NULL,
                          col_lab = NULL,
                          row_lab = NULL,
                          na.strings = "Missing") { # nolint

  isnotnull_isatomic(row_grp)
  isnotnull_isatomic(col)
  isnotnull_isatomic(row)

  if (!(length(row_grp) == length(col) && length(row_grp) == length(row))) {
    stop("`row_grp`, `col`, `row` have different lengths.")
  }

  # Labels are managed first since attributes are "volatile".
  if (is.null(row_grp_lab)) row_grp_lab <- attr(row_grp, "label")
  if (is.null(col_lab)) col_lab <- attr(col, "label")
  # An empty content in header cell is not accepted by `rtables`.
  if (is.null(col_lab)) col_lab <- " "
  if (is.null(row_lab)) row_lab <- attr(row, "label")

  # Prepare a list of dataframes corresponding to `row_grp`.
  row_grp <- add_na(row_grp, ifany = TRUE, na.strings = na.strings)
  col <- add_na(col, ifany = TRUE, na.strings = na.strings)
  row <- add_na(row, ifany = TRUE, na.strings = na.strings)

  df <- split(
    x = data.frame(col = col, row = row),
    f = row_grp
  )

  # Contingency tables comparable to the results of `table`.
  t_el_tables <- lapply(
    df, function(x) {
      rtables::rtabulate(
        x = x$row,
        col_by = x$col,
        FUN = n_freq,
        N = nrow(x),
        format = "xx (xx.x%)",
        indent = 1
      )
    }
  )

  # Every level of row_grp is associated with a headline where N is reported.
  t_el_headlines <- Map(
    function(name, df) paste0(name, " (N=", nrow(df), ")"),
    name = names(df),
    df = df
  )

  # Stack healines and tables.
  tbl <- rtables::rbindl_rtables(
    Map(
      f = stack_t_el,
      headline = t_el_headlines,
      table = t_el_tables
    ),
    gap = 1
  )

  # Use the variable labels in the final (r)table.
  rtables::header(tbl) <-  do.call(
    rtables::rheader,
    c(
      list(
        rtables::rrow(
          NULL, rtables::rcell(col_lab, colspan = nlevels(col)))
        ),
      rtables::header(tbl)
    )
  )

  if (!is.null(row_grp_lab)) rtables::header_row.names(tbl)[1] <- row_grp_lab
  if (!is.null(row_lab)) rtables::header_row.names(tbl)[2] <- row_lab
  rtables::header_indent(tbl) <- c(0, 1)

  return(tbl)

}

#' Absolute and relative frequency in a vector of length 2.
#'
#' This is especially use full when big "N" is not defined by the column sum.
#' In that case, the denominator is directly specified.
#'
#' @param x An absolute frequency.
#' @param N The specified denominator for the relative frequency.
#'
#' @noRd
#' @md
#' @return A numeric vector of lenght 2

n_freq <- function(x,
                   N) { # nolint

  n <- length(x)
  y <- c(n, n / N)

  return(y)

}

#' Stack the headlines and contingency tabulation
#'
#' The `t_contingency` is a stacked list of contingency table, each contingency
#' table is preceeded by a headline accouting for group description.
#'
#' @param headline A character string.
#' @param table A `rtable` obtained with `rtabulate` (the contingency table).
#'
#' @noRd
#' @md
#'
#' @template return_rtable
stack_t_el <- function(headline,
                       table) {

  ncell <- ncol(table)
  tbl <- rbind(
    rtables::rtable(
      header = rtables::header(table),
      rtables::rrowl(headline, rep(rtables::rcell(NULL), ncell))
    ),
    table
  )
  return(tbl)

}

#' Add a factor level for missing data.
#'
#' Encode a vector as a factor with a level for missing values.
#'
#' @param x A vector potentially with missing data.
#' @inheritParams base::addNA
#' @param na.strings Label for the missing values transformed into explicit
#'   factor level.
#'
#' @note
#' Adaptation of `base::addNA`, as required for a better handling of NA values
#' by `rtables::rtabulate`.
#'
#' @return A factor.
#'
#' @noRd
#' @md
#' @examples
#' dta <- c(rep("A", 5), rep("B", 2), rep("C", 3))
#'
#' # Default: all factor levels are interpreted and "Missing" is added:
#' add_na(dta)
#'
#' # All factor levels are interpreted and "Missing" is added only if any:
#' add_na(dta, ifany = TRUE)
#'
#' # The character string for NA can be modified:
#' add_na(c(dta, NA), ifany = TRUE, na.strings = "Not estimated")
add_na <- function (x, ifany = FALSE, na.strings = "Missing")  { # nolint

  if (!is.factor(x)) x <- factor(x)

  if (ifany && !anyNA(x)) return(x)

  ll <- levels(x)

  if (!anyNA(ll)) {

    ll <- c(ll, na.strings)
    x <- as.character(x)
    x[is.na(x)] <- na.strings
    x <- factor(x, levels = ll, exclude = NULL)

  }

  return(x)

}


#' Check if not null and is atomic
#'
#' @param x An object which is supposed to be not null and being atomic
#' @noRd
#' @md
isnotnull_isatomic <- function(x) {

  if (is.null(x)) {
    stop(paste("`tern::t_contingency`:", paste(substitute(x), "is NULL.")))
  }

  if (!is.atomic(x)) {
    stop(paste("`tern::t_contingency`:", paste(substitute(x), "is not atomic.")))
  }

  invisible()

}
