#' Summarize an Object for Different Groups
#'
#' @inheritParams t_summary.data.frame
#' @param x an object to dispatch on
#' @param ... arguments passed on to methods
#'
#' @export
#'
#' @seealso \code{\link{t_summary.data.frame}},
#'   \code{\link{t_summary.numeric}}, \code{\link{t_summary.factor}},
#'   \code{\link{t_summary.logical}}, \code{\link{t_summary.Date}},
#'   \code{\link{t_summary.character}}, \code{\link{t_summary_by}}
#'
#' @examples
#' t_summary(iris$Sepal.Length, iris$Species)
#'
#' library(random.cdisc.data)
#' ADSL <- cadsl # radsl(N = 100, seed = 1)
#'
#' t_summary(ADSL$AGE, ADSL$ARMCD)
#' t_summary(ADSL[, c("AGE", "SEX", "RACE")], ADSL$ARMCD)
#' with(ADSL, t_summary(AGE > 65, ARMCD))
t_summary <- function(x,
                      col_by,
                      col_N = default_col_N(col_by), # nolint
                      ...) {
  UseMethod("t_summary", x)
}

# todo: to utils.R
#' adds col_by to col_by if it is non-null
#' @importFrom rtables by_add_total
treat_total <- function(x, col_by, total) {
  if (is.null(total)) {
    col_by
  } else {
    stopifnot(is.character.single(total))
    warning("total argument is deprecated")
    by_add_total(col_by, label = total, n = length(x))
  }
}

default_col_N <- function(col_by) {
  stopifnot(is.data.frame(col_by))
  colSums(col_by)
}
# total makes things quite a bit more complicated

#' Return an \code{rtable} with a no Method Message
#'
#' If there is no explicit method for an object an \code{rtable} with one row with an
#' appropriate message is returned.
#'
#' @inheritParams t_summary.data.frame
#' @param ... not used arguments
#'
#' @return rtable
#'
#' @export
#'
#' @template author_waddella
#'
#' @examples
#' t_summary(structure(1:5, class = "aaa"), factor(LETTERS[c(1,2,1,1,2)]))
#'
#' @importFrom rtables col_by_to_matrix
t_summary.default <- function(x, # nolint
                              col_by,
                              col_N = default_col_N(col_by), # nolint
                              total = NULL,
                              ...) {
  col_by <- col_by_to_matrix(col_by, x)
  col_by <- treat_total(x, col_by, total)
  force(col_N) # only force col_N now
  check_col_by(x, col_by, col_N, min_num_levels = 1)

  tbl <- rtable(
    header = colnames(col_by),
    rrowl(paste("no t_summary method for class:", class(x)), lapply(colnames(col_by), function(x) rcell("-")))
  )
  header_add_N(tbl, col_N)
}

#' Variables Summary Table
#'
#' Similar as the demographic table in STREAM
#'
#' You can use the label attribute of the columns in the data frame to modify the node value / displayed value,
#' e.g. AGE -> "Baseline Age of patient"
#'
#' @inheritParams argument_convention
#' @param x data frame with variables to be summarized as described in the
#'   details section. If the variable has a \code{label} attribute then it will
#'   be used for the row name.
#' @param ... arguments passed on to methods
#' @template param_table_tree
#'
#' @details
#' Every variable in \code{x} will be mapped to a summary table using
#' \code{\link{t_summary}} and then be stacked.
#'
#' @export
#'
#' @template author_waddella
#'
#' @seealso \code{\link{t_summary}},
#'   \code{\link{t_summary.numeric}}, \code{\link{t_summary.factor}},
#'   \code{\link{t_summary.logical}}, \code{\link{t_summary.Date}},
#'   \code{\link{t_summary.character}}, \code{\link{t_summary_by}}
#'
#' @examples
#' # with iris data
#' t_summary(iris$Sepal.Length, iris$Species)
#'
#' library(random.cdisc.data)
#' library(dplyr)
#' library(rtables)
#' ADSL <- radsl(N = 100, seed = 1)
#'
#' t_summary(ADSL[, c("SEX", "AGE")], col_by  = by_all("All"), col_N = nrow(ADSL))
#'
#' # control the label
#' ADSL <- var_relabel(ADSL, AGE = "Baseline Age of patient")
#'
#' # control categorical order
#' ADSL$SEX <- relevel(ADSL$SEX, "M", "F")
#'
#' t_summary(ADSL[, c("SEX", "AGE")], col_by = ADSL$ARM, total = "All Patients")
#' t_summary(ADSL[, c("SEX", "AGE")], col_by = ADSL$ARM, total = "All Patients", useNA = 'always')
#'
#' ADSL$SEX[1:10] <- NA
#' t_summary(ADSL[, c("SEX", "AGE")], col_by = ADSL$ARM, total = "All Patients", useNA = 'ifany')
#' t_summary(ADSL[, c("SEX", "AGE")], col_by = ADSL$ARM, total = "All Patients", denominator = "N",
#'           useNA = 'ifany')
#'
#' ADSL_AGE65 <- ADSL %>%
#'   dplyr::filter(AGE > 65)
#' t_summary(ADSL_AGE65[, c("AGE", "SEX")], ADSL_AGE65$ARM, total = "All Patients",
#'           col_N = table(ADSL_AGE65$ARM), denominator = "N", drop_levels = TRUE)
#'
#'
#' tbls <- t_summary(ADSL[, c("SEX", "AGE")], col_by = ADSL$ARM, table_tree = TRUE)
#' summary(tbls)
#' rbind_table_tree(tbls)
#'
#' @importFrom rtables col_by_to_matrix
t_summary.data.frame <- function(x, # nolint
                                 col_by,
                                 col_N = default_col_N(col_by), # nolint
                                 total = NULL,
                                 ...,
                                 table_tree = FALSE) {
  col_by <- col_by_to_matrix(col_by, x)
  col_by <- treat_total(x, col_by, total)
  force(col_N) # only force col_N now
  check_col_by(x, col_by, col_N, min_num_levels = 1)

  # one child per column of x
  children <- Map(function(col, node_name) {
    node(
      name = node_name,
      content = t_summary(x = col, col_by = col_by, col_N = col_N, ...),
      children = list()
    )
  }, x, var_labels(x, fill = TRUE))
  res <- node(name = invisible_node_name("root"),
       content = NULL,
       children = children
  )

  if (table_tree) {
    res
  } else {
    to_rtable(res)
  }
}

#' Summarize Numeric Variables
#'
#' Tabulate the number of non-missing observations, mean, sd, median, and range
#' for different groups.
#'
#' @inheritParams t_summary.data.frame
#' @param x numeric variable
#'
#' @template return_rtable
#'
#' @export
#'
#' @template author_waddella
#'
#' @seealso \code{\link{t_summary}},
#'   \code{\link{t_summary.data.frame}}, \code{\link{t_summary.factor}},
#'   \code{\link{t_summary.logical}}, \code{\link{t_summary.Date}},
#'   \code{\link{t_summary.character}}, \code{\link{t_summary_by}}
#'
#' @examples
#' # with iris data
#' t_summary(iris$Sepal.Length, iris$Species)
#'
#' library(random.cdisc.data)
#' library(rtables)
#' ADSL <- radsl(N = 100, seed = 1)
#'
#' t_summary(ADSL$AGE, ADSL$ARM)
#' t_summary(ADSL$AGE, col_by = add_total_by(factor_to_matrix_by(ADSL$ARM), label = "All"))
#' t_summary(ADSL$AGE, col_by = ADSL$ARM, total = "All")
#'
#' ADSL$AGE[1:10] <- NA
#' t_summary(ADSL$AGE, no_by("All"), col_N = nrow(ADSL) )
#'
#' @importFrom rtables col_by_to_matrix
t_summary.numeric <- function(x, # nolint
                              col_by,
                              col_N = default_col_N(col_by),
                              total = NULL,
                              ...) {
  col_by <- col_by_to_matrix(col_by, x)
  col_by <- treat_total(x, col_by, total)
  force(col_N) # only force col_N now
  check_col_by(x, col_by, col_N, min_num_levels = 1)

  tbl <- rbind(
    rtabulate(x, col_by, count_n, row.name = "n"),
    rtabulate(x, col_by, mean_sd, format = "xx.xx (xx.xx)", row.name = "Mean (SD)"),
    rtabulate(x, col_by, median, row.name = "Median", format = "xx.xx", na.rm = TRUE),
    rtabulate(x, col_by, range, format = "xx.xx - xx.xx", row.name = "Min - Max", na.rm = TRUE)
  )

  header_add_N(tbl, col_N)
}

#' Summarize Categorical Data
#'
#' Tabulate the number of non-missing observations, number of observations per level and
#' percentage.
#'
#' @inheritParams t_summary.data.frame
#' @param x factor variable
#' @param useNA choose whether missing data (NAs) should be displayed as a level.
#' @param denominator either n or N for calculating the level associated
#'   percentage. With option N, the reference population from \code{col_N} is used as the denominator.
#'   With option n, the number of non-missing records from \code{x} is used as the denominator.
#' @param drop_levels boolean whether to drop zero count levels
#'
#' @template return_rtable
#'
#' @export
#' @template author_waddella
#'
#' @seealso \code{\link{t_summary}},
#'   \code{\link{t_summary.data.frame}}, \code{\link{t_summary.numeric}},
#'   \code{\link{t_summary.logical}}, \code{\link{t_summary.Date}},
#'   \code{\link{t_summary.character}}, \code{\link{t_summary_by}}
#'
#'
#' @examples
#' # with iris data
#' t_summary(iris$Species, iris$Species)
#'
#' library(random.cdisc.data)
#' ADSL <- cadsl
#'
#' t_summary(ADSL$SEX, ADSL$ARM, total = "All")
#' t_summary(ADSL$SEX, ADSL$ARM, useNA = "always")
#'
#' ADSL$SEX[1:10] <- NA
#' t_summary(ADSL$SEX, ADSL$ARM, denominator = "N", useNA = "ifany", total = "All")
#' t_summary(ADSL$SEX, ADSL$ARM, denominator = "n", useNA = "no", total = "All")
t_summary.factor <- function(x, # nolint
                             col_by,
                             col_N = default_col_N(col_by), # nolint
                             total = NULL,
                             useNA = c("no", "ifany", "always"), # nolint
                             denominator = c("n", "N"),
                             drop_levels = FALSE, ...) {
  col_by <- col_by_to_matrix(col_by, x)
  col_by <- treat_total(x, col_by, total)
  force(col_N) # only force col_N now
  check_col_by(x, col_by, col_N, min_num_levels = 1)

  useNA <- match.arg(useNA) # nolint
  denominator <- match.arg(denominator)

  if (drop_levels) {
    x <- droplevels(x)
  }

  if (denominator == "n") {
    denom <- colSums(col_by[!is.na(x), , drop = FALSE])
  } else {
    denom <- col_N
  }

  tbl <- rbind(
    rtabulate(as.numeric(x), col_by, count_n, row.name = "n"),
    rtabulate(
      x = x,
      col_by = col_by,
      FUN = function(x_cell, denom) {
        if (length(x_cell) > 0) {
          length(x_cell) * c(1, 1 / denom)
        } else {
          rcell("-", format = "xx")
        }
      },
      format = "xx (xx.xx%)",
      col_wise_args = list(denom = denom)
    )
  )

  if (useNA == "always" || (useNA == "ifany" && any(is.na(x)))) {
    na_row <- lapply(col_by, function(rows) {
      sum(is.na(x[rows]))
    })
    tbl <- insert_rrow(tbl, rrowl("<NA>", na_row, format = "xx"), nrow(tbl) + 1)
  }

  header_add_N(tbl, col_N)
}
