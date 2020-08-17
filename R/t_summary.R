#' Summary
#'
#' Summarize an Object for Different Groups
#'
#' @inheritParams t_summary.data.frame
#' @param x an object to dispatch on, see \code{methods("t_summary")}
#' @param ... (optional)\cr
#'   additional arguments passed on to methods
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
#' library(dplyr)
#' ADSL <- radsl(cached = TRUE)
#'
#' t_summary(x = ADSL$AGE, col_by = ADSL$ARMCD)
#'
#' t_summary(x = ADSL[, c("AGE", "SEX", "RACE")], col_by = ADSL$ARMCD)
#'
#' t_summary(
#'   x = ADSL[, c("AGE", "SEX", "RACE")],
#'   col_by = ADSL$ARMCD,
#'   total = "All Patients"
#' )
#'
#' with(
#'   ADSL,
#'   t_summary(AGE > 65, ARMCD)
#' )
#'
#' # examples with hierarchical header
#' ADSL <- radsl(cached = TRUE)
#' t_summary(
#'   x = ADSL[, c("SEX", "AGE")],
#'   col_by = ADSL$ARM
#' )
#'
#' ADSL_f <- ADSL %>%
#'   dplyr::filter(SEX %in% c("M", "F")) %>%
#'   mutate(SEX = droplevels(SEX))
#' t_summary(
#'   x = ADSL_f$SEX,
#'   col_by = by_hierarchical(ADSL_f$ARM, ADSL_f$SEX, sep = "-")
#' )
#'
#' t_summary(
#'   x = ADSL_f[, c("SEX", "AGE")],
#'   col_by = by_hierarchical(ADSL_f$ARM, ADSL_f$SEX, sep = "-")
#' )
#'
#' ADSL_f <- ADSL %>%
#'   dplyr::filter(SEX %in% c("M", "F"), RACE %in% c("ASIAN", "WHITE")) %>%
#'   mutate(
#'     SEX = droplevels(SEX),
#'     RACE = droplevels(as.factor(RACE))
#'   )
#' t_summary(
#'   x = ADSL_f[, c("SEX", "RACE")],
#'   col_by = by_hierarchical(ADSL_f$SEX, ADSL_f$RACE)
#' )
#'
#' t_summary(
#'   x = ADSL_f[, c("SEX", "RACE")],
#'   col_by = by_hierarchical(ADSL_f$RACE, ADSL_f$SEX)
#' )
#'
#' t_summary(
#'   x = ADSL_f[, c("ITTFL", "BMRKR1", "BEP01FL")],
#'   col_by = by_hierarchical(ADSL_f$RACE, ADSL_f$SEX)
#' )
#'
#' # add total
#' t_summary(
#'   x = ADSL_f[, c("ITTFL", "BMRKR1", "BEP01FL")],
#'   col_by = by_hierarchical(ADSL_f$RACE, ADSL_f$SEX),
#'   total = "All"
#' )
t_summary <- function(x,
                      col_by,
                      col_N = NULL, # nolint
                      total = NULL,
                      ...) {
  UseMethod("t_summary", x)
}

#' Return an \code{rtable} with a no Method Message
#'
#' If there is no explicit method for an object an \code{rtable} with one row with an
#' appropriate message is returned.
#'
#' @inheritParams t_summary.data.frame
#' @param ... (not used with this object)\cr
#' not used arguments
#'
#' @return \code{rtable}
#'
#' @export
#'
#' @template author_waddella
#'
#' @examples
#' t_summary(
#'   x = structure(1:5, class = "aaa"),
#'   col_by = factor(LETTERS[c(1,2,1,1,2)])
#' )
#'
t_summary.default <- function(x, # nolint
                              col_by,
                              col_N = NULL, # nolint
                              total = NULL,
                              ...) {
  stopifnot(is.null(total) || is_character_single(total))
  col_by <- col_by_to_matrix(col_by, x)
  col_N <- col_N %||% get_N(col_by) #nolintr
  if (!is.null(total)) {
    col_by <- by_add_total(col_by, label = total)
    col_N <- col_N_add_total(col_N) #nolintr
    total <- NULL
  }
  check_col_by(x, col_by, col_N, min_num_levels = 1)

  tbl <- rtable(
    header = by_header(col_by),
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
#' @param x (\code{data.frame})\cr
#'   Contains variables to be summarized as described in the details section.
#'   If the variable has a \code{label} attribute then it will be used for the row name.
#' @param ... (optional)\cr
#'   additional arguments passed on to methods.
#'   + \code{q_type} an integer between 1 and 9 selecting one of the nine
#'   quantile algorithms as detailed in \code{\link[stats:quantile]{quantile(), type argument}}
#'   + \code{conf_level} the confidence level between 0 and 1 (default: 0.95)
#'
#' @details
#' Every variable in \code{x} will be mapped to a summary table using
#' \code{\link{t_summary}} and then be stacked.
#'
#' @export
#' @md
#' @template author_waddella
#'
#' @seealso \code{\link{t_summary}},
#'   \code{\link{t_summary.numeric}}, \code{\link{t_summary.factor}},
#'   \code{\link{t_summary.logical}}, \code{\link{t_summary.Date}},
#'   \code{\link{t_summary.character}}, \code{\link{t_summary_by}}
#'
#' @examples
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' # with CDISC like data
#' ADSL <- radsl(cached = TRUE)
#' t_summary(
#'   x = ADSL[, c("SEX", "AGE")],
#'   col_by = ADSL$ARM
#' )
#'
#' t_summary(
#'   x = ADSL[, c("SEX", "AGE")],
#'   col_by = ADSL$ARM,
#'   total = "All Patients"
#' )
#'
#' # or
#' t_summary(
#'   x = ADSL[, c("SEX", "AGE")],
#'   col_by = ADSL$ARM %>% by_add_total("All Patients")
#' )
#'
#' # Special Variants
#' t_summary(
#'   x = ADSL[, c("SEX", "AGE")],
#'   col_by = by_all("All"),
#'   col_N = nrow(ADSL)
#' )
#'
#' # control the label
#' ADSL <- var_relabel(ADSL, AGE = "Baseline Age of patient")
#'
#' # control categorical order
#' ADSL$SEX <- relevel(ADSL$SEX, "M", "F")
#'
#' t_summary(
#'   x = ADSL[, c("SEX", "AGE")],
#'   col_by = ADSL$ARM,
#'   total = "All Patients"
#' )
#'
#' t_summary(
#'   x = ADSL[, c("SEX", "AGE")],
#'   col_by = ADSL$ARM,
#'   total = "All Patients",
#'   useNA = "always"
#' )
#'
#' # use different col_by
#'
#' ## Biomarker Evaluable Population
#' t_summary(
#'   x = ADSL[, c("SEX", "AGE")],
#'   col_by = ADSL$ARM %>%
#'     by_compare_subset(
#'       ADSL$BEP01FL == "Y",
#'       label_all = "ALL",
#'       label_subset = "BEP"
#'     )
#' )
#'
#'
#' # With Missing Data
#' ADSL$SEX[1:10] <- NA
#' t_summary(
#'   x = ADSL[, c("SEX", "AGE")],
#'   col_by = ADSL$ARM,
#'   total = "All Patients",
#'   useNA = 'ifany'
#' )
#'
#' t_summary(
#'   x = ADSL[, c("SEX", "AGE")],
#'   col_by = ADSL$ARM,
#'   total = "All Patients",
#'   denominator = "N",
#'   useNA = "ifany"
#' )
#'
#' # Table tree
#' tbls <- t_summary(
#'   ADSL[, c("SEX", "AGE")],
#'   col_by = ADSL$ARM,
#'   table_tree = TRUE
#' )
#' summary(tbls)
#' to_rtable(tbls)
#'
t_summary.data.frame <- function(x, # nolint
                                 col_by,
                                 col_N = NULL, # nolint
                                 total = NULL,
                                 ...,
                                 table_tree = FALSE) {
  stopifnot(is.null(total) || is_character_single(total))
  col_by <- col_by_to_matrix(col_by, x)
  col_N <- col_N %||% get_N(col_by) #nolintr
  if (!is.null(total)) {
    col_by <- by_add_total(col_by, label = total)
    col_N <- col_N_add_total(col_N) #nolintr
    total <- NULL
  }
  check_col_by(x, col_by, col_N, min_num_levels = 1)

  # each column of the data frame x is an element of the list
  # replicate col_by ncol times, rep does not work
  colnames(x) <- var_labels(x, fill = TRUE)
  t_summary.list(
    x_list = x,
    col_by_list = replicate(length(x), col_by, simplify = FALSE),
    col_N = col_N,
    total = total,
    ...,
    table_tree = table_tree
  )
}

#' Summarize Numeric Variables
#'
#' Tabulate the number of non-missing observations, mean, standard error, confidence interval, median, and range
#' for different groups.
#'
#' @inheritParams t_summary.data.frame
#' @param x (\code{numeric}) variable
#' @param f_numeric a combination of the analysis functions to be evaluated \code{"count_n", "mean_sd", "ci",
#'  "median", "q1_q3", "range", "se"}, as well as the functions which are wrapped by
#'   \code{\link{patient_numeric_fcns}} (\code{f_numeric = patient_numeric_fcns()}) for summarizing
#'   patient data as for \code{\link{t_summarize_by_visit}}
#'
#' @template return_rtable
#'
#' @details The analysis function "ci" uses a two-sided confidence interval based on the t-test statistic, see
#'   \code{\link{ttest_ci_one_arm}} for details. By default, the confidence level is 95%, and can be specified
#'   with the optional `conf_level` parameter.
#'
#' @export
#'
#' @importFrom rtables col_by_to_matrix
#'
#' @seealso \code{\link{t_summary}},
#'   \code{\link{t_summary.data.frame}}, \code{\link{t_summary.factor}},
#'   \code{\link{t_summary.logical}}, \code{\link{t_summary.Date}},
#'   \code{\link{t_summary.character}}, \code{\link{t_summary_by}}
#'
#' @examples
#' # with iris data
#' t_summary(x = iris$Sepal.Length, col_by = iris$Species)
#'
#' library(random.cdisc.data)
#' ADSL <- cadsl
#'
#' t_summary(x = ADSL$AGE, col_by = ADSL$ARM)
#'
#' t_summary(
#'   x = ADSL$AGE,
#'   col_by = ADSL$ARM,
#'   f_numeric = c("count_n", "mean_sd", "se", "ci")
#' )
#'
#' t_summary(
#'   x = ADSL$AGE,
#'   col_by = col_by_to_matrix(ADSL$ARM),
#'   total = "All"
#' )
#'
#' t_summary(
#'   x = ADSL$AGE,
#'   col_by = ADSL$ARM,
#'   total = "All"
#' )
#'
#' ADSL$AGE[1:10] <- NA
#' t_summary(
#'   x = ADSL$AGE,
#'   col_by = by_all("All"),
#'   col_N = nrow(ADSL)
#' )
t_summary.numeric <- function(x,
                              col_by,
                              col_N = NULL, # nolint
                              total = NULL,
                              f_numeric = c("count_n", "mean_sd", "median", "range"),
                              ...) {
  stopifnot(
    is.null(total) || is_character_single(total),
    length(f_numeric) > 0
  )

  allowed_f_numeric <- c(
    "count_n", "mean_sd", "median", "q1_q3", "range",
    "n_not_na3", "mean_sd3", "median_t3", "iqr_num3", "range_t3", "se", "ci"
  )

  if (!all(f_numeric %in% allowed_f_numeric)) {
    stop("f_numeric needs to be one of ", paste(allowed_f_numeric, collapse = ", "))
  }

  if (any(c("q1_q3", "iqr_num3") %in% f_numeric)) {
    q_type <- 7
    if ("q_type" %in% names(list(...))) {
      q_type <- list(...)$q_type
      if (!(q_type %in% 1:9)) {
        message(
          "`q_type` was provided but was not an integer between 1 and 9, see `?quantile`. Default was used instead: 7."
          )
        q_type <- 7
      }
    }
  }

  if ("ci" %in% f_numeric) {
    conf_level <- 0.95
    if ("conf_level" %in% names(list(...))) {
      spec_conf_level <- try(
        check_conf_level(list(...)$conf_level),
        silent = TRUE
      )
      conf_level <-
        if (inherits(spec_conf_level, "try-error")) {
          message(
            "`conf_level` was provided but was not a proportion between 0 and 1. Default was used instead: 0.95."
          )
          0.95
        } else {
          spec_conf_level
        }
    }
  }

  col_by <- col_by_to_matrix(col_by, x)
  col_N <- col_N %||% get_N(col_by) #nolintr
  if (!is.null(total)) {
    col_by <- by_add_total(col_by, label = total)
    col_N <- col_N_add_total(col_N) #nolintr
    total <- NULL
  }
  check_col_by(x, col_by, col_N, min_num_levels = 1)


  tbl <- rbindl_rtables(
    lapply(f_numeric, function(f_name) switch(
      f_name,
      count_n = rtabulate(x, col_by, count_n, row.name = "n"),
      mean_sd = rtabulate(x, col_by, mean_sd, format = "xx.xx (xx.xx)", row.name = "Mean (SD)"),
      se = rtabulate(x, col_by, function(x, na_rm) {
        sd(x) / sqrt(length(x))
      }, row.name = "SE", format = "xx.xx", na_rm = TRUE),
      ci = rtabulate(
        x,
        col_by,
        ttest_ci_one_arm,
        row.name = paste0(round(conf_level * 100), "% CI"),
        format = "xx.xx - xx.xx",
        # Arguments for `ttest_ci_one_arm`:
        conf_level = conf_level
      ),
      median = rtabulate(x, col_by, median, row.name = "Median", format = "xx.xx", na.rm = TRUE),
      q1_q3 = rtabulate(x, col_by, q1_q3, row.name = "Q1 - Q3", format = "xx.xx - xx.xx", na.rm = TRUE, type = q_type),
      range = rtabulate(x, col_by, range, format = "xx.xx - xx.xx", row.name = "Min - Max", na.rm = TRUE),

      # for patient data
      n_not_na3 = rtabulate(x, col_by, n_not_na3, row.name = "n"),
      mean_sd3 = rtabulate(x, col_by, mean_sd3, format = "xx.xx (xx.xx)", row.name = "Mean (SD)"),
      median_t3 = rtabulate(x, col_by, median_t3, row.name = "Median", format = "xx.xx"),
      iqr_num3 = rtabulate(x, col_by, iqr_num3, row.name = "IQR", format = "xx.xx", type = q_type),
      range_t3 = rtabulate(x, col_by, range_t3, format = "xx.xx - xx.xx", row.name = "Min - Max"),

      NULL # default, should never happen
    ))
  )

  header_add_N(tbl, col_N)
}

#' Functions
#'
#' Get the names of functions to be used for t_summary.numeric to summarize patient
#' data as for \code{\link{t_summarize_by_visit}}
#'
#' @export
patient_numeric_fcns <- function() {
  c("n_not_na3", "mean_sd3", "median_t3", "iqr_num3", "range_t3")
}

#' Summarize Categorical Data
#'
#' Tabulate the number of non-missing observations, number of observations per level and
#' percentage.
#'
#' @inheritParams t_summary.data.frame
#' @param x (\code{factor}) variable
#' @param useNA (\code{logical})\cr
#'   choose whether missing data (NAs) should be displayed as a level.
#' @param denominator (one of \code{("n", "N", "omit"})\cr
#'   Value set to \code{"n"} or \code{"N"} are for calculating the level associated percentage.
#'   With option \code{"N"}, the reference population from \code{col_N} is used as
#'   the denominator. With option \code{"n"}, the number of non-missing records from \code{x} is used as
#'   the denominator. If \code{"omit"} is chosen the percentage is omitted.
#' @param drop_levels (\code{logical} value)\cr
#'   Whether to drop zero count levels
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
#' t_summary(
#'   x = ADSL$SEX,
#'   col_by = ADSL$ARM,
#'   total = "All"
#' )
#'
#' t_summary(
#'   x = ADSL$SEX,
#'   col_by = ADSL$ARM,
#'   useNA = "always"
#' )
#'
#' ADSL$SEX[1:10] <- NA
#' t_summary(
#'   x = ADSL$SEX,
#'   col_by = ADSL$ARM,
#'   total = "All",
#'   denominator = "N",
#'   useNA = "ifany"
#'  )
#'
#' t_summary(
#'   x = ADSL$SEX,
#'   col_by = ADSL$ARM,
#'   total = "All",
#'   denominator = "n",
#'   useNA = "no"
#' )
t_summary.factor <- function(x,
                             col_by,
                             col_N = NULL, #nolintr
                             total = NULL,
                             useNA = c("ifany", "no", "always"), # nolint
                             denominator = c("N", "n", "omit"),
                             drop_levels = FALSE,
                             ...) {
  stopifnot(is.null(total) || is_character_single(total))
  col_by <- col_by_to_matrix(col_by, x)
  col_N <- col_N %||% get_N(col_by) #nolintr
  if (!is.null(total)) {
    col_by <- by_add_total(col_by, label = total)
    col_N <- col_N_add_total(col_N) #nolintr
    total <- NULL
  }
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

  omit_perc <- denominator == "omit"

  tbl <- rbind(
    rtabulate(as.numeric(x), col_by, count_n, row.name = "n"),
    rtabulate(
      x = x,
      col_by = col_by,
      FUN = function(x_cell, denom) {
        if (omit_perc) {
          rcell(length(x_cell), format = "xx")
        } else if (length(x_cell) > 0) {
          rcell(length(x_cell) * c(1, 1 / denom), format = "xx (xx.xx%)")
        } else {
          rcell(0, format = "xx")
        }
      },
      col_wise_args = list(denom = denom),
      useNA = useNA
    )
  )

  header_add_N(tbl, col_N)
}

#' Summarize Character Data
#'
#' Currently treated as factors
#'
#' @inheritParams t_summary.data.frame
#' @param x (\code{character} vector)
#' @param ... (optional)\cr
#'  additional arguments passed on to \code{\link{t_summary.factor}}
#'
#' @template author_waddella
#'
#' @export
#'
#' @seealso \code{\link{t_summary}},
#'   \code{\link{t_summary.data.frame}}, \code{\link{t_summary.factor}},
#'   \code{\link{t_summary.logical}}, \code{\link{t_summary.Date}},
#'   \code{\link{t_summary.numeric}}, \code{\link{t_summary_by}}
t_summary.character <- function(x,
                                col_by,
                                col_N = NULL, # nolint
                                total = NULL,
                                ...) {
  t_summary(as.factor(x), col_by = col_by, col_N = col_N, total = total, ...)
}

#' Summarize Date Data
#'
#' Tabulate the range of dates.
#'
#' @inheritParams t_summary.data.frame
#' @param x (\code{Date}) object
#'
#' @template author_waddella
#'
#' @export
#'
#' @seealso \code{\link{t_summary}},
#'   \code{\link{t_summary.data.frame}}, \code{\link{t_summary.factor}},
#'   \code{\link{t_summary.logical}}, \code{\link{t_summary.numeric}},
#'   \code{\link{t_summary.character}}, \code{\link{t_summary_by}}
#'
#'
#' @examples
#' today <- Sys.Date()
#' tenweeks <- seq(today, length.out=10, by="1 week")
#' t_summary(
#'   x = tenweeks,
#'   col_by = by_all("all"),
#'   col_N = length(tenweeks)
#' )
#'
#' t_summary(
#'   x = tenweeks,
#'   col_by = factor(LETTERS[c(1,1,1,2,2,3,3,3,4,4)])
#' )
#'
#' t_summary(
#'   x = tenweeks,
#'   col_by = factor(LETTERS[c(1,1,1,2,2,3,3,3,4,4)]),
#'   total = "All"
#' )
t_summary.Date <- function(x,
                           col_by,
                           col_N = NULL, # nolint
                           total = NULL,
                           ...) {
  stopifnot(is.null(total) || is_character_single(total))
  col_by <- col_by_to_matrix(col_by, x)
  col_N <- col_N %||% get_N(col_by) #nolintr
  if (!is.null(total)) {
    col_by <- by_add_total(col_by, label = total)
    col_N <- col_N_add_total(col_N) #nolintr
    total <- NULL
  }
  check_col_by(x, col_by, col_N, min_num_levels = 1)

  df <- data.frame(date = x)
  tbl <- rtabulate(df, row_by = by_all("range of dates"), col_by = col_by, FUN = function(x) {
    paste(range(na.omit(x)), collapse = " to ")
  }, ...)

  header_add_N(tbl, col_N)

}

#' Summarize Boolean Data
#'
#' Boolean data will be converted to factor
#'
#' @inheritParams t_summary.data.frame
#' @param x (\code{logical} vector)
#' @param row_name_true (\code{character} value)\cr
#'   Contains row.name for TRUE summary
#' @param row_name_false (\code{character} value)
#'   Contains row.name for FALSE summary
#' @param ... (optional)\cr
#'   additional arguments passed on to \code{\link{t_summary.factor}}
#
#' @template author_waddella
#'
#' @export
#'
#' @seealso \code{\link{t_summary}},
#'   \code{\link{t_summary.numeric}}, \code{\link{t_summary.factor}},
#'   \code{\link{t_summary.data.frame}}, \code{\link{t_summary.Date}},
#'   \code{\link{t_summary.character}}, \code{\link{t_summary_by}}
#'
#' @examples
#'
#' t_summary(
#'  x = c(TRUE,FALSE,NA,TRUE,FALSE,FALSE,FALSE,TRUE),
#'  col_by = factor(LETTERS[c(1,1,1,2,2,3,3,2)])
#' )
#'
#' library(random.cdisc.data)
#' ADSL <- cadsl
#'
#' ADSL$AGE[1:10] <- NA
#'
#' with(
#'   ADSL,
#'   t_summary(
#'     AGE > 50,
#'     ARM,
#'     useNA = "ifany"
#'   )
#' )
#'
#' with(
#'   ADSL,
#'   t_summary(
#'     AGE > 50,
#'     ARM,
#'     total = "All",
#'     denominator = "N",
#'     useNA = "ifany",
#'     row_name_true = "Baseline Age > 50",
#'     row_name_false = "Baseline Age <= 50"
#'   )
#' )
t_summary.logical <- function(x,
                              col_by,
                              col_N = NULL, # nolint
                              total = NULL,
                              row_name_true = "TRUE",
                              row_name_false = "FALSE",
                              ...) {

  xf <- factor(x, levels = c(TRUE, FALSE), labels = c(row_name_true, row_name_false))
  t_summary(xf, col_by = col_by, col_N = col_N, total = total, ...)
}
