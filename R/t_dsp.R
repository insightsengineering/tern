#' Single element for disposition table
#'
#' @inheritParams rtables::rtabulate.numeric
#' @param x a factor or logical vector.
#' @param col_by The group variable to define columns.
#' @param col_N  The column total for each group that is displayed in the table header with (N=xx).
#' @param row.name Only applicable when x is a logical vector. A string to label the row name. Default is "TRUE".
#' @param indent An integer specifing the spaces before row.name.
#' @param subset A logical vector with the same length as x that defines the subset
#' @param ... arguments passed on to methods
#'
#' @export
#'
#' @examples
#'
#' library(purrr)
#' dsp <- purrr::partial(t_dsp, col_by = ADSL0$ARM)
#'
#' rbind(
#'   dsp(ADSL0$COMPSTUD == "Y", row.name = "Completed study"),
#'   dsp(ADSL0$STUDONS == "Alive: In Follow-up", row.name = "Alive: In follow-up"),
#'   dsp(ADSL0$DISSTDFL == "Y",  row.name = "Discontinued study"),
#'   dsp(as.factor(ADSL0$STDDRS), indent = 1),
#'   rrow(),
#'   dsp(ADSL0$GOTTRT == "Y", row.name = "Received treatment"),
#'   dsp(ADSL0$DISTRTFL == "Y", row.name = "Discontinued treatment"),
#'   dsp(ADSL0$DRSCAT == "Safety", row.name = "Safety", indent = 1),
#'   dsp(ADSL0$TRTDRS, subset = ADSL0$DRSCAT %in% "Safety", indent = 2),
#'   dsp(ADSL0$DRSCAT == "Other", row.name = "Other", indent = 1),
#'   dsp(ADSL0$TRTDRS, subset = ADSL0$DRSCAT %in% "Other", indent = 2)
#' )
#'

t_dsp <- function(x = x, col_by, col_N = table(col_by), subset = NULL,
                  indent = 0, row.name = NULL) {

  stopifnot(is.factor(col_by), length(col_N) == nlevels(col_by))
  is.atomic(x) & (is.factor(x) | is.logical(x)) || stop("x is required to be atomic factor or logical vector")

  if (is.null(subset)) {
    subset <- rep(TRUE, length(x))
  }

  stopifnot(is.logical(subset), length(subset) == length(x))
  subset[is.na(subset)] <- FALSE

  x_anl <- x[subset]
  col_by <- col_by[subset]

  if(is.factor(x) & !is.na(row.name)) {
    warning('x is factor and row.name will be ignored')
  }
  label <- if (is.null(row.name)) {
    "TRUE"
  } else {
    row.name
  }

  if(is.logical(x)){
    tbl <- t_summary(
      x = x_anl,
      col_by = col_by,
      col_N = col_N,
      total = total
    )
    row.names(tbl)[2] <- label

    # n row is not shown in disposition table
    tbl <- tbl[2, ]


  }else if(is.factor(x)){
    x_anl <- droplevels(x_anl)
    tbl <- t_summary(
      x = x_anl,
      col_by = col_by,
      col_N = col_N,
      total = total
    )

    if(!show_n){
      tbl <- tbl[-1, ]
    }

  }

  tbl <- indent(tbl, indent)

  tbl
}

