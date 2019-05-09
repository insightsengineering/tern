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
#'   dsp(ADSL0$TRTDRS, subcategory = ADSL0$DRSCAT %in% "Safety", indent = 2),
#'   dsp(ADSL0$DRSCAT == "Other", row.name = "Other", indent = 1),
#'   dsp(ADSL0$TRTDRS, subcategory = ADSL0$DRSCAT %in% "Other", indent = 2)
#' )
#'

t_dsp <- function(x = x, col_by, col_N = table(col_by), subcategory = NULL,
                  total = "All Patients", indent = 0, row.name = "TRUE",
                  show_n = FALSE) {

  stopifnot(is.factor(col_by), length(col_N) == nlevels(col_by))
  is.atomic(x) & (is.factor(x) | is.logical(x)) || stop("x is required to be atomic factor or logical vector")

  if (is.null(subcategory)) {
    subcategory <- rep(TRUE, length(x))
  }

  stopifnot(is.logical(subcategory), length(subcategory) == length(x))
  subcategory[is.na(subcategory)] <- FALSE

  x_anl <- x[subcategory]
  col_by <- col_by[subcategory]

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

    if(!show_n){
      tbl <- tbl[2, ]
    }

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

