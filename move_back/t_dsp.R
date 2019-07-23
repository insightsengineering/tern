#' Single element for disposition table
#'
#' @inheritParams t_summary
#' @param x a factor or logical vector.
#' @param col_by The group variable to define columns.
#' @param col_N  The column total for each group that is displayed in the table header with (N=xx).
#' @param row.name Only applicable when x is a logical vector. A string to label the row name. Default is "TRUE".
#' @param indent An integer specifing the spaces before row.name.
#' @param subset A logical vector with the same length as x that defines the subset.
#' @param show_n Logic value to determine whether the "n" row is displayed. Default is FALSE.
#' @param ... arguments passed on to methods
#'
#' @export
#'
#' @examples
#'
#' library(random.cdisc.data)
#' library(dplyr)
#' library(utils.nest)
#'
#' ADSL <- radsl(seed = 1)
#'
#' ADSL0 <- ADSL %>%
#'  mutate(
#'    COMPSTUD = sample(c('Y','N'),
#'                      size=nrow(ADSL),
#'                      replace = TRUE) %>% as.factor,
#'    STUDONS = sample(c('Alive: On Treatment', 'Alive: In Follow-up', NA),
#'                     size=nrow(ADSL),
#'                     replace = TRUE) %>% as.factor,
#'    STDDRS = sample(c('Death', 'Lost To Follow-Up',
#'                      'Protocol Violation', 'Withdrawal By Subject',
#'                      'Other'),
#'                    size=nrow(ADSL),
#'                    replace = TRUE) %>% as.factor,
#'    GOTTRT = ifelse(!is.na(ACTARMCD), 'Y', 'N') %>% as.factor,
#'    DISTRTFL = sample(c('Y','N'),
#'                      size=nrow(ADSL),
#'                      replace = TRUE) %>% as.factor,
#'    TRTDRS = sample(c('ADVERSE EVENT', 'PROGRESSIVE DISEASE',
#'                      'PHYSICIAN DECISION', 'LACK OF EFFICACY',
#'                      'OTHER'),
#'                    size=nrow(ADSL),
#'                    replace = TRUE) %>% as.factor,
#'    STUDONS = case_when(
#'      COMPSTUD == 'N' ~ STUDONS
#'    ),
#'    STDDRS = case_when(
#'      COMPSTUD == 'N' & is.na(STUDONS) ~ STDDRS
#'    ),
#'    DISSTDFL = case_when(
#'      !is.na(STDDRS) ~ "Y"
#'    ),
#'    DISTRTFL = case_when(
#'      GOTTRT == 'Y' ~ DISTRTFL
#'    ),
#'    TRTDRS = case_when(
#'      DISTRTFL == 'Y' ~ TRTDRS
#'    ),
#'    DRSCAT = case_when(
#'      TRTDRS %in% c('ADVERSE EVENT', 'PHYSICIAN DECISION') ~ "Safety",
#'      !is.na(TRTDRS) ~ "Other"
#'    )
#'  ) %>% var_relabel(COMPSTUD = "Complete Study",
#'                    STUDONS = "On-study Status",
#'                    DISSTDFL = "Discontinued Study",
#'                    STDDRS = "Reason for Study \r\nDiscontinuation",
#'                    GOTTRT = "Received Treatment",
#'                    DISTRTFL = "Discontinued Treatment",
#'                    TRTDRS = "Reason for Treatment \r\nDiscontinuation"
#'                    )
#'
#' library(purrr)
#' dsp <- purrr::partial(t_dsp, col_by = ADSL0$ARM, denominator = "N", total = "All Patients")
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

t_dsp <- function(x = x, col_by, col_N = table(col_by), row.name = NULL,
                  indent = 0, subset = NULL, show_n = FALSE, ... ) {

  stopifnot(is.factor(col_by), length(col_N) == nlevels(col_by))
  is.atomic(x) & (is.factor(x) | is.logical(x)) || stop("x is required to be atomic factor or logical vector")

  if (is.null(subset)) {
    subset <- rep(TRUE, length(x))
  }

  stopifnot(is.logical(subset), length(subset) == length(x))
  subset[is.na(subset)] <- FALSE

  x_anl <- x[subset]
  col_by <- col_by[subset]

  if(is.factor(x) & !is.null(row.name)) {
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
      ...
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
      ...
    )

    if(!show_n){
      tbl <- tbl[-1, ]
    }

  }

  tbl <- indent(tbl, indent)

  tbl
}

