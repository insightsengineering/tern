#' Single element for disposition table
#'
#' @inheritParams argument_convention
#' @inheritParams t_summary.factor
#' @param x A factor or logical vector
#' @param row.name Only applicable when x is a logical vector. A string to label the row name.
#'   Default is "TRUE".
#' @param subset A logical vector with the same length as x that defines the subset.
#' @param show_n Logic value to determine whether the "n" row is displayed. Default is FALSE.
#'
#' @export
#'
#' @examples
#'
#' library(random.cdisc.data)
#' library(dplyr)
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
#' dsp <- partial(t_el_disposition, col_by = ADSL0$ARM, denominator = "N",
#'  total = "All Patients")
#'
#' rbind(
#'   dsp(ADSL0$COMPSTUD == "Y", row.name = "Completed study"),
#'   dsp(ADSL0$STUDONS == "Alive: In Follow-up", row.name = "Alive: In follow-up"),
#'   dsp(ADSL0$DISSTDFL == "Y",  row.name = "Discontinued study"),
#'   dsp(as.factor(ADSL0$STDDRS), indent = 1),
#'   rrow(),
#'   rrow("Show example of using n as denominator"),
#'   t_el_disposition(as.factor(ADSL0$STDDRS), col_by = ADSL0$ARM, denominator = "n",
#'      total = "All Patients", indent = 1),
#'   rrow(),
#'   dsp(ADSL0$GOTTRT == "Y", row.name = "Received treatment"),
#'   dsp(ADSL0$DISTRTFL == "Y", row.name = "Discontinued treatment"),
#'   dsp(ADSL0$DRSCAT == "Safety", row.name = "Safety", indent = 1),
#'   dsp(ADSL0$TRTDRS, subset = ADSL0$DRSCAT %in% "Safety", indent = 2),
#'   dsp(ADSL0$DRSCAT == "Other", row.name = "Other", indent = 1),
#'   dsp(ADSL0$TRTDRS, subset = ADSL0$DRSCAT %in% "Other", indent = 2)
#' )
#'
#'
t_el_disposition <- function(x = x, col_by, col_N = table(col_by), row.name = NULL, # nolint
                             indent = 0, subset = NULL, show_n = FALSE, # nolint
                             useNA = c("no", "ifany", "always"), drop_levels = TRUE, # nolint
                             total = NULL, denominator = "N") { # nolint

  check_col_by(col_by, col_N, min_num_levels = 1)
  if (!(is.atomic(x) & (is.factor(x) | is.logical(x)))) {
    stop("x is required to be atomic factor or logical vector")
  }

  if (is.null(subset)) {
    subset <- rep(TRUE, length(x))
  }

  stopifnot(is.logical(subset), length(subset) == length(x))
  subset[is.na(subset)] <- FALSE

  x <- x[subset]
  col_by <- col_by[subset]

  if (is.factor(x) & !is.null(row.name)) {
    warning("x is factor and row.name will be ignored")
  }
  label <- if (is.null(row.name)) {
    "TRUE"
  } else {
    row.name
  }

  tbl <- if (is.logical(x)) {
    ttbl <- t_summary.logical(
      x = x,
      col_by = col_by,
      col_N = col_N,
      row_name_true = label,
      useNA = useNA, drop_levels = FALSE, # do not drop level since logical will only have levels TRUE and FALSE
      total = total, denominator = denominator

    )[2, ]  # n row is not shown in disposition table

    indent(ttbl, indent)

  } else if (is.factor(x) & length(x) > 0) {

    ttbl <- t_summary.factor(
      x = x,
      col_by = col_by,
      col_N = col_N,
      useNA = useNA, drop_levels = drop_levels,
      total = total, denominator = denominator
    )

    if (!show_n) {
      ttbl <- ttbl[-1, ]
    }

    indent(ttbl, indent)
  }


}
