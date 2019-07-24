#' Single element for disposition table
#'
#' @inheritParams argument_convention
#' @inheritParams t_summary.factor
#' @param x A factor or logical vector
#' @param row.name Only applicable when x is a logical vector. A string to label the row name.
#'   Default is "TRUE".
#' @param subset A logical vector with the same length as x that defines the subset. Applies to \code{x} and \code{subset}.
#' @param show_n Logic value to determine whether the "n" row is displayed. Default is FALSE. Only when x is of type factor
#'
#' @export
#'
#' @examples
#'
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' ADSL <- radsl(cached = TRUE)
#'
#' ADSL0[, c("COMPSTUD", "STUDONS")]
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
#' dsp <- partial(t_el_disposition, col_by = ADSL0$ARM %>% by_add_total("All Patients"), denominator = "N")
#' # Only using rtables
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
#' # new way:
#' treatment_section_rtable <- rbindl_rtables(
#' list(
#'   dsp(ADSL0$GOTTRT == "Y", row.name = "Received treatment"),
#'   dsp(ADSL0$DISTRTFL == "Y", row.name = "Discontinued treatment"),
#'   indent(
#'     rbindl_rtables(
#'       list(
#'         dsp(ADSL0$DRSCAT == "Safety", row.name = "Safety"),
#'         indent(
#'           dsp(ADSL0$TRTDRS, subset = ADSL0$DRSCAT %in% "Safety"),
#'           1
#'         ),
#'         dsp(ADSL0$DRSCAT == "Other", row.name = "Other"),
#'         indent(
#'           dsp(ADSL0$TRTDRS, subset = ADSL0$DRSCAT %in% "Other"),
#'           1
#'         )
#'       )
#'     ),
#'     1
#'   )
#' )
#' )
#' treatment_section_rtable
#'
#' # easier to manipulate for their purposes by other users
#' treatment_section_tree <- node(
#'   invisible_node_name("treatment"),
#'   content = NULL,
#'   format_data = list(children_gap = 0),
#'   children = list(
#'     node(
#'       invisible_node_name("received_treatment"),
#'       content = dsp(ADSL0$GOTTRT == "Y", row.name = "Received treatment")
#'     ),
#'     node(
#'       invisible_node_name("discontinuedtreatment"),
#'       dsp(ADSL0$DISTRTFL == "Y", row.name = "Discontinued treatment"),
#'       format_data = list(gap_to_children = 0, children_gap = 0, children_indent = 1),
#'       children = list(
#'         node(
#'           invisible_node_name("safety"),
#'           rbindl_rtables(
#'             list(
#'               dsp(ADSL0$DRSCAT == "Safety", row.name = "Safety"),
#'               indent(
#'                 dsp(ADSL0$TRTDRS, subset = ADSL0$DRSCAT %in% "Safety"),
#'                 1
#'               )
#'             )
#'           )
#'         ),
#'         node(
#'           invisible_node_name("other"),
#'           rbindl_rtables(
#'             list(
#'               dsp(ADSL0$DRSCAT == "Other", row.name = "Other"),
#'               indent(
#'                 dsp(ADSL0$TRTDRS, subset = ADSL0$DRSCAT %in% "Other"),
#'                 1
#'               )
#'             )
#'           )
#'         )
#'       )
#'     )
#'   )
#' )
#' to_rtable(treatment_section_tree)
#'
#' to_rtable(invisible_node(
#'   name = "t_el_disp",
#'   children = list(
#'     node(
#'       name = invisible_node_name("Study status"),
#'       content = rbindl_rtables(
#'         list(
#'           dsp(ADSL0$COMPSTUD == "Y", row.name = "Completed study"),
#'           dsp(ADSL0$STUDONS == "Alive: In Follow-up", row.name = "Alive: In follow-up")
#'         )
#'       ),
#'       children = list(
#'         node(
#'           name = invisible_node_name("Reasons for study discontinuation (using capital N)"),
#'           rbindl_rtables(
#'             list(
#'               dsp(ADSL0$DISSTDFL == "Y",  row.name = "Discontinued study"),
#'               indent(dsp(as.factor(ADSL0$STDDRS)), 1)
#'             )
#'           )
#'         ),
#'         node(
#'           name = "Reasons for study discontinuation (using small n)",
#'           content = t_el_disposition(as.factor(ADSL0$STDDRS), col_by = ADSL0$ARM %>% by_add_total("All Patients"), denominator = "n")
#'         )
#'       )
#'     ),
#'     treatment_section_tree
#'   )
#' ))
#'
#' # todo: remove subset
t_el_disposition <- function(x = x, col_by, col_N = NULL, row.name = NULL, # nolint
                             subset = NULL, show_n = FALSE, # nolint
                             useNA = c("no", "ifany", "always"), drop_levels = NULL, # nolint
                             denominator = "N") { # nolint
  # treat x and col_by
  if (!(is.atomic(x) & (is.factor(x) | is.logical(x)))) {
    stop("x is required to be atomic factor or logical vector")
  }
  col_by <- col_by_to_matrix(col_by, x)
  col_N <- col_N %||% get_N(col_by)
  check_col_by(x, col_by, col_N, min_num_levels = 1)

  # treat subset
  if (is.null(subset)) {
    subset <- rep(TRUE, length(x))
  }
  subset[is.na(subset)] <- FALSE
  stopifnot(
    is.logical(subset),
    length(subset) == length(x)
  )
  x <- x[subset]
  col_by <- col_by[subset,]

  if (is.logical(x)) {
    if (show_n) {
      stop("n is never shown for logicals")
    }
    drop_levels <- drop_levels %||% FALSE
    t_summary.logical(
      x = x,
      col_by = col_by,
      col_N = col_N,
      row_name_true = `if`(is.null(row.name), "TRUE", row.name),
      useNA = useNA, drop_levels = drop_levels, # do not drop level since logical will only have levels TRUE and FALSE
      denominator = denominator
    )[2, ]  # n row is not shown in disposition table

  } else { #todo: taken out:  & length(x) > 0
    stopifnot(is.factor(x))
    if (!is.null(row.name)) {
      warning("x is factor and row.name will be ignored")
    }
    drop_levels <- drop_levels %||% TRUE
    res <- t_summary.factor(
      x = x,
      col_by = col_by,
      col_N = col_N,
      useNA = useNA, drop_levels = drop_levels,
      denominator = denominator
    )

    if (!show_n) {
      res <- res[-1, ]
    }

    res
  }
}
