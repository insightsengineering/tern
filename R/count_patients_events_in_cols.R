#' Count patient events in columns
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The summarize function [summarize_patients_events_in_cols()] creates a layout element to summarize patient
#' event counts in columns.
#'
#' This function analyzes the elements (events) supplied via the `filters_list` parameter and returns a row
#' with counts of number of patients for each event as well as the total numbers of patients and events.
#' The `id` variable is used to indicate unique subject identifiers (defaults to `USUBJID`).
#'
#' If there are multiple occurrences of the same event recorded for a patient, the event is only counted once.
#'
#' @inheritParams argument_convention
#' @param filters_list (named `list` of `character`)\cr list where each element in this list describes one
#'   type of event describe by filters, in the same format as [s_count_patients_with_event()].
#'   If it has a label, then this will be used for the column title.
#' @param empty_stats (`character`)\cr optional names of the statistics that should be returned empty such
#'   that corresponding table cells will stay blank.
#' @param custom_label (`string` or `NULL`)\cr if provided and `labelstr` is empty then this will
#'   be used as label.
#' @param .stats (`character`)\cr statistics to select for the table.
#'
#'   In addition to any statistics added using `filters_list`, statistic options are:
#'   ``r shQuote(get_stats("summarize_patients_events_in_cols"), type = "sh")``
#'
#' @name count_patients_events_in_cols
#' @order 1
NULL

#' @describeIn count_patients_events_in_cols Statistics function which counts numbers of patients and multiple
#'   events defined by filters. Used as analysis function `afun` in `summarize_patients_events_in_cols()`.
#'
#' @return
#' * `s_count_patients_and_multiple_events()` returns a list with the statistics:
#'   - `unique`: number of unique patients in `df`.
#'   - `all`: number of rows in `df`.
#'   - one element with the same name as in `filters_list`: number of rows in `df`,
#'     i.e. events, fulfilling the filter condition.
#'
#' @keywords internal
s_count_patients_and_multiple_events <- function(df,
                                                 id,
                                                 filters_list,
                                                 empty_stats = character(),
                                                 labelstr = "",
                                                 custom_label = NULL) {
  checkmate::assert_list(filters_list, names = "named")
  checkmate::assert_data_frame(df)
  checkmate::assert_string(id)
  checkmate::assert_disjunct(c("unique", "all"), names(filters_list))
  checkmate::assert_character(empty_stats)
  checkmate::assert_string(labelstr)
  checkmate::assert_string(custom_label, null.ok = TRUE)

  # Below we want to count each row in `df` once, therefore introducing this helper index column.
  df$.row_index <- as.character(seq_len(nrow(df)))
  y <- list()
  row_label <- if (labelstr != "") {
    labelstr
  } else if (!is.null(custom_label)) {
    custom_label
  } else {
    "counts"
  }
  y$unique <- formatters::with_label(
    s_num_patients_content(df = df, .N_col = 1, .var = id, required = NULL)$unique[1L],
    row_label
  )
  y$all <- formatters::with_label(
    nrow(df),
    row_label
  )
  events <- Map(
    function(filters) {
      formatters::with_label(
        s_count_patients_with_event(df = df, .var = ".row_index", filters = filters, .N_col = 1, .N_row = 1)$count,
        row_label
      )
    },
    filters = filters_list
  )
  y_complete <- c(y, events)
  y <- if (length(empty_stats) > 0) {
    y_reduced <- y_complete
    for (stat in intersect(names(y_complete), empty_stats)) {
      y_reduced[[stat]] <- formatters::with_label(character(), obj_label(y_reduced[[stat]]))
    }
    y_reduced
  } else {
    y_complete
  }
  y
}

#' @describeIn count_patients_events_in_cols Layout-creating function which can take statistics function
#'   arguments and additional format arguments. This function is a wrapper for [rtables::summarize_row_groups()].
#'
#' @param col_split (`flag`)\cr whether the columns should be split.
#'   Set to `FALSE` when the required column split has been done already earlier in the layout pipe.
#'
#' @return
#' * `summarize_patients_events_in_cols()` returns a layout object suitable for passing to further layouting functions,
#'   or to [rtables::build_table()]. Adding this function to an `rtable` layout will add formatted content rows
#'   containing the statistics from `s_count_patients_and_multiple_events()` to the table layout.
#'
#' @examples
#' df <- data.frame(
#'   USUBJID = rep(c("id1", "id2", "id3", "id4"), c(2, 3, 1, 1)),
#'   ARM = c("A", "A", "B", "B", "B", "B", "A"),
#'   AESER = rep("Y", 7),
#'   AESDTH = c("Y", "Y", "N", "Y", "Y", "N", "N"),
#'   AEREL = c("Y", "Y", "N", "Y", "Y", "N", "Y"),
#'   AEDECOD = c("A", "A", "A", "B", "B", "C", "D"),
#'   AEBODSYS = rep(c("SOC1", "SOC2", "SOC3"), c(3, 3, 1))
#' )
#'
#' # `summarize_patients_events_in_cols()`
#' basic_table() %>%
#'   summarize_patients_events_in_cols(
#'     filters_list = list(
#'       related = formatters::with_label(c(AEREL = "Y"), "Events (Related)"),
#'       fatal = c(AESDTH = "Y"),
#'       fatal_related = c(AEREL = "Y", AESDTH = "Y")
#'     ),
#'     custom_label = "%s Total number of patients and events"
#'   ) %>%
#'   build_table(df)
#'
#' @export
#' @order 2
summarize_patients_events_in_cols <- function(lyt,
                                              id = "USUBJID",
                                              filters_list = list(),
                                              empty_stats = character(),
                                              na_str = default_na_str(),
                                              ...,
                                              .stats = c(
                                                "unique",
                                                "all",
                                                names(filters_list)
                                              ),
                                              .labels = c(
                                                unique = "Patients (All)",
                                                all = "Events (All)",
                                                labels_or_names(filters_list)
                                              ),
                                              col_split = TRUE) {
  extra_args <- list(id = id, filters_list = filters_list, empty_stats = empty_stats, ...)

  afun_list <- Map(
    function(stat) {
      make_afun(
        s_count_patients_and_multiple_events,
        .stats = stat,
        .formats = "xx."
      )
    },
    stat = .stats
  )
  if (col_split) {
    lyt <- split_cols_by_multivar(
      lyt = lyt,
      vars = rep(id, length(.stats)),
      varlabels = .labels[.stats]
    )
  }
  summarize_row_groups(
    lyt = lyt,
    cfun = afun_list,
    na_str = na_str,
    extra_args = extra_args
  )
}
