#' Counting Patients and Events in Columns
#'
#' Counting the number of unique patients and the total number of all and specific events
#' when a column table layout is required.
#'
#' @name count_patients_events_in_cols
#'
NULL

#' @describeIn count_patients_events_in_cols Statistics function which counts numbers of patients and multiple events
#'   defined by filters.
#' @inheritParams argument_convention
#' @param filters_list (named `list` of `character`)\cr each element in this list describes one
#'   type of event describe by filters, in the same format as [s_count_patients_with_event()].
#'   If it has a label, then this will be used for the column title.
#' @param empty_stats (`character`)\cr optional names of the statistics that should be returned empty such
#'   that corresponding table cells will stay blank.
#' @param custom_label (`string` or `NULL`)\cr if provided and `labelstr` is empty then this will
#'   be used as label.
#' @export
#' @return [s_count_patients_and_multiple_events()] returns a list with the statistics:\cr
#' - `unique`: number of unique patients in `df`.
#' - `all`: number of rows in `df`.
#' - one element with the same name as in `filters_list`: number of rows in `df`,
#'   i.e. events, fulfilling the filter condition.
#' @examples
#'
#' # `s_count_patients_and_multiple_events()`
#' df <- data.frame(
#'   USUBJID = rep(c("id1", "id2", "id3", "id4"), c(2, 3, 1, 1)),
#'   ARM = c("A", "A", "B", "B", "B", "B", "A"),
#'   AESER = rep("Y", 7),
#'   AESDTH = c("Y", "Y", "N", "Y", "Y", "N", "N"),
#'   AEREL = c("Y", "Y", "N", "Y", "Y", "N", "Y"),
#'   AEDECOD = c("A", "A", "A", "B", "B", "C", "D"),
#'   AEBODSYS = rep(c("SOC1", "SOC2", "SOC3"), c(3, 3, 1))
#' )
#' s_count_patients_and_multiple_events(
#'   df = df,
#'   id = "USUBJID",
#'   filters_list = list(
#'     serious = c(AESER = "Y"),
#'     fatal = c(AESDTH = "Y")
#'   )
#' )
s_count_patients_and_multiple_events <- function(df, # nolint
                                                 id,
                                                 filters_list,
                                                 empty_stats = character(),
                                                 labelstr = "",
                                                 custom_label = NULL) {
  checkmate::assert_list(filters_list, names = "named")

  assertthat::assert_that(
    is.data.frame(df),
    assertthat::is.string(id),
    !(any(c("unique", "all") %in% names(filters_list))),
    is.character(empty_stats),
    assertthat::is.string(labelstr),
    is.null(custom_label) || assertthat::is.string(custom_label)
  )
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
  y$unique <- formatable::with_label(
    s_num_patients_content(df = df, .N_col = 1, .var = id, required = NULL)$unique[1L],
    row_label
  )
  y$all <- formatable::with_label(
    nrow(df),
    row_label
  )
  events <- Map(
    function(filters) {
      formatable::with_label(
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
      y_reduced[[stat]] <- formatable::with_label(character(), obj_label(y_reduced[[stat]]))
    }
    y_reduced
  } else {
    y_complete
  }
  y
}

#' @describeIn count_patients_events_in_cols Layout creating function which adds the count
#'   statistics of patients and events in the column layout as content rows.
#' @inheritParams argument_convention
#' @param col_split (`flag`)\cr whether the columns should be split.
#'  Set to `FALSE` when the required column split has been done already earlier in the layout pipe.
#' @export
#' @examples
#'
#' # `summarize_patients_events_in_cols()`
#' basic_table() %>%
#'   summarize_patients_events_in_cols(
#'     filters_list = list(
#'       related = formatable::with_label(c(AEREL = "Y"), "Events (Related)"),
#'       fatal = c(AESDTH = "Y"),
#'       fatal_related = c(AEREL = "Y", AESDTH = "Y")
#'     ),
#'     custom_label = "%s Total number of patients and events"
#'   ) %>%
#'   build_table(df)
summarize_patients_events_in_cols <- function(lyt, # nolint
                                              id = "USUBJID",
                                              filters_list = list(),
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
  afun_list <- Map(
    function(stat) {
      make_afun(
        s_count_patients_and_multiple_events,
        id = id,
        filters_list = filters_list,
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
    extra_args = list(...)
  )
}
