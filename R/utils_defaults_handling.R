#' Defaults for stats methods names and their relative formats/labels
#'
#' @description
#' Utility functions to get valid statistic methods for different method groups
#' (`.stats`) and their relative formats (`.formats`) and labels ``
#'
#' @param method_group (`string`) \cr Indicates the group of statistical methods that
#'  we need the defaults from.
#'
#' @export
#' @name default_stats_and_formats
get_stat <- function(method_group) {
  checkmate::assert_string(method_group)
  switch (method_group,
          "count_occurrences" = c("count", "count_fraction", "fraction"),
          "summarize_num_patients" = c("unique", "nonunique", "unique_count"),
          stop(method_group, " is a method_group that has no default statistical method.")
  )
}

#' @describeIn default_stats_and_formats
#'
#' @export
get_format_from_stat <- function(stats) {
  checkmate::assert_character(stats, min.len = 1)
  # XXX this is experimental and needs #983 first
  # it is like tern:::summary_formats() but it should be exported and more general
  default_formats <- tern_default_formats()
  which_fmt <- match(stats, names(default_formats))

  ret <- vector("list", length = length(stats))
  ret[which(!is.na(which_fmt))] <- default_formats[which_fmt]

  setNames(ret, stats)
}

#' @describeIn default_stats_and_formats
#'
#' @export
get_label_from_stat <- function(stats) {
  checkmate::assert_character(stats, min.len = 1)
  default_lbl <- tern_default_labels()
  which_lbl <- match(stats, names(default_lbl))

  ret <- vector("list", length = length(stats))
  ret[which(!is.na(which_lbl))] <- default_lbl[which_lbl]

  setNames(ret, stats)
}
#' @describeIn default_stats_and_formats XXX This will be very general
#'
#' @export
tern_default_formats <- function() {
  c(
    list(
      "count" = "xx",
      "count_fraction" = format_count_fraction_fixed_dp,
      "fraction" = format_fraction_fixed_dp
    ),
    list(
      "unique" = format_count_fraction_fixed_dp,
      "nonunique" = "xx",
      "unique_count" = "xx"
    )
  )
}
#' @describeIn default_stats_and_formats XXX This will be very general
#'
#' @export
tern_default_labels <- function() {
  c(
    list(
      "unique" = "Number of patients with at least one event",
      "nonunique" = "Number of events"
    )
  )
}
