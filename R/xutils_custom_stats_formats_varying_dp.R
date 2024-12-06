#' Get statistical methods and formats for custom variants
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'
#' @param stats (`character`)\cr statistical methods to get defaults for.
#'
#' @details
#' Currently only available for usage within `a_summary`, `analyze_vars`, and hope to extend to at least `a_ancova` and `summarize_ancova`.
#' Question to Roche: is there an intention to refactor a_ancova and summarize_ancova to not use make_afun, but same approach of in_rows as in a_summary?
#'
#' @note
#' These defaults are experimental because we use the names of functions to retrieve the default
#' statistics. This should be generalized in groups of methods according to more reasonable groupings.
#'
#' @name custom_stats_formats
#' @include utils_default_stats_formats_labels.R
#' @include formatting_functions.R
#' @order 1
NULL

#' @describeIn custom_stats_formats Get formats corresponding to a list of statistics.
#'
#' @param formats_in (named `vector`)\cr inserted formats to replace defaults. It can be a
#'   character vector from [formatters::list_valid_format_labels()] or a custom format function.
#'
#' @param fmts_specs (named `list`) with specifications (
#' TO EXPAND)
#'
#' @return
#' * `get_formats_from_stats_custom()` returns a 3 component list. The primary one (named .fmt) is a named vector of formats (if present in either
#'   `fmts_df` or `formats_in`, otherwise `NULL`). Values can be taken from
#'   [formatters::list_valid_format_labels()] or a custom function (e.g. [formatting_functions]).
#'   .fmt_char is the character representation.
#'   .fmt_fun is the name of the formatting function applied to the character representation
#'
#' @note Formats in `tern` and `rtables` can be functions that take in the table cell value and
#'   return a string. This is well documented in `vignette("custom_appearance", package = "rtables")`.
#'
#' @family formatting functions
#' @seealso [default_stats_formats_labels]
#'
#' @export
#'
#' @examples
#' # Defaults formats
#' get_formats_from_stats_custom(
#' stats = c("mean", "sd"),
#' fmts_df = tern_formats_custom_df(),
#' fmts_df_var = "variant1",
#' d = 1)
#'
#' get_formats_from_stats_custom(stats = c("mean", "sd"))$fmt_char
#'
#' get_formats_from_stats(stats = c("mean", "sd"))
#'
#' get_formats_from_stats_custom(
#' stats = c("mean", "sd"),
#' fmts_df = tern_formats_custom_df(),
#' fmts_df_var = "variant2",
#' d = 2,
#' formatting_function = format_xx)$fmt_char
#'
#' get_formats_from_stats_custom(
#' stats = c("mean", "sd"),
#' fmts_df = tern_formats_custom_df(),
#' fmts_df_var = "variant2",
#' d = 2,
#' formatting_function = format_xx)$fmt_char
#'
#'
#' # Addition of customs including xx.d style notation
#' get_formats_from_stats_custom(
#' stats = c("mean", "sd"),
#' formats_in = c("mean" = "xx.dxxxx"),
#' fmts_df = tern_formats_custom_df(),
#' fmts_df_var = "variant2",
#' d = 0)$fmt
#'
#' get_formats_from_stats_custom(
#' stats = c("mean_pval", "mean", "sd" ),
#' formats_in = c("mean" = "xx.dxxxx"),
#' fmts_specs = list(
#' fmts_df = tern_formats_custom_df(),
#' fmts_df_var = "variant2",
#' d = 0,
#' formatting_function = format_xx_fixed_dp))$fmt
#'
#' get_formats_from_stats_custom(
#' stats = c("mean_pval", "mean", "sd" ),
#' formats_in = c("mean" = "xx.xxxx"),
#' fmts_specs = list(
#' fmts_df = tern_formats_custom_df(),
#' fmts_df_var = "default"
#' ))$fmt

#' get_formats_from_stats_custom(
#' stats = c("mean_pval", "mean", "sd" ),
#' formats_in = c("mean" = "xx.xxxx"),
#' fmts_specs = list(
#' fmts_df = tern_formats_custom_df(),
#' fmts_df_var = "default"
#' ))$fmt_fun
#'
#' get_formats_from_stats_custom(
#' stats = c("mean_pval", "mean", "sd" ),
#' formats_in = c("mean" = "xx.xxxx"),
#' fmts_specs = list(
#' fmts_df = tern_formats_custom_df(),
#' fmts_df_var = "default"
#' ))$fmt_fun
#'
#' # example using analyze_vars on continuous data
#' dt2 <- data.frame("VAR" = c(0.001, 0.2, 0.0011000, 3, 4, 0.002, 0.004, 0.006), decimal = c(rep(2, 4), rep(1, 4)), by = c(rep("by1", 4), rep("by2", 4)))
#' our_fmt_specs_variant2 <-  list(
#' fmts_df = tern_formats_custom_df(),
#' fmts_df_var = "variant2",
#' formatting_function = "format_xx_fixed_dp",
#' d = "decimal",
#' d_cap = 0)
#' basic_table() %>%
#'   split_rows_by("by") %>%
#'   analyze_vars(
#'     vars = "VAR",
#'     .stats = c("n", "mean", "mean_sd", "range"),
#'     fmt_specs = our_fmt_specs_variant2
#'   ) %>%
#'   build_table(dt2)
#'
get_formats_from_stats_custom <- function(stats,
                                            formats_in = NULL,
                                            fmts_specs = list(
                                              fmts_df = tern_formats_custom_df(),
                                              fmts_df_var = "default",
                                              d = 0)
                                            )
                                            {

  # It may be a list if there is a function in the formats
  if (checkmate::test_list(formats_in, null.ok = TRUE)) {
    checkmate::assert_list(formats_in, null.ok = TRUE)
    # Or it may be a vector of characters
  } else {
    checkmate::assert_character(formats_in, null.ok = TRUE)
  }

  checkmate::assert_list(fmts_specs)

  checkmate::assert_subset(names(fmts_specs), c("fmts_df", "fmts_df_var", "d", "d_cap", "formatting_function", "formatting_function_exclude"))

  list2env(fmts_specs, environment())
  reqvars <- c(fmts_df_var, "stat")
  reqvars <- list(reqvars)
  names(reqvars) <- reqvars
  assert_df_with_variables(fmts_df, variables = reqvars)

  ls <- ls(environment())
  if (!("formatting_function_exclude" %in% ls)) {formatting_function_exclude <- NULL}
  if (!("formatting_function" %in% ls)) {formatting_function <- NULL}
  if ("fmts_df_var" %in% ls && fmts_df_var != "default" &&!("fmts_df" %in% ls)) {
    stop("fmts_df should be added to fmts_specs")}
  if (!("fmts_df" %in% ls)) { fmts_df <- NULL}
  if (!("d" %in% ls)) { d <- NULL}

  if (is.null(stats)){
    stats <- fmts_df$stat
  } else {
    checkmate::assert_character(stats, min.len = 1)
  }

  if (!is.null(formatting_function)){
    checkmate::assert_character(formatting_function)
    a_formatting_function <- get(formatting_function)
    checkmate::assert_function(a_formatting_function)
  }

  checkmate::assert_character(formatting_function_exclude, null.ok = TRUE)


  # Extract defaults from the fmts_df dataframe
    which_fmt <- match(stats, fmts_df$stat)



  def_formats <- get_formats_from_df(fmts_df = fmts_df,
                                     fmts_df_var = fmts_df_var,
                                     stats = stats)

  # Select only needed formats from stats
  ret <- vector("list", length = length(stats)) # Returning a list is simpler
  ret[!is.na(which_fmt)] <- def_formats[!is.na(which_fmt)]

  out <- setNames(ret, stats)

  # Modify some with custom formats
  if (!is.null(formats_in)) {
    # Stats is the main
    common_names <- intersect(names(out), names(formats_in))
    out[common_names] <- formats_in[common_names]
  }
  # convert the xx.d notation into full xx.x notation
  out <- xxd_to_xx(out, d)

  out_char <- out

  if (!is.null(formatting_function)){

    # split format vector into ones that should not utilize formatting function and ones that do need
    non_fmt_function_stats <- c("n", "mean_pval", "pval", formatting_function_exclude)
    out1 <- out[names(out) %in% non_fmt_function_stats]
    out2 <- out[!(names(out) %in% non_fmt_function_stats)]
    out2 <- sapply(out2, function(x) {
      a_formatting_function(x)
      })
    out <- c(out1, out2)

    # reset the formats in the proper order
    out <- out[stats]

  }

  return(list(fmt = out,
              fmt_char = out_char,
              fmt_fun = formatting_function))
}



#' @describeIn custom_stats_formats Approach for custom formats using d-style formats where d is a parameter for the decimal precision.
#' The construction of the dataframe can be customized and used as input dataframe in further processing (eg afun = `a_summary`)
#'
#' @return
#' * The result of `tern_formats_custom_df` is a dataframe of available default formats, with each element
#'   named for their corresponding statistic.
#'   xx.d will be translated into xx. for d=0, xx.x for d=1, and xx.xx for d=2
#'   xx.dx will be translated into xx.x for d=0, xx.xx for d=1, and xx.xxx for d=2
#'
#' @include utils_default_stats_formats_labels.R
#'
#' @export
#'
#' @examples
#'
#' our_custom_fmts <- tern_formats_custom_df()
#'
#'
tern_formats_custom_df <- function(){

  start_stats <- get_stats(method_groups = c("analyze_vars_numeric", "summarize_ancova"))

  start_fmts <- get_formats_from_stats(start_stats)

  start_fmts <- sapply(start_fmts, function(x){
    if (is.null(x)){
      NA_character_
    } else x
  })

  start_fmts[c("lsmean", "lsmean_diff")] <- "xx.xx"
  start_fmts[c("lsmean_diff_ci")] <- "(xx.xx, xx.xx)"

  fmts_df <- data.frame( stat = start_stats, default = start_fmts)

  fmts_df$variant1 <- fmts_df$default

  selstats <- !(start_stats %in% c("n", "pval", "mean_pval"))
  fmts_df$variant1[selstats] <- gsub("xx.x", "xx.dx", fmts_df$variant1[selstats], fixed = TRUE)




  fmts_df$variant2 <- fmts_df$variant1

  ## modify precision for some
  fmts_df$variant2[start_stats %in% c("sd", "se")] <- "xx.dxx"

  fmts_df$variant2[start_stats %in% c("mean_sd", "mean_se")] <- "xx.dx (xx.dxx)"

  return(fmts_df)
}


xxd_to_xx <- function(str, d = 0){

  checkmate::assert_integerish(d, null.ok = TRUE)
  if (checkmate::test_list(str, null.ok = FALSE)) {
    checkmate::assert_list(str, null.ok = FALSE)
    # Or it may be a vector of characters
  } else {
    checkmate::assert_character(str, null.ok = FALSE)
  }

  nmstr <- names(str)

  if (any(grepl("xx.d", str, fixed = TRUE))){
    checkmate::assert_integerish(d)
    str <-
      gsub("xx.d", paste0("xx.", paste0(rep("x", times = d), collapse = "")), str, fixed = TRUE)

  }
  str <- setNames(str, nmstr)
  return(str)
}


#' @describeIn custom_stats_formats Approach for custom formats using d-style formats where d is a parameter for the decimal precision.
#' @param fmts_df dataframe Any dataframe can be used. Expect to have the following columns: `stat`, `default`, and any other column with custom formatting definitions.
#' @param fmts_df_var Column name from the `fmts_df` that should be used for the format definitions.
#' @param stats Character vector with the names of the stats to define the custom format.
#' @examples
#' # Defaults formats
#'
#' get_formats_from_df(
#' stats = c("mean", "sd"),
#' fmts_df = tern_formats_custom_df(),
#' fmts_df_var = "variant1")
#' #' get_formats_from_df(
#' stats = c("mean", "sd"),
#' fmts_df = tern_formats_custom_df(),
#' fmts_df_var = "variant2")
#'
#' get_formats_from_df(
#' fmts_df = tern_formats_custom_df(),
#' fmts_df_var = "variant2")
#'
get_formats_from_df <- function(fmts_df = tern_formats_custom_df(),
                                fmts_df_var = "default",
                                stats = NULL){

  checkmate::assert_data_frame(fmts_df)

  check <- all(c("stat", fmts_df_var) %in% names(fmts_df))
  if (!check) {
    stop(paste0("The dataframe fmts_df should have at least variables stat and ", fmts_df_var))
  }


  if (is.null(stats)) {
    fmts <- fmts_df[, fmts_df_var]
    names(fmts) <- fmts_df[, "stat"]
  } else {
    fmts <- fmts_df[fmts_df$stat %in% stats, fmts_df_var]
    names(fmts) <- fmts_df[fmts_df$stat %in% stats, "stat"]
  }

  if (!is.null(stats)){
    fmts <- fmts[stats]
  }

  return(fmts)

}


default_fmt_specs <- list(fmts_df = NULL)

default_fmt_specs_variant <-  list(
  fmts_df = tern_formats_custom_df(),
  fmts_df_var = "default",
  formatting_function = "format_xx_fixed_dp",
  d = 0)


derive_d_from_fmt_specs <- function(fmt_specs, .df_row){
  ## core code on varying formats
  ## d is the requested precision of the incoming data (d = 0 will lead to showing mean at 1 decimal precision)
  ## this is the reason to set d <- 0 when not available
  if (exists("fmt_specs") && "d" %in% names(fmt_specs)) {
    d <- fmt_specs$d
  } else {
    d <- 0
  }
  ## set a cap to it, to avoid when precision of data is too high, too many digits will be included in the format
  if (exists("fmt_specs") && "d_cap" %in% names(fmt_specs)) {
    d_cap <- fmt_specs$d_cap
  } else {
    # if not available, set default to 3 (ie still showing 4 digits)
    d_cap <- 3
  }

  # when d has been defined as a character string
  # we assume this is a variable name available on the input dataset
  if (is.character(d)){
    # d is expected to be a variable name on the input dataframe .df_row
    check <- d %in% names(.df_row)
    if (!check){
      stop(paste0("variable ", d, " is not a variable name on the input dataset"))
    }
    d <- max(.df_row[[d]])
  }

  # now apply the cap
  d <- min(d, d_cap)

  return(d)
}
