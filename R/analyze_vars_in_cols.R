#' Summary numeric variables in columns
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Layout-creating function which can be used for creating column-wise summary tables.
#' This function sets the analysis methods as column labels and is a wrapper for
#' [rtables::analyze_colvars()]. It was designed principally for PK tables.
#'
#' @inheritParams argument_convention
#' @inheritParams rtables::analyze_colvars
#' @param imp_rule (`character`)\cr imputation rule setting. Defaults to `NULL` for no imputation rule. Can
#'   also be `"1/3"` to implement 1/3 imputation rule or `"1/2"` to implement 1/2 imputation rule. In order
#'   to use an imputation rule, the `avalcat_var` argument must be specified. See [imputation_rule()]
#'   for more details on imputation.
#' @param avalcat_var (`character`)\cr if `imp_rule` is not `NULL`, name of variable that indicates whether a
#'   row in the data corresponds to an analysis value in category `"BLQ"`, `"LTR"`, `"<PCLLOQ"`, or none of
#'   the above (defaults to `"AVALCAT1"`). Variable must be present in the data and should match the variable
#'   used to calculate the `n_blq` statistic (if included in `.stats`).
#' @param cache (`flag`)\cr whether to store computed values in a temporary caching environment. This will
#'   speed up calculations in large tables, but should be set to `FALSE` if the same `rtable` layout is
#'   used for multiple tables with different data. Defaults to `FALSE`.
#' @param row_labels (`character`)\cr as this function works in columns space, usual `.labels`
#'   character vector applies on the column space. You can change the row labels by defining this
#'   parameter to a named character vector with names corresponding to the split values. It defaults
#'   to `NULL` and if it contains only one `string`, it will duplicate that as a row label.
#' @param do_summarize_row_groups (`flag`)\cr defaults to `FALSE` and applies the analysis to the current
#'   label rows. This is a wrapper of [rtables::summarize_row_groups()] and it can accept `labelstr`
#'   to define row labels. This behavior is not supported as we never need to overload row labels.
#' @param split_col_vars (`flag`)\cr defaults to `TRUE` and puts the analysis results onto the columns.
#'   This option allows you to add multiple instances of this functions, also in a nested fashion,
#'   without adding more splits. This split must happen only one time on a single layout.
#'
#' @return
#' A layout object suitable for passing to further layouting functions, or to [rtables::build_table()].
#' Adding this function to an `rtable` layout will summarize the given variables, arrange the output
#' in columns, and add it to the table layout.
#'
#' @note This is an experimental implementation of [rtables::summarize_row_groups()] and
#'   [rtables::analyze_colvars()] that may be subjected to changes as `rtables` extends its
#'   support to more complex analysis pipelines on the column space. For the same reasons,
#'   we encourage to read the examples carefully and file issues for cases that differ from
#'   them.
#'
#'   Here `labelstr` behaves differently than usual. If it is not defined (default as `NULL`),
#'   row labels are assigned automatically to the split values in case of `rtables::analyze_colvars`
#'   (`do_summarize_row_groups = FALSE`, the default), and to the group label for
#'   `do_summarize_row_groups = TRUE`.
#'
#' @seealso [analyze_vars()], [rtables::analyze_colvars()].
#'
#' @examples
#' library(dplyr)
#'
#' # Data preparation
#' adpp <- tern_ex_adpp %>% h_pkparam_sort()
#'
#' lyt <- basic_table() %>%
#'   split_rows_by(var = "STRATA1", label_pos = "topleft") %>%
#'   split_rows_by(
#'     var = "SEX",
#'     label_pos = "topleft",
#'     child_label = "hidden"
#'   ) %>% # Removes duplicated labels
#'   analyze_vars_in_cols(vars = "AGE")
#' result <- build_table(lyt = lyt, df = adpp)
#' result
#'
#' # By selecting just some statistics and ad-hoc labels
#' lyt <- basic_table() %>%
#'   split_rows_by(var = "ARM", label_pos = "topleft") %>%
#'   split_rows_by(
#'     var = "SEX",
#'     label_pos = "topleft",
#'     child_labels = "hidden",
#'     split_fun = drop_split_levels
#'   ) %>%
#'   analyze_vars_in_cols(
#'     vars = "AGE",
#'     .stats = c("n", "cv", "geom_mean"),
#'     .labels = c(
#'       n = "aN",
#'       cv = "aCV",
#'       geom_mean = "aGeomMean"
#'     )
#'   )
#' result <- build_table(lyt = lyt, df = adpp)
#' result
#'
#' # Changing row labels
#' lyt <- basic_table() %>%
#'   analyze_vars_in_cols(
#'     vars = "AGE",
#'     row_labels = "some custom label"
#'   )
#' result <- build_table(lyt, df = adpp)
#' result
#'
#' # Pharmacokinetic parameters
#' lyt <- basic_table() %>%
#'   split_rows_by(
#'     var = "TLG_DISPLAY",
#'     split_label = "PK Parameter",
#'     label_pos = "topleft",
#'     child_label = "hidden"
#'   ) %>%
#'   analyze_vars_in_cols(
#'     vars = "AVAL"
#'   )
#' result <- build_table(lyt, df = adpp)
#' result
#'
#' # Multiple calls (summarize label and analyze underneath)
#' lyt <- basic_table() %>%
#'   split_rows_by(
#'     var = "TLG_DISPLAY",
#'     split_label = "PK Parameter",
#'     label_pos = "topleft"
#'   ) %>%
#'   analyze_vars_in_cols(
#'     vars = "AVAL",
#'     do_summarize_row_groups = TRUE # does a summarize level
#'   ) %>%
#'   split_rows_by("SEX",
#'     child_label = "hidden",
#'     label_pos = "topleft"
#'   ) %>%
#'   analyze_vars_in_cols(
#'     vars = "AVAL",
#'     split_col_vars = FALSE # avoids re-splitting the columns
#'   )
#' result <- build_table(lyt, df = adpp)
#' result
#'
#' @export
analyze_vars_in_cols <- function(lyt,
                                 vars,
                                 ...,
                                 .stats = c(
                                   "n",
                                   "mean",
                                   "sd",
                                   "se",
                                   "cv",
                                   "geom_cv"
                                 ),
                                 .labels = c(
                                   n = "n",
                                   mean = "Mean",
                                   sd = "SD",
                                   se = "SE",
                                   cv = "CV (%)",
                                   geom_cv = "CV % Geometric Mean"
                                 ),
                                 row_labels = NULL,
                                 do_summarize_row_groups = FALSE,
                                 split_col_vars = TRUE,
                                 imp_rule = NULL,
                                 avalcat_var = "AVALCAT1",
                                 cache = FALSE,
                                 .indent_mods = NULL,
                                 na_level = lifecycle::deprecated(),
                                 na_str = default_na_str(),
                                 nested = TRUE,
                                 .formats = NULL,
                                 .aligns = NULL) {
  extra_args <- list(...)
  if (lifecycle::is_present(na_level)) {
    lifecycle::deprecate_warn("0.9.1", "analyze_vars_in_cols(na_level)", "analyze_vars_in_cols(na_str)")
    na_str <- na_level
  }

  checkmate::assert_string(na_str, na.ok = TRUE, null.ok = TRUE)
  checkmate::assert_character(row_labels, null.ok = TRUE)
  checkmate::assert_int(.indent_mods, null.ok = TRUE)
  checkmate::assert_flag(nested)
  checkmate::assert_flag(split_col_vars)
  checkmate::assert_flag(do_summarize_row_groups)

  # Filtering
  met_grps <- paste0("analyze_vars", c("_numeric", "_counts"))
  .stats <- get_stats(met_grps, stats_in = .stats)
  formats_v <- get_formats_from_stats(stats = .stats, formats_in = .formats)
  labels_v <- get_labels_from_stats(stats = .stats, labels_in = .labels)
  if ("control" %in% names(extra_args)) labels_v <- labels_v %>% labels_use_control(extra_args[["control"]], .labels)

  # Check for vars in the case that one or more are used
  if (length(vars) == 1) {
    vars <- rep(vars, length(.stats))
  } else if (length(vars) != length(.stats)) {
    stop(
      "Analyzed variables (vars) does not have the same ",
      "number of elements of specified statistics (.stats)."
    )
  }

  if (split_col_vars) {
    # Checking there is not a previous identical column split
    clyt <- tail(clayout(lyt), 1)[[1]]

    dummy_lyt <- split_cols_by_multivar(
      lyt = basic_table(),
      vars = vars,
      varlabels = labels_v
    )

    if (any(sapply(clyt, identical, y = get_last_col_split(dummy_lyt)))) {
      stop(
        "Column split called again with the same values. ",
        "This can create many unwanted columns. Please consider adding ",
        "split_col_vars = FALSE to the last call of ",
        deparse(sys.calls()[[sys.nframe() - 1]]), "."
      )
    }

    # Main col split
    lyt <- split_cols_by_multivar(
      lyt = lyt,
      vars = vars,
      varlabels = labels_v
    )
  }

  env <- new.env() # create caching environment

  if (do_summarize_row_groups) {
    if (length(unique(vars)) > 1) {
      stop("When using do_summarize_row_groups only one label level var should be inserted.")
    }

    # Function list for do_summarize_row_groups. Slightly different handling of labels
    cfun_list <- Map(
      function(stat, use_cache, cache_env) {
        function(u, .spl_context, labelstr, .df_row, ...) {
          # Statistic
          var_row_val <- paste(
            gsub("\\._\\[\\[[0-9]+\\]\\]_\\.", "", paste(tail(.spl_context$cur_col_split_val, 1)[[1]], collapse = "_")),
            paste(.spl_context$value, collapse = "_"),
            sep = "_"
          )
          if (use_cache) {
            if (is.null(cache_env[[var_row_val]])) cache_env[[var_row_val]] <- s_summary(u, ...)
            x_stats <- cache_env[[var_row_val]]
          } else {
            x_stats <- s_summary(u, ...)
          }

          if (is.null(imp_rule) || !stat %in% c("mean", "sd", "cv", "geom_mean", "geom_cv", "median", "min", "max")) {
            res <- x_stats[[stat]]
          } else {
            timept <- as.numeric(gsub(".*?([0-9\\.]+).*", "\\1", tail(.spl_context$value, 1)))
            res_imp <- imputation_rule(
              .df_row, x_stats, stat,
              imp_rule = imp_rule,
              post = grepl("Predose", tail(.spl_context$value, 1)) || timept > 0,
              avalcat_var = avalcat_var
            )
            res <- res_imp[["val"]]
            na_str <- res_imp[["na_str"]]
          }

          # Label check and replacement
          if (length(row_labels) > 1) {
            if (!(labelstr %in% names(row_labels))) {
              stop(
                "Replacing the labels in do_summarize_row_groups needs a named vector",
                "that contains the split values. In the current split variable ",
                .spl_context$split[nrow(.spl_context)],
                " the labelstr value (split value by default) ", labelstr, " is not in",
                " row_labels names: ", names(row_labels)
              )
            }
            lbl <- unlist(row_labels[labelstr])
          } else {
            lbl <- labelstr
          }

          # Cell creation
          rcell(res,
            label = lbl,
            format = formats_v[names(formats_v) == stat][[1]],
            format_na_str = na_str,
            indent_mod = ifelse(is.null(.indent_mods), 0L, .indent_mods),
            align = .aligns
          )
        }
      },
      stat = .stats,
      use_cache = cache,
      cache_env = replicate(length(.stats), env)
    )

    # Main call to rtables
    summarize_row_groups(
      lyt = lyt,
      var = unique(vars),
      cfun = cfun_list,
      na_str = na_str,
      extra_args = extra_args
    )
  } else {
    # Function list for analyze_colvars
    afun_list <- Map(
      function(stat, use_cache, cache_env) {
        function(u, .spl_context, .df_row, ...) {
          # Main statistics
          var_row_val <- paste(
            gsub("\\._\\[\\[[0-9]+\\]\\]_\\.", "", paste(tail(.spl_context$cur_col_split_val, 1)[[1]], collapse = "_")),
            paste(.spl_context$value, collapse = "_"),
            sep = "_"
          )
          if (use_cache) {
            if (is.null(cache_env[[var_row_val]])) cache_env[[var_row_val]] <- s_summary(u, ...)
            x_stats <- cache_env[[var_row_val]]
          } else {
            x_stats <- s_summary(u, ...)
          }

          if (is.null(imp_rule) || !stat %in% c("mean", "sd", "cv", "geom_mean", "geom_cv", "median", "min", "max")) {
            res <- x_stats[[stat]]
          } else {
            timept <- as.numeric(gsub(".*?([0-9\\.]+).*", "\\1", tail(.spl_context$value, 1)))
            res_imp <- imputation_rule(
              .df_row, x_stats, stat,
              imp_rule = imp_rule,
              post = grepl("Predose", tail(.spl_context$value, 1)) || timept > 0,
              avalcat_var = avalcat_var
            )
            res <- res_imp[["val"]]
            na_str <- res_imp[["na_str"]]
          }

          if (is.list(res)) {
            if (length(res) > 1) {
              stop("The analyzed column produced more than one category of results.")
            } else {
              res <- unlist(res)
            }
          }

          # Label from context
          label_from_context <- .spl_context$value[nrow(.spl_context)]

          # Label switcher
          if (is.null(row_labels)) {
            lbl <- label_from_context
          } else {
            if (length(row_labels) > 1) {
              if (!(label_from_context %in% names(row_labels))) {
                stop(
                  "Replacing the labels in do_summarize_row_groups needs a named vector",
                  "that contains the split values. In the current split variable ",
                  .spl_context$split[nrow(.spl_context)],
                  " the split value ", label_from_context, " is not in",
                  " row_labels names: ", names(row_labels)
                )
              }
              lbl <- unlist(row_labels[label_from_context])
            } else {
              lbl <- row_labels
            }
          }

          # Cell creation
          rcell(res,
            label = lbl,
            format = formats_v[names(formats_v) == stat][[1]],
            format_na_str = na_str,
            indent_mod = ifelse(is.null(.indent_mods), 0L, .indent_mods),
            align = .aligns
          )
        }
      },
      stat = .stats,
      use_cache = cache,
      cache_env = replicate(length(.stats), env)
    )

    # Main call to rtables
    analyze_colvars(lyt,
      afun = afun_list,
      na_str = na_str,
      nested = nested,
      extra_args = extra_args
    )
  }
}

# Help function
get_last_col_split <- function(lyt) {
  tail(tail(clayout(lyt), 1)[[1]], 1)[[1]]
}
