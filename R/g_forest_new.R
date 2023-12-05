#' #' Create a Forest Plot based on a Table
#' #'
#' #' Create a forest plot from any [rtables::rtable()] object that has a
#' #' column with a single value and a column with 2 values.
#' #'
#' #' @description `r lifecycle::badge("stable")`
#' #'
#' #' @inheritParams grid::gTree
#' #' @inheritParams argument_convention
#' #' @param tbl (`rtable`)
#' #' @param col_x (`integer`)\cr column index with estimator. By default tries to get this from
#' #'   `tbl` attribute `col_x`, otherwise needs to be manually specified.
#' #' @param col_ci (`integer`)\cr column index with confidence intervals. By default tries
#' #'   to get this from `tbl` attribute `col_ci`, otherwise needs to be manually specified.
#' #' @param vline (`number`)\cr x coordinate for vertical line, if `NULL` then the line is omitted.
#' #' @param forest_header (`character`, length 2)\cr text displayed to the left and right of `vline`, respectively.
#' #'   If `vline = NULL` then `forest_header` needs to be `NULL` too.
#' #'   By default tries to get this from `tbl` attribute `forest_header`.
#' #' @param xlim (`numeric`)\cr limits for x axis.
#' #' @param logx (`flag`)\cr show the x-values on logarithm scale.
#' #' @param x_at (`numeric`)\cr x-tick locations, if `NULL` they get automatically chosen.
#' #' @param width_row_names (`unit`)\cr width for row names.
#' #'   If `NULL` the widths get automatically calculated. See [grid::unit()].
#' #' @param width_columns (`unit`)\cr widths for the table columns.
#' #'   If `NULL` the widths get automatically calculated. See [grid::unit()].
#' #' @param width_forest (`unit`)\cr width for the forest column.
#' #'   If `NULL` the widths get automatically calculated. See [grid::unit()].
#' #' @param col_symbol_size (`integer`)\cr column index from `tbl` containing data to be used
#' #'   to determine relative size for estimator plot symbol. Typically, the symbol size is proportional
#' #'   to the sample size used to calculate the estimator. If `NULL`, the same symbol size is used for all subgroups.
#' #'   By default tries to get this from `tbl` attribute `col_symbol_size`, otherwise needs to be manually specified.
#' #' @param col (`character`)\cr color(s).
#' #'
#' #' @return `gTree` object containing the forest plot and table.
#' #'
#' #' @examples
#' #' \donttest{
#' #' library(dplyr)
#' #' library(forcats)
#' #' library(nestcolor)
#' #'
#' #' adrs <- tern_ex_adrs
#' #' n_records <- 20
#' #' adrs_labels <- formatters::var_labels(adrs, fill = TRUE)
#' #' adrs <- adrs %>%
#' #'   filter(PARAMCD == "BESRSPI") %>%
#' #'   filter(ARM %in% c("A: Drug X", "B: Placebo")) %>%
#' #'   slice(seq_len(n_records)) %>%
#' #'   droplevels() %>%
#' #'   mutate(
#' #'     # Reorder levels of factor to make the placebo group the reference arm.
#' #'     ARM = fct_relevel(ARM, "B: Placebo"),
#' #'     rsp = AVALC == "CR"
#' #'   )
#' #' formatters::var_labels(adrs) <- c(adrs_labels, "Response")
#' #' df <- extract_rsp_subgroups(
#' #'   variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "STRATA2")),
#' #'   data = adrs
#' #' )
#' #' # Full commonly used response table.
#' #'
#' #' tbl <- basic_table() %>%
#' #'   tabulate_rsp_subgroups(df)
#' #' p <- g_forest_new(tbl, gp = grid::gpar(fontsize = 10))
#' #'
#' #' draw_grob(p)
#' #'
#' #' # Odds ratio only table.
#' #'
#' #' tbl_or <- basic_table() %>%
#' #'   tabulate_rsp_subgroups(df, vars = c("n_tot", "or", "ci"))
#' #' tbl_or
#' #' p <- g_forest_new(
#' #'   tbl_or,
#' #'   forest_header = c("Comparison\nBetter", "Treatment\nBetter")
#' #' )
#' #'
#' #' draw_grob(p)
#' #'
#' #' # Survival forest plot example.
#' #' adtte <- tern_ex_adtte
#' #' # Save variable labels before data processing steps.
#' #' adtte_labels <- formatters::var_labels(adtte, fill = TRUE)
#' #' adtte_f <- adtte %>%
#' #'   filter(
#' #'     PARAMCD == "OS",
#' #'     ARM %in% c("B: Placebo", "A: Drug X"),
#' #'     SEX %in% c("M", "F")
#' #'   ) %>%
#' #'   mutate(
#' #'     # Reorder levels of ARM to display reference arm before treatment arm.
#' #'     ARM = droplevels(fct_relevel(ARM, "B: Placebo")),
#' #'     SEX = droplevels(SEX),
#' #'     AVALU = as.character(AVALU),
#' #'     is_event = CNSR == 0
#' #'   )
#' #' labels <- list(
#' #'   "ARM" = adtte_labels["ARM"],
#' #'   "SEX" = adtte_labels["SEX"],
#' #'   "AVALU" = adtte_labels["AVALU"],
#' #'   "is_event" = "Event Flag"
#' #' )
#' #' formatters::var_labels(adtte_f)[names(labels)] <- as.character(labels)
#' #' df <- extract_survival_subgroups(
#' #'   variables = list(
#' #'     tte = "AVAL",
#' #'     is_event = "is_event",
#' #'     arm = "ARM", subgroups = c("SEX", "BMRKR2")
#' #'   ),
#' #'   data = adtte_f
#' #' )
#' #' table_hr <- basic_table() %>%
#' #'   tabulate_survival_subgroups(df, time_unit = adtte_f$AVALU[1])
#' #' g_forest_new(table_hr)
#' #' # Works with any `rtable`.
#' #' tbl <- rtable(
#' #'   header = c("E", "CI", "N"),
#' #'   rrow("", 1, c(.8, 1.2), 200),
#' #'   rrow("", 1.2, c(1.1, 1.4), 50)
#' #' )
#' #' g_forest_new(
#' #'   tbl = tbl,
#' #'   col_x = 1,
#' #'   col_ci = 2,
#' #'   xlim = c(0.5, 2),
#' #'   x_at = c(0.5, 1, 2),
#' #'   col_symbol_size = 3
#' #' )
#' #' tbl <- rtable(
#' #'   header = rheader(
#' #'     rrow("", rcell("A", colspan = 2)),
#' #'     rrow("", "c1", "c2")
#' #'   ),
#' #'   rrow("row 1", 1, c(.8, 1.2)),
#' #'   rrow("row 2", 1.2, c(1.1, 1.4))
#' #' )
#' #' g_forest_new(
#' #'   tbl = tbl,
#' #'   col_x = 1,
#' #'   col_ci = 2,
#' #'   xlim = c(0.5, 2),
#' #'   x_at = c(0.5, 1, 2),
#' #'   vline = 1,
#' #'   forest_header = c("Hello", "World")
#' #' )
#' #' }
#' #'
#' #' @export
#' g_forest_new <- function(tbl,
#'                          col_x = attr(tbl, "col_x"),
#'                          col_ci = attr(tbl, "col_ci"),
#'                          vline = 1,
#'                          forest_header = attr(tbl, "forest_header"),
#'                          xlim = c(0.1, 10),
#'                          logx = TRUE,
#'                          x_at = c(0.1, 1, 10),
#'                          width_row_names = NULL,
#'                          width_columns = NULL,
#'                          width_forest = grid::unit(1, "null"),
#'                          col_symbol_size = attr(tbl, "col_symbol_size"),
#'                          col = getOption("ggplot2.discrete.colour")[1],
#'                          gp = NULL,
#'                          draw = TRUE,
#'                          newpage = TRUE) {
#'   checkmate::assert_class(tbl, "VTableTree")
#'
#'   nr <- nrow(tbl)
#'   nc <- ncol(tbl)
#'   if (is.null(col)) {
#'     col <- "blue"
#'   }
#'
#'   checkmate::assert_number(col_x, lower = 0, upper = nc, null.ok = FALSE)
#'   checkmate::assert_number(col_ci, lower = 0, upper = nc, null.ok = FALSE)
#'   checkmate::assert_number(col_symbol_size, lower = 0, upper = nc, null.ok = TRUE)
#'   checkmate::assert_true(col_x > 0)
#'   checkmate::assert_true(col_ci > 0)
#'   checkmate::assert_character(col)
#'   if (!is.null(col_symbol_size)) {
#'     checkmate::assert_true(col_symbol_size > 0)
#'   }
#'
#'   x_e <- vapply(seq_len(nr), function(i) {
#'     # If a label row is selected NULL is returned with a warning (suppressed)
#'     xi <- suppressWarnings(as.vector(tbl[i, col_x, drop = TRUE]))
#'
#'     if (!is.null(xi) && !(length(xi) <= 0) && is.numeric(xi)) {
#'       xi
#'     } else {
#'       NA_real_
#'     }
#'   }, numeric(1))
#'
#'   x_ci <- lapply(seq_len(nr), function(i) {
#'     xi <- suppressWarnings(as.vector(tbl[i, col_ci, drop = TRUE])) # as above
#'
#'     if (!is.null(xi) && !(length(xi) <= 0) && is.numeric(xi)) {
#'       if (length(xi) != 2) {
#'         stop("ci column needs two elements")
#'       }
#'       xi
#'     } else {
#'       c(NA_real_, NA_real_)
#'     }
#'   })
#'
#'   lower <- vapply(x_ci, `[`, numeric(1), 1)
#'   upper <- vapply(x_ci, `[`, numeric(1), 2)
#'
#'   symbol_size <- if (!is.null(col_symbol_size)) {
#'     tmp_symbol_size <- vapply(seq_len(nr), function(i) {
#'       suppressWarnings(xi <- as.vector(tbl[i, col_symbol_size, drop = TRUE]))
#'
#'       if (!is.null(xi) && !(length(xi) <= 0) && is.numeric(xi)) {
#'         xi
#'       } else {
#'         NA_real_
#'       }
#'     }, numeric(1))
#'
#'     # Scale symbol size.
#'     tmp_symbol_size <- sqrt(tmp_symbol_size)
#'     max_size <- max(tmp_symbol_size, na.rm = TRUE)
#'     # Biggest points have radius is 2 * (1/3.5) lines not to overlap.
#'     # See forest_dot_line.
#'     2 * tmp_symbol_size / max_size
#'   } else {
#'     NULL
#'   }
#'
#'   grob_forest <- forest_grob(
#'     tbl,
#'     x_e,
#'     lower,
#'     upper,
#'     vline,
#'     forest_header,
#'     xlim,
#'     logx,
#'     x_at,
#'     width_row_names,
#'     width_columns,
#'     width_forest,
#'     symbol_size = symbol_size,
#'     col = col,
#'     gp = gp,
#'     vp = grid::plotViewport(margins = rep(1, 4))
#'   )
#'
#'   if (draw) {
#'     if (newpage) grid::grid.newpage()
#'     grid::grid.draw(grob_forest)
#'   }
#'
#'   invisible(grob_forest)
#' }

g_forest_new <- function(tbl,
                         vline = 1,
                         fontsize = 4,
                         xlim = c(0.1, 10),
                         logx = TRUE,
                         x_at = c(0.1, 1, 10),
                         symbol_size = attr(tbl, "col_symbol_size"),
                         col = getOption("ggplot2.discrete.colour")[1],
                         width_plot = 0.25) {
  checkmate::assert_class(tbl, "VTableTree")

  gg_table <- rtable2gg(tbl) +
    theme(plot.margin = margin(0, 0, 0, 0.025, "npc"))
  gg_table$scales$scales[[1]]$expand <- c(0.01, 0.01)

  mat <- matrix_form(tbl)
  mat_strings <- formatters::mf_strings(mat)
  nlines_hdr <- formatters::mf_nlheader(mat)
  nrows_body <- nrow(mat_strings) - nlines_hdr
  if (nlines_hdr == 2) {
    gg_table$scales$scales[[2]]$limits[2] <- nrow(mat_strings) + 1
    gg_table$scales$scales[[2]]$expand <- c(0, 0)
    arms <- unique(mat_strings[1, ][nzchar(trimws(mat_strings[1, ]))])
  } else {
    arms <- NULL
  }
  tbl_stats <- mat_strings[nlines_hdr, -1]

  tbl_df <- as_result_df(tbl)
  dat_cols <- seq(which(names(tbl_df) == "node_class") + 1, ncol(tbl_df))
  tbl_df <- tbl_df[, c(which(names(tbl_df) == "row_num"), dat_cols)]
  names(tbl_df) <- c("row_num", tbl_stats)

  tbl_df[, c("ci_lwr", "ci_upr")] <- t(sapply(tbl_df[["95% CI"]], unlist))
  tbl_df <- tbl_df[names(tbl_df) != "95% CI"]
  tbl_df[, -1] <- apply(tbl_df[, -1], 2, unlist)

  tbl_df[["row_num"]] <- nlines_hdr + 1 + nrow(tbl_df) - tbl_df[["row_num"]]

  if (is.null(x_at)) x_at <- c(0.1, 1, 10)
  x_labels <- x_at
  if (logx) {
    xlim_t <- log(xlim)
    tbl_df[["Odds Ratio"]] <- log(tbl_df[["Odds Ratio"]])
    tbl_df[["ci_lwr"]] <- log(tbl_df[["ci_lwr"]])
    tbl_df[["ci_upr"]] <- log(tbl_df[["ci_upr"]])
  } else {
    xlim_t <- xlim
  }
  # # Rescale input values for modified x-axis
  # rescale_x_val <- function(x) if (x < 1) (x - 0.1) / (1 - 0.1) * (1 - -9) + -9 else x
  # tbl_df[["Odds Ratio"]] <- sapply(tbl_df[["Odds Ratio"]], rescale_x_val)
  # tbl_df[["ci_lwr"]] <- sapply(tbl_df[["ci_lwr"]], rescale_x_val)
  # tbl_df[["ci_upr"]] <- sapply(tbl_df[["ci_upr"]], rescale_x_val)
  # vline <- rescale_x_val(vline)

  gg_plt <- ggplot(data = tbl_df) +
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.line.x = element_line(),
      legend.position = "none",
      plot.margin = margin(0, 0.1, 0.05, 0, "npc")
    ) +
    scale_x_continuous(
      trans = ifelse(logx, "log", "identity"),
      limits = xlim,
      breaks = x_at,
      labels = x_labels,
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = c(0, nrow(mat_strings) + as.numeric(nlines_hdr == 2)),
      breaks = NULL,
      expand = c(0, 0)
    ) +
    coord_cartesian(clip = "off") +
    geom_rect(data = NULL, aes(xmin = xlim[1], xmax = xlim[2], ymin = 0, ymax = nrows_body + 0.5), fill = "grey92") +
    geom_segment(aes(x = vline, xend = vline, y = 0, yend = nrows_body + 0.5)) +
    geom_point(x = tbl_df[["Odds Ratio"]], y = tbl_df[["row_num"]], aes(size = `Total n`, color = col))

  mid_pt <- if (!is.null(vline)) vline else if (length(x_at) == 3) x_at[2] else mean(xlim)
  gg_plt <- gg_plt +
    geom_text(
      x = mean(log(c(xlim[1], mid_pt))), y = nrows_body + 1.25,
      label = paste(if (!is.null(arms)) arms[1] else "Comparison", "Better", sep = "\n"),
      size = fontsize,
      lineheight = 0.9
    ) +
    geom_text(
      x = mean(log(c(mid_pt, xlim[2]))), y = nrows_body + 1.25,
      label = paste(if (!is.null(arms)) arms[2] else "Treatment", "Better", sep = "\n"),
      size = fontsize,
      lineheight = 0.9
    )

  for (i in seq_len(nrow(tbl_df))) {
    which_arrow <- c(tbl_df[i, "ci_lwr"] < xlim_t[1], tbl_df[i, "ci_upr"] > xlim_t[2])
    which_arrow <- case_when(
      all(which_arrow) ~ "both",
      which_arrow[1] ~ "first",
      which_arrow[2] ~ "last",
      TRUE ~ NA
    )

    gg_plt <- gg_plt +
      geom_segment(
        x = if (!which_arrow %in% c("first", "both")) tbl_df[["ci_lwr"]][i] else xlim_t[1],
        xend = if (!which_arrow %in% c("last", "both")) tbl_df[["ci_upr"]][i] else xlim_t[2],
        y = tbl_df[["row_num"]][i], yend = tbl_df[["row_num"]][i],
        color = col,
        arrow = if (is.na(which_arrow)) NULL else arrow(length = unit(0.05, "npc"), ends = which_arrow)
      )
  }

  cowplot::plot_grid(
    gg_table,
    gg_plt,
    align = "h",
    rel_widths = c(1 - width_plot, width_plot)
  )
}
