#' Create a Forest Plot based on a Table
#'
#' Create a forest plot from any [rtables::rtable()] object that has a
#' column with a single value and a column with 2 values.
#'
#' @inheritParams argument_convention
#' @param tbl (`rtable`)
#' @param col_x (`integer`)\cr column index with estimator. By default tries to get this from
#'   `tbl` attribute `col_x`, otherwise needs to be manually specified.
#' @param col_ci (`integer`)\cr column index with confidence intervals. By default tries
#'   to get this from `tbl` attribute `col_ci`, otherwise needs to be manually specified.
#' @param vline (`number`)\cr
#'   x coordinate for vertical line, if `NULL` then the line is omitted.
#' @param forest_header (`character`, length 2)\cr
#'   text displayed to the left and right of `vline`, respectively.
#'   If `vline = NULL` then `forest_header` needs to be `NULL` too.
#'   By default tries to get this from `tbl` attribute `forest_header`.
#' @param xlim (`numeric`)\cr limits for x axis.
#' @param logx (`flag`)\cr show the x-values on logarithm scale.
#' @param x_at (`numeric)\cr x-tick locations, if `NULL` they get automatically chosen.
#' @param width_row_names (`unit`)\cr
#'  width for row names. If `NULL` the widths get automatically calculated. See [grid::unit()].
#' @param width_columns (`unit`)\cr
#'  widths for the table columns. If `NULL` the widths get automatically calculated. See [grid::unit()].
#' @param width_forest (`unit`)\cr
#'  width for the forest column. If `NULL` the widths get automatically calculated. See [grid::unit()].
#' @param col_symbol_size (`integer`)\cr
#'  column index from `tbl` containing data to be used to determine relative
#'  size for estimator plot symbol. Typically, the symbol size is proportional to the
#'  sample size used to calculate the estimator. If `NULL`, the same symbol
#'  size is used for all subgroups.
#'
#' @import grid
#'
#' @export
#'
#' @examples
#'
#' library(scda)
#' library(dplyr)
#' library(forcats)
#' library(rtables)
#'
#' adrs <- synthetic_cdisc_data("latest")$adrs
#'
#' n_records <- 20
#' adrs_labels <- var_labels(adrs)
#' adrs <- adrs %>%
#'   filter(PARAMCD == "BESRSPI") %>%
#'   filter(ARM %in% c("A: Drug X", "B: Placebo")) %>%
#'   slice(seq_len(n_records)) %>%
#'   droplevels() %>%
#'   mutate(
#'     # Reorder levels of factor to make the placebo group the reference arm.
#'     ARM = fct_relevel(ARM, "B: Placebo"),
#'     rsp = AVALC == "CR"
#'   )
#' var_labels(adrs) <- c(adrs_labels, "Response")
#'
#' df <- extract_rsp_subgroups(
#'   variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "STRATA2")),
#'   data = adrs
#' )
#'
#' # Full commonly used response table.
#' tbl <- basic_table() %>%
#'   tabulate_rsp_subgroups(df)
#'
#' p <- g_forest(tbl)
#'
#' draw_grob(p)
#'
#' # Odds ratio only table.
#' tbl_or <- basic_table() %>%
#'   tabulate_rsp_subgroups(df, vars = c("n_tot", "or", "ci"))
#' tbl_or
#'
#' p <- g_forest(
#'   tbl_or,
#'   forest_header = c("Comparison\nBetter", "Treatment\nBetter")
#' )
#'
#' draw_grob(p)
#'
#' # Works with any rtable
#'
#' tbl <- rtable(
#'   header = c("E", "CI", "N"),
#'   rrow("", 1, c(.8, 1.2), 200),
#'   rrow("", 1.2, c(1.1, 1.4), 50)
#' )
#'
#' g_forest(
#'   tbl = tbl,
#'   col_x = 1,
#'   col_ci = 2,
#'   xlim = c(0.5, 2),
#'   x_at = c(0.5, 1, 2),
#'   col_symbol_size = 3
#' )
#'
#' tbl <- rtable(
#'   header = rheader(
#'     rrow("", rcell("A", colspan = 2)),
#'     rrow("", "c1", "c2")
#'   ),
#'   rrow("row 1", 1, c(.8, 1.2)),
#'   rrow("row 2", 1.2, c(1.1, 1.4))
#' )
#'
#' g_forest(
#'   tbl = tbl,
#'   col_x = 1,
#'   col_ci = 2,
#'   xlim = c(0.5, 2),
#'   x_at = c(0.5, 1, 2),
#'   vline = 1,
#'   forest_header = c("Hello", "World")
#' )
#'
g_forest <- function(tbl,
                     col_x = attr(tbl, "col_x"),
                     col_ci = attr(tbl, "col_ci"),
                     vline = 1,
                     forest_header = attr(tbl, "forest_header"),
                     xlim = c(0.1, 10),
                     logx = TRUE,
                     x_at = c(0.1, 1, 10),
                     width_row_names = NULL,
                     width_columns = NULL,
                     width_forest = unit(1, "null"),
                     col_symbol_size = 1,
                     draw = TRUE,
                     newpage = TRUE) {

  stopifnot(is(tbl, "VTableTree"))

  nr <- nrow(tbl)
  nc <- ncol(tbl)

  assert_that(!is.null(col_x), msg = "Please specify `col_x` manually.")
  assert_that(!is.null(col_ci), msg = "Please specify `col_ci` manually.")
  stopifnot(
    col_x > 0 && col_x <= nc,
    col_ci > 0 && col_ci <= nc,
    is.null(col_symbol_size) ||  col_symbol_size > 0 && col_symbol_size <= nc
  )

  x_e <- vapply(seq_len(nr), function(i) {
    xi <- as.vector(tbl[i, col_x, drop = TRUE])

    if (!is.null(xi) && !(length(xi) <= 0) && is.numeric(xi)) {
      xi
    } else {
      NA_real_
    }
  }, numeric(1))

  x_ci <- lapply(seq_len(nr), function(i) {
    xi <- as.vector(tbl[i, col_ci, drop = TRUE])

    if (!is.null(xi) && !(length(xi) <= 0) && is.numeric(xi)) {
      if (length(xi) != 2) {
        stop("ci column needs two elements")
      }
      xi
    } else {
      c(NA_real_, NA_real_)
    }
  })

  lower <- vapply(x_ci, `[`, numeric(1), 1)
  upper <- vapply(x_ci, `[`, numeric(1), 2)

  symbol_size <- if (!is.null(col_symbol_size)) {
    tmp_symbol_size <- vapply(seq_len(nr), function(i) {
      xi <- as.vector(tbl[i, col_symbol_size, drop = TRUE])

      if (!is.null(xi) && !(length(xi) <= 0) && is.numeric(xi)) {
        xi
      } else {
        NA_real_
      }
    }, numeric(1))

    # Scale symbol size.
    tmp_symbol_size <- sqrt(tmp_symbol_size)
    max_size <- max(tmp_symbol_size, na.rm = TRUE)
    # Biggest points have radius is 2 * (1/3.5) lines not to overlap.
    # See forest_dot_line.
    2 * tmp_symbol_size / max_size

  } else {
    NULL
  }

  grob_forest <- forest_grob(
    tbl,
    x_e,
    lower,
    upper,
    vline,
    forest_header,
    xlim,
    logx,
    x_at,
    width_row_names,
    width_columns,
    width_forest,
    symbol_size = symbol_size,
    vp = plotViewport(margins = rep(1, 4))
  )

  fn <- footnotes(tbl)
  if (!is.null(fn)) {
    footnotes(grob_forest) <- fn
    message("grob footnote is not added to plot; suggest to use decorate_grob() to further decorate the grob")
  }

  if (draw) {
    if (newpage) grid.newpage()
    grid.draw(grob_forest)
  }

  invisible(grob_forest)
}


#' Forest Plot Grob
#'
#' @inheritParams g_forest
#' @param tbl ([rtables::rtable()])
#' @param x (`numeric`)\cr coordinate of point.
#' @param lower,upper (`numeric`)\cr lower/upper bound of the confidence interval.
#' @param symbol_size (`numeric`)\cr vector with relative size for plot symbol.
#' If `NULL`, the same symbol size is used.
#'
#' @details
#' The heights get automatically determined.
#'
#' @noRd
#'
#' @importFrom grDevices extendrange
#' @importFrom grid dataViewport gList gTree gpar rectGrob
#' @importFrom stats na.omit
#'
#' @examples
#' tbl <- rtable(
#'   header = rheader(
#'    rrow("", "E", rcell("CI", colspan = 2), "N"),
#'    rrow("", "A", "B", "C", "D")
#'   ),
#'   rrow("row 1", 1, 0.8, 1.1, 16),
#'   rrow("row 2", 1.4, 0.8, 1.6, 25),
#'   rrow("row 3", 1.2, 0.8, 1.6, 36)
#' )
#'
#' x <- c(1, 1.4, 1.2)
#' lower <- c(0.8, 0.8, 0.8)
#' upper <- c(1.1, 1.6, 1.6)
#' # numeric vector with multiplication factor to scale each circle radius
#' # default radius is 1/3.5 lines
#' symbol_scale <- c(1, 1.25, 1.5)
#'
#' p <- tern:::forest_grob(tbl, x, lower, upper, vline = 1, forest_header = c("A", "B"),
#'   x_at = c(.1 , 1, 10), xlim = c(0.1, 10), logx = TRUE, symbol_size = symbol_scale,
#'   vp = plotViewport(margins = c(1, 1, 1, 1))
#' )
#'
#'  draw_grob(p)
#'
forest_grob <- function(tbl,
                        x,
                        lower,
                        upper,
                        vline,
                        forest_header,
                        xlim = NULL,
                        logx = FALSE,
                        x_at = NULL,
                        width_row_names = NULL,
                        width_columns = NULL,
                        width_forest = unit(1, "null"),
                        symbol_size = NULL,
                        name = NULL,
                        gp = NULL,
                        vp = NULL) {

  stopifnot(
    !is.null(vline) || is.null(forest_header),
    is.null(forest_header) || length(forest_header) == 2,
    is.null(vline) || length(vline) == 1
  )

  nr <- nrow(tbl)
  stopifnot(
    length(x) == nr,
    length(lower) == nr,
    length(upper) == nr,
    is.null(symbol_size) || length(symbol_size) == nr
  )

  if (is.null(symbol_size)) {
    symbol_size <- rep(1, nr)
  }

  if (is.null(xlim)) {
    xlim <- extendrange(c(x, lower, upper))
  }

  if (logx) {
    if (is.null(x_at)) {
      x_at <- pretty(log(na.omit(c(x, lower, upper))))
      x_labels <- exp(x_at)
    } else {
      x_labels <- x_at
      x_at <- log(x_at)
    }
    xlim <- log(xlim)
    x <- log(x)
    lower <- log(lower)
    upper <- log(upper)
    if (!is.null(vline)) {
      vline <- log(vline)
    }
  } else {
    x_labels <- TRUE
  }

  data_forest_vp <- dataViewport(xlim, c(0, 1))

  # Get table content as matrix form.
  mf <- matrix_form(tbl)

  # Use rtables indent_string eventually.
  mf$strings[, 1] <- paste0(
    strrep("    ", c(rep(0, attr(mf, "nrow_header")), mf$row_info$indent)),
    mf$strings[, 1]
  )

  n_header <- attr(mf, "nrow_header")

  if (any(mf$display[, 1] == FALSE)) stop("row names need to be always displayed")

  # Preprocess the data to be used in lapply and cell_in_rows.
  to_args_for_cell_in_rows_fun <- function(part = c("body", "header"),
                                           underline_colspan = FALSE) {
    part <- match.arg(part)
    if (part == "body") {
      mat_row_indices <- seq_len(nrow(tbl)) + n_header
      row_ind_offset <- -n_header
    } else {
      mat_row_indices <- seq_len(n_header)
      row_ind_offset <- 0
    }

    lapply(mat_row_indices, function(i) {
      disp <- mf$display[i, -1]
      list(
        row_name = mf$strings[i, 1],
        cells = mf$strings[i, -1][disp],
        cell_spans = mf$spans[i, -1][disp],
        row_index = i + row_ind_offset,
        underline_colspan = underline_colspan
      )
    })
  }

  args_header <- to_args_for_cell_in_rows_fun("header", underline_colspan = TRUE)
  args_body <- to_args_for_cell_in_rows_fun("body", underline_colspan = FALSE)

  gTree(
    name = name,
    children = gList(
      gTree(
        children = do.call(gList, lapply(args_header, do.call, what = cell_in_rows)),
        vp = vpPath("vp_table_layout", "vp_header")
      ),
      gTree(
        children = do.call(gList, lapply(args_body, do.call, what = cell_in_rows)),
        vp = vpPath("vp_table_layout", "vp_body")
      ),
      linesGrob(unit(c(0, 1), "npc"), y = unit(c(.5, .5), "npc"), vp = vpPath("vp_table_layout", "vp_spacer")),
      # forest part
      if (is.null(vline)) {
        NULL
      } else {
        gTree(
          children = gList(
            gTree(
              children = gList(
                # this may overflow, to fix, look here
                # https://stackoverflow.com/questions/33623169/add-multi-line-footnote-to-tablegrob-while-using-gridextra-in-r #nolintr
                textGrob(
                  forest_header[1],
                  x = unit(vline, "native") - unit(1, "lines"),
                  just = c("right", "center")
                ),
                textGrob(
                  forest_header[2],
                  x = unit(vline, "native") + unit(1, "lines"),
                  just = c("left", "center")
                )
              ),
              vp = vpStack(viewport(layout.pos.col = ncol(tbl) + 2), data_forest_vp)
            )
          ),
          vp = vpPath("vp_table_layout", "vp_header")
        )
      },
      gTree(
        children = gList(
          gTree(
            children = gList(
              rectGrob(gp = gpar(col = "gray90", fill = "gray90")),
              if (is.null(vline)) {
                NULL
              } else {
                linesGrob(
                  x = unit(rep(vline, 2), "native"),
                  y = unit(c(0, 1), "npc"),
                  gp = gpar(lwd = 2),
                  vp = data_forest_vp
                )
              },
              xaxisGrob(at = x_at,  label = x_labels, vp = data_forest_vp)
            ),
            vp = viewport(layout.pos.col = ncol(tbl) + 2)
          )
        ),
        vp = vpPath("vp_table_layout", "vp_body")
      ),
      gTree(
        children = do.call(
          "gList",
          Map(
            function(xi, li, ui, row_index, size_i) {
              forest_dot_line(
                xi,
                li,
                ui,
                row_index,
                xlim,
                symbol_size = size_i,
                datavp = data_forest_vp
              )
            },
            x,
            lower,
            upper,
            seq_along(x),
            symbol_size, USE.NAMES = FALSE
          )
        ),
        vp = vpPath("vp_table_layout", "vp_body")
      )
    ),
    childrenvp = forest_viewport(tbl, width_row_names, width_columns, width_forest),
    vp = vp,
    gp = gp
  )

}


#' @importFrom grid gList gTree textGrob unit unit.c
cell_in_rows <- function(row_name,
                         cells,
                         cell_spans,
                         row_index,
                         underline_colspan = FALSE) {

  stopifnot(
    is_character_single(row_name),
    is_character_vector(cells),
    is_numeric_vector(cell_spans),
    length(cells) == length(cell_spans),
    is_numeric_single(row_index),
    is_logical_single(underline_colspan)
  )

  vp_name_rn <- paste0("rowname-", row_index)
  g_rowname <- if (!is.null(row_name) && row_name != "") {
    textGrob(
      name = vp_name_rn,
      label = row_name,
      x = unit(0, "npc"),
      just = c("left", "center"),
      vp = vpPath(paste0("rowname-", row_index))
    )
  } else {
    NULL
  }

  gl_cols <- if (!(length(cells) > 0)) {
    list(NULL)
  } else {
    j <- 1 # column index of cell

    lapply(seq_along(cells), function(k) {

      cell_ascii <- cells[[k]]
      cs <- cell_spans[[k]]

      if (is.na(cell_ascii) || is.null(cell_ascii)) {
        cell_ascii <- "NA"
      }

      cell_name <- paste0("g-cell-", row_index, "-", j)

      cell_grobs <- if (identical(cell_ascii, "")) {
        NULL
      } else {
        if (cs == 1) {
          textGrob(
            label = cell_ascii,
            name = cell_name,
            vp = vpPath(paste0("cell-", row_index, "-", j))
          )
        } else {
          # +1 because of rowname
          vp_joined_cols <- viewport(layout.pos.row = row_index, layout.pos.col = seq(j + 1, j + cs))

          lab <- textGrob(
            label = cell_ascii,
            name = cell_name,
            vp = vp_joined_cols
          )

          if (!underline_colspan || grepl("^[[:space:]]*$", cell_ascii)) {
            lab
          } else {
            gList(
              lab,
              linesGrob(
                x = unit.c(unit(.2, "lines"), unit(1, "npc") - unit(.2, "lines")),
                y = unit(c(0, 0), "npc"),
                vp = vp_joined_cols
              )
            )
          }
        }
      }
      j <<- j + cs

      cell_grobs
    })
  }

  gList(
    g_rowname,
    do.call(gList, gl_cols)
  )
}

#' Graphic Object: Forest Dot Line
#'
#' Calculate the `grob` corresponding to the dot line within the forest plot.
#' @noRd
#'
#' @importFrom grid gList gTree unit
forest_dot_line <- function(x,
                            lower,
                            upper,
                            row_index,
                            xlim,
                            symbol_size = 1,
                            datavp) {
  ci <- c(lower, upper)
  if (any(!is.na(c(x, ci)))) {
    # line
    y <- unit(c(0.5, 0.5), "npc")

    g_line <- if (all(!is.na(ci)) && ci[2] > xlim[1] && ci[1] < xlim[2]) {
      # -
      if (ci[1] >= xlim[1] && ci[2] <= xlim[2]) {
        linesGrob(x = unit(c(ci[1], ci[2]), "native"), y = y)
      } else if (ci[1] < xlim[1] && ci[2] > xlim[2]) {
        # <->
        linesGrob(
          x = unit(xlim, "native"),
          y = y,
          arrow = arrow(angle = 30, length = unit(0.5, "lines"), ends = "both")
        )
      } else if (ci[1] < xlim[1] && ci[2] <= xlim[2]) {
        # <-
        linesGrob(x = unit(c(xlim[1], ci[2]), "native"), y = y,
                  arrow = arrow(angle = 30, length = unit(0.5, "lines"), ends = "first"))
      } else if (ci[1] >= xlim[1] && ci[2] > xlim[2]) {
        # ->
        linesGrob(x = unit(c(ci[1], xlim[2]), "native"), y = y,
                  arrow = arrow(angle = 30, length = unit(0.5, "lines"), ends = "last"))
      }
    } else {
      NULL
    }

    g_circle <- if (!is.na(x) && x >= xlim[1] && x <= xlim[2]) {
      circleGrob(
        x = unit(x, "native"),
        y = y,
        r = unit(1 / 3.5 * symbol_size, "lines"),
        name = "point"
      )
    } else {
      NULL
    }

    gTree(
      children = gList(
        gTree(
          children = gList(
            gList(
              g_line,
              g_circle
            )
          ),
          vp = datavp,
          gp = gpar(col = "blue", fill = "blue")
        )
      ),
      vp = vpPath(paste0("forest-", row_index))
    )
  } else {
    NULL
  }
}

#' Create a Viewport Tree for the Forest Plot
#'
#' @noRd
#'
#' @importFrom grid grid.layout stringWidth unit unit.c
#' @importFrom rtables matrix_form
#'
#' @examples
#' library(grid)
#'
#' tbl <- rtable(
#'   header = rheader(
#'    rrow("", "E", rcell("CI", colspan = 2)),
#'    rrow("", "A", "B", "C")
#'   ),
#'   rrow("row 1", 1, 0.8, 1.1),
#'   rrow("row 2", 1.4, 0.8, 1.6),
#'   rrow("row 3", 1.2, 0.8, 1.2)
#' )
#'
#' v <- tern:::forest_viewport(tbl)
#'
#' grid.newpage()
#' showViewport(v)
#'
forest_viewport <- function(tbl,
                            width_row_names = NULL,
                            width_columns = NULL,
                            width_forest = unit(1, "null"),
                            gap_column = unit(1, "lines"),
                            gap_header = unit(1, "lines"),
                            mat_form = NULL) {

  stopifnot(
    is(tbl, "VTableTree"),
    is.null(width_row_names) || is.unit(width_row_names),
    is.null(width_columns) || is.unit(width_columns),
    is.unit(width_forest)
  )

  if (is.null(mat_form)) mat_form <- matrix_form(tbl)

  mat_form$strings[!mat_form$display] <- ""

  nr <- nrow(tbl)
  nc <- ncol(tbl)
  nr_h <- attr(mat_form, "nrow_header")

  if (is.null(width_row_names) || is.null(width_columns)) {
    tbl_widths <- propose_column_widths(tbl, mat_form)
    strs_with_width <- strrep("x", tbl_widths) # that works for mono spaced fonts
    if (is.null(width_row_names)) width_row_names <- stringWidth(strs_with_width[1])
    if (is.null(width_columns)) width_columns <- stringWidth(strs_with_width[-1])
  }

  # Widths for row name, cols, forest.
  widths <- unit.c(
    width_row_names + gap_column,
    width_columns + gap_column,
    width_forest
  )

  n_lines_per_row <- apply(
    X = mat_form$strings,
    MARGIN = 1,
    FUN = function(row) {
      tmp <- vapply(
        gregexpr("\n", row, fixed = TRUE),
        attr, numeric(1),
        "match.length"
      ) + 1
      max(c(tmp, 1))
    }
  )

  i_header <- seq_len(nr_h)

  height_body_rows <- unit(n_lines_per_row[-i_header] * 1.2, "lines")
  height_header_rows <- unit(n_lines_per_row[i_header] * 1.2, "lines")

  height_body <-  unit(sum(n_lines_per_row[-i_header]) * 1.2, "lines")
  height_header <- unit(sum(n_lines_per_row[i_header]) * 1.2, "lines")

  nc_g <- nc + 2 # number of columns incl. row names and forest

  vp_tbl <- vpTree(
    parent = viewport(
      name = "vp_table_layout",
      layout = grid.layout(
        nrow = 3, ncol = 1,
        heights = unit.c(height_header, gap_header, height_body)
      )
    ),
    children = vpList(
      vp_forest_table_part(nr_h, nc_g, 1, 1, widths, height_header_rows, "vp_header"),
      vp_forest_table_part(nr, nc_g, 3, 1, widths, height_body_rows, "vp_body"),
      viewport(name = "vp_spacer", layout.pos.row = 2, layout.pos.col = 1)
    )
  )
  vp_tbl
}

#' Viewport Forest Plot: Table Part
#'
#' Prepares a viewport for the table included in the forest plot.
#'
#' @noRd
#'
#' @importFrom grid grid.layout viewport vpList vpTree
#'
vp_forest_table_part <- function(nrow,
                                 ncol,
                                 l_row,
                                 l_col,
                                 widths,
                                 heights,
                                 name) {
  vpTree(
    viewport(
      name = name,
      layout.pos.row = l_row,
      layout.pos.col = l_col,
      layout = grid.layout(nrow = nrow, ncol = ncol, widths = widths, heights = heights)
    ),
    children = vpList(
      do.call(
        vpList,
        lapply(
          seq_len(nrow), function(i) {
            viewport(layout.pos.row = i, layout.pos.col = 1, name = paste0("rowname-", i))
          }
        )
      ),
      do.call(
        vpList,
        apply(
          expand.grid(seq_len(nrow), seq_len(ncol - 2)),
          1,
          function(x) {
            i <- x[1]
            j <- x[2]
            viewport(layout.pos.row = i, layout.pos.col = j + 1, name = paste0("cell-", i, "-", j))
          }
        )
      ),
      do.call(
        vpList,
        lapply(
          seq_len(nrow),
          function(i) {
            viewport(layout.pos.row = i, layout.pos.col = ncol, name = paste0("forest-", i))
          }
        )
      )
    )
  )
}

#' Forest Rendering
#'
#' Renders the forest grob.
#'
#' @noRd
#'
#' @importFrom grid grid.draw
#'
grid.forest <- function(...) {# nousage # nolint
  grid.draw(forest_grob(...)) # nolint
}

## To be deprecated ----

#' Assign value to attribute footnote of object x
#'
#' @param x an object
#' @param value character vector
#'
#' @export
#'
#' @examples
#' x <- table(iris$Species)
#' footnotes(x) <- "Species are equally distributed"
#' attributes(x)
`footnotes<-` <- function(x, value = NULL) { # nolint
  attr(x, "footnote") <- value
  x
}


#' Retrieve value from attribute footnote of object x
#'
#' @param x an object
#'
#' @export
#'
#' @examples
#' x <- table(iris$Species)
#' footnotes(x) <- "Species are equally distributed"
#' footnotes(x)
#'
footnotes <- function(x) {
  attr(x, "footnote")
}

#' Add more footnotes
#'
#' @param x an object
#' @param value character vector
#'
#' @export
#'
#' @examples
#' x <- table(iris$Species)
#' footnotes(x) <- "Species are equally distributed"
#' footnotes(x)
#' add_footnotes(x) <- "Add more footnotes"
#' footnotes(x)

`add_footnotes<-` <- function(x, value) { # nolint
  footnotes(x) <- c(footnotes(x), value)
  x
}
