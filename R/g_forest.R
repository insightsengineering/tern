#' Create a Forest Plot based on a Table
#'
#' Create a forest plot from any \code{\link[rtables]{rtable}} object that has a
#' column with a single value and a column with 2 values
#'
#' @param tbl (\code{rtable} object)
#' @param col_x (\code{integer} value)\cr
#'   column index with estimator
#' @param col_ci (\code{integer} value)\cr
#'   column index with confidence intervals
#' @param vline (\code{numeric} value)\cr
#'   x coordinate for vertical line, if \code{NULL} then the line is
#'   omitted
#' @param forest_header (\code{character(2)})\cr
#'   text displayed to the left and right of \code{vline}, respectively.
#'   If \code{vline = NULL} then \code{forest_header} needs to be \code{NULL} too
#' @param xlim (\code{numeric(2)})\cr
#'   x limits for x-scales
#' @param logx (\code{logical} value)\cr
#'   whether to show x-values on logarithm scale
#' @param x_at (\code{numeric} vector)\cr
#'   Contains x tick locations, if \code{NULL} they get automatically chosen
#' @param width_row_names (\code{\link[grid]{unit}} object)\cr
#'   Contains width for row names. If \code{NULL} the widths get automatically calculated.
#' @param width_columns (\code{\link[grid]{unit}} object)\cr
#'   Contains widths for the table columns. If \code{NULL} the widths get automatically calculated.
#' @param width_forest (\code{\link[grid]{unit}} object)\cr
#'   Contains width for the forest column. If \code{NULL} the widths get automatically calculated.
#' @param draw (\code{logical} value)\cr
#'   Whether plot should be drawn
#' @param newpage (\code{logical} value)\cr
#'  if \code{draw=TRUE} should plot be drawn on a new page
#' @param col_symbol_size (\code{integer} value)\cr
#'  Column index from \code{tbl} containing data to be used to determine relative
#'  size for estimator plot symbol. Typically, symbol size is proportional to the
#'  sample size used to calculate the estimator. If \code{NULL}, the same symbol
#'  size is used for all subgroups.
#'
#' @template author_waddella
#'
#' @import grid
#'
#' @export
#'
#' @seealso \code{\link{t_forest_tte}}, \code{\link{t_forest_rsp}}
#'
#' @examples
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADTTE <- radtte(ADSL, cached = TRUE)
#' ADTTE_f <- ADTTE %>%
#'   dplyr::filter(PARAMCD == "OS" & ARMCD %in% c("ARM B", "ARM A")) %>%
#'   mutate(ARMCD = droplevels(ARMCD))
#' var_labels(ADTTE_f) <- var_labels(ADTTE)
#'
#' tbl <- t_forest_tte(
#'   tte = ADTTE_f$AVAL,
#'   is_event = ADTTE_f$CNSR == 0,
#'   col_by = ADTTE_f$ARMCD,
#'   row_by_list = ADTTE_f[, c("SEX", "RACE")], # note factors required
#'   ties = "exact",
#'   dense_header = TRUE
#' )
#'
#' # note plot requires a certain width
#' p <- g_forest(
#'   tbl = tbl,
#'   col_x = 8,
#'   col_ci = 9,
#'   vline = 1,
#'   forest_header = c("Treatement\nBetter", "Comparison\nBetter"),
#'   xlim = c(.1, 10),
#'   logx = TRUE,
#'   x_at = c(.1, 1, 10),
#'   col_symbol_size = 1, #draw symbol proportional to column 1 values
#'   draw = FALSE
#' )
#' grid.newpage()
#' grid.draw(p)
#'
#'
#' # For response table
#'
#' ADSL <- radsl(cached = TRUE)
#' ADRS <- radrs(ADSL, cached = TRUE)
#'
#' ADRS_f <- ADRS %>%
#'   dplyr::filter(PARAMCD == "OVRINV" & ARMCD %in% c("ARM A","ARM B")) %>%
#'   mutate(ARMCD = droplevels(ARMCD))
#' var_labels(ADRS_f) <- var_labels(ADRS)
#'
#' \dontrun{
#' tbl <- t_forest_rsp(
#'   rsp = ADRS_f$AVALC %in% c("CR", "PR"),
#'   col_by = factor(ADRS_f$ARM),
#'   row_by_list = ADRS_f[, c("SEX", "RACE", "STRATA2", "BMRKR2")],
#'   strata_data = ADRS_f[ , c("STRATA1")]
#' )
#'
#' tbl
#'
#' p <- g_forest(
#'   tbl = tbl,
#'   col_x = 8,
#'   col_ci = 9,
#'   vline = 1,
#'   forest_header = c("Comparison\nBetter", "Treatement\nBetter"),
#'   xlim = c(.1, 10),
#'   logx = TRUE,
#'   x_at = c(.1, 1, 10),
#'   col_symbol_size = NULL,
#'   draw = FALSE
#' )
#' p <- decorate_grob(p, titles =  "forest plot", footnotes = footnotes(p))
#' grid.newpage()
#' grid.draw(p)
#'
#'
#' tbl2 <- t_forest_rsp(
#'   rsp = ADRS_f$AVALC %in% c("CR", "PR"),
#'   col_by = factor(ADRS_f$ARM),
#'   row_by_list = ADRS_f[, c("BMRKR2")],
#'   strata_data = ADRS_f[ , "STRATA1"]
#' )
#'
#' tbl2
#'
#' # stratified analysis noted in footnote
#' p <- g_forest(
#'   tbl = tbl2,
#'   col_x = 8,
#'   col_ci = 9,
#'   vline = 1,
#'   forest_header = c("Comparison\nBetter", "Treatement\nBetter"),
#'   xlim = c(.1, 10),
#'   logx = TRUE,
#'   x_at = c(.1, 1, 10),
#'   col_symbol_size = 1,
#'   draw = FALSE
#' )
#' p <- decorate_grob(p, titles =  "forest plot", footnotes = footnotes(p))
#' grid.newpage()
#' grid.draw(p)
#' }
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
#'   vline = 1,
#'   forest_header = c("Hello", "World")
#' )
g_forest <- function(tbl,
                     col_x,
                     col_ci,
                     vline = NULL,
                     forest_header = NULL,
                     xlim = NULL,
                     logx = FALSE,
                     x_at = NULL,
                     width_row_names = NULL,
                     width_columns = NULL,
                     width_forest = unit(1, "null"),
                     col_symbol_size = NULL,
                     draw = TRUE,
                     newpage = TRUE) {


  stopifnot(is(tbl, "rtable"))

  nr <- nrow(tbl)
  nc <- ncol(tbl)

  stopifnot(
    col_x > 0 && col_x <= nc,
    col_ci > 0 && col_ci <= nc,
    is.null(col_symbol_size) ||  col_symbol_size > 0 && col_symbol_size <= nc
  )

  x_e <- vapply(seq_len(nr), function(i) {
    xi <- as.vector(tbl[i, col_x])

    if (!is.null(xi) && !(length(xi) <= 0) && is.numeric(xi)) {
      xi
    } else {
      NA_real_
    }
  }, numeric(1))

  x_ci <- lapply(seq_len(nr), function(i) {
    xi <- as.vector(tbl[i, col_ci])

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
      xi <- as.vector(tbl[i, col_symbol_size])

      if (!is.null(xi) && !(length(xi) <= 0) && is.numeric(xi)) {
        xi
      } else {
        NA_real_
      }
    }, numeric(1))

    # scale symbol size
    tmp_symbol_size <- sqrt(tmp_symbol_size)
    max_size <- max(tmp_symbol_size, na.rm = TRUE)
    # Biggest points have radius is 2 * (1/3.5) lines not to overlap
    # see forest_dot_line
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
    if (newpage) {
      grid.newpage()
    }
    grid.draw(grob_forest)
  }

  invisible(grob_forest)
}

#' forest plot grob
#'
#' @inheritParams g_forest
#' @param tbl an \code{\link[rtables]{rtable}} object
#' @param x coordinate of point
#' @param lower lower bound of ci
#' @param upper upper bound of ci
#' @param symbol_size vector with relative size for plot symbol.
#' If \code{NULL}, the same symbol size is used.
#'
#' @details
#' The heights get automatically determined
#'
#' @noRd
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
#' tern:::forest_grob(tbl, x, lower, upper, vline = 1, forest_header = c("A", "B"),
#'   x_at = c(.1 , 1, 10), xlim = c(0.1, 10), logx = TRUE, symbol_size = symbol_scale,
#'   vp = plotViewport(margins = c(1, 1, 1, 1))
#' )
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

  g_forest <- gTree(
    name = name,
    children = gList(
      gTree(
        children = do.call("gList", Map(function(row, i) {
          cell_in_rows(row, i, TRUE)
        }, header(tbl), seq_len(nrow(header(tbl))), USE.NAMES = FALSE)),
        vp = vpPath("vp_table_layout", "vp_header")
      ),
      gTree(
        children = do.call("gList", Map(cell_in_rows, tbl, seq_len(nrow(tbl)), USE.NAMES = FALSE)),
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
                textGrob(forest_header[1], x = unit(vline, "native") - unit(1, "lines"), just = c("right", "center")),
                textGrob(forest_header[2], x = unit(vline, "native") + unit(1, "lines"), just = c("left", "center"))
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
        children = do.call("gList", Map(function(xi, li, ui, row_index, size_i) {
          forest_dot_line(xi, li, ui, row_index, xlim, symbol_size = size_i, datavp = data_forest_vp)
        }, x, lower, upper, seq_along(x), symbol_size, USE.NAMES = FALSE)),
        vp = vpPath("vp_table_layout", "vp_body")
      )
    ),
    childrenvp = forest_viewport(tbl, width_row_names, width_columns, width_forest),
    vp = vp,
    gp = gp
  )

  g_forest
}

cell_in_rows <- function(row, row_index, underline_colspan = FALSE) {
  stopifnot(is(row, "rrow"))

  row_name <- attr(row, "row.name")

  g_rowname <- if (!is.null(row_name) && row_name != "") {

    indent <- attr(row, "indent")
    if (is.null(indent)) {
      indent <- 0
    }

    if (indent > 0) {
      row_name <- gsub("\n", paste0("\n", strrep(" ", 2 * (indent + 1) + 1)), row_name, fixed = TRUE)
    }

    vp_name_rn <- paste0("rowname-", row_index)
    textGrob(
      name = vp_name_rn,
      label = paste0(strrep(" ", 2 * indent), row_name),
      x = unit(0, "npc"),
      just = c("left", "center"),
      vp = vpPath(vp_name_rn)
    )
  } else {
    NULL
  }

  gl_cols <- if (!(length(row) > 0)) {
    list(NULL)
  } else {
    j <- 1 # column index of cell

    lapply(seq_along(row), function(k) {

      cell <- row[[k]]
      cs <- attr(cell, "colspan")
      if (is.null(cs)) {
        cs <- 1
      }

      cell_ascii <- format_rcell(cell, output = "ascii")

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

          if (!underline_colspan) {
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

forest_dot_line <- function(x, lower, upper, row_index, xlim, symbol_size = 1, datavp) {

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
        linesGrob(x = unit(xlim, "native"), y = y,
                  arrow = arrow(angle = 30, length = unit(0.5, "lines"), ends = "both"))
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
          children = gList(gList(
            g_line,
            g_circle
          )),
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

#' Create A viewport tree for the forest plot
#'
#' @noRd
#'
#' @importFrom grDevices extendrange
#'
#' @examples
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
forest_viewport <- function(tbl,
                            width_row_names = NULL,
                            width_columns = NULL,
                            width_forest = unit(1, "null"),
                            gap_column = unit(1, "lines"),
                            gap_header = unit(1, "lines")) {

  stopifnot(
    is(tbl, "rtable"),
    is.null(width_row_names) || is.unit(width_row_names),
    is.null(width_columns) || is.unit(width_columns),
    is.unit(width_forest)
  )

  nr <- nrow(tbl)
  nc <- ncol(tbl)

  tbl_header <- header(tbl)
  nr_h <- nrow(tbl_header)

  # widths for row name, cols, forest

  if (is.null(width_row_names)) {

    all_row_names <- c(indented_row.names(tbl, 2), indented_row.names(tbl_header, 2))

    longest_row_name <- all_row_names[which.max(vapply(all_row_names, nchar, numeric(1)))]

    width_row_names <- stringWidth(longest_row_name)
  }

  if (!is.null(width_columns)) {
    if (length(width_columns) == 1) {
      width_columns <- unit.rep(width_columns, nc)
    } else if (length(width_columns) != nc) {
      stop("length of width_columns must be either 1 or the number of columns of tbl")
    }
  } else {
    width_columns <- do.call(unit.c, lapply(seq_len(nc), function(j) {

      width_body <- do.call(unit.pmax, lapply(seq_len(nr), function(i) {
        stringWidth(format_rcell(tbl[i, j], output = "ascii"))
      }))

      width_header <- do.call(unit.pmax, lapply(seq_len(nr_h), function(i) {

        # for now we avoide the multicolumn cell to get the column width
        cell <- tbl_header[i, j]
        cs <- attr(cell, "colspan")
        if (is.null(cs)) {
          cs <- 1
        }
        if (cs == 1) {
          stringWidth(format_rcell(cell, output = "ascii"))
        } else {
          unit(1, "lines")
        }

      }))

      unit.pmax(width_body, width_header)
    }))
  }

  widths <- unit.c(
    width_row_names + gap_column,
    width_columns + gap_column,
    width_forest
  )

  get_num_lines_per_row <- function(x) {
    vapply(lapply(x, function(row) {

      cell_text <- c(attr(row, "row.name"), unlist(Filter(Negate(is.null), lapply(row, function(cell) {
        format_rcell(cell, output = "ascii")
      }))))

      if (is.null(cell_text)) {
        1
      } else {
        vapply(strsplit(cell_text, "\n"), length, numeric(1))
      }

    }), max, numeric(1))
  }

  num_lines_body <- get_num_lines_per_row(tbl)
  num_lines_header <- get_num_lines_per_row(header(tbl))

  height_body_rows <- unit(num_lines_body * 1.2, "lines")
  height_header_rows <- unit(num_lines_header * 1.2, "lines")

  height_body <-  unit(sum(num_lines_body * 1.2), "lines")
  height_header <- unit(sum(num_lines_header * 1.2), "lines")

  nc_g <- nc + 2 # number of columns incl. row names and forest

  vp_tbl <- vpTree(
    parent = viewport(
      name = "vp_table_layout",
      layout = grid.layout(
        nrow = 3, ncol = 1,
        heights = unit.c(height_header, gap_column, height_body))
    ),
    children = vpList(
      vp_forest_table_part(nr_h, nc_g, 1, 1, widths, height_header_rows, "vp_header"),
      vp_forest_table_part(nr, nc_g, 3, 1, widths, height_body_rows, "vp_body"),
      viewport(name = "vp_spacer", layout.pos.row = 2, layout.pos.col = 1)
    )
  )
  vp_tbl
}

vp_forest_table_part <- function(nrow, ncol, l_row, l_col, widths, heights, name) {
  vpTree(
    viewport(
      name = name,
      layout.pos.row = l_row,
      layout.pos.col = l_col,
      layout = grid.layout(nrow = nrow, ncol = ncol, widths = widths, heights = heights)
    ),
    children = vpList(
      do.call(vpList, lapply(seq_len(nrow), function(i) {
        viewport(layout.pos.row = i, layout.pos.col = 1, name = paste0("rowname-", i))
      })),
      do.call(vpList, apply(expand.grid(seq_len(nrow), seq_len(ncol - 2)), 1, function(x) {
        i <- x[1]; j <- x[2]
        viewport(layout.pos.row = i, layout.pos.col = j + 1, name = paste0("cell-", i, "-", j))
      })),
      do.call(vpList, lapply(seq_len(nrow), function(i) {
        viewport(layout.pos.row = i, layout.pos.col = ncol, name = paste0("forest-", i))
      }))
    )
  )
}

grid.forest <- function(...) { # nolint # nousage
  grid.draw(forest_grob(...))
}


#' Assign value to attribute footnote of object x
#' @param x an object
#' @param value character vector
#' @export
#' @examples
#' x <- table(iris$Species)
#' footnotes(x) <- "Species are equally distributed"
#' attributes(x)

`footnotes<-` <- function(x, value = NULL) { # nolint
  attr(x, "footnote") <- value
  x
}


#' Retrieve value from attribute footnote of object x
#' @param x an object
#' @export
#' @examples
#' x <- table(iris$Species)
#' footnotes(x) <- "Species are equally distributed"
#' footnotes(x)
#'
footnotes <- function(x) {
  attr(x, "footnote")
}

#' Add more footnotes
#' @param x an object
#' @param value character vector
#' @export
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
