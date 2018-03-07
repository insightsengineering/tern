#' Forest plot 
#' 
#' Create a forest plot from any \code{\link[rtables]{rtable}} object that has a
#' column with a single value and a column with 2 values
#'
#' @param tbl an rtable object
#' @param col_x column index with estimator 
#' @param col_ci column index with confidence intervals
#' @param vline x coordinate for vertical line, if \code{NULL} then the line is
#'   omitted
#' @param forest_header character vector of length 2, diplayed to the left and
#'   right of \code{vline}, respectively. If \code{vline=NULL} then
#'   \code{forest_header} needs to be \code{NULL} too
#' @param xlim x limits for x-scales
#' @param logx boolean for showing x-values on logrithm scale
#' @param x_at numeric vector with x tick locations, if \code{NULL} they get
#'   automatically chosen
#' @param width_row.names \code{\link[grid]{unit}} object with width for row
#'   names. If \code{NULL} the widths get automatically calculated.
#' @param width_columns \code{\link[grid]{unit}} object with widths for the
#'   table columns. If \code{NULL} the widths get automatically calculated.
#' @param width_forest \code{\link[grid]{unit}} object with width for the forest
#'   column. If \code{NULL} the widths get automatically calculated.
#' @param draw boolean, should plot be drawn
#' @param newpage boolean if \code{draw=TRUE} should plot be drawn on a new page
#' 
#' @template author_waddella
#' 
#' @import grid
#' 
#' @export
#' 
#' @examples 
#' 
#' library(random.cdisc.data)
#' 
#' ASL <- radam("ASL")
#' ASL$RACE <- factor(sapply(as.character(ASL$RACE), function(x) if (nchar(x)>9) paste0(substr(x, 1,9), "...") else x))
#' ATE <- radam("ATE", ADSL = ASL)
#' 
#' ATE_f <- subset(ATE, PARAMCD == "OS") 
#' 
#' ANL <- merge(ASL, ATE_f)
#' 
#' tbl <- t_forest_tte(
#'   tte = ANL$AVAL,
#'   is_event = ANL$CNSR == 0,
#'   col_by = factor(ANL$ARM), 
#'   group_data = as.data.frame(lapply(ANL[, c("SEX", "RACE")], as.factor))
#' )
#' 
#' ## note plot requires a certain width
#' g_forest(
#'   tbl = tbl,
#'   col_x = 8,
#'   col_ci = 9,
#'   vline = 1,
#'   forest_header = c("Treatement\nBetter", "Comparison\nBetter"),
#'   xlim = c(.1, 10),
#'   logx = TRUE,
#'   x_at = c(.1, 1, 10)
#' )
#' 
#' # For response table
#' 
#' ASL <- radam("ASL")
#' ARS <- radam("ARS", ADSL = ASL)
#' 
#' ARS_f <- subset(ARS, PARAMCD == "OVRSPI")
#' ANL <- merge(ASL, ARS_f)
#' 
#' tbl <- t_forest_rsp(
#'   rsp = ANL$AVALC %in% c("CR", "PR"),
#'   col_by = ANL$ARM, 
#'   group_data = ANL[, c("SEX", "RACE")]
#' )
#' 
#' tbl
#' 
#' g_forest(
#'   tbl = tbl,
#'   col_x = 8,
#'   col_ci = 9,
#'   vline = 1,
#'   forest_header = c("Comparison\nBetter", "Treatement\nBetter"),
#'   xlim = c(.1, 10),
#'   logx = TRUE,
#'   x_at = c(.1, 1, 10)
#' )
#' 
#' 
#' 
#' # Works with any rtable
#' 
#' tbl <- rtable(
#'   header = c("E", "CI"),
#'   rrow("", 1, c(.8, 1.2)),
#'   rrow("", 1.2, c(1.1, 1.4))
#' )
#' 
#' g_forest(
#'   tbl = tbl,
#'   col_x = 1,
#'   col_ci = 2
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
#' 
#' 
#' 
g_forest <- function(tbl, col_x, col_ci, vline = NULL, forest_header = NULL,
                     xlim = NULL, logx = FALSE, x_at = NULL,
                     width_row.names = NULL,
                     width_columns = NULL,
                     width_forest = unit(1, "null"),
                     draw = TRUE, newpage = TRUE) {
  
  
  if (!is(tbl, "rtable")) stop("tbl needs to be of class rtable")
  
  nr <- nrow(tbl)
  nc <- ncol(tbl)
  
  if (!(col_x > 0 && col_x <= nc)) stop("i_col_est out of bounds")
  if (!(col_ci > 0 && col_ci <= nc)) stop("i_col_ci out of bounds")
  
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
      if (length(xi) != 2) stop("ci column needs two elements")
      xi
    } else {
      c(NA_real_, NA_real_)
    }
  })
  
  lower <- vapply(x_ci, `[`, numeric(1), 1)
  upper <- vapply(x_ci, `[`, numeric(1), 2)
  
  grobForest <- forestGrob(tbl, x_e, lower, upper, vline, forest_header, xlim, logx, x_at,
                           width_row.names, width_columns, width_forest,
                           vp = plotViewport(margins = rep(1, 4))
                           )
 
  if (draw) {
    if (newpage) grid.newpage()
    grid.draw(grobForest)
  }
  
  invisible(grobForest)
}


#' forest plot grob
#' 
#' 
#' @inheritParams g_forest
#' @param tbl an \code{\link[rtables]{rtable}} object
#' @param x coordinate of point
#' @param lower lower bound of ci
#' @param upper upper bound of ci
#'   
#' 
#' @details 
#' The heights get automatically determined
#' 
#' 
#' @noRd
#' 
#' @examples 
#' \dontrun{
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
#' x <- c(.4, .9, 7)
#' lower <- x - .2
#' upper <- x + 2
#' 
#' tern:::forestGrob(tbl, x, lower, upper, vline = 1, forest_header = c("A", "B"),
#'   x_at = c( .1 , 1, 10), xlim = c(0.1, 10), logx = TRUE,
#'   vp = plotViewport(margins = c(1, 1, 1, 1))
#' )
#' }
forestGrob <- function(tbl, x, lower, upper, vline, forest_header,
                       xlim = NULL, logx = FALSE, x_at = NULL,
                       width_row.names = NULL,
                       width_columns = NULL,
                       width_forest = unit(1, "null"), 
                       name=NULL, gp=NULL, vp=NULL) {

  
  if (is.null(vline) && !is.null(forest_header)) stop("if vline is null then forest_header needs to be NULL")
  if (!is.null(forest_header) && length(forest_header) != 2) stop("length of forest_header needs to be 2")
  if (!is.null(vline) && length(vline) != 1) stop("length of vline needs to be 1")
  
  nr <- nrow(tbl)
  if (length(x) != nr) stop("dimension missmatch x")
  if (length(lower) != nr) stop("dimension missmatch lower")
  if (length(upper) != nr) stop("dimension missmatch upper")

  if (is.null(xlim)) xlim <- extendrange(c(x, lower, upper))
  
  
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
     
    if (!is.null(vline)) vline <- log(vline)
    
  } else {
    x_labels <- TRUE
  }
  
  dataForestVp <- dataViewport(xlim, c(0,1))
  
  g_forest <- gTree(
    name = name,
    children = gList(
      gTree(
        children = do.call('gList', Map(function(row, i) cell_in_rows(row, i, TRUE), header(tbl), 1:nrow(header(tbl)))),
        vp = vpPath("vp_table_layout", "vp_header")
      ),
      gTree(
        children = do.call('gList', Map(cell_in_rows, tbl, 1:nrow(tbl))),
        vp = vpPath("vp_table_layout", "vp_body")
      ),
      linesGrob(unit(c(0,1), "npc"), y = unit(c(.5, .5), "npc"), vp = vpPath("vp_table_layout", "vp_spacer")),
      # forest part
      if (is.null(vline)) {
        NULL
      } else {
        gTree(
          children = gList(
            gTree(
              children = gList(
                textGrob(forest_header[1], x = unit(vline, "native") - unit(1, "lines"), just = c("right", "center")),
                textGrob(forest_header[2], x = unit(vline, "native") + unit(1, "lines"), just = c("left", "center"))
              ),
              vp = vpStack(viewport(layout.pos.col = ncol(tbl) + 2), dataForestVp)
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
             if (is.null(vline)) NULL else linesGrob(x = unit(rep(vline, 2), "native"),
                                                     y = unit(c(0,1), "npc"),
                                                     gp = gpar(lwd = 2),
                                                     vp = dataForestVp),
             xaxisGrob(at = x_at,  label = x_labels, vp = dataForestVp)
           ),
           vp = viewport(layout.pos.col = ncol(tbl) + 2)
         )
       ),
       vp = vpPath("vp_table_layout", "vp_body")
      ),
      gTree(
       children = do.call('gList', Map( function(xi, li, ui, row_index) {
         forest_dot_line(xi, li, ui, row_index, xlim, datavp = dataForestVp)
       }, x, lower, upper, 1:length(x))),
       vp = vpPath("vp_table_layout", "vp_body")
      )
    ),
    childrenvp = forestViewport(tbl, width_row.names, width_columns, width_forest),
    vp = vp,
    gp = gp
  )

  g_forest
}


cell_in_rows <- function(row, row_index, underline_colspan = FALSE) {
  
  if (!is(row, "rrow")) stop("row needs to be of class rrow")

  
  row_name <- attr(row, "row.name")
  
  g.rowname <- if (!is.null(row_name) && row_name != "") {
    
    indent <- attr(row, "indent")
    if (is.null(indent)) indent <- 0
    
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
  
  gl.cols <- if (!(length(row) > 0)) {
    list(NULL)
  } else {
    
    j <- 1 # column index of cell
    
    lapply(seq_along(row), function(k) {
      
      cell <- row[[k]]
      cs <- attr(cell, "colspan")
      if (is.null(cs)) cs <- 1
      
      cell_ascii <- format_rcell(cell, output = "ascii")
      
      if (is.na(cell_ascii)) cell_ascii <- "NA"
      
      cell_name <- paste0("g-cell-", row_index, "-", j)
      
      cell.grobs <- if (identical(cell_ascii, "")) {
        NULL
      } else {
        if (cs == 1) {
          textGrob(
            label = cell_ascii,
            name = cell_name,
            vp = vpPath(paste0("cell-", row_index, "-", j))
          )
        } else {
          
          vp_joined_cols <- viewport(layout.pos.row = row_index, layout.pos.col = seq(j + 1, j + cs)) # +1 because of rowname
          
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
                y = unit(c(0,0), "npc"),
                vp = vp_joined_cols
              )
            )
          }
        }
      } 
      j <<- j + cs
      
      cell.grobs
    })
  }
  
  gList(
    g.rowname,
    do.call(gList, gl.cols)
  )
}

forest_dot_line <- function(x, lower, upper, row_index, xlim, datavp) {
  
  ci <- c(lower, upper)
  
  if (any(!is.na(c(x, ci)))) {
    # line
    y <- unit(c(0.5, 0.5), "npc")
    
    g.line <- if (all(!is.na(ci)) && ci[2] > xlim[1] && ci[1] < xlim[2]) {
      # -
      if (ci[1] >= xlim[1] && ci[2] <= xlim[2] ){
        linesGrob(x = unit(c(ci[1], ci[2]), "native"), y = y) 
      } else if (ci[1] < xlim[1] && ci[2] > xlim[2] ){
        # <->
        linesGrob(x = unit(ylim, "native"), y = y,
                  arrow = arrow(angle = 30, length = unit(0.5, "lines"), ends = "both")) 
      } else if ( ci[1] < xlim[1] && ci[2] <= xlim[2] ){
        # <- 
        linesGrob(x = unit(c(xlim[1], ci[2]), "native"), y = y, 
                  arrow = arrow(angle = 30, length = unit(0.5, "lines"), ends = "first")) 
      } else if (ci[1] >= xlim[1] && ci[2] > xlim[2]){
        # ->
        linesGrob(x = unit(c(ci[1], xlim[2]), "native"), y = y, 
                  arrow = arrow(angle = 30, length = unit(0.5, "lines"), ends = "last")) 
      }
    } else {
      NULL
    }  
    
    g.circle <- if (!is.na(x) && x >= xlim[1] && x <= xlim[2]) {
      circleGrob(
        x = unit(x, "native"), 
        y = y, r = unit(1/3.5, "lines"), 
        name = "point"
      )
    } else {
      NULL
    }
    
    gTree(
      children = gList(
        gTree(
          children = gList(gList(        
            g.line,
            g.circle
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
#' @examples 
#' \dontrun{
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
#' 
#' v <- tern:::forestViewport(tbl)
#' 
#' grid.newpage()
#' showViewport(v)
#' }
forestViewport <- function(tbl, width_row.names = NULL, width_columns = NULL, width_forest = unit(1, "null"),
                           gap_column = unit(1, "lines"), gap.header = unit(1, "lines")) {
  
  if (!is(tbl, "rtable")) stop("tbl needs to be an rtable object")
  if (!is.null(width_row.names) && !is(width_row.names, "unit")) stop("width_row.names needs to be NULL or a unit object")
  if (!is.null(width_columns) && !is(width_columns, "unit")) stop("width_columns needs to be NULL or a unit object")
  if (!is(width_forest, "unit")) stop("width_forest needs to be a unit object")
  
  
  nr <- nrow(tbl)
  nc <- ncol(tbl)
  
  tbl_header <- header(tbl)
  nr_h <- nrow(tbl_header)

  
  # widths for row name, cols, forest
  
  if (is.null(width_row.names)) {
    
    all_row_names <- c(indented_row.names(tbl, 2),
                       indented_row.names(tbl_header, 2))
    
    longest_row_name <- all_row_names[which.max(vapply(all_row_names, nchar, numeric(1)))]    
    
    width_row.names <- stringWidth(longest_row_name)
  }
  
  
  if (!is.null(width_columns)) {
    if (length(width_columns) == nc ) {
      # do nothing
    } else if (length(width_columns) == 1) {
      width_columns <- unit.rep(width_columns, nc)
    } else {
      stop("length of width_columns must be either 1 or the number of columns of tbl") 
    }
  } else {
    width_columns <- do.call(unit.c, lapply(seq_len(nc), function(j) {
      
      width_body <- do.call(unit.pmax, lapply(seq_len(nr), function(i) {
        stringWidth(format_rcell(tbl[i,j], output = "ascii"))
      }))
      
      width_header <- do.call(unit.pmax, lapply(seq_len(nr_h), function(i) {
        
        # for now we avoide the multicolumn cell to get the column width
        cell <- tbl_header[i,j]
        cs <- attr(cell, "colspan")
        if (is.null(cs)) cs <- 1
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
    width_row.names + gap_column,
    width_columns + gap_column,
    width_forest
  )
  
  get_num_lines_per_row <- function(x) {
    vapply(lapply(x, function(row) {
      
      cell_text <- unlist(Filter(Negate(is.null), lapply(row, function (cell) {
        format_rcell(cell, output = "ascii")
      })))
      
      if (is.null(cell_text)) 1 else vapply(strsplit(cell_text, "\n"), length, numeric(1))
      
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
      name="vp_table_layout",
      layout = grid.layout(
      nrow = 3, ncol = 1,
      heights = unit.c(height_header, gap_column, height_body))
    ),
    children = vpList(
      vpForestTablePart(nr_h, nc_g, 1, 1, widths, height_header_rows, "vp_header"),
      vpForestTablePart(nr, nc_g, 3, 1, widths, height_body_rows, "vp_body"),
      viewport(name = "vp_spacer", layout.pos.row = 2, layout.pos.col = 1)
    )
  )
  vp_tbl
}

vpForestTablePart <- function(nrow, ncol, l.row, l.col, widths, heights, name) {
  vpTree(
    viewport(name = name, layout.pos.row = l.row, layout.pos.col = l.col,
             layout = grid.layout(nrow = nrow, ncol = ncol, 
                                  widths = widths, heights = heights)),
    children = vpList(
      do.call(vpList, lapply(seq_len(nrow), function(i) {
        viewport(layout.pos.row = i, layout.pos.col = 1, name = paste0("rowname-",i))
      })),
      do.call(vpList, apply(expand.grid(seq_len(nrow), seq_len(ncol-2)), 1, function(x) {
        i <- x[1]; j <- x[2]
        viewport(layout.pos.row = i, layout.pos.col = j+1, name = paste0("cell-",i,"-",j))
      })),
      do.call(vpList, lapply(seq_len(nrow), function(i) {
        viewport(layout.pos.row = i, layout.pos.col = ncol, name = paste0("forest-",i))
      }))
    )
  )
}

grid.forest <- function(...) {
  grid.draw(forestGrob(...))
}