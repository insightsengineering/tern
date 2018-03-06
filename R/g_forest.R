#' Forest plot 
#' 
#' Create a forest plot from any \code{\link[rtables]{rtable}} object that has a
#' column with a single value and a column with 2 values
#'
#' @param tbl an rtable object
#' @param i_col_est column index with estimator 
#' @param i_col_ci column index with confidence intervals
#' @param header_forest how to label the forest
#' @param padx gap between two columns
#' @param cex multiplier applied to overall fontsize
#' 
#' @template author_song24
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
#'   i_col_est = 8,
#'   i_col_ci = 9,
#'   header_forest = c("Treatement\nBetter", "Comparison\nBetter")
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
#'   i_col_est = 8,
#'   i_col_ci = 9,
#'   header_forest = c("Comparison\nBetter", "Treatement\nBetter")
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
#'   i_col_est = 1,
#'   i_col_ci = 2,
#'   header_forest = c("Hello", "World")
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
#'   i_col_est = 1,
#'   i_col_ci = 2,
#'   header_forest = c("Hello", "World")
#' )
#' 
#' 
#' 
g_forest <- function(tbl,
                     i_col_est,
                     i_col_ci,
                     header_forest = c("", ""),
                     xlim = NULL,
                     log = FALSE,
                     width_columns = NULL,
                     width_row.names = NULL,
                     width_forest = unit(1, "null"),
                     padx = unit(1, "lines"),
                     draw = TRUE) {
  
  # start with the plotting code
  nr <- nrow(tbl)
  nc <- ncol(tbl)
  
  if (!(i_col_est > 0 && i_col_est <= nc)) stop("i_col_est out of bounds")
  if (!(i_col_ci > 0 && i_col_ci <= nc)) stop("i_col_ci out of bounds")
  
  if (!(is.character(header_forest) && length(header_forest) == 2))
    stop("header_forest is required to be a character vector of length two")
  
  if (!is.null(width_columns) && !is(width_columns, "unit"))
    stop("widths currently needs to be NULL or an object of class unit")
  
  if (!is.null(width_row.names) && !is(width_row.names, "unit"))
    stop("width_row.names currently needs to be NULL or an object of class unit")
  
  x_e <- vapply(seq_len(nr), function(i) {
    xi <- as.vector(tbl[i, i_col_est])
    
    if (!is.null(xi) && !(length(xi) <= 0) && is.numeric(xi)) {
      xi
    } else {
      NA_real_
    }
  }, numeric(1))
  
  x_ci <- lapply(seq_len(nr), function(i) {
    xi <- as.vector(tbl[i, i_col_ci])
    
    if (!is.null(xi) && !(length(xi) <= 0) && is.numeric(xi)) {
      xi
    } else {
      NULL
    }
  })
  
  if (is.null(xlim)) {
    vals <- c(x_e, unlist(x_ci))
    xlim <- extendrange(vals)
  }
  
  
  tbl_header <- header(tbl)
  nr_h <- nrow(tbl_header)
  all_row_names <- c(indented_row.names(tbl, 2),
                     indented_row.names(tbl_header, 2))
  
  if (is.null(width_row.names)) {
    longest_row_name <- all_row_names[which.max(vapply(all_row_names, nchar, numeric(1)))]    
    width_row.names <- unit(1, "strwidth", longest_row_name)
  }
  
  
  # for now lets make the table part fix-width and the forst part flexible width
  if (!is.null(width_columns)) {
    if (length(width_columns) == nc ) {
      # do nothing
    } else if (length(width_columns) == 1) {
      width_columns <- unit.rep(width_columns, nc)
    } else {
      stop("length of widths must be either 1 or the number of columns of tbl") 
    }
  } else {
    # calculate widths
    width_columns <- do.call(unit.c, lapply(seq_len(nc), function(j) {
      width_body <- do.call(unit.pmax, lapply(seq_len(nr), function(i) {
        stringWidth(format_rcell(tbl[i,j], output = "ascii")) + padx
      }))
      
      width_header <- do.call(unit.pmax, lapply(seq_len(nr_h), function(i) {
        stringWidth(format_rcell(tbl_header[i,j], output = "ascii")) + padx
      }))
      
      unit.pmax(width_body, width_header)
    }))
  }
  
  ## Now start building the table
  ## ----------------------------
  
  # now start with the grid layouts
  vp_col_layout <- vpTree(
    parent = viewport(
      name = "vp_col_layout",
      layout = grid.layout(
        nrow = 1, ncol = nc + 2,
        widths = unit.c(
          width_row.names + padx,
          width_columns + padx,
          unit(1, "null") # remaining space for forest plot
        )
      )
    ),
    children = do.call(vpList, c(
      list(viewport(name = "vp_row.names", layout.pos.col=1, layout.pos.row=1)),
      lapply(seq_len(nc), function(i) {
        viewport(name = paste0("vp_col_", i), layout.pos.col=i+1, layout.pos.row=1)
      }),
      list(dataViewport(name = "vp_forest", layout.pos.col=nc+2, layout.pos.row=1,
                        xscale = xlim, yscale = c(0,1)))
    ))
  )
  
  # showViewport(vp_col_layout)
  gl_anchors <- gList(
    nullGrob(name = "left_row.names", x = unit(0, "npc"), vp =  vpPath("vp_col_layout", "vp_row.names")),
    do.call(gList, lapply(seq_len(nc), function(j) {
      vp <- vpPath("vp_col_layout", paste0("vp_col_", j))
      gList(
        nullGrob(x=unit(.5, "npc"), name = paste0("center_col_", j), vp = vp),
        nullGrob(x=unit(0, "npc"),  name = paste0("left_col_", j), vp = vp),
        nullGrob(x=unit(1, "npc"),  name = paste0("right_col_", j), vp = vp)   
      )
    }))
  )
  # vp_col_layout & gl_anchors are then used to make the
  # table header and body, e.g.
  # p <- gTree(
  #  children = gList(
  #    gl_anchors,
  #    textGrob("Hello World", vp = vpPath("vp_col_layout", "vp_forest")),
  #    rectGrob(x= grobX("center_col_2", 0), width = unit(1, "lines"),
  #             height = unit(1, "lines"))
  #  ),
  #  childrenvp = vp_col_layout
  # )
  # grid.newpage(); grid.draw(p)
  
  header_line_factor <- get.gpar("lineheight")$lineheight
  
  
  # the rtables API is currently not well enough developped in order to
  # draw the table without knowing the underlying data structure
  rowGrob <- function(row, y = unit(.5, "npc"), underline_colspan = FALSE) {
    
    if (!is(row, "rrow")) stop("object of class rrow expected")
    
    row_name <- paste0(strrep(" ", 2* attr(row, "indent")), attr(row, "row.name"))
    
    # 
    g.rowname <- if (!is.null(row_name) && row_name != "") {
      textGrob(
        name = "row.name",
        label = row_name,
        x = grobX("left_row.names", 0),
        y = y,
        just = c("left", "center")
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
        
        cell_name <- paste0("cell-col-", j)

        cell.grob <- if (identical(cell_ascii, "")) {
          NULL
        } else {
          if (cs == 1) {
            textGrob(
              label = cell_ascii,
              x = grobX(paste0("center_col_", j), 0),
              y = y,
              just = c("center", "center"),
              name = cell_name
            )
          } else {
            x0 <- grobX(paste0("center_col_", j), 0)
            x1 <- grobX(paste0("center_col_", j + cs-1), 0)
            
            lab <- textGrob(
              label = cell_ascii,
              x = x0 + 0.5 * (x1 - x0),
              y = y,
              just = c("center", "center"),
              name = cell_name
            )
            
            if (!underline_colspan) {
              lab
            } else {
              x0 <- grobX(paste0("left_col_", j), 0)
              x1 <- grobX(paste0("right_col_", j+cs-1), 0)
              
              gList(
                lab,
                linesGrob(x = unit.c(x0 + 0.2*padx, x1 - 0.2*padx),
                          y = rep(y - unit(.5, "lines")), 2)
              )
            }
          }
        } 
        j <<- j + cs
        
        cell.grob
      })
    }
    
    gList(
      g.rowname,
      do.call(gList, gl.cols)
    )
  }
  
  
  
  
  # now create the grobs
  y_header <- unit(1, "npc") - unit( seq_len(nr_h) * header_line_factor , "lines") 
  l_g_header_rows <- do.call(gList, lapply(seq_len(nr_h), function(i) {
    rowGrob(tbl_header[[i]], y_header[i],  underline_colspan = TRUE)
  }))

  p_header <- gTree(
    children = gList(
      gl_anchors,
      l_g_header_rows
    ),
    childrenvp = vp_col_layout
  )
  # grid.newpage(); grid.draw(p_header)
  #showGrob(p_header)
  
  #grid.text("c1", x = grobX("center_col_1",0), vp = "vp_col_layout")
  
  #grid.ls(viewports = TRUE)
  
  # grid.ls(p_header, viewports = TRUE)
  # grid.newpage(); grid.draw(p_header)
  
  y_body <- unit(1, "npc") - unit(seq_len(nr) * header_line_factor, "lines")
  l_g_body_rows <- do.call(gList, lapply(seq_len(nr), function(i) {
    rowGrob(tbl[[i]], y_body[i], underline_colspan = FALSE) 
  }))
  
  l_g_forest_rows <- do.call(gList, lapply(seq_len(nr), function(i) {
    pointLineGrob(x_e[i], x_ci[[i]], y_body[i],
                  vp = vpPath("vp_col_layout", "vp_forest")) 
  }))

  p_body <- gTree(
    children = gList(
      gl_anchors,
      l_g_body_rows,
      l_g_forest_rows,
      xaxisGrob(vp = vpPath("vp_col_layout", "vp_forest"))
    ),
    childrenvp = vp_col_layout
  )
  
  # grid.newpage(); grid.draw(p_body)
  # showViewport()
  
  p_table <- gTree(
    children = gList(
      editGrob(p_header, vp = viewport(layout.pos.row = 1)),
      linesGrob(y = unit(c(.5, .5), "npc"), vp = viewport(layout.pos.row = 2)),
      editGrob(p_body, vp = viewport(layout.pos.row = 3))
    ),
    vp = vpStack(plotViewport(margins = c(2, 1, 1, 1)),
                 viewport(layout = grid.layout(
      nrow = 4, ncol = 1,
      heights = unit(c(header_line_factor*nr_h+2, 2,
                       header_line_factor * nr+2, 1),
                     c("lines", "lines", "lines", "null"))
    )))
  )
  
  if (draw) {
    grid.newpage(); grid.draw(p_table)    
  }
  
  invisible(p_table)
}


#' forest plot grob
#' 
#' @export
#' 
#' @param tbl an \code{\link[rtables]{rtable}} object
#' @param x coordinate of point
#' @param lower lower bound of ci
#' @param upper upper bound of ci
#' @param xlim x limits for x-scales
#' @param width_row.name \code{\link[grid]{unit}} object with width for row
#'   names. If \code{NULL} the widths get automatically calculated.
#' @param width_columns \code{\link[grid]{unit}} object with widths for the
#'   table columns. If \code{NULL} the widths get automatically calculated.
#' @param width_forest \code{\link[grid]{unit}} object with width for the
#'   forest column. If \code{NULL} the widths get automatically calculated.
#'   
#' 
#' @details 
#' The heights get automatically determined
#' 
#' @export
#' 
#' @examples 
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
#' x <- c(.2, .3, .1)
#' lower <- x - .2
#' upper <- x + .3
#' 
#' forestGrob(tbl, x, lower, upper)
#' 
forestGrob <- function(tbl, x, lower, upper,
                       xlim = NULL,
                       width_row.names = NULL,
                       width_columns = NULL,
                       width_forest = unit(1, "null"), 
                       name=NULL, gp=NULL, vp=NULL) {

  
  nr <- nrow(tbl)
  if (length(x) != nr) stop("dimension missmatch x")
  if (length(lower) != nr) stop("dimension missmatch lower")
  if (length(upper) != nr) stop("dimension missmatch upper")

  if (is.null(xlim)) xlim <- extendrange(c(x, lower, upper))
  
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
      gTree(
        children = do.call('gList', Map( function(xi, li, ui, row_index) {
          forest_dot_line(xi, li, ui, row_index, xlim, datavp = dataForestVp)
        }, x, lower, upper, 1:length(x))),
        vp = vpPath("vp_table_layout", "vp_body")
      ),
      gTree(
        children = gList(
          xaxisGrob(vp = dataForestVp)
        ),
        vp = vpPath("vp_table_layout", "vp_body", paste0("forest-", nrow(tbl)))
      )
    ),
    childrenvp = forestViewport(tbl, width_row.names, width_columns, width_forest),
    vp = vp,
    gp = gp
  )

  grid.newpage()
  grid.draw(g_forest)
 # showViewport()
}


cell_in_rows <- function(row, row_index, underline_colspan = FALSE) {
  
  if (!is(row, "rrow")) stop("row needs to be of class rrow")

  
  row_name <- attr(row, "row.name")
  cat(row_name); cat(" "); cat(row_index); cat("\n")
  
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
                x = unit(c(.1, .9), "npc"),
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
#' 
#' v <- forestViewport(tbl)
#' 
#' grid.newpage()
#' showViewport(v)
#' 
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
        stringWidth(format_rcell(tbl_header[i,j], output = "ascii"))
      }))
      
      unit.pmax(width_body, width_header)
    }))
  }
  

  widths <- unit.c(
    width_row.names + gap_column,
    width_columns + gap_column,
    width_forest
  )
  
  get_row_heights <- function(x) {
    unit(1.2 * vapply(lapply(x, function(row) {
      vapply(strsplit(unlist(Filter(Negate(is.null), lapply(row, function (cell) {
        format_rcell(cell, output = "ascii")
      }))), "\n"), length, numeric(1))
    }), max, numeric(1)) , "lines") 
  }
  
  height_body_rows <- get_row_heights(tbl)
  height_header_rows <-get_row_heights(tbl_header) 
  
  height_header <- sum(height_body_rows)
  height_body <- sum(height_body_rows)
  
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

validDetails.forest <- function(x) {
  
  if (!is(x$tbl, "rtable")) stop("tbl needs to be an object of class rtable")
  nr <- nrow(x$tbl)
  
  if (!is.numeric(x$x))
  
  
  
  x
}