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
#' 
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
#'   header = rheader(rrow("", rcell("A", colspan = 2))
#'   ),
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
#' 
#' 
g_forest <- function(tbl, i_col_est, i_col_ci,
                     header_forest = c("", ""),
                     xlim = NULL,
                     log = FALSE,
                     widths = NULL,
                     width_row.names = NULL,
                     padx = unit(1, "lines"),
                     draw = TRUE) {
  
  # start with the plotting code
  nr <- nrow(tbl)
  nc <- ncol(tbl)
  
  if (!(i_col_est > 0 && i_col_est <= nc)) stop("i_col_est out of bounds")
  if (!(i_col_ci > 0 && i_col_ci <= nc)) stop("i_col_ci out of bounds")
  
  if (!(is.character(header_forest) && length(header_forest) == 2))
    stop("header_forest is required to be a character vector of length two")
  
  if (!is.null(widths) && !is(widths, "unit"))
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
  all_row_names <- c(indented_row.names(tbl, 2),
                     indented_row.names(tbl_header, 2))
  
  if (is.null(width_row.names)) {
    longest_row_name <- all_row_names[which.max(vapply(all_row_names, nchar, numeric(1)))]    
    width_row.names <- unit(1, "strwidth", longest_row_name)
  }
  
  
  # for now lets make the table part fix-width and the forst part flexible width
  if (!is.null(widths)) {
    if (length(widths) == nc) {
      # do nothing
    } else if (length(widths == 1)) {
      widths <- unit.rep(widths, nc)
    } else {
      stop("length of widths must be either 1 or the number of columns of tbl") 
    }
  } else {
    # calculate widths
    nrh <- nrow(tbl_header)
    widths <- do.call(unit.c, lapply(seq_len(nc), function(j) {
      width_body <- do.call(unit.pmax, lapply(seq_len(nr), function(i) {
        stringWidth(format_rcell(tbl[i,j], output = "ascii")) + padx
      }))
      
      width_header <- do.call(unit.pmax, lapply(seq_len(nrh), function(i) {
        stringWidth(format_rcell(tbl_header[i,j], output = "ascii")) + padx
      }))
      
      unit.pmax(width_body, width_header)
    }))
  }
  
  
  # now start with the grid layouts
  col_layout <- grid.layout(
    nrow = 1, ncol = nc + 2,
    widths = unit.c(
      width_row.names,
      widths,
      unit(1, "null") # for forest plot
    )
  )
  
  col_vps <- do.call(vpList, c(
    list(viewport(name = "vp_row.names", layout.pos.col=1, layout.pos.row=1)),
    lapply(1:nc, function(i) {
      viewport(name = paste0("vp_col_", i), layout.pos.col=i+1, layout.pos.row=1)
    }),
    list(dataViewport(name = "vp_forest", layout.pos.col=nc+2, layout.pos.row=1,
                      xscale = xlim, yscale = c(0,1)))
  ))
  
  
  gL_col_anchors <- do.call(gList, lapply(seq_len(nc), function(j) {
    vp <- vpPath(paste0("vp_col_", j))
    gTree(
      children = gList(
        nullGrob(name = paste0("center_col_", j), vp = vp),
        nullGrob(x=unit(0, "npc"), name = paste0("left_col_", j), vp = vp),
        nullGrob(x=unit(1, "npc"), name = paste0("right_col_", j), vp = vp)        
      ),
      name = paste0("column_", j, "_anchors")
    )
  }))


  
  tbl_grob <- gTree(
    children = gList(
      nullGrob(name = "AAA", vp = vpPath("vp_forest"))
    ), #gL_col_anchors,
    childrenvp = col_vps,
    vp = viewport(name = "vp_table_layout", layout = col_layout),
    name = "tbl_grob"
  )
  
  # grid.ls(tbl_grob, viewports = TRUE)
  # grid.ls(tbl_grob, grobs = FALSE, viewports = TRUE)

?grid.ls
  
  a <- gTree(children = gList(rectGrob()), vp=plotViewport(name = "a_margins"))  
  tbl_grob <- gTree(
    children = gList(a),
    vp = plotViewport(name = "margins")
  )
  
  grid.newpage();
  grid.draw(tbl_grob)
  grid.draw(a)
  
  showViewport()
  #showGrob()
  
  grid.text("hello World",
            unit(0, "npc"),
            vp = vpPath("vp_table_layout", "vp_col_2"), 
            just = c("left", "bottom"))
  
  seekViewport("vp_table_layout")
  upViewport(0)

  grid.null(name = "AAA", vp = vpPath("vp_table_layout", "vp_forest"))  
  
  grid.text("hello grobX",
            x = grobX("AAA", 0),
            just = c("left", "bottom"))
  
  
  grid.ls(viewports = TRUE)
  grid.ls()
  header_line_factor <- 1.1
  
  # the rtables API is currently not well enough developped in order to
  # draw the table without knowing the underlying data structure
  rowGrob <- function(row, y = unit(.5, "npc"), underline_colspan = FALSE, name = NULL) {
    
    if (!is(row, "rrow")) stop("object of class rrow expected")
    
    row_name <- paste0(strrep(" ", 2* attr(row, "indent")), attr(row, "row.name"))
    
    # 
    g.rowname <- if (!is.null(row_name) && row_name != "") {
      textGrob(
        name = "row.name",
        label=row_name,
        x=unit(0, "npc"), y = y,
        just = c("left", "center"),
        vp = vpPath("vp_row.names")
      )
    } else {
      NULL
    }
    
    g.cols <- if (!(length(row) > 0)) {
      list(NULL)
    } else {
      j <- 1 # column index of cell
      lapply(1:length(row), function (k) {
        
        cell <- row[[k]]
        cs <- attr(cell, "colspan")
        cell_ascii <- format_rcell(cell, output = "ascii")
        
        if (is.na(cell_ascii)) cell_ascii <- "NA"
        
        cell_name <- paste0("cell-col-", j)

        cell.grob <- if (cell_ascii == "") {
          NULL
        } else {
          if (cs == 1) {
            textGrob(
              label = cell_ascii, x = unit(0.5, "npc"), y = y,
              just = c("center", "center"),
              name = cell_name,
              vp = vpPath(paste0("vp_col_", j))
            )
          } else {
            x0 <- grobX(paste0("center_col_", j), 0)
            x1 <- grobX(paste0("center_col_", j + cs-1), 0)
            
            lab <- textGrob(label = cell_ascii, x = x0 + 0.5 * (x1 - x0), y = y)
            
            if (!underline_colspan) {
              editGrob(lab, name = cell_name)
            } else {
              x0 <- grobX(paste0("left_col_", j), 0)
              x1 <- grobX(paste0("right_col_", j+cs-1), 0)
              
              gTree(
                children = gList(
                  lab,
                  linesGrob(x = unit.c(x0 + 0.2*padx, x1 - 0.2*padx), y = y - unit(header_line_factor/2, "lines"))
                ),
                name = cell_name
              )
            }
          }
        } 
        j <<- j + cs
        
        cell.grob
      })
    }
    gTree(
       children = do.call(gList, Filter(Negate(is.null), c(list(g.rowname), g.cols))),
       name = name  
    )
  }
  
  pointLineGrob <- function(x, ci, y = unit(.5, "npc"), gp = gpar(col = "blue", fill = "blue"), name = NULL) {
    
    if (is.na(x) && is.null(ci)) {
      NULL
    } else {
      # line
      g.line <- if (all(!is.na(ci)) && ci[2] > xlim[1] && ci[1] < xlim[2]) {
        # -
        if (ci[1] >= xlim[1] && ci[2] <= xlim[2] ){
          linesGrob(x = unit(c(ci[1], ci[2]), "native"), 
                     y = unit.c(y, y), gp = gp) 
        } else if (ci[1] < xlim[1] && ci[2] > xlim[2] ){
          # <->
          linesGrob(x = unit(ylim, "native"), 
                     y = unit.c(y, y), gp = gp,
                     arrow = arrow(angle = 30, length = unit(0.5, "lines"), ends = "both")) 
        } else if ( ci[1] < xlim[1] && ci[2] <= xlim[2] ){
          # <- 
          linesGrob(x = unit(c(xlim[1], ci[2]), "native"), 
                     y = unit.c(y, y), gp = gp, 
                     arrow = arrow(angle = 30, length = unit(0.5, "lines"), ends = "first")) 
        } else if (ci[1] >= xlim[1] && ci[2] > xlim[2]){
          # ->
          linesGrob(x = unit(c(ci[1], xlim[2]), "native"), 
                     y = unit.c(y, y), gp = gp, 
                     arrow = arrow(angle = 30, length = unit(0.5, "lines"), ends = "last")) 
        }
      } else {
        NULL
      }  
      
      g.circle <- if (!is.na(x) && x >= xlim[1] && x <= xlim[2]) {
        grid.circle(
          x = unit(x, "native"), 
          y = y, r = unit(1/3.5, "lines"), 
          gp = gp,
          name = "point"
        )
      }
      
      gTree(
        children = gList(
          g.line,
          g.circle
        ),
        name = name,
        vp = vpPath("vp_forest")
      )
    }
  }
  
  
  # now create the grobs
  nr_h <- nrow(tbl_header)
  y_header <- unit(1, "npc") - unit( (1:nr_h - .5) * header_line_factor , "lines") 
  l_g_header_rows <- lapply(seq_len(nr_h), function(i) rowGrob(tbl_header[[i]], y_header[i], TRUE, paste0("header_row_", i)))
  
  g_header <- gTree(
    tbl_grob,
    children = do.call(gList, l_g_header_rows),
    childrenvp =  vpPath("vp_table_layout"),
    name = "header_table_content"
  )
  
  # grid.ls(g_header, viewports = TRUE)
  # grid.newpage(); grid.draw(g_header)
  
  y_body <- unit(1, "npc") - unit((1:nr - .5) *header_line_factor, "lines")
  l_g_body_rows <- lapply(seq_len(nr), function(i) rowGrob(tbl[[i]], y_body[i], paste0("row_", i)))
  g_body <- gTree(
    children = do.call(gList, l_g_body_rows),
    name = "body_table_content"
  )
  
  l_g_forest_rows <- lapply(seq_len(nr), function(i) {
    pointLineGrob(x_e[i], x_ci[[i]], y_body[i], name = paste0("tree_",i)) 
  })
  g_forest <- gTree(
    children = do.call(gList, l_g_forest_rows),
    name = "body_forest_content"
  )
  

  p <- gTree(
    children = gList(
      editGrob(g_col_anchors, vp = vpPath("vp_header_body_layout", "vp_header"))
#      rectGrob(grobX("center_col_2",0), grobY("center_col_2",0), 
#               unit(1, "lines"), unit(1, "lines"),
#               vp = vpPath("vp_header_body_layout", "vp_header", "vp_col_2"))
      #editGrob(g_header, vp = vpPath("vp_header_body_layout", "vp_header"))
#      linesGrob(y = unit(0, "npc"), vp = vpPath("vp_header_body_layout", "vp_header")),
#      editGrob(g_col_anchors, vp = vpPath("vp_header_body_layout", "vp_body")),
#      editGrob(g_body, vp = vpPath("vp_header_body_layout", "vp_body")),
#      xaxisGrob(name = "xaxis", vp = vpPath("vp_header_body_layout", "vp_body", "vp_forest")),
#      editGrob(g_forest, vp = vpPath("vp_header_body_layout", "vp_body")),
#      gTree(
#        children = gList(
#          textGrob(name = "forest_header_left", label = header_forest[1],
#                   x = unit(1, "lines"), just = c("left", "center"),
#                   y = unit(.5, "npc")),
#          textGrob(name = "forest_header_right", label = header_forest[2],
#                   x = unit(1, "npc"), just = c("right", "center"),
#                   y = unit(.5, "npc"))
#        ),
#        name = "forest_header",
#        vp = vpPath("vp_header_body_layout", "vp_header", "vp_forest")
#      )

    ),
    childrenvp = vp_table,
    vp = vpStack(
      viewport(name = "gpar_user", gp = gpar()), # user settings (later to be added as an argument)
      plotViewport(name = "margins", margins = c(3,2,2,2))
    )
  )
  
  # grid.ls(p, viewports = TRUE)
  
  if (draw) {
    grid.newpage(); grid.draw(p)    
  }
  
  invisible(p)
}
