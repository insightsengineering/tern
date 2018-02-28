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
#' 
g_forest <- function(tbl, i_col_est, i_col_ci,
                     header_forest,
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
    nrow = 1, ncol = 11,
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
  
  header_line_factor <- 1.1
  
  vp_table <- vpTree(
    parent = viewport(
      name = "vp_header_body_layout",
      layout = grid.layout(
        nrow = 3, ncol = 1,
        heights = unit.c(unit((nrow(tbl_header) + 1)*header_line_factor, "lines"),
                         unit(nrow(tbl)*header_line_factor, "lines"),
                         unit(1, "null"))
      )),
    children = vpList(
      vpTree(
        parent = viewport(name = "vp_header", layout = col_layout, layout.pos.col=1, layout.pos.row=1),
        children = col_vps
      ),
      vpTree(
        parent = viewport(name = "vp_body", layout = col_layout, layout.pos.col=1, layout.pos.row=2),
        children = col_vps
      ),
      viewport(name = "vp_space_filler", layout.pos.col=1, layout.pos.row=3)
    ) 
  )
  # grid.newpage(); showViewport(vp_table)
  
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
            x1 <- grobX(paste0("center_col_", j+cs-1), 0)
            
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
  y_header <- unit(1, "npc") - unit( (1:nr_h - .5) *header_line_factor, "lines") 
  l_g_header_rows <- lapply(seq_len(nr_h), function(i) rowGrob(tbl_header[[i]], y_header[i], TRUE, paste0("header_row_", i)))
  g_header <- gTree(
    children = do.call(gList, l_g_header_rows),
    name = "header_table_content"
  )
  
  
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
  
  g_col_anchors <-  gTree(
    children = do.call(gList, lapply(seq_len(nc), function(j) {
    gTree(
      children = gList(
        nullGrob(name = paste0("center_col_", j)),
        nullGrob(x=unit(0, "npc"), name = paste0("left_col_", j)),
        nullGrob(x=unit(1, "npc"), name = paste0("right_col_", j))
      ),
      vp =  vpPath(paste0("vp_col_", j)),
      name = paste0("column_", j, "_anchors")
    )
  })))
  
  
  p <- gTree(
    children = gList(
      editGrob(g_col_anchors, vp = vpPath("vp_header_body_layout", "vp_header")),
      editGrob(g_header, vp = vpPath("vp_header_body_layout", "vp_header")),
      editGrob(g_col_anchors, vp = vpPath("vp_header_body_layout", "vp_body")),
      editGrob(g_body, vp = vpPath("vp_header_body_layout", "vp_body")),
      rectGrob(vp = vpPath("vp_header_body_layout", "vp_body"))
    ),
    childrenvp = vp_table,
    vp = vpStack(
      viewport(name = "gpar_user", gp = gpar()), # user settings (later to be added as an argument)
      plotViewport(name = "margins", margins = c(3,2,2,2))
    )
  )
  
  grid.ls(p, viewports = TRUE)

  grid.newpage(); grid.draw(p)
    
  p <- gTree(
    children = gList(
      rectGrob(name = "a"), 
      rectGrob(name = "b"), 
      rectGrob(name = "c")
    ),
    name = "row1"
  )
  
  
  grid.ls(p, viewports = TRUE)
  
  grid.newpage(); grid.draw(p)
  showViewport()
  grid.ls(viewports = TRUE)
  
    
  p <- gTree(
    nullGrob(),
    childrenvp = vp_table,
    children = do.call(gList, lapply(seq_along(nc), function(j) {
      
      # "gpar_user", "margins", "header_body_layout", 
      vp_path <-  vpPath("body", paste0("col_", j))
      
      nullGrob(name = paste0("center_col_", j), vp = vp_path)
      nullGrob(x=unit(0, "npc"), name = paste0("left_col_", j), vp = vp_path)
      nullGrob(x=unit(1, "npc"), name = paste0("right_col_", j), vp = vp_path)
    })),
    vp = 
  )
  # grid.newpage(); grid.draw(plot_setup)
  
  grid.ls(plot_setup, viewports = TRUE)
  
  # 
  
  grid.newpage()
  pushViewport(vp)
  upViewport()
  downViewport(vpPath("header"), strict = TRUE)# need to go to body: current.viewport()
  ## create anchor points
  for (j in 1:nc) {
    grid.null(name = paste0("center_col_", j), vp = vpPath(paste0("col_", j)))
    grid.null(x=unit(0, "npc"), name = paste0("left_col_", j), vp = vpPath(paste0("col_", j)))
    grid.null(x=unit(1, "npc"), name = paste0("right_col_", j), vp = vpPath(paste0("col_", j)))
  }
  grid.lines(x = unit(c(0,1), "npc"), y = unit(c(0.5, 0.5), "lines"))
  upViewport()
  
  # grid.ls(viewports = TRUE)
  # showViewport(current.viewport())
  # current.viewport()
  
  # draw header
  downViewport(vpPath("header"), strict = TRUE)
  
  # grid.rect(gp = gpar(fill = "gray90", col = NA))
  tbl_header <- header(tbl)
  nrh <- nrow(tbl_header)
  y_header <- unit(1, "npc") - unit( (1:nrh - .5) *header_line_factor, "lines") 
  for (i in 1:nrow(tbl_header)) {
    draw_row(tbl_header[[i]], y = y_header[i], TRUE )
  } 
  upViewport()
  
  
  # draw the table body
  downViewport(vpPath("body"))
  y_body <- unit(1, "npc") - unit((1:nr - .5) *header_line_factor, "lines")
  for (i in 1:nr) {
    draw_row(tbl[[i]], y_body[i])
  }
  
  # now draw the forest
  downViewport(vpPath("forest"), strict = TRUE)
  
  # break arm labels to muliple lines as needed
  header_left <- wrap_text(header_forest[1], width = unit(4, "cm"), collapse = "\n")
  header_right <- wrap_text(header_forest[2], width = unit(4, "cm"), collapse = "\n")
  
  # need once: mid-line OR/HR = 1
  grid.xaxis(at = c(log(0.1), log(0.5), log(1), log(2), log(5), log(10)),
             label = c(0.1, 0.5, 1, 2, 5, 10))
  grid.lines(x = unit(c(0,0), "native"), y = unit(c(0, 1), "npc"),
             gp = gpar(lty = 2))
  
  
  #Only draw if the CI is within the range of 0.1-10
  
  
  
  for (i in 1:nr) {
    est <- tbl[i, i_col_est]
    ci <-  tbl[i, i_col_ci]
    if (!is.null(est) && !is.null(ci)) {
      draw_point_line(x = log(est), ci = log(ci), y = y_body[i])
    }
  }
  upViewport(2)
  
  # Add header
  downViewport(vpPath("header", "forest"), strict = TRUE)
  grid.text(header_left, x = unit(0, "native") - unit(1, "lines"), just = c("right", "center"))
  grid.text(header_right, x = unit(0, "native") + unit(1, "lines"), just = c("left", "center"))
  
}
