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
#' @author Yuyao Song (songy24) \email{yuyao.song@roche.com}
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
#'   group_data = as.data.frame(lapply(ANL[, c("SEX", "RACE")], as.factor)),
#'   dense_header = TRUE
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
#' 
g_forest <- function(tbl, i_col_est, i_col_ci, header_forest,  padx = unit(1, "lines")) {
  
  ## args to add
  # xlim <- NULL
   width <- NULL
   
  
  # start with the plotting code
  nr <- nrow(tbl)
  nc <- ncol(tbl)
  
  if (!(i_col_est > 0 && i_col_est <= nc)) stop("i_col_est out of bounds")
  if (!(i_col_ci > 0 && i_col_ci <= nc)) stop("i_col_ci out of bounds")
  
  if (!(is.character(header_forest) && length(header_forest) == 2))
    stop("header_forest is required to be a character vector of length two")
  
  if (!is.null(width) && !is(width, "unit")) stop("width currently needs to be NULL or an object of class unit")
  
 # if (missing(xlim)) {
 #   vals <- unlist(lapply(seq_len(nr), function(i) c(tbl[i,i_col_ci], tbl[i, i_col_est])))
 #   xlim <- extendrange(vals)
 # }
  
  # for now lets make the table part fix-width and the forst part flexible width
  # (expanding)
  rn <- c(indented_row.names(tbl, 2), indented_row.names(header(tbl), 2))
  longest_row_name <- rn[which.max(vapply(rn, nchar, numeric(1)))]
  width_row_names <- unit(1, "strwidth", longest_row_name)
  
  h <- header(tbl)
  nrh <- nrow(h)
  if (is.null(width)) {
    width <- do.call(unit.c, lapply(seq_len(nc), function(j) {
      
      width_body <- do.call(unit.pmax, lapply(seq_len(nr), function(i) {
          stringWidth(format_rcell(tbl[i,j], output = "ascii")) + padx
      }))
      
      width_header <- do.call(unit.pmax, lapply(seq_len(nrh), function(i) {
        stringWidth(format_rcell(h[i,j], output = "ascii")) + padx
      }))

      unit.pmax(width_body, width_header)
   }))
  }
  
  if (length(width) != nc) stop("widths specifications wrong dimension")
  
  col_layout <- grid.layout(
    nrow = 1, ncol = 11,
    widths = unit.c(
      width_row_names,
      width,
      unit(1, "null") # for forest plot
    )
  )
  
  xlim <- log(c(0.1, 10))
  col_vps <- do.call(vpList, c(
    list(viewport(name = "row.names", layout.pos.col=1, layout.pos.row=1)),
    lapply(1:nc, function(i) {
      viewport(name = paste0("col_", i), layout.pos.col=i+1, layout.pos.row=1)
    }),
    list(dataViewport(name = "forest", layout.pos.col=nc+2, layout.pos.row=1,
                      xscale = xlim, yscale = c(0,1)))
  ))
  
  header_line_factor <- 1.1
  vp <- vpStack(
    viewport(name = "gpar_user", gp = gpar()), # user settings (later to be added as an argument)
    plotViewport(name = "margins", margins = c(3,2,2,2)),
    vpTree(
      parent = viewport(name = "header_body_layout", layout = grid.layout(
        nrow = 3, ncol = 1,
        heights = unit.c(unit((nrow(header(tbl))+1)*header_line_factor, "lines"),
                         unit(nrow(tbl)*header_line_factor, "lines"),
                         unit(1, "null"))
      )),
      children = vpList(
        vpTree(
          parent = viewport(name = "header", layout = col_layout, layout.pos.col=1, layout.pos.row=1),
          children = col_vps
        ),
        vpTree(
          parent = viewport(name = "body", layout = col_layout, layout.pos.col=1, layout.pos.row=2),
          children = col_vps
        ),
        viewport(name = "space_filler", layout.pos.col=1, layout.pos.row=3)
      ) 
    )
  )
  # grid.newpage(); showViewport(vp)

  
  # this API of rtables is currently missing, so we use the underlying data
  # structure
  draw_row <- function(row, y, underline_colspan = FALSE) {
    
    if (!is(row, "rrow")) stop("object of class rrow expected")
    
    row_name <- paste0(strrep(" ", 2* attr(row, "indent")), attr(row, "row.name"))
    
    if (!is.null(row_name) && row_name != "") {
      grid.text(label=row_name, x=unit(0, "npc"), y = y,
                just = c("left", "center"), vp = vpPath("row.names"))      
    }
    
    if (length(row) > 0) {
      j <- 1
      for (k in 1:length(row)) {
        cell <- row[[k]]
        cs <- attr(cell, "colspan")
        cell_ascii <- format_rcell(cell, output = "ascii")
        
        if (is.na(cell_ascii)) cell_ascii <- "NA"
        
        if (cell_ascii != "") {
          vp_cell <- if (cs == 1) {
            grid.text(label = cell_ascii, x = unit(0.5, "npc"), y = y,
                      just = c("center", "center"), vp = vpPath(paste0("col_", j)))
          } else {
            x0 <- grobX(paste0("center_col_", j), 0)
            x1 <- grobX(paste0("center_col_", j+cs-1), 0)
            grid.text(label = cell_ascii, x = x0 + 0.5 * (x1 - x0), y = y)
            
            if (underline_colspan) {
              x0 <- grobX(paste0("left_col_", j), 0)
              x1 <- grobX(paste0("right_col_", j+cs-1), 0)
              grid.lines(x = unit.c(x0 + 0.2*padx, x1 - 0.2*padx), y = y - unit(header_line_factor/2, "lines"))
            }
          }
          
        }
        j <- j + cs
      }
    }      
  }

  
  
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
  h <- header(tbl)
  nrh <- nrow(h)
  y_header <- unit(1, "npc") - unit( (1:nrh - .5) *header_line_factor, "lines") 
  for (i in 1:nrow(h)) {
    draw_row(h[[i]], y = y_header[i], TRUE )
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
  draw_point_line <- function(x, ci, y, gp = gpar(col = "blue", fill = "blue")) {
    # line
    if (all(!is.na(ci)) && ci[2] > xlim[1] && ci[1] < xlim[2]) {
      
      # -
      if (ci[1] >= xlim[1] && ci[2] <= xlim[2] ){
        grid.lines(x = unit(c(ci[1], ci[2]), "native"), 
                   y = unit.c(y, y), gp = gp) 
      } else if (ci[1] < xlim[1] && ci[2] > xlim[2] ){
        # <->
        grid.lines(x = unit(ylim, "native"), 
                   y = unit.c(y, y), gp = gp,
                   arrow = arrow(angle = 30, length = unit(0.5, "lines"), ends = "both")) 
      } else if ( ci[1] < xlim[1] && ci[2] <= xlim[2] ){
        # <- 
        grid.lines(x = unit(c(xlim[1], ci[2]), "native"), 
                   y = unit.c(y, y), gp = gp, 
                   arrow = arrow(angle = 30, length = unit(0.5, "lines"), ends = "first")) 
      } else if (ci[1] >= xlim[1] && ci[2] > xlim[2]){
        # ->
        grid.lines(x = unit(c(ci[1], xlim[2]), "native"), 
                   y = unit.c(y, y), gp = gp, 
                   arrow = arrow(angle = 30, length = unit(0.5, "lines"), ends = "last")) 
      }
    }  
    
    if (!is.na(x) && x >= xlim[1] && x <= xlim[2]){
      grid.circle(x = unit(x, "native"), gp = gp,
                  y = y, r = unit(1/3.5, "lines"))
    }
  }
  
  
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
