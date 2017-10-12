#' Create a Venn Diagram Plot with 2 groups
#' 
#' @param x boolean has biomarker or not
#' @param y boolean has biomarker of not
#' 
#' @export
#' 
#' @return  list with absolute and percentage cross table
#' 
#' 
#' @examples 
#' 
#' n <- 100
#' tGE <- sample(c(TRUE, FALSE), n, replace=TRUE, prob = c(.2, .8))
#' IHC <- sample(c(TRUE, FALSE), n, replace=TRUE, prob = c(.6, .4))
#' 
#' \dontrun{
#' library(atezo.data)
#' ASL <- asl(com.roche.cdpt7722.wo29637.rl)
#' 
#' IC <- ASL$IC %in% c("2", "3")
#' TC <- ASL$TC %in% c(2, 3)
#' 
#' x <- venn2(x = IC, y = TC)
#' plot(x)
#' 
#' venn2(x = IC, y = TC, "biomarker IT", "biamrker TC")
#' 
#' }
#' 
venn2 <- function(x, y, xlab, ylab) {

  
  if (missing(xlab)) xlab <- deparse(substitute(x))
  if (missing(ylab)) ylab <- deparse(substitute(y))
  
  if (length(x) != length(y)) stop("x and y need to be of the same length")
  if (!is.logical(x) || !is.logical(y)) stop("x and y need to be boolean")
  
  # what to do with NA?
  if (any(is.na(c(x, y)))) stop("can currently not deal with NA")
  
  abs <- table(x, y)
  per <- abs/length(x)
  
  structure(list(absolute = abs, perentage = per, xlab = xlab, ylab = ylab), class = "venn2")
}

#' plot venn2 object
#' 
#' @param x an object returned by \code{\link{venn2}}
#' 
#' @import grid
#' @export 
plot.venn2 <- function(x, ...) {
  
  abs <- x$absolute
  per <- apply(x$perentage, c(1,2), function(xi) round(xi*100,1))
  

  
  # solve for radius of circles using area
  
  ax <- sqrt((abs[1,2]+abs[2,2])/pi) #radius of 1st circle
  ay <- sqrt((abs[2,1]+abs[2,2])/pi) #radius of 2nd circle
  
  #solve for d, the distance between the 2 centers of the cicles
  
  d_solve <- uniroot(function(d) ay^2*acos((d^2+ay^2-ax^2)/(2*d*ay)) 
                     + ax^2*acos((d^2+ax^2-ay^2)/(2*d*ax)) 
                     - 1/2 * sqrt((-d+ay+ax)*(d+ay-ax)*(d-ay+ax)*(d+ay+ax))-abs[2,2], 
                     lower=abs(ax-ay)+1e-9, upper=ax+ay-1e-9,tol = 1e-9)$root
  
  # solve for a (the cord connecting the cusps of the lens)
  
  a <- 1/d_solve * sqrt((-d_solve+ay+ax)*(d_solve+ay-ax)*(d_solve-ay+ax)*(d_solve+ay+ax))
  
  # find dx and dy using pythagorean theorm
  # dx and dy are distances from center of cusp to the respective centers of the circles
  # sacle d and r to viewport width, making 2x diameter 90% width of viewport

  min_side <- unit(1, "snpc")
  
  dx_num <- sqrt(ax^2-(a/2)^2)
  dy_num <- sqrt(ay^2-(a/2)^2)
  
  dx <- dx_num/(2*(ax+ay)) * min_side
  dy <- dy_num/(2*(ax+ay)) * min_side
  
  rx <- ax/(2*(ax+ay)) * min_side
  ry <- ay/(2*(ax+ay)) * min_side

  # draw graphic
  grid.newpage()
  
  pushViewport(plotViewport(margins = c(2,2,2,2))) # add margins
  grid.rect()
  
  # helper lines
  # grid.lines(x = c(0, 1), y = c(.5, .5), default.units = "npc")
  # grid.lines(x =  c(.5, .5), y =c(0, 1), default.units = "npc")

  #draw circles
  
  grid.circle(x = unit(0.5, "npc") - dx, y = unit(0.5, "npc"), r = rx,
              gp = gpar(fill = "thistle", alpha = .4))
  grid.circle(x = unit(0.5, "npc") + dy, y = unit(0.5, "npc"), r = ry,
              gp = gpar(fill = "orange", alpha = .4))
  
  
  # add labels
  
  grid.text(
    x$xlab,
    x = unit(0.5, "npc") - dx - 1.2 * cos(pi/4) * rx ,
    y = unit(0.5, "npc") - 1.2 * sin(pi/4) * rx ,
    just = c("right", "center")
  )
  grid.text(
    x$ylab,
    x = unit(0.5, "npc") + dy + 1.2 * cos(pi/4) * ry,
    y = unit(0.5, "npc") - 1.2 * sin(pi/4) * ry ,
    just = c("left", "center")
  )

  grid.text(
    paste0(abs[1,1],"\n(",per[1,1],"%)"),
    x = unit(0.5, "npc"),
    y = unit(0.5, "npc") + max(rx, ry) + unit(2, "lines"),
    just = c("center", "center"),
    gp = gpar(lineheight = .9)
  )

  grid.text(
    paste0(abs[2,2],"\n(",per[2,2],"%)"),
    x = unit(0.5, "npc"),
    y = unit(0.5, "npc"),
    just = c("center", "center"),
    gp = gpar(lineheight = .9)
  )
  
  grid.text(
    paste0(abs[2,1],"\n(",per[2,1],"%)"),
    x = unit(0.5, "npc") - ax/(2*(ax+ay))*min_side,
    y = unit(0.5, "npc"),
    just = c("center", "center"),
    gp = gpar(lineheight = .9)
  )
  
  grid.text(
    paste0(abs[1,2],"\n(",per[1,2],"%)"),
    x = unit(0.5, "npc") + ay/(2*(ax+ay)) * min_side,
    y = unit(0.5, "npc"),
    just = c("center", "center"),
    gp = gpar(lineheight = .9)
  )
  
}