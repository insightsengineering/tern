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
#' 
#' x = c(rep(T,5), rep(F,3), rep(T, 4), rep(F, 9))
#' y = c(rep(T,5), rep(F,3), rep(F, 4), rep(T, 9))
#' table(x,y)
#' y <- venn2(x,y, "X", "Y")
#' plot(y)
#' 
#' 
#' # if too few then table is plotted
#' x = c(F, F,F,F, T,T)
#' y = c(F, T,T,T, F,F)
#' table(x,y)
#' y <- venn2(x, y, "X", "Y")
#' plot(y)
#' 
venn2 <- function(x, y, xlab, ylab) {


  if (length(x) <= 0) stop("lenght of x must be > 0")
  
  if (missing(xlab)) xlab <- deparse(substitute(x))
  if (missing(ylab)) ylab <- deparse(substitute(y))
  
  if (length(x) != length(y)) stop("x and y need to be of the same length")
  if (!is.logical(x) || !is.logical(y)) stop("x and y need to be boolean")
  
  # what to do with NA?
  if (any(is.na(c(x, y)))) stop("can currently not deal with NA")

  x <- factor(x, levels = c(FALSE, TRUE))
  y <- factor(y, levels = c(FALSE, TRUE))
    
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
  
  if (!all(dim(abs) == c(2,2))) stop("dimension of x$absolute is not 2x2")
  
  
  label_xy <- paste0(abs[1,1],"\n(",per[1,1],"%)")
  label_XY <- paste0(abs[2,2],"\n(",per[2,2],"%)")
  label_xY <- paste0(abs[1,2],"\n(",per[1,2],"%)")
  label_Xy <- paste0(abs[2,1],"\n(",per[2,1],"%)")


  plot_grob <- if (any(abs<=2)) {
    ## plot a table
    
    labels <- c(label_xy, label_Xy, label_XY, label_xY)
    w2 <- .5 * do.call(max, lapply(labels, stringWidth)) 
    h2 <- .5 * do.call(max, lapply(labels, stringHeight))
    
    u1 <- unit(1, "lines")
    
    gTree(
      children = gList(
        textGrob(label_xy,
                  x = unit(0.5, "npc") - (w2 + u1),
                  y = unit(0.5, "npc") + (h2 + u1)),
        textGrob(label_Xy,
                  x = unit(0.5, "npc") - (w2 + u1),
                  y = unit(0.5, "npc") - (h2 + u1)),
        textGrob(label_xY,
                  x = unit(0.5, "npc") + (w2 + u1),
                  y = unit(0.5, "npc") + (h2 + u1)),
        textGrob(label_XY,
                  x = unit(0.5, "npc") + (w2 + u1),
                  y = unit(0.5, "npc") - (h2 + u1)),
        textGrob(x$xlab,
                  y = unit(.5, "npc") + 2 * (h2 + u1),
                  gp = gpar(fontface = "bold")),
        textGrob(x$ylab,
                  x = unit(.5, "npc") - 2 * (w2 + u1),
                  gp = gpar(fontface = "bold"), rot = 90)
      )
    )

    
  } else {
    ## plot venn diagram
    
    # solve for radius of circles using area
    
    ax <- sqrt((abs[2,1]+abs[2,2])/pi) #radius of 1st circle
    ay <- sqrt((abs[1,2]+abs[2,2])/pi) #radius of 2nd circle
    
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

    gTree(
      children = gList(
        #draw circles
        circleGrob(x = unit(0.5, "npc") - dx, y = unit(0.5, "npc"), r = rx,
                    gp = gpar(fill = "thistle", alpha = .4)),
        circleGrob(x = unit(0.5, "npc") + dy, y = unit(0.5, "npc"), r = ry,
                    gp = gpar(fill = "orange", alpha = .4)),
        # add labels
        textGrob(
          x$xlab,
          x = unit(0.5, "npc") - dx - 1.2 * cos(pi/4) * rx ,
          y = unit(0.5, "npc") - 1.2 * sin(pi/4) * rx ,
          just = c("right", "center")
        ),
        textGrob(
          x$ylab,
          x = unit(0.5, "npc") + dy + 1.2 * cos(pi/4) * ry,
          y = unit(0.5, "npc") - 1.2 * sin(pi/4) * ry ,
          just = c("left", "center")
        ),
        textGrob(
          label_xy,
          x = unit(0.5, "npc"),
          y = unit(0.5, "npc") + max(rx, ry) + unit(2, "lines"),
          just = c("center", "center"),
          gp = gpar(lineheight = .9)
        ),
        textGrob(
          label_XY,
          x = unit(0.5, "npc"),
          y = unit(0.5, "npc"),
          just = c("center", "center"),
          gp = gpar(lineheight = .9)
        ),
        textGrob(
          label_Xy,
          x = unit(0.5, "npc") - ax/(2*(ax+ay))*min_side,
          y = unit(0.5, "npc"),
          just = c("center", "center"),
          gp = gpar(lineheight = .9)
        ),
        textGrob(
          label_xY,
          x = unit(0.5, "npc") + ay/(2*(ax+ay)) * min_side,
          y = unit(0.5, "npc"),
          just = c("center", "center"),
          gp = gpar(lineheight = .9)
        )   
      )
    )
  }

  


  # draw graphic
  grid.newpage()
  pushViewport(plotViewport(margins = c(2,2,2,2))) # add margins
  grid.rect()
  grid.draw(plot_grob)
  
  # helper lines
  # grid.lines(x = c(0, 1), y = c(.5, .5), default.units = "npc")
  # grid.lines(x =  c(.5, .5), y =c(0, 1), default.units = "npc")

  

  
}



#' Venn2 teal module
#' 
#' @export
#' 
#' @examples  
#' 
#' N <- 100
#' 
#' var_biomarkers <- paste0("B", 1:10) 
#' sample_bm_data <- lapply(1:10, function(x)sample(c(TRUE, FALSE), N, replace = TRUE))
#' names(sample_bm_data) <- var_biomarkers
#' 
#' ASL <- do.call(data.frame, c(
#'   list(USUBJID = paste("ID", 1:N),STUDYID = "1"), sample_bm_data
#' ))
#' 
#' x <- teal::init(
#'   data = list(ASL = ASL),
#'   modules = root_modules(
#'     tm_variable_browser(),
#'     tm_data_table(),
#'     tm_venn2("Venn Diagram", "ASL", "B1", "B2", var_biomarkers, var_biomarkers)
#'   )
#' )   
#' shinyApp(x$ui, x$server)     
tm_venn2 <- function(label, dataname, bm1_var, bm2_var,
                     bm1_var_choices = bm1_var,
                     bm2_var_choices = bm2_var,
                     plot_height = c(600, 200, 2000),
                     alpha = c(1, 0, 1),
                     pre_output = NULL, post_output = NULL) {
  
  args <- as.list(environment())
  
  module(
    label = label,
    server = srv_venn2,
    ui = ui_venn2,
    ui_args = args,
    server_args = list(dataname = dataname),
    filters = dataname
  )
}

ui_venn2 <- function(id, label, dataname, bm1_var, bm2_var,
                     bm1_var_choices,
                     bm2_var_choices,
                     plot_height,
                     alpha,
                     pre_output,
                     post_output) {
  ns <- NS(id)
  
  standard_layout(
    output = uiOutput(ns("plot_ui")),
    encoding = div(
      tags$label("Encodings", class="text-primary"),
      helpText("Analysis data:", tags$code(dataname)),
      optionalSelectInput(ns("bm1_var"), "Biomarker 1", bm1_var_choices, bm1_var, multiple = FALSE),
      optionalSelectInput(ns("bm2_var"), "Biomarker 2", bm2_var_choices, bm2_var, multiple = FALSE),
      if (all(c(
        length(plot_height) == 1,
        length(alpha) == 1
      ))) {
        NULL
      } else {
        tags$label("Plot Settings", class="text-primary", style="margin-top: 15px;")
      },
      optionalSliderInputValMinMax(ns("plot_height"), "plot height", plot_height, ticks = FALSE),
      optionalSliderInputValMinMax(ns("alpha"), "opacity", alpha, ticks = FALSE)
    ),
    #forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = pre_output,
    post_output = post_output
  )
} 

srv_venn2 <- function(input, output, session, datasets, dataname) {
  
  ## dynamic plot height
  output$plot_ui <- renderUI({
    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))
    plotOutput(session$ns("scatterplot"), height=plot_height)
  })
  
  output$scatterplot <- renderPlot({
    
    ANL <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)
    
    validate(need(!is.null(ANL) && is.data.frame(ANL), "no data left"))
    validate(need(nrow(ANL) > 0 , "no observations left"))
    
    
    bm1_var <- input$bm1_var
    bm2_var <- input$bm2_var
    alpha <- input$alpha

    validate(need(bm1_var != bm2_var, "Please choose different Biomarker 1 and 2"))
    
    bm1 <- ANL[[bm1_var]]
    bm2 <- ANL[[bm2_var]]
    
    validate(need(!is.null(bm1), "biomarker 1 does not exist"))
    validate(need(!is.null(bm2), "biomarker 2 does not exist"))
    
    x <- venn2(bm1, bm2, bm1_var, bm2_var)
    
    plot(x)
  })
}

