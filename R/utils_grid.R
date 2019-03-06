#' Stack multiple grobs
#'
#' Stack grobs as a new grob with 1 column and multiple rows layout
#'
#' @param ... grobs.
#' @param grobs list of grobs.
#' @param padding unit of length 1, space between each grob.
#' @param vp a \code{\link{viewport}} object (or \code{NULL}).
#' @param name a character identifier for the grob.
#' @param gp A \code{\link{gpar}} object.
#'
#' @export
#'
#' @examples
#' g1 <- circleGrob(gp = gpar(col = "blue"))
#' g2 <- circleGrob(gp = gpar(col = "red"))
#' g3 <- textGrob("TEST TEXT")
#' grid.newpage()
#' grid.draw(stack_grobs(g1, g2, g3))
#'
#' showViewport()
#'
#' grid.newpage()
#' pushViewport(viewport(layout = grid.layout(1,2)))
#' vp1 <- viewport(layout.pos.row = 1, layout.pos.col = 2)
#' grid.draw(stack_grobs(g1, g2, g3, vp = vp1, name = "test"))
#'
#' showViewport()
#' grid.ls(grobs = TRUE, viewports = TRUE)
stack_grobs <- function(..., grobs = list(...), padding = unit(2, "line"), vp = NULL, gp = NULL, name = NULL) {

  if (!all(vapply(grobs, is, logical(1), "grob")))
    stop("not all objects are of class grob")

  if (length(grobs) == 1)
    return(grobs[[1]])

  n_layout <- 2 * length(grobs) - 1
  hts <- lapply(seq(1, n_layout), function(i) if (i %% 2 != 0) unit(1, "null") else padding)
  hts <- do.call("unit.c", hts)

  main_vp <- viewport(
    layout = grid.layout(nrow = n_layout, ncol = 1, heights = hts)
  )

  nested_grobs <- Map(function(g, i) {
    gTree(
      children = gList(g),
      vp = viewport(layout.pos.row = i, layout.pos.col = 1)
    )
  }, grobs, seq_along(grobs) * 2 - 1)

  grobs_mainvp <-   gTree(
    children = do.call("gList", nested_grobs),
    vp = main_vp
  )

  gTree(
    children = gList(grobs_mainvp),
    vp = vp,
    gp = gp,
    name = name
  )
}


#' Arrange multiple grobs
#'
#' Arrange grobs as a new grob with n*m (rows*cols) layout
#'
#' @inheritParams stack_grobs
#' @param ncol number of columns in layout.
#' @param nrow number of rows in layout.
#' @param padding_ht unit of length 1, vertical space between each grob.
#' @param padding_wt unit of length 1, horizontal space between each grob.
#'
#' @import grid
#'
#' @export
#'
#' @examples
#' library(grid)
#'
#' num <- lapply(1:9, textGrob)
#' grid.newpage()
#' grid.draw(arrange_grobs(grobs = num, ncol = 2))
#'
#' showViewport()
#'
#' g1 <- circleGrob(gp = gpar(col = "blue"))
#' g2 <- circleGrob(gp = gpar(col = "red"))
#' g3 <- textGrob("TEST TEXT")
#' grid.newpage()
#' grid.draw(arrange_grobs(g1, g2, g3, nrow = 2))
#'
#' showViewport()
#'
#' grid.newpage()
#' grid.draw(arrange_grobs(g1, g2, g3, ncol = 3))
#'
#' grid.newpage()
#' pushViewport(viewport(layout = grid.layout(1,2)))
#' vp1 <- viewport(layout.pos.row = 1, layout.pos.col = 2)
#' grid.draw(arrange_grobs(g1, g2, g3, ncol = 2, vp = vp1))
#'
#' showViewport()
arrange_grobs <- function(...,
                          grobs = list(...),
                          ncol = NULL, nrow = NULL,
                          padding_ht = unit(2, "line"),
                          padding_wt = unit(2, "line"),
                          vp = NULL,
                          gp = NULL,
                          name = NULL) {
  if (!all(vapply(grobs, is, logical(1), "grob")))
    stop("not all objects are of class grob")

  if (length(grobs) == 1)
    return(grobs[[1]])

  if (is.null(ncol) && is.null(nrow)) {
    ncol <- 1
    nrow <- ceiling(length(grobs) / ncol)
  } else if (!is.null(ncol) && is.null(nrow)) {
    nrow <- ceiling(length(grobs) / ncol)
  } else if (is.null(ncol) && !is.null(nrow)) {
    ncol <- ceiling(length(grobs) / nrow)
  }

  if (ncol * nrow < length(grobs))
    stop("specififed ncol and nrow are not enough for arranging the grobs ")

  if (ncol == 1)
    return(stack_grobs(grobs = grobs, padding = padding_ht, vp = vp, gp = gp, name = name))

  n_col <- 2 * ncol - 1
  n_row <- 2 * nrow - 1
  hts <- lapply(seq(1, n_row), function(i) if (i %% 2 != 0) unit(1, "null") else padding_ht)
  hts <- do.call("unit.c", hts)

  wts <- lapply(seq(1, n_col), function(i) if (i %% 2 != 0) unit(1, "null") else padding_wt)
  wts <- do.call("unit.c", wts)

  main_vp <- viewport(
    layout = grid.layout(nrow = n_row, ncol = n_col, widths = wts, heights = hts)
  )

  nested_grobs <- list()
  k <- 0
  for (i in seq(nrow) * 2 - 1) {
    for (j in seq(ncol) * 2 - 1) {
      k <- k + 1
      if (k <= length(grobs)) {
        nested_grobs <- c(nested_grobs,
                          list(gTree(
                            children = gList(grobs[[k]]),
                            vp = viewport(layout.pos.row = i, layout.pos.col = j)
                          )))
      }
    }
  }
  grobs_mainvp <-   gTree(
    children = do.call("gList", nested_grobs),
    vp = main_vp
  )

  gTree(
    children = gList(grobs_mainvp),
    vp = vp,
    gp = gp,
    name = name
  )
}


#' Draw grob
#' Draw grob on device page
#'
#' @param grob grid object
#' @param newpage draw on a new page
#' @param vp a \code{\link{viewport}} object (or \code{NULL}).
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' rect <- rectGrob(width = unit(0.5, "npc"), height = unit(0.5, "npc"))
#' rect %>% draw_grob(vp = viewport(angle = 45))
#'
#' num <- lapply(1:10, textGrob)
#' num %>%
#'   arrange_grobs(grobs = .) %>%
#'   draw_grob()
#' showViewport()
draw_grob <- function(grob, newpage = TRUE, vp = NULL) {

  if (newpage)
    grid.newpage()
  if (!is.null(vp))
    pushViewport(vp)
  grid.draw(grob)

}


#' Label Panel Grob
#' Create a panel style plotting of labels
#'
#' @param label a vector of plot contents.
#' @param x a vector of xticks for each label to be plotted.
#' @param group a vector of group indicators each label content belongs to. It's a factor.
#' @param name name of grob.
#' @param gp graphical paramter.
#' @param vp a \code{\link{viewport}}.
#'
#' @noRd
#'
#' @examples
#' label <- c("a", "b", "c", "d", "T", "R", "E", "G")
#' x <- rep(1:4, 2)
#' group <- factor(rep(c("GROUP1", "GROUP_new"), each = 4))
#' grid.newpage()
#' grid.draw(label_panel_grob(label, x, group))
#' grid.newpage()
#' pushViewport(plotViewport())
#' grid.draw(label_panel_grob(label, x, group))
#'
#' label <- 1:18
#' x <- c(rep(1:4, 2), rep(1:5, 2))
#' group <- factor(c(rep(c("G1", "G2"), each = 4), rep(c("G3", "G4"), each = 5)))
#' col <- c("black", "blue", "red","green" )
#'
#' g <- label_panel_grob(label, x, group, col)
#' grid.newpage()
#' pushViewport(plotViewport())
#' grid.draw(g)
#'
#' g <- addGrob(g, xaxisGrob(at = unique(x), vp = "labelPlot"))
#' g <- addGrob(g, textGrob(levels(group), x = unit(-3, "lines"),
#'                          y = unit(1:nlevels(group)/(nlevels(group)+1), "npc"),
#'                          just = "left",  gp = gpar(col = col),
#'                          vp = "labelPlot"))
#' grid.newpage()
#' pushViewport(plotViewport(margins = c(5, 4, 4, 2)))
#' grid.draw(g)
label_panel_grob <- function(label,
                             x,
                             group,
                             col = NA,
                             name = NULL,
                             gp = NULL,
                             vp = NULL) {

  if (!is.factor(group))
    stop("group is required to be a factor")
  if (length(label) != length(x))
    stop("lengths of label and x are not equal")
  if (length(label) != length(group))
    stop("lengths of label and group are not equal")
  grp <- levels(group)
  col <- to_group_color(col, grp)

  gTree(
    label = label,
    x = x,
    group = group,
    vp = vp,
    name = name,
    gp = gp,
    childrenvp = dataViewport(xData = x, yData = c(0, 1), name = "labelPlot"),
    children = gList(
      rectGrob(vp = "labelPlot"),
      textGrob(
        label,
        x = unit(x, "native"),
        y = unit(as.numeric(group) / (nlevels(group) + 1), "npc"),
        gp = gpar(col = col[group]),
        vp = "labelPlot"
      )
    ),
    cl = "labelPanel"
  )
}


to_group_color <- function(col, grps) {
  if (is.null(col) || is.null(grps)) {
    stop("col and grps can not be NULL")
  } else if (length(col) == 1) {
    if (is.na(col)) {
      col_pal <- scales::col_factor("Set1", domain = grps)
      col <- col_pal(grps)
    } else {
      col <- rep(col, length(grps))
    }
  } else if (length(col) == length(grps)) {
    col
  } else {
    stop("dimension missmatch")
  }
  names(col) <- grps
  col
}
