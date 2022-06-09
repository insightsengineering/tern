#' Stack Multiple Grobs
#'
#' Stack grobs as a new grob with 1 column and multiple rows layout.
#'
#' @description `r lifecycle::badge("stable")`
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
#' library(grid)
#'
#' g1 <- circleGrob(gp = gpar(col = "blue"))
#' g2 <- circleGrob(gp = gpar(col = "red"))
#' g3 <- textGrob("TEST TEXT")
#' grid.newpage()
#' grid.draw(stack_grobs(g1, g2, g3))
#'
#' showViewport()
#'
#' grid.newpage()
#' pushViewport(viewport(layout = grid.layout(1, 2)))
#' vp1 <- viewport(layout.pos.row = 1, layout.pos.col = 2)
#' grid.draw(stack_grobs(g1, g2, g3, vp = vp1, name = "test"))
#'
#' showViewport()
#' grid.ls(grobs = TRUE, viewports = TRUE)
stack_grobs <- function(...,
                        grobs = list(...),
                        padding = grid::unit(2, "line"),
                        vp = NULL,
                        gp = NULL,
                        name = NULL) {
  stopifnot(all(vapply(grobs, grid::is.grob, logical(1))))

  if (length(grobs) == 1) {
    return(grobs[[1]])
  }

  n_layout <- 2 * length(grobs) - 1
  hts <- lapply(
    seq(1, n_layout),
    function(i) {
      if (i %% 2 != 0) {
        grid::unit(1, "null")
      } else {
        padding
      }
    }
  )
  hts <- do.call(grid::unit.c, hts)

  main_vp <- grid::viewport(
    layout = grid::grid.layout(nrow = n_layout, ncol = 1, heights = hts)
  )

  nested_grobs <- Map(function(g, i) {
    grid::gTree(
      children = grid::gList(g),
      vp = grid::viewport(layout.pos.row = i, layout.pos.col = 1)
    )
  }, grobs, seq_along(grobs) * 2 - 1)

  grobs_mainvp <- grid::gTree(
    children = do.call(grid::gList, nested_grobs),
    vp = main_vp
  )

  grid::gTree(
    children = grid::gList(grobs_mainvp),
    vp = vp,
    gp = gp,
    name = name
  )
}


#' Arrange Multiple Grobs
#'
#' Arrange grobs as a new grob with \verb{n*m (rows*cols)} layout.
#'
#' @inheritParams stack_grobs
#' @param ncol number of columns in layout.
#' @param nrow number of rows in layout.
#' @param padding_ht unit of length 1, vertical space between each grob.
#' @param padding_wt unit of length 1, horizontal space between each grob.
#'
#' @export
#'
#' @examples
#' library(grid)
#'
#' num <- lapply(1:9, textGrob)
#' grid::grid.newpage()
#' grid.draw(arrange_grobs(grobs = num, ncol = 2))
#'
#' showViewport()
#'
#' g1 <- circleGrob(gp = gpar(col = "blue"))
#' g2 <- circleGrob(gp = gpar(col = "red"))
#' g3 <- textGrob("TEST TEXT")
#' grid::grid.newpage()
#' grid.draw(arrange_grobs(g1, g2, g3, nrow = 2))
#'
#' showViewport()
#'
#' grid::grid.newpage()
#' grid.draw(arrange_grobs(g1, g2, g3, ncol = 3))
#'
#' grid::grid.newpage()
#' grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, 2)))
#' vp1 <- grid::viewport(layout.pos.row = 1, layout.pos.col = 2)
#' grid.draw(arrange_grobs(g1, g2, g3, ncol = 2, vp = vp1))
#'
#' showViewport()
arrange_grobs <- function(..., # nolint
                          grobs = list(...),
                          ncol = NULL, nrow = NULL,
                          padding_ht = grid::unit(2, "line"),
                          padding_wt = grid::unit(2, "line"),
                          vp = NULL,
                          gp = NULL,
                          name = NULL) {
  stopifnot(all(vapply(grobs, grid::is.grob, logical(1))))

  if (length(grobs) == 1) {
    return(grobs[[1]])
  }

  if (is.null(ncol) && is.null(nrow)) {
    ncol <- 1
    nrow <- ceiling(length(grobs) / ncol)
  } else if (!is.null(ncol) && is.null(nrow)) {
    nrow <- ceiling(length(grobs) / ncol)
  } else if (is.null(ncol) && !is.null(nrow)) {
    ncol <- ceiling(length(grobs) / nrow)
  }

  if (ncol * nrow < length(grobs)) {
    stop("specififed ncol and nrow are not enough for arranging the grobs ")
  }

  if (ncol == 1) {
    return(stack_grobs(grobs = grobs, padding = padding_ht, vp = vp, gp = gp, name = name))
  }

  n_col <- 2 * ncol - 1
  n_row <- 2 * nrow - 1
  hts <- lapply(
    seq(1, n_row),
    function(i) {
      if (i %% 2 != 0) {
        grid::unit(1, "null")
      } else {
        padding_ht
      }
    }
  )
  hts <- do.call(grid::unit.c, hts)

  wts <- lapply(
    seq(1, n_col),
    function(i) {
      if (i %% 2 != 0) {
        grid::unit(1, "null")
      } else {
        padding_wt
      }
    }
  )
  wts <- do.call(grid::unit.c, wts)

  main_vp <- grid::viewport(
    layout = grid::grid.layout(nrow = n_row, ncol = n_col, widths = wts, heights = hts)
  )

  nested_grobs <- list()
  k <- 0
  for (i in seq(nrow) * 2 - 1) {
    for (j in seq(ncol) * 2 - 1) {
      k <- k + 1
      if (k <= length(grobs)) {
        nested_grobs <- c(
          nested_grobs,
          list(grid::gTree(
            children = grid::gList(grobs[[k]]),
            vp = grid::viewport(layout.pos.row = i, layout.pos.col = j)
          ))
        )
      }
    }
  }
  grobs_mainvp <- grid::gTree(
    children = do.call(grid::gList, nested_grobs),
    vp = main_vp
  )

  grid::gTree(
    children = grid::gList(grobs_mainvp),
    vp = vp,
    gp = gp,
    name = name
  )
}


#' Draw `grob`
#'
#' Draw grob on device page.
#'
#' @param grob grid object
#' @param newpage draw on a new page
#' @param vp a \code{\link{viewport}} object (or \code{NULL}).
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(grid)
#'
#' rect <- rectGrob(width = grid::unit(0.5, "npc"), height = grid::unit(0.5, "npc"))
#' rect %>% draw_grob(vp = grid::viewport(angle = 45))
#'
#' num <- lapply(1:10, textGrob)
#' num %>%
#'   arrange_grobs(grobs = .) %>%
#'   draw_grob()
#' showViewport()
draw_grob <- function(grob, newpage = TRUE, vp = NULL) {
  if (newpage) {
    grid::grid.newpage()
  }
  if (!is.null(vp)) {
    grid::pushViewport(vp)
  }
  grid::grid.draw(grob)
}

tern_grob <- function(x) { # nolint
  class(x) <- unique(c("ternGrob", class(x)))
  x
}

print.ternGrob <- function(x, ...) { # nolint
  grid::grid.newpage()
  grid::grid.draw(x)
}
