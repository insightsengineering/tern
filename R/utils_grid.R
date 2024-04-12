#' Stack multiple grobs
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#' Stack grobs as a new grob with 1 column and multiple rows layout.
#'
#' @param ... grobs.
#' @param grobs (`list` of `grob`)\cr a list of grobs.
#' @param padding (`grid::unit`)\cr unit of length 1, space between each grob.
#' @param vp (`viewport` or `NULL`)\cr a [viewport()] object (or `NULL`).
#' @param name (`string`)\cr a character identifier for the grob.
#' @param gp (`gpar`)\cr a [gpar()] object.
#'
#' @return A `grob`.
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
#' grid.ls(grobs = TRUE, viewports = TRUE, print = FALSE)
#'
#' @export
stack_grobs <- function(...,
                        grobs = list(...),
                        padding = grid::unit(2, "line"),
                        vp = NULL,
                        gp = NULL,
                        name = NULL) {
  lifecycle::deprecate_warn(
    "0.9.4",
    "stack_grobs()",
    details = "`tern` plotting functions no longer generate `grob` objects."
  )

  checkmate::assert_true(
    all(vapply(grobs, grid::is.grob, logical(1)))
  )

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

#' Arrange multiple grobs
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#' Arrange grobs as a new grob with `n * m (rows * cols)` layout.
#'
#' @inheritParams stack_grobs
#' @param ncol (`integer(1)`)\cr number of columns in layout.
#' @param nrow (`integer(1)`)\cr number of rows in layout.
#' @param padding_ht (`grid::unit`)\cr unit of length 1, vertical space between each grob.
#' @param padding_wt (`grid::unit`)\cr unit of length 1, horizontal space between each grob.
#'
#' @return A `grob`.
#'
#' @examples
#' library(grid)
#'
#' \donttest{
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
#' }
#' @export
arrange_grobs <- function(...,
                          grobs = list(...),
                          ncol = NULL, nrow = NULL,
                          padding_ht = grid::unit(2, "line"),
                          padding_wt = grid::unit(2, "line"),
                          vp = NULL,
                          gp = NULL,
                          name = NULL) {
  lifecycle::deprecate_warn(
    "0.9.4",
    "arrange_grobs()",
    details = "`tern` plotting functions no longer generate `grob` objects."
  )

  checkmate::assert_true(
    all(vapply(grobs, grid::is.grob, logical(1)))
  )

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
#' @description `r lifecycle::badge("deprecated")`
#'
#' Draw grob on device page.
#'
#' @param grob (`grob`)\cr grid object.
#' @param newpage (`flag`)\cr draw on a new page.
#' @param vp (`viewport` or `NULL`)\cr a [viewport()] object (or `NULL`).
#'
#' @return A `grob`.
#'
#' @examples
#' library(dplyr)
#' library(grid)
#'
#' \donttest{
#' rect <- rectGrob(width = grid::unit(0.5, "npc"), height = grid::unit(0.5, "npc"))
#' rect %>% draw_grob(vp = grid::viewport(angle = 45))
#'
#' num <- lapply(1:10, textGrob)
#' num %>%
#'   arrange_grobs(grobs = .) %>%
#'   draw_grob()
#' showViewport()
#' }
#'
#' @export
draw_grob <- function(grob, newpage = TRUE, vp = NULL) {
  lifecycle::deprecate_warn(
    "0.9.4",
    "draw_grob()",
    details = "`tern` plotting functions no longer generate `grob` objects."
  )

  if (newpage) {
    grid::grid.newpage()
  }
  if (!is.null(vp)) {
    grid::pushViewport(vp)
  }
  grid::grid.draw(grob)
}

tern_grob <- function(x) {
  class(x) <- unique(c("ternGrob", class(x)))
  x
}

#' @keywords internal
print.ternGrob <- function(x, ...) {
  grid::grid.newpage()
  grid::grid.draw(x)
}
