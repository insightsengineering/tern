# Stack multiple grobs

**\[deprecated\]**

Stack grobs as a new grob with 1 column and multiple rows layout.

## Usage

``` r
stack_grobs(
  ...,
  grobs = list(...),
  padding = grid::unit(2, "line"),
  vp = NULL,
  gp = NULL,
  name = NULL
)
```

## Arguments

- ...:

  grobs.

- grobs:

  (`list` of `grob`)  
  a list of grobs.

- padding:

  ([`grid::unit`](https://rdrr.io/r/grid/unit.html))  
  unit of length 1, space between each grob.

- vp:

  (`viewport` or `NULL`)  
  a [`viewport()`](https://rdrr.io/r/grid/viewport.html) object (or
  `NULL`).

- gp:

  (`gpar`)  
  a [`gpar()`](https://rdrr.io/r/grid/gpar.html) object.

- name:

  (`string`)  
  a character identifier for the grob.

## Value

A `grob`.

## Examples

``` r
library(grid)

g1 <- circleGrob(gp = gpar(col = "blue"))
g2 <- circleGrob(gp = gpar(col = "red"))
g3 <- textGrob("TEST TEXT")
grid.newpage()
grid.draw(stack_grobs(g1, g2, g3))

showViewport()


grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
vp1 <- viewport(layout.pos.row = 1, layout.pos.col = 2)
grid.draw(stack_grobs(g1, g2, g3, vp = vp1, name = "test"))

showViewport()

grid.ls(grobs = TRUE, viewports = TRUE, print = FALSE)
```
