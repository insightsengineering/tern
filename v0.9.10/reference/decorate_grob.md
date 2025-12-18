# Add titles, footnotes, page Number, and a bounding box to a grid grob

**\[stable\]**

This function is useful to label grid grobs (also `ggplot2`, and
`lattice` plots) with title, footnote, and page numbers.

## Usage

``` r
decorate_grob(
  grob,
  titles,
  footnotes,
  page = "",
  width_titles = grid::unit(1, "npc"),
  width_footnotes = grid::unit(1, "npc"),
  border = TRUE,
  padding = grid::unit(rep(1, 4), "lines"),
  margins = grid::unit(c(1, 0, 1, 0), "lines"),
  outer_margins = grid::unit(c(2, 1.5, 3, 1.5), "cm"),
  gp_titles = grid::gpar(),
  gp_footnotes = grid::gpar(fontsize = 8),
  name = NULL,
  gp = grid::gpar(),
  vp = NULL
)
```

## Arguments

- grob:

  (`grob`)  
  a grid grob object, optionally `NULL` if only a `grob` with the
  decoration should be shown.

- titles:

  (`character`)  
  titles given as a vector of strings that are each separated by a
  newline and wrapped according to the page width.

- footnotes:

  (`character`)  
  footnotes. Uses the same formatting rules as `titles`.

- page:

  (`string` or `NULL`)  
  page numeration. If `NULL` then no page number is displayed.

- width_titles:

  ([`grid::unit`](https://rdrr.io/r/grid/unit.html))  
  width of titles. Usually defined as all the available space
  `grid::unit(1, "npc")`, it is affected by the parameter
  `outer_margins`. Right margins (`outer_margins[4]`) need to be
  subtracted to the allowed width.

- width_footnotes:

  ([`grid::unit`](https://rdrr.io/r/grid/unit.html))  
  width of footnotes. Same default and margin correction as
  `width_titles`.

- border:

  (`flag`)  
  whether a border should be drawn around the plot or not.

- padding:

  ([`grid::unit`](https://rdrr.io/r/grid/unit.html))  
  padding. A unit object of length 4. Innermost margin between the plot
  (`grob`) and, possibly, the border of the plot. Usually expressed in 4
  identical values (usually `"lines"`). It defaults to
  `grid::unit(rep(1, 4), "lines")`.

- margins:

  ([`grid::unit`](https://rdrr.io/r/grid/unit.html))  
  margins. A unit object of length 4. Margins between the plot and the
  other elements in the list (e.g. titles, plot, and footers). This is
  usually expressed in 4 `"lines"`, where the lateral ones are 0s, while
  top and bottom are 1s. It defaults to
  `grid::unit(c(1, 0, 1, 0), "lines")`.

- outer_margins:

  ([`grid::unit`](https://rdrr.io/r/grid/unit.html))  
  outer margins. A unit object of length 4. It defines the general
  margin of the plot, considering also decorations like titles,
  footnotes, and page numbers. It defaults to
  `grid::unit(c(2, 1.5, 3, 1.5), "cm")`.

- gp_titles:

  (`gpar`)  
  a `gpar` object. Mainly used to set different `"fontsize"`.

- gp_footnotes:

  (`gpar`)  
  a `gpar` object. Mainly used to set different `"fontsize"`.

- name:

  a character identifier for the grob. Used to find the grob on the
  display list and/or as a child of another grob.

- gp:

  A `"gpar"` object, typically the output from a call to the function
  [`gpar`](https://rdrr.io/r/grid/gpar.html). This is basically a list
  of graphical parameter settings.

- vp:

  a [`viewport`](https://rdrr.io/r/grid/viewport.html) object (or
  `NULL`).

## Value

A grid grob (`gTree`).

## Details

The titles and footnotes will be ragged, i.e. each title will be wrapped
individually.

## Examples

``` r
library(grid)

titles <- c(
  "Edgar Anderson's Iris Data",
  paste(
    "This famous (Fisher's or Anderson's) iris data set gives the measurements",
    "in centimeters of the variables sepal length and width and petal length",
    "and width, respectively, for 50 flowers from each of 3 species of iris."
  )
)

footnotes <- c(
  "The species are Iris setosa, versicolor, and virginica.",
  paste(
    "iris is a data frame with 150 cases (rows) and 5 variables (columns) named",
    "Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, and Species."
  )
)

## empty plot
grid.newpage()

grid.draw(
  decorate_grob(
    NULL,
    titles = titles,
    footnotes = footnotes,
    page = "Page 4 of 10"
  )
)


# grid
p <- gTree(
  children = gList(
    rectGrob(),
    xaxisGrob(),
    yaxisGrob(),
    textGrob("Sepal.Length", y = unit(-4, "lines")),
    textGrob("Petal.Length", x = unit(-3.5, "lines"), rot = 90),
    pointsGrob(iris$Sepal.Length, iris$Petal.Length, gp = gpar(col = iris$Species), pch = 16)
  ),
  vp = vpStack(plotViewport(), dataViewport(xData = iris$Sepal.Length, yData = iris$Petal.Length))
)
grid.newpage()
grid.draw(p)


grid.newpage()
grid.draw(
  decorate_grob(
    grob = p,
    titles = titles,
    footnotes = footnotes,
    page = "Page 6 of 129"
  )
)


## with ggplot2
library(ggplot2)

p_gg <- ggplot2::ggplot(iris, aes(Sepal.Length, Sepal.Width, col = Species)) +
  ggplot2::geom_point()
p_gg

p <- ggplotGrob(p_gg)
grid.newpage()
grid.draw(
  decorate_grob(
    grob = p,
    titles = titles,
    footnotes = footnotes,
    page = "Page 6 of 129"
  )
)


## with lattice
library(lattice)

xyplot(Sepal.Length ~ Petal.Length, data = iris, col = iris$Species)

p <- grid.grab()

grid.newpage()
grid.draw(
  decorate_grob(
    grob = p,
    titles = titles,
    footnotes = footnotes,
    page = "Page 6 of 129"
  )
)


# with gridExtra - no borders
library(gridExtra)
#> 
#> Attaching package: ‘gridExtra’
#> The following object is masked from ‘package:dplyr’:
#> 
#>     combine
grid.newpage()
grid.draw(
  decorate_grob(
    tableGrob(
      head(mtcars)
    ),
    titles = "title",
    footnotes = "footnote",
    border = FALSE
  )
)

```
