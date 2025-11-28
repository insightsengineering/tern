# Decorate set of `grob`s and add page numbering

**\[stable\]**

Note that this uses the
[`decorate_grob_factory()`](https://insightsengineering.github.io/tern/reference/decorate_grob_factory.md)
function.

## Usage

``` r
decorate_grob_set(grobs, ...)
```

## Arguments

- grobs:

  (`list` of `grob`)  
  a list of grid grobs.

- ...:

  arguments passed on to
  [`decorate_grob()`](https://insightsengineering.github.io/tern/reference/decorate_grob.md).

## Value

A decorated grob.

## Examples

``` r
library(ggplot2)
library(grid)
g <- with(data = iris, {
  list(
    ggplot2::ggplotGrob(
      ggplot2::ggplot(mapping = aes(Sepal.Length, Sepal.Width, col = Species)) +
        ggplot2::geom_point()
    ),
    ggplot2::ggplotGrob(
      ggplot2::ggplot(mapping = aes(Sepal.Length, Petal.Length, col = Species)) +
        ggplot2::geom_point()
    ),
    ggplot2::ggplotGrob(
      ggplot2::ggplot(mapping = aes(Sepal.Length, Petal.Width, col = Species)) +
        ggplot2::geom_point()
    ),
    ggplot2::ggplotGrob(
      ggplot2::ggplot(mapping = aes(Sepal.Width, Petal.Length, col = Species)) +
        ggplot2::geom_point()
    ),
    ggplot2::ggplotGrob(
      ggplot2::ggplot(mapping = aes(Sepal.Width, Petal.Width, col = Species)) +
        ggplot2::geom_point()
    ),
    ggplot2::ggplotGrob(
      ggplot2::ggplot(mapping = aes(Petal.Length, Petal.Width, col = Species)) +
        ggplot2::geom_point()
    )
  )
})
lg <- decorate_grob_set(grobs = g, titles = "Hello\nOne\nTwo\nThree", footnotes = "")

draw_grob(lg[[1]])
#> Warning: `draw_grob()` was deprecated in tern 0.9.4.
#> ℹ `tern` plotting functions no longer generate `grob` objects.

draw_grob(lg[[2]])

draw_grob(lg[[6]])

```
