

tern_grob <- function(x) {
  class(x) <- unique(c("ternGrob", class(x)))
  x
}

print.ternGrob <- function(x, ...) {
  grid.newpage()
  grid.draw(x)
}