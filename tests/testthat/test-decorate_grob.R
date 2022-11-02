testthat::test_that("decorate_grob returns no warnings when creating an empty plot", {
  titles <- "Edgar Anderson's Iris Data"
  footnotes <- "The species are Iris setosa, versicolor, and virginica."

  grid::grid.newpage()
  testthat::expect_silent(grid::grid.draw(
    decorate_grob(
      NULL,
      titles = titles,
      footnotes = footnotes,
      page = "Page 4 of 10",
      outer_margins = c(1, 1, 1, 1),
      margins = c(1, 1, 1, 1),
      padding = c(1, 1, 1, 1)
    )
  ))
})

testthat::test_that("decorate_grob returns no warnings when creating a non-empty plot", {
  titles <- "Edgar Anderson's Iris Data"
  footnotes <- "The species are Iris setosa, versicolor, and virginica."
  p <- grid::gTree(
    children = grid::gList(
      grid::rectGrob(),
      grid::xaxisGrob(),
      grid::yaxisGrob(),
      grid::textGrob("Sepal.Length", y = grid::unit(-4, "lines")),
      grid::textGrob("Petal.Length", x = grid::unit(-3.5, "lines"), rot = 90),
      grid::pointsGrob(iris$Sepal.Length, iris$Petal.Length, gp = grid::gpar(col = iris$Species), pch = 16)
    ),
    vp = grid::vpStack(grid::plotViewport(), grid::dataViewport(xData = iris$Sepal.Length, yData = iris$Petal.Length))
  )
  grid::grid.newpage()
  testthat::expect_silent(grid::grid.draw(
    decorate_grob(
      grob = p,
      titles = titles,
      footnotes = footnotes,
      page = "Page 6 of 129"
    )
  ))
})

testthat::test_that("split_string works with default settings", {
  result <- split_string(
    "The species are Iris setosa, versicolor, and virginica.",
    width = grid::unit(3, "cm")
  )
  expected <- "The species\nare Iris setosa,\nversicolor, and\nvirginica."
  testthat::expect_identical(result, expected)
})

testthat::test_that("decorate_grob_factory returns page warning correctly", {
  pf <- decorate_grob_factory(
    titles = "This is a test\nHello World",
    footnotes = "Here belong the footnotess",
    npages = 0
  )
  testthat::expect_error(draw_grob(pf(NULL)), "current page is 1 but max. 0 specified.")
})

testthat::test_that("decorate_grob_set returns no warnings when creating a non-empty plot", {
  g <- with(iris, {
    list(
      ggplot2::ggplotGrob(ggplot2::qplot(Sepal.Length, Sepal.Width, col = Species)),
      ggplot2::ggplotGrob(ggplot2::qplot(Sepal.Length, Petal.Length, col = Species)),
      ggplot2::ggplotGrob(ggplot2::qplot(Sepal.Length, Petal.Width, col = Species)),
      ggplot2::ggplotGrob(ggplot2::qplot(Sepal.Width, Petal.Length, col = Species)),
      ggplot2::ggplotGrob(ggplot2::qplot(Sepal.Width, Petal.Width, col = Species)),
      ggplot2::ggplotGrob(ggplot2::qplot(Petal.Length, Petal.Width, col = Species))
    )
  })
  lg <- testthat::expect_silent(
    decorate_grob_set(grobs = g, titles = "Hello\nOne\nTwo\nThree", footnotes = "")
  )
  testthat::expect_silent(draw_grob(lg[[1]]))
})
