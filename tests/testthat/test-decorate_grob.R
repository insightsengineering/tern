testthat::test_that("decorate_grob returns no warnings when creating an empty plot", {
  titles <- "Edgar Anderson's Iris Data"
  footnotes <- "The species are Iris setosa, versicolor, and virginica."
  all_ones <- grid::unit(c(1, 1, 1, 1), "cm")

  grid::grid.newpage()
  testthat::expect_silent(
    grid::grid.draw(
      decorate_grob(
        NULL,
        titles = titles,
        footnotes = footnotes,
        page = "Page 4 of 10",
        outer_margins = all_ones,
        margins = all_ones,
        padding = all_ones
      )
    )
  )
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
  testthat::expect_silent(
    grid::grid.draw(
      decorate_grob(
        grob = p,
        titles = titles,
        footnotes = footnotes,
        page = "Page 6 of 129"
      )
    )
  )
})

testthat::test_that("split_string works with default settings", {
  res <- split_string(
    "The species are Iris setosa, versicolor, and virginica.",
    width = grid::unit(4, "cm")
  )

  testthat::expect_snapshot(res)
})

testthat::test_that("decorate_grob_factory returns page warning correctly", {
  pf <- decorate_grob_factory(
    titles = "This is a test\nHello World",
    footnotes = "Here belong the footnotess",
    npages = 0
  )
  suppressWarnings(testthat::expect_error(draw_grob(pf(NULL)), "current page is 1 but max. 0 specified."))
})

testthat::test_that("decorate_grob_set returns no warnings when creating a non-empty plot", {
  g <- withr::with_options(
    opts_partial_match_old,
    with(data = iris, {
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
  )
  testthat::expect_silent(
    lg <- decorate_grob_set(grobs = g, titles = "Hello\nOne\nTwo\nThree", footnotes = "")
  )
  testthat::expect_warning(draw_grob(lg[[1]]))
})

testthat::test_that("text wrapping works as expected", {
  g <- ggplot2::ggplot(iris) +
    ggplot2::geom_point(aes(x = Sepal.Length, y = Sepal.Width))

  testthat::expect_silent(
    deco_grob_text_wrap <- decorate_grob(
      grob = ggplot2::ggplotGrob(g),
      titles = paste(
        "this is title that is very long dasd asdas dasljdklasjdklasjlk dakldsj akldjakls jkald jaklsj dklsajklaj",
        "skldajkl jsakldjal jsadlk dasj lasjdlkasjkl ajskld asl jalksjd lkasjlk alkj dlkadlka sjd lakjsdl a"
      ),
      footnotes = paste(
        "this is footnotes that is super super supre long long asdad as dasd ad ada ad asdadkhasdalksjdlkaj kdlajskl",
        "dsajlkd ajldja lkdjas jdklas jdkasj dlasl;jd klasjdkl aldja lkjdlkaj lkfjalksd a"
      ),
      page = "Page 1 of 10"
    )
  )

  expect_snapshot_ggplot(title = "deco_grob_text_wrap", fig = deco_grob_text_wrap, width = 10, height = 8)
})

testthat::test_that("Edge cases work for titles and footers in split_text_grob", {
  # regression test #1254
  testthat::expect_silent(
    split_text_grob(NULL)
  )
  testthat::expect_silent(
    split_text_grob("")
  )
  testthat::expect_silent(
    split_text_grob(c("", NA))
  )
  testthat::expect_silent(
    split_text_grob(NA)
  )
  testthat::expect_silent(
    split_text_grob(c("", "a a"))
  )
})

testthat::test_that("Wrapping works consistently", {
  # ggplot
  g <- ggplot2::ggplot(iris) +
    ggplot2::geom_point(aes(x = Sepal.Length, y = Sepal.Width))

  # decoration text
  eg_text <- c(
    paste( # titles
      rep("issues come in long pairs", 10),
      collapse = " "
    ),
    c( # subtitles
      "something\nwith\\n", "", "and such"
    )
  )
  # example width (it is default for A4 with 1.5cm margin)
  eg_width <- grid::unit(11.63, "inches") - grid::unit(1.5, "cm")

  # Main call to text grob split
  out <- split_text_grob(eg_text,
    x = 0, y = 1,
    just = c("left", "top"),
    width = eg_width,
    vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1),
    gp = grid::gpar()
  )

  # This is what (roughly w/o font correction from gpar) happens inside the split
  eg_width <- grid::convertUnit(eg_width, "npc")
  # Fix for split_string in case of residual \n (otherwise is counted as character)
  text_fin <- split_string( # copied fnc (NOT formatters')
    unlist(
      strsplit(
        paste0(gsub("\\\\n", "\n", eg_text), collapse = "\n"), # for "" cases
        "\n"
      )
    ),
    eg_width
  )

  # number of characters
  nchar_lab_extracted <- nchar(strsplit(out$label, "\n")[[1]])
  nchar_lab_test <- nchar(strsplit(text_fin, "\n")[[1]])
  exp_nchar_lab <- c(144, 114, 9, 4, 0, 0, 8)

  # Force informative error
  if (!checkmate::check_set_equal(nchar_lab_extracted, exp_nchar_lab)) {
    stop(
      "width:", eg_width,
      "\nnchar_out_label  : ", paste(nchar_lab_extracted, collapse = " "),
      "\nnchar_label_free : ", paste(nchar_lab_test, collapse = " ")
    )
  }

  # Default passing tests
  testthat::expect_equal(nchar_lab_extracted, nchar_lab_test)
  testthat::expect_equal(nchar_lab_extracted, exp_nchar_lab)
})
