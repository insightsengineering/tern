testthat::test_that("stack_grobs works with default settings", {
  g1 <- circleGrob(gp = gpar(col = "blue"))
  g2 <- circleGrob(gp = gpar(col = "red"))
  g3 <- textGrob("TEST TEXT")
  grid.newpage()
  testthat::expect_silent(grid.draw(stack_grobs(g1, g2, g3)))
})

testthat::test_that("stack_grobs works with a single grob", {
  g1 <- circleGrob(gp = gpar(col = "blue"))
  grid.newpage()
  testthat::expect_silent(grid.draw(stack_grobs(g1)))
})

testthat::test_that("arrange_grobs works with default settings", {
  num <- lapply(1:9, textGrob)
  grid::grid.newpage()
  testthat::expect_silent(grid.draw(arrange_grobs(grobs = num)))
})

testthat::test_that("arrange_grobs works with multiple dimensions", {
  num <- lapply(1:9, textGrob)
  grid::grid.newpage()
  testthat::expect_silent(grid.draw(arrange_grobs(grobs = num, ncol = 3, nrow = 3)))
})

testthat::test_that("arrange_grobs works with a single column", {
  num <- lapply(1:9, textGrob)
  grid::grid.newpage()
  testthat::expect_silent(grid.draw(arrange_grobs(grobs = num, ncol = 1)))
})

testthat::test_that("arrange_grobs returns error when invalid dimensions are given", {
  num <- lapply(1:9, textGrob)
  grid::grid.newpage()
  testthat::expect_error(
    grid.draw(arrange_grobs(grobs = num, ncol = 3, nrow = 2)),
    "specififed ncol and nrow are not enough for arranging the grobs"
  )
})

testthat::test_that("arrange_grobs works with a single grob", {
  num <- list(textGrob(1))
  grid::grid.newpage()
  testthat::expect_silent(grid.draw(arrange_grobs(grobs = num)))
})

testthat::test_that("draw_grob works with viewport specified", {
  rect <- rectGrob(width = grid::unit(0.5, "npc"), height = grid::unit(0.5, "npc"))
  testthat::expect_silent(rect %>% draw_grob(vp = grid::viewport(angle = 45)))
})
