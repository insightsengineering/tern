# All functions deprecated

testthat::test_that("stack_grobs works with default settings", {
  g1 <- grid::circleGrob(gp = grid::gpar(col = "blue"))
  g2 <- grid::circleGrob(gp = grid::gpar(col = "red"))
  g3 <- grid::textGrob("TEST TEXT")
  grid::grid.newpage()
  testthat::expect_silent(grid::grid.draw(stack_grobs(g1, g2, g3)))
})

testthat::test_that("stack_grobs works with a single grob", {
  g1 <- grid::circleGrob(gp = grid::gpar(col = "blue"))
  grid::grid.newpage()
  testthat::expect_silent(grid::grid.draw(stack_grobs(g1)))
})

testthat::test_that("arrange_grobs works with default settings", {
  num <- lapply(1:9, grid::textGrob)
  grid::grid.newpage()
  testthat::expect_silent(grid::grid.draw(arrange_grobs(grobs = num)))
})

testthat::test_that("arrange_grobs works with multiple dimensions", {
  num <- lapply(1:9, grid::textGrob)
  grid::grid.newpage()
  testthat::expect_silent(grid::grid.draw(arrange_grobs(grobs = num, ncol = 3, nrow = 3)))
})

testthat::test_that("arrange_grobs works with a single column", {
  num <- lapply(1:9, grid::textGrob)
  grid::grid.newpage()
  testthat::expect_silent(grid::grid.draw(arrange_grobs(grobs = num, ncol = 1)))
})

testthat::test_that("arrange_grobs returns error when invalid dimensions are given", {
  num <- lapply(1:9, grid::textGrob)
  grid::grid.newpage()
  testthat::expect_error(
    grid::grid.draw(arrange_grobs(grobs = num, ncol = 3, nrow = 2)),
    "specififed ncol and nrow are not enough for arranging the grobs"
  )
})

testthat::test_that("arrange_grobs works with a single grob", {
  num <- list(grid::textGrob(1))
  grid::grid.newpage()
  testthat::expect_silent(grid::grid.draw(arrange_grobs(grobs = num)))
})

testthat::test_that("draw_grob works with viewport specified", {
  rect <- grid::rectGrob(width = grid::unit(0.5, "npc"), height = grid::unit(0.5, "npc"))
  testthat::expect_silent(rect %>% draw_grob(vp = grid::viewport(angle = 45)))
})
