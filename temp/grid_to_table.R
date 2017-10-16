

library(grid)


grid.newpage()

grid.rect(gp = gpar(fill = "blue"))
grid.points(x = unit(.5, "npc"), y = unit(.3, "npc"),
            gp = gpar(cex = 3, col = "red"))

pushViewport(
  plotViewport(margins = c(2,6,3,0))
)

grid.rect(gp = gpar(fill = "white"))
grid.points(x = unit(.5, "npc"), y = unit(.3, "npc"),
            gp = gpar(cex = 3, col = "red"))

grid.text("HELLO WORLD", x = unit(1, "lines"), y = unit(1, "npc") - unit(1, "lines"),
          just = c("left", "top"))
