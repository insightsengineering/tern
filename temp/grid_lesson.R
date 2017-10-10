

library(grid)

grid.newpage()

pushViewport(plotViewport(margins = c(2,2,2,2))) # add margins
grid.rect()

## draw the center
grid.circle(x = unit(0.5, "npc"), y = unit(0.5, "npc"), r = unit(.2, "cm"),
            gp = gpar(fill = "magenta"))

# helper lines
grid.lines(x = c(0, 1), y = c(.5, .5), default.units = "npc")
grid.lines(x =  c(.5, .5), y =c(0, 1), default.units = "npc")

## absolute
d <- unit(3, "lines")
rx <- unit(10, "lines")
ry <- unit(8, "lines")

grid.circle(x = unit(0.5, "npc") - d, y = unit(0.5, "npc"), r = rx,
            gp = gpar(fill = "thistle", alpha = .3))
grid.circle(x = unit(0.5, "npc") + d, y = unit(0.5, "npc"), r = rx,
            gp = gpar(fill = "orange", alpha = .3))


grid.text("ABC")

