

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


pushViewport(plotViewport(c(5, 4, 2, 2)))
pushViewport(dataViewport(pressure$temperature,
                          pressure$pressure,
                          name="plotRegion"))

grid.points(pressure$temperature, pressure$pressure,
            name="dataSymbols")
grid.rect()
grid.xaxis()
grid.yaxis()
grid.text("temperature", y=unit(-3, "line"))
grid.text("pressure", x=unit(-3, "line"), rot=90)
grid.edit("dataSymbols", pch=2)
upViewport(2)
grid.rect(gp=gpar(lty="dashed"))
downViewport("plotRegion")
grid.text("Pressure (mm Hg)\nversus\nTemperature (Celsius)",
          x=unit(150, "native"), y=unit(600, "native"))

.GlobalEnv$var <- var



grid.circle(x=seq(0.1, 0.9, length=100),   y=0.5 + 0.4*sin(seq(0, 2*pi, length=100)),   
            r=abs(0.1*cos(seq(0, 2*pi, length=100))))

grid.circle(.25, .5, r=unit(1, "mm"),
           gp=gpar(fill="black"))

grid.text("A label", .75, .5)
grid.rect(.75, .5,
            width=stringWidth("A label") + unit(2, "mm"),
            height=unit(1, "line"),
            name="labelbox")

grid.segments(.25, .5,
              grobX("labelbox", 180), .5,
              arrow=arrow(angle=15, type="closed"),
              gp=gpar(fill="black"))
