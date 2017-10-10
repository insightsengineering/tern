
library(atezo.data)
library(grid)

ASL <- asl(com.roche.cdt30019.go29436.re)
IC <- ASL$ICLEVEL %in% c("2", "3")
TC <- ASL$TCLEVEL %in% c(2, 3)
abs <- table(IC, TC)
per <- round(abs/length(IC)*100,digits = 1)

grid.newpage()

pushViewport(plotViewport(margins = c(2,2,2,2))) # add margins
grid.rect()

## draw the center
#grid.circle(x = unit(0.5, "npc"), y = unit(0.5, "npc"), r = unit(.2, "cm"),
#           gp = gpar(fill = "magenta"))

# helper lines
grid.lines(x = c(0, 1), y = c(.5, .5), default.units = "npc")
grid.lines(x =  c(.5, .5), y =c(0, 1), default.units = "npc")

# solve for radius of circles using area

ax <- sqrt((abs[1,2]+abs[2,2])/pi) #radius of 1st circle
ay <- sqrt((abs[2,1]+abs[2,2])/pi) #radius of 2nd circle

#solve for d, the distance between the 2 centers of the cicles

d_solve<- uniroot(function(d) ay^2*acos((d^2+ay^2-ax^2)/(2*d*ay)) 
        + ax^2*acos((d^2+ax^2-ay^2)/(2*d*ax)) 
        - 1/2 * sqrt((-d+ay+ax)*(d+ay-ax)*(d-ay+ax)*(d+ay+ax))-abs[2,2], 
        lower=abs(ax-ay)+1e-9, upper=ax+ay-1e-9,tol = 1e-9)$root

#solve for a (the cord connecting the cusps of the lens)

a <- 1/d_solve * sqrt((-d_solve+ay+ax)*(d_solve+ay-ax)*(d_solve-ay+ax)*(d_solve+ay+ax))

#find dx and dy using pythagorean theorm
#dx and dy are distances from center of cusp to the respective centers of the circles
#sacle d and r to viewport width, making 2x diameter 90% width of viewport

viewport_width <- convertWidth(unit(1,'npc'), 'cm', TRUE)

dx_num <- sqrt(ax^2-(a/2)^2)
dy_num <- sqrt(ay^2-(a/2)^2)

dx_num_scale <- dx_num/(2*(ax+ay))*0.9*viewport_width
dy_num_scale <- dy_num/(2*(ax+ay))*0.9*viewport_width

dx <- unit(dx_num_scale, "cm")
dy <- unit(dy_num_scale, "cm")

rx <- unit(ax/(2*(ax+ay))*0.9*viewport_width, "cm") 
ry <- unit(ay/(2*(ax+ay))*0.9*viewport_width, "cm") 

#draw circles

grid.circle(x = unit(0.5, "npc") - dx, y = unit(0.5, "npc"), r = rx,
            gp = gpar(fill = "thistle", alpha = .4))
grid.circle(x = unit(0.5, "npc") + dy, y = unit(0.5, "npc"), r = ry,
            gp = gpar(fill = "orange", alpha = .4))

#write text

draw.text <- function(just, text, text2, i, j, k=unit(0,"cm")) {
  grid.text(text, x=unit(i, "npc")+k, y=unit(j, "npc"), just=just)
  grid.text(text2, x=unit(i, "npc")+k, y=unit(j, "npc") - unit(1, "lines"),
            gp=gpar(fontsize=8))
}

draw.text(c("left","bottom"),"Biomarker abc","",0.05,0.1)
draw.text(c("right","bottom"),"Biomarker def","",0.95,0.1)
draw.text(c("center","center"),abs[1,1],paste("(",per[1,1],"%)"),0.5,0.95)
draw.text(c("center","center"),abs[2,2],paste("(",per[2,2],"%)"),0.5,0.5)
draw.text(c("center","center"),abs[1,2],paste("(",per[1,2],"%)"),0.5,0.5,unit(-ax/(2*(ax+ay))*0.9*viewport_width,"cm"))
draw.text(c("center","center"),abs[2,1],paste("(",per[2,1],"%)"),0.5,0.5,unit(ay/(2*(ax+ay))*0.9*viewport_width,"cm"))

