
# (cases)
# 
# # control functions
# settings_coxph <- function(
#   ties = "exact",
#   ...
# ) {
#   
#   c(as.list(environment()), list(...))
#   
# }
# 
# 
# tte_tbl <- function(formula, data, ...) {
#   
#   coxph_args <- settings_coxph(...)
#   
#   do.call(coxph, c(list(formula = formula, data = data), coxph_args))
#   
# }
# 
# tte_tbl(Surv(AVAL, 1-CNSR) ~ ARM +  RACE + strata(SEX), data = ANL)
# 


### 

# kmplot <- function() {}
# forest_tte <- function() {}
# tte_tbl(Surv(AVAl, 1-CNSR) ~ ARM +  RACE + strata(SEX, AAA), data = ATE)
# ATE$ARM <- factor(ATE$ARM, levels = "ref")
# kmplot(Surv(AVAl, 1-CNSR) ~ ARM +  RACE + strata(SEX, AAA), data = ATE)
# forest_tte(Surv(AVAl, 1-CNSR) ~ ARM +  RACE + strata(SEX, AAA), data = ATE )
# coxph() 


library(forcats)
library(atezo.data)
library(dplyr)
library(survival)
library(grid)
library(scales)


ATE <- ate(com.roche.cdt30019.go29436.re)

ANL <- ATE %>%
  filter(PARAMCD == "OS") %>%
  select(AVAL, CNSR, ARM, SEX, RACE, MLIVER) %>%
  mutate(ARM = factor(ARM)) %>%
  mutate(ARM = fct_relevel(ARM, "DUMMY C"))


fit <- survfit(Surv(AVAL, 1-CNSR) ~ ARM, data = ANL)

survminer::ggsurvplot(fit, risk.table = TRUE, break.time.by = 2)


df <- data.frame(
  time = fit$time,
  surv = fit$surv,
  n.risk = fit$n.risk,
  n.censor = fit$n.censor,
  n.event = fit$n.event,
  std.err = fit$std.err,
  upper = fit$upper,
  lower = fit$lower,
  strata = rep(names(fit$strata), fit$strata)
)
df_s <- split(df, df$strata)


col_pal <- col_factor("Set1", domain = names(df_s))

grid.newpage()
pushViewport(plotViewport(margins = c(3, 10, 2, 2)))

pushViewport(viewport(layout = grid.layout(
  nrow = 3, ncol = 1,
  heights = unit(c(5, 4, 3), c("null", "lines", "null")),
  widths = unit(1, "npc"))))

pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 1))

pushViewport(dataViewport(xData = df$time, yData = c(0,1)))
grid.xaxis()
grid.yaxis()
grid.rect()


Map(function(x, col) {
  grid.lines(
    x = c(0, rep(x$time, each = 2)),
    y = c(rep(c(1, head(x$surv, -1)), each = 2), tail(x$surv, 1)),
    default.units = "native",
    gp = gpar(col = col)
  )
  grid.points( 
    x = x[x$n.censor !=0, "time"],
    y = x[x$n.censor !=0, "surv"],
    pch = 3, 
    size = unit(0.5, "char"),
    gp = gpar(col = col))
  
}, df_s, col_pal(names(df_s)))


grid.text(
  label = "Survival Probability",
  x = unit(-9, "lines"),
  rot = 90
)

popViewport(2)

pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 2))
grid.text(label = "No of Patients at Risk",
          x = unit(0.1, "npc"),
          y = unit(0.3, "npc"),
          just = "right")
popViewport(1)

pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 3))

pushViewport(dataViewport(xData = df$time, yData = c(0,1)))
grid.xaxis()
grid.rect()




xpos <- seq(0, 28, by = 2)

Map(function(x, ypos, strata, col) {
  
  n.r <- vapply(xpos, function(xi) {
     if (xi == 0){
       i <- head(which(x$time >= xi), 1)
       x$n.risk[i]
     } else{
       i <- tail(which(x$time <= xi), 1)
       x$n.risk[i] - x$n.censor[i] - x$n.event[i]
     }
  }, numeric(1))
  
  grid.text(
    label = n.r,
    x = unit(xpos, "native"),
    y = unit(ypos, "npc"),
    gp = gpar(col = col)
  )
  
  grid.text(
    label = strata,
    x = unit(-9, "lines"),
    y = unit(ypos, "npc"),
    just = c("left", "center"),
    gp = gpar(col = col)
  )
  
}, df_s, 1 - 1:length(df_s)/(length(df_s) + 1), names(df_s), col_pal(names(df_s)))



