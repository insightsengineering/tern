
(cases)

# control functions
settings_coxph <- function(
  ties = "exact",
  ...
) {
  
  c(as.list(environment()), list(...))
  
}


tte_tbl <- function(formula, data, ...) {
  
  coxph_args <- settings_coxph(...)
  
  do.call(coxph, c(list(formula = formula, data = data), coxph_args))
  
}

tte_tbl(Surv(AVAL, 1-CNSR) ~ ARM +  RACE + strata(SEX), data = ANL)



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

ANL$ARM

fit <- survfit(Surv(AVAL, 1-CNSR) ~ ARM, data = ANL)

survival:::plot.survfit(fit, mark.time = TRUE)


df <- data.frame(
  time = fit$time,
  surv = fit$surv,
  n.risk = fit$n.risk,
  strata = rep(names(fit$strata), fit$strata)
)
df_s <- split(df, df$strata)


col_pal <- col_factor("Set1", domain = names(df_s))

grid.newpage()
pushViewport(plotViewport(margins = c(3, 10, 2, 2)))

# grid.rect()

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
    gp = gpar(col = col, lwd = 2)
  )
}, df_s, col_pal(names(df_s)))

popViewport(2)

pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 3))

pushViewport(dataViewport(xData = df$time, yData = c(0,1)))
grid.xaxis()
grid.rect()




xpos <- c(5, 10, 20)

Map(function(x, ypos, strata) {
  
  n.r <- vapply(xpos, function(xi) {
    i <- tail(which(x$time <= xi), 1)
    x$n.risk[i]
  }, numeric(1))
  
  grid.text(
    label = n.r,
    x = unit(xpos, "native"),
    y = unit(ypos, "npc")
  )
  
  grid.text(
    label = strata,
    x = unit(-9, "lines"),
    y = unit(ypos, "npc"),
    just = c("left", "center")
  )
  
}, df_s, 1 - 1:length(df_s)/(length(df_s) + 1), names(df_s))



