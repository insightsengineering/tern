


g_forest <- function(snapshot, ASL, ATE) {
  
  df <- ATE %>% filter(ARM %in% c("ARM B", "ARM C"), PARAMCD == "OS") %>%
    select(AVAL, CNSR, ARM)
  
  df$ARM <- df$ARM %>% as.factor() %>% fct_reoder("ARM C")
  
  
  forstplot(Surv(AVAL, 1-CNSR) ~ ARM + SEX + strata(LIVER, ABC), dat = df)
  
}  

forstplot <- function(tte, event, arm) {

  fit <- survfit(Surv(tte, event) ~ arm)
  
  
  
}

forstplot <- function(formula, data) {
  
  fit <- survfit(formula, data = data)
  
  
}

library(survival)

ATE <- data.frame(
  AVAL = abs(rnorm(100)),
  CNSR = sample(c(TRUE, FALSE), 100, TRUE, prob = c(.7,.3)),
  ARM = factor(sample(c("ARM A", "ARM B"), 100, TRUE), levels = c("ARM A", "ARM B"))
)

kmPlot <- function(formula, data) {
  fit <- survfit(formula, data)
  fit 
}


kmPlot(Surv(tte, event) ~ arm, data = df)

kmPlot(formula("Surv(AVAL, I(1-CNSR)) ~ ARM"), data = ATE)


##


forestplot <- function(...) {
  obj <- forest_table(...)
  plot(obj)
}

# this is useful
# also easy comparable to say stream output
forest_table <- function(...) {
  ## this is your current surv_subgroup function
}


plot.forest_table <- function(x, ...) {
  ## write the plot here
}





