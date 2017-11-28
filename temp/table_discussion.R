


library(atezo.data)


AQS <- aqs(com.roche.cdpt7722.wo29637.rl)


iris$Sepal.Length %>% fivenum()


library(tidyr)

iris %>% head()

gather(iris, key = "key", value = "value", Sepal.Length, Species)


## data
##   - specify what to tabulate
##   - split
##     - what are the rows
##         - group
##     - what are the columns
##         - by
## 

## function can return a table
##  data = data.frame(
##     time = c(1:10),
##     arm = rep(c("b", "b"), each = 5),
##     v1 = sample(LETTES[1:3], 10, TRUE),
##     v2 = sample(letters[1:3], 10, TRUE)
##  )
##  
##   - what are the columns
##        - factor levels of variable y (arm here)
##   - what are the rows
##        - factor levels of variable x (time here)
itable(data, x = time , y = arm, FUN = function(dfij) {
  with(table(v1, v2))
})


dm_table <- function(data, anl_var, by_row_var=NULL, by_col_var, FUN = ...) {
  
  gather(data, vars)
  itable(data, x = ... , y = by, FUN )
  
}

visit_table <- function(data, value_var, visit_var, arm_var, FUN) {
  
  ...
  itable()
}


visit_table_bl <- function(data, value_var, chngbl_var, visit_var, arm_var, FUN) {
  
  ...
  itable()
  
}

tbl[3,3] = 0.17
row_attr(tbl, 1, indent = 1)$time = c1d1


# make a line per column and rows are x points and y arm multiple elements from row
plot_time_series <- function(visit_table, cols, x = attr_row$time , y = mean, lb = mean - 2*sd, ub = mean+2*sd) {
  
}
  


  


chng_bl(data, val, time, by, is_bl)



