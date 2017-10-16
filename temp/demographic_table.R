
# separate data from view
X <- list(
  "SEX" = NULL,
  "  n" = list(416, 41),
  "  Male" = list(c(24, .85), c(36, .59)),
  "  Female" = list(c(16, .45), c(15, .41)),
  "_" = NULL,
  "AGE" = NULL
)

X[[2]][[1]]

as_html_table(X)


demographic_table <- function(data, group_by = "ARM") {
  data %needs% c("SEX", "AGE", "other var", group_by)
  
  ## get the values
  #sex
  sex_n_A <- ...
  sex_n_B <- ...
  
  sex_male_A <- ...
  
  
  
  #Age
  
  ## create the data structure
  
  list(
    "SEX" = NULL,
    "  n" = list(sex_n_A, sex_n_B),
    "  Male" = list(),
    "  Female" = list(),
    "" = NULL,
    "AGE" = NULL
  )
  
}



