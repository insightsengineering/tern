

#' Create Demographics Table from ADSL data
#' 
#' Similiar as in STREAM
#' 
#' @param ADSL ADSL dataset with the following variables: USUBJID, STUDYID, AGE,
#'   and SEX
#' 
#' @details
#' Give a detailed description of what this function does.
#' 
#' @export
#' 
#' @author Xiao Yu Mo (mox5), \email{xiao_yu.mo@roche.com}
#' 
#' @examples 
#' n <- 100
#' ASL <- data.frame(
#'    USUBJID = paste0("id-", 1:n),
#'    STUDYID = "study 1",
#'    AGE = 40 + rnorm(n, 0, 20),
#'    SEX = sample(c("M", "F", "UNDEFINED", NA), n, replace = TRUE),
#'    stringAsFactors = FALSE
#' )
#' 
#' demographic_table(ASL)
#' 
#' \dontrun{
#' library(atezo.data)
#' library(dplyr)
#' 
#' ASL <- asl(com.roche.cdt30019.go29436.re)
#' tbl_expected <- get_demographic_table(com.roche.cdt30019.go29436.re)
#' Viewer(tbl_expected)
#' 
#' 
#' ADSL <- ASL %>% filter(ITTFL == 'Y') 
#' 
#' tbl <- demographic_table(
#'    data = ADSL,
#'    arm_var = "ARM",
#'    all.patients = TRUE
#' )
#' 
#' compare_rtables(tbl, tbl_expected)
#'
#' # if all is good then
#' tbl <- tbl_expected
#' compare_rtables(tbl, tbl_expected)
#' 
#' }
#' 
#' `%needs%` <- teal.oncology:::`%needs%`
#' 
#' 
demographic_table <- function(data,
                              arm_var = "ARM",
                              all.patients = TRUE,
                              group_by_vars = c("SEX", "AGE", "RACE", "ETHNIC")) {
  
  data %needs% c(arm_var, group_by_vars)
  
  arm <- factor(data[[arm_var]])
  
  if (all.patients) {
    if ("All Patients" %in% names(arm)) stop("if argument all.patients = TRUE then there cannot be an arm 'All Patients'")
    n <- nrow(data)
    data <- rbind(data, data)
    arm <- factor(c(as.character(arm), rep("All Patients", n)), levels = c(levels(arm), "All Patients"))
  }
  
  
  ## get a list with each variables tht we want to summarize
  
  var_collection <- data[group_by_vars]
  
  ## x <- var_collection[[1]]
  row_info <- lapply(var_collection, function(x) {
     


    if (is.numeric(x)) {
      ## then return the n, mean median, and range
      df <- data.frame(x = x, arm = arm) %>% filter(!is.na(x))
      lapply(split(df, df$arm), function(dfi) {
        n <- nrow(dfi)
        c(list(n = n), lapply(
          split(dfi, dfi$x), function(dfii) {
            xii <- dfii$x
            lapply(c(length, mean, sd, median, range), function(fun)fun(xii))
          }))
      })
    } else {
      ## categorical count
      df <- data.frame(x = factor(x), arm = arm) %>% filter(!is.na(x))
      lapply(split(df, df$arm), function(dfi) {
        n <- nrow(dfi)
        c(list(n = n), lapply(split(dfi, dfi$x), function(dfii) list(n_cat = nrow(dfii), p_cat = nrow(dfii)/n)))
      })
    }
  })
  
  #first.row <- TRUE
  #rrow_collection <- unlist(lapply(row_info, function(ri) {
  #  l <- list(
  #    if (first.row) NULL else rrow(),
  #    rrow("Sex", ...),
  #    rrow("n", ..., indent = 1)
  #  )
  #  first.row <<- FALSE
  #  l
  #}), recursive = FALSE)
  
  
  #do.call(rtable, c(list(col.names = ...), rrow_collection))
  
  
  row_info
}
