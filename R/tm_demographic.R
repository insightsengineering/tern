

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
#' Viewer(tbl)
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
  if (any(is.na(arm))) stop("currently cannot deal with missing values in arm")
  
  if (all.patients) {
    if ("All Patients" %in% names(arm)) stop("if argument all.patients = TRUE then there cannot be an arm 'All Patients'")
    n <- nrow(data)
    data <- rbind(data, data)
    arm <- factor(c(as.character(arm), rep("All Patients", n)), levels = c(levels(arm), "All Patients"))
  }
  
  
  ## get a list with each variables tht we want to summarize
  
  var_collection <- data[group_by_vars]
  type <- lapply(var_collection, function(var) if(is.numeric(var)) "numeric" else "categorical")
  
  ## x <- var_collection[[1]]
  var_col_info <- Map(function(x, xtype) {
    if (xtype == "numeric") {
      ## then return the n, mean median, and range
      df <- data.frame(x = x, arm = arm) %>% filter(!is.na(x))
      
      lapply(split(df, df$arm), function(dfi) {
        xii <- dfi$x
        
        #lapply(c(length, mean, sd, median, range), function(fun)fun(xii))
        list(
          "n" = rcell(length(xii), format = "xx"),
          "Mean (SD)" = rcell(c(mean(xii), sd(xii)), format = "xx.x (xx.x)"),
          "Median" = rcell(median(xii), format = "xx.x"),
          "Min - Max" = rcell(range(xii), format = "xx.xx - xx.xx")
        )
      })
    } else if (xtype == "categorical") {
      ## categorical count
      df <- data.frame(x = factor(x), arm = arm) %>% filter(!is.na(x))
      
      lapply(split(df, df$arm), function(dfi) {
        xii <- dfi$x
        n <- length(xii)
        c(
          list(
            "n" = rcell(n, format = "xx")
          ),
          lapply(split(dfi, dfi$x), function(dfii) {
            nii <- nrow(dfii)
            rcell(c(nii, nii/n), format = "xx (xx.xx%)")
          })
        )
      })
    } else {
      stop("unknown type ", xtype)
    }
  }, var_collection, type)
  
  var_row_info <- lapply(var_col_info, list_transpose)
  
  n <- vapply(var_row_info[[1]][[1]], as.vector, numeric(1))
  
  ## create the rtable object
  first.row <- TRUE
  
  do.call(rtable, c(
    list(
      col.names = paste0(levels(arm), "\n", paste0("(N=",n, ")")),
      format = "xx"
    ),
    Filter(function(x)!is.null(x), unlist(Map(function(rows_for_var, var) {
      c(
        list(
          if (first.row) {first.row <<- FALSE; NULL} else  rrow(),
          rrow(var)
        ),
        Map(function(ri, category) {
          do.call(rrow, c(list(row.name = category, indent = 1), ri))
        }, rows_for_var, names(rows_for_var))
      )
    }, var_row_info, names(var_row_info)), recursive = FALSE))
  ))
}
