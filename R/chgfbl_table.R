#' Change from Baseline Table Caclulation Function 
#' 
#' @param response Tumor response data
#' @param arm Arm information data as factor
#' @param 
#'                     
#' @details 
#'       
                                                                                
#' @export
#' 
#' @author Chendi Liao (liaoc10), \email{chendi.liao@roche.com}
#' 
#' @examples 
#' 
#' 
#' \dontrun{
#' library(dplyr)
#' library(tidyr)
#' library(atezo.data)
#' library(teal.oncology)
#' 
#' ASL <- asl(com.roche.cdpt7722.wo29637.rl)
#' AQS <- aqs(com.roche.cdpt7722.wo29637.rl)
#' 
#' parammap <- unique(AQS[c("PARAM", "PARAMCD")]) 
#' 
#' ASL_f <- ASL %>% select(STUDYID, USUBJID, ARM, ARMCD)
#' AQS_f <- AQS %>% select(STUDYID, USUBJID, PARAMCD, PARAM, AVAL, AVISIT, AVISITN, ADY, ABLFL, APBFL, CHG, BASE)
#'  
#' ANL <- inner_join(ASL_f, AQS_f, by = c("STUDYID", "USUBJID")) %>% filter(PARAMCD == "MDASI23")
#' 
#' # now 
#' chng_data <- ... # you already have this # do not export
#' change_table(chng_data) # split by arm_name make columns and then combine
#' change_plot(chng_data) # group_by(arm_name) make plotA
#' 
#' # maybe in future
#' chng_table <- table...
#' change_plot(chng_table)
#' 
chgfbl_table <- function(data = ANL,
                         arm_var = "ARM",
                         group_by = c("MDASI23", "FATIGI")
                         ) {

  #####################
  # Argument checking #
  #####################
  if (any(is.na(data[[arm_var]]))) stop("currently cannot deal with missing values in arm")
  if (!all(group_by %in% unique(data$PARAMCD))) stop("Selected parameters not found in analysis data")
  
  testdupvars <- c("STUDYID", "USUBJID",arm_var,"PARAMCD", "PARAM", "AVISIT", "AVISITN")
  
  #Remove any duplicated records and filter on param values
  data_f <- data[!duplicated(data[testdupvars]), c(testdupvars, "AVAL", "CHG")] %>% 
    filter(PARAMCD %in% group_by)
  attr(data_f$CHG, "label") <- attr(data_f$AVAL, "label")
  
  #Convert AVAL and CHG from wide to long format
  df <- gather(data_f, type, value, c("AVAL", "CHG")) %>%
    mutate(arm_name = factor(paste(data_f[[arm_var]], type, sep="_")))
  
  df.sum <- df %>% filter(!is.na(value)) %>%
    group_by(arm_name, PARAMCD, PARAM, AVISIT, AVISITN) %>%
    summarise(n = n(),
              mean = mean(value, na.rm=T),
              sd = sd(value, na.rm=T),
              median = median (value, na.rm=T),
              min = min(value, na.rm=T),
              max = max(value, na.rm=T))
  
  df.final <- data.frame(df.sum) %>% 
    arrange(AVISITN) %>% 
    mutate(visit = factor(AVISIT, unique(AVISIT)),
           paramcd = factor(PARAMCD, group_by)) %>%
    arrange(AVISITN, paramcd) %>%
    mutate(param = factor(PARAM, unique(PARAM))) %>%
    select(arm_name, n:visit, param)

  
  df.s <- split(df.final, df.final$arm_name)

  ## every line in dfi is a point on the plot 
  dfi <- df.s[[1]]
  
  # and a column in rtables lets make this later
  # coli <- apply(dfi, 1, FUN = function(row) {
  #   list(
  #     rrow("name", indent = 1, ...),
  #     ...
  #   )
  # })
  # cbind(col1, col2, col3)
  
  
  
  #Formatting into rtable object
  split(df.final, df.final$param)
  outsum <- lapply(split(df.final, df.final$param), function(x) {
      lapply(lapply(split(x, x$visit), function(xi) {
        lapply(split(xi, xi$arm_name), function(xii) {
          list(
            "n"         = rcell(xii$n, format = "xx"),
            "Mean (SD)" = rcell(c(xii$mean, xii$sd), format = "xx.x (xx.x)"),
            "Median"    = rcell(xii$median, format = "xx.x"),
            "Min - Max" = rcell(c(xii$min, xii$max), format = "xx.xx - xx.xx")
          )
        })
      }), list_transpose)
    })
      
  
  
  
  
 
  
  n <- vapply(var_row_info[[1]][[1]], as.vector, numeric(1))
  
  ## create the rtable object
  first.row <- TRUE
  
  do.call(rtable, c(
    list(
      col.names = paste0(levels(arm), "\n", paste0("(N=",n, ")")),
      format = "xx"
    ),
    Filter(function(x)!is.null(x), unlist(Map(function(rows_for_var, var, label) {
      c(
        list(
          if (first.row) {first.row <<- FALSE; NULL} else  rrow(),
          rrow(label)
        ),
        Map(function(ri, category) {
          do.call(rrow, c(list(row.name = category, indent = 1), ri))
        }, rows_for_var, names(rows_for_var))
      )
    }, var_row_info, names(var_row_info), var_lables), recursive = FALSE))
  ))
}


# ifelse(type == "AVAL", paste0(data_f[[arm_var]], "\nValue at Visit"), paste0(data_f[[arm_var]], "\nChange from Baseline"))

