#' Response Forest Plot Table
#'
#' @template param_rsp
#' @template param_col_by
#' @param group_data data frame with one column per grouping variable
#' @param total character with the row name of the analysis run on all data. If
#'   \code{NULL} analysis is omitted
#' @param na.omit.group boolean, do not list NAs
#'    
#' @details 
#' Logistic model is used for odds ratio calculation.
#' 
#' The returned table contains one row per analysis applied on a subset of data
#' (indicated by the row name). The analysis is summarized with the following 9
#' columns:
#' 
#' \describe{
#'   \item{1}{\emph{Total n} the total number of subjects used for analysis}
#'   \item{3-4}{analysis for reference arm, \emph{n}, \emph{Responders},
#'   \code{Response.Rate}}
#'   \item{5-7}{same analysis as for reference arm now for comparison arm}
#'   \item{8}{\emph{Odds Ratio}}
#'   \item{9}{\emph{95 \% CI}}
#' }
#' 
#' @template return_rtable 
#' 
#' @export
#' 
#' @template author_song24
#' 
#' @examples 
#' library(random.cdisc.data)
#' 
#' ASL <- radam("ASL")
#' ARS <- radam("ARS", ADSL = ASL)
#' 
#' ARS_f <- subset(ARS, PARAMCD == "OVRSPI")
#' ANL <- merge(ASL, ARS_f)
#' 
#' tbl <- t_forest_rsp(
#'   rsp = ANL$AVALC %in% c("CR", "PR"),
#'   col_by = ANL$ARM, 
#'   group_data = ANL[, c("SEX", "RACE")]
#' )
#' 
#' tbl
#'    
t_forest_rsp <- function(rsp, col_by, group_data = NULL,
                         total = 'ALL', na.omit.group = TRUE) {
  
  if (!is.logical(rsp)) stop("rsp is required to be boolean")
  check_same_N(rsp = rsp, col_by = col_by, group_data = group_data)
  
  check_col_by(col_by)
  if (length(levels(col_by)) != 2) stop("col_by can only have two levels")
  
  if (!is.null(group_data)) {
    check_data_frame(group_data, allow_missing = TRUE)
    group_data <- all_as_factor(group_data)
  }
  
  table_header <- rheader(
    rrow(row.name = "",
         rcell(NULL),
         rcell(levels(col_by)[1], colspan = 3),
         rcell(levels(col_by)[2], colspan = 3),
         rcell(NULL),
         rcell(NULL)
    ),
    rrow(row.name = "Baseline Risk Factors",
         "Total n",
         "n", "Responders", "Response.Rate",
         "n", "Responders", "Response.Rate",
         "Odds Ratio",
         "95% CI"
    )
  )
  
  glm_data <- data.frame(response = rsp, arm = col_by)
  
  tbl_total <- if(is.null(total)) {
    NULL
  } else {
    rtable(header = table_header, 
           rrowl(row.name = total, 
                 format_logistic(
                     glm_results(glm_data)
                 )
    )) 
  }
  
  
  tbl_group_data <- if (is.null(group_data)) {
    NULL
  } else {
    
    # split data into a tree for data
    # where each leaf is a data.frame with 
    # the data to compute the survival analysis with
    data_tree <- lapply(group_data, function(var) {
      if (!na.omit.group) var <- na_as_level(var)
      split(glm_data, var, drop = FALSE)
    })
    
    names(data_tree) <- var_labels(group_data, fill = TRUE)
    
    list_of_tables <- Map(function(dfs, varlabel) {
      
      tbls_var <- Map(function(dfi, level) {
        rtable(header = table_header,
               rrowl(
                 row.name = level,
                 indent = 1,
                 format_logistic(
                   glm_results(dfi)
                 )
               )) 
      }, dfs, names(dfs))
      
      rbind(
        rtable(header = table_header, rrow(row.name = varlabel)),
        Reduce(rbind, tbls_var)
      )
    }, data_tree, names(data_tree))
    
    stack_rtables_l(list_of_tables)
  }
  
  stack_rtables(
    tbl_total,
    tbl_group_data
  )
}  

# glm_results(glm_data)
# data = glm_data 
glm_results <- function(data){
  
  #Response Rate
  resp_n <- setNames(table(data$arm), c("resp_ref_n", "resp_comp_n"))

  tbl_freq <- table(data$response,data$arm)
  resp_ref_event <- tbl_freq[rownames(tbl_freq)=="TRUE",colnames(tbl_freq)==levels(data$arm)[1]]
  resp_comp_event <- tbl_freq[rownames(tbl_freq)=="TRUE",colnames(tbl_freq)==levels(data$arm)[2]]
  if (length(resp_ref_event)==0) resp_ref_event = 0
  if (length(resp_comp_event)==0) resp_comp_event = 0
  
  #Logistic Model
  if (length(levels(factor(data$arm))) == 2) {
     glm_fit <- try(
       glm(response ~ arm, family=binomial(link='logit'), data = data)
     )
     
     if (is(glm_fit, "try-error")) {
       glm_or <- NA; glm_lcl <- NA; glm_ucl <- NA; glm_pval <- NA
     } else {
       glm_sum  <- summary(glm_fit)
       #glm_or   <- ifelse(exp(glm_sum$coefficient[2,1]) > 999, ">999.99", exp(glm_sum$coefficient[2,1]))
       glm_or   <- exp(glm_sum$coefficient[2,1])
       
       suppressWarnings(
         suppressMessages({
           glm_lcl  <- tryCatch(
             exp(confint(glm_fit)[2,1]),
             error = function(e) NA
           )
           
           glm_ucl  <- tryCatch(
             exp(confint(glm_fit)[2,2]),
             error = function(e) NA
           )
         })
       )
       
       glm_pval <- glm_sum$coefficients[2,4]
     }
     
     resp_table <- data.frame(resp_ref_n = resp_n[1], resp_comp_n = resp_n[2], 
                              resp_ref_event, resp_comp_event, 
                              glm_or, glm_lcl, glm_ucl, glm_pval)
  } else {
  resp_table <- data.frame(resp_ref_n = resp_n[1], resp_comp_n = resp_n[2],  
                             resp_ref_event, resp_comp_event, 
                             glm_or = NA, glm_lcl = NA, glm_ucl = NA, glm_pval = NA)
  }
  resp_table
}

format_logistic <- function(x) {
  list(
    rcell(x[["resp_comp_n"]] + x[["resp_ref_n"]], "xx"),
    rcell(x[["resp_ref_n"]], "xx"),
    rcell(x[["resp_ref_event"]], "xx"),
    rcell(x[["resp_ref_event"]] / x[["resp_ref_n"]], "xx.xx"),
    rcell(x[["resp_comp_n"]], "xx"),
    rcell(x[["resp_comp_event"]], "xx"),
    rcell(x[["resp_comp_event"]] / x[["resp_comp_n"]], "xx.xx"),
    rcell(x[["glm_or"]], format = "xx.xx"),
    rcell(c(x[['glm_lcl']], x[["glm_ucl"]]), format = "(xx.xx, xx.xx)")
  )
}
