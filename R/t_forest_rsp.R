#' Response Forest Plot Table
#'
#' @param rep Tumor Response data
#' @param group_data data frame with one column per grouping variable
#' @param col_by factor with reference and comparison group information, the
#'   first \code{level} indicates the reference group
#'   
#' @details 
#' Logistic model is used for odds ratio calculation
#' 
#' @export
#' 
#' @author Yuyao Song (songy24), \email{yuyao.song@roche.com}
#' 
#' @examples 
#' 
#' library(random.cdisc.data)
#' ASL <- radam("ASL")
#' ARS <- radam("ARS", ADSL = ASL)
#' ASL$PSUEDO <- ASL$SEX
#' 
#' ARS_f <- ARS %>% filter(PARAMCD == "OVRSPI") 
#' ANL <- merge(ASL %>% select(USUBJID, STUDYID, SEX, RACE, PSUEDO, ARM), ARS_f)
#' 
#' tbl <- t_forest_rsp(
#'   rsp = ANL$AVALC %in% c("CR", "PR"),
#'   col_by = factor(ANL$ARM), 
#'   group_data = as.data.frame(lapply(ANL[, c("SEX", "RACE")], as.factor))
#' )
#' 
#' tbl
#'    
t_forest_rsp <- function(rsp, col_by, group_data = NULL, total = 'ALL', na.omit.group = TRUE) {
  
  check_same_N(rsp = rsp, col_by = col_by, group_data = group_data)
  check_col_by(col_by)
  if (length(levels(col_by)) != 2) stop("col_by can only have two levels")
  
  ## note that there is no missing
  check_data_frame(group_data) # change name of check_strata_data
  if (!is.null(group_data)) {
    is_fct <- vapply(group_data, is.factor, logical(1)) 
    if (!all(is_fct)) stop("not all variables in group_data are factors: ", paste(names(is_fct)[!is_fct], collapse = ", "))
  }
  
  glm_data <- data.frame(response = rsp, arm = col_by)
  
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
      dt <- if (na.omit.group) subset(glm_data, !is.na(var)) else glm_data
      split(dt, var, drop = FALSE)
    })
    
    list_of_tables <- Map(function(dfs, varname) {
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
        rtable(header = table_header, rrow(row.name = varname)),
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

#' glm_results(glm_data)
#' data = glm_data 
glm_results <- function(data){
  
  #Response Rate
  resp_n <- setNames(table(data$arm), c("resp_ref_n", "resp_comp_n"))

  tbl_freq <- table(data$response,data$arm)
  resp_ref_event <- tbl_freq[rownames(tbl_freq)=="TRUE",colnames(tbl_freq)==levels(data$arm)[1]]
  resp_comp_event <- tbl_freq[rownames(tbl_freq)=="TRUE",colnames(tbl_freq)==levels(data$arm)[2]]
  if (length(resp_ref_event)==0) resp_ref_event = 0
  if (length(resp_comp_event)==0) resp_comp_event = 0
  
  #Logistic Model
  if (length(levels(factor(data$arm))) == 2){
     glm_model <- glm(response ~ arm, family=binomial(link='logit'), data = data)
     glm_sum  <- summary(glm_model )
     #glm_or   <- ifelse(exp(glm_sum$coefficient[2,1]) > 999, ">999.99", exp(glm_sum$coefficient[2,1]))
     glm_or   <- exp(glm_sum$coefficient[2,1])
     glm_lcl  <- suppressWarnings(exp(suppressMessages(confint(glm_model)[2,1])))
     glm_ucl  <- suppressWarnings(exp(suppressMessages(confint(glm_model)[2,2])))
     glm_pval <- glm_sum$coefficients[2,4]
  
  resp_table <- data.frame(resp_ref_n = resp_n[1], resp_comp_n = resp_n[2], 
                           resp_ref_event, resp_comp_event, 
                           glm_or, glm_lcl, glm_ucl, glm_pval)
  }else {
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
    rcell(x[["glm_or"]], format = "xx"),
    rcell(c(x[['glm_lcl']], x[["glm_ucl"]]), format = "(xx.xx, xx.xx)")
  )
}
