#' Response Forest Plot Table
#'
#' @param response Tumor Response data
#' @param event is boolean, \code{TRUE} if responder, \code{FALSE} if
#'   non-responder
#' @param group_data data frame with one column per sub-group variable
#' @param arm vector with arm information
#' @param covariates set to NULL; currently not available for multivariate
#'   survival analysis
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
#' 
#' ARS_f <- ARS %>% filter(PARAMCD == "OVRSPI") 
#' 
#' ASL_f <- right_join(ASL %>% select(USUBJID, STUDYID, SEX, RACE, ARM),
#'                         ARS_f %>% select(USUBJID, STUDYID))
#' 
#' tbl <- forest_rsp(
#'   response = ARS_f$AVAL,
#'   event = ARS_f$AVALC %in% c("CR","PR"),
#'   arm = ASL_f$ARM, 
#'   group_data = ASL_f %>% select("SEX", "RACE")
#' )
#' 
#' tbl
#' 
#' \dontrun{
#' library(atezo.data)
#' library(dplyr) 
#' library(grid)
#' library(teal.oncology)
#' library(forcats)
#' devtools::install_github("Roche/rtables")
#' '%needs%' <- teal.oncology:::'%needs%'
#' ARS <- ars(com.roche.cdt30019.go29436.re)
#' ASL <- asl(com.roche.cdt30019.go29436.re)
#' ASL$temp <- c(rep("A", 200), rep("B", 250), rep(NA,1202-450))
#' 
#' ASL_f <- subset(ASL, 
#'                 subset = (ITTWTFL == "N" & ARM %in% c("DUMMY A", "DUMMY C")), 
#'                 select = c("USUBJID", "STUDYID", "ARM", "ARMCD", "SEX", "MLIVER", "TCICLVL2", "AGE4CAT", "RACE", "temp") )
#' ARS_f <- subset(ARS, 
#'                 subset = (PARAMCD == "OVRSPI"),
#'                 select = c("STUDYID", "USUBJID", "PARAMCD", "PARAM", "AVAL", "AVALC"))
#' ANL <- merge(ASL_f, ARS_f, by = c("STUDYID", "USUBJID"), all.x = TRUE, all.y = FALSE)
#' 
#' group_data <- subset(ASL_f, select = c("USUBJID", "STUDYID", "SEX", "MLIVER", "TCICLVL2", "AGE4CAT", "RACE"))
#' 
#' for(x in names(group_data)){
#'   attr(group_data[[x]], "label") <- attr(ASL[[x]], "label")
#' }   
#'
#' names(group_data) <- labels_over_names(group_data)
#' 
#' head(group_data)
#' 
#' arm <- fct_relevel(ASL_f$ARM, "DUMMY C")
#' 
#' tbl <- forest_rsp(
#'           response = ANL$AVAL,
#'           event = ANL$AVALC %in% c("CR","PR"),
#'           arm = arm, 
#'           group_data = group_data[, -c(1,2), drop=FALSE]
#' )
#' Viewer(tbl)
#' 
#' }
#' 
#' 
#'   
t_forest_rsp <- function(response, event,
                         arm, group_data, covariates = NULL) {
  
  # argument checking
  n <- length(response)
  if (length(event) != n) stop("event has wrong length")
  if (length(arm) != n) stop("arm has wrong length")
  if (length(levels(arm)) <2) stop("need at least two levels in arm for logistic regression") 
  if (!is.data.frame(group_data)) stop("group_data is expected to be a data.frame")
  if (nrow(group_data) != n) stop("group_data has wrong number of rows")
  
  
  glm_data <- data.frame(response, event,arm)

  #Split data to subgroup lists. e.g. data$SEX$FEMALE
  data_list <- c(
    list(ALL = list(ALL = glm_data)),
    lapply(group_data, function(var) {
      sub_data <- cbind(glm_data, var)
      sub_data <- subset(sub_data, var != "")
      if ("" %in% levels(sub_data$var)) sub_data$var <- factor(sub_data$var, levels = levels(sub_data$var)[-which(levels(sub_data$var) == "")])
      sub_data$var <- as.factor(sub_data$var)
      lapply(split(sub_data, sub_data$var), function(x){
        x[,-4]
      })
    })
  )
  
  #varname=data_list$AGE4CAT
  #data_for_value = varname[[4]]
  #apply the glm analysis
  results_glm <- lapply(data_list, function(varname) {
    lapply(varname, function(data_for_value) {
      glm_results(data_for_value)
    })
  })
  
  # reduce results into a table
  results_glm2 <- unlist(results_glm, recursive = FALSE)
  X <- Reduce(rbind, results_glm2)
  row.names(X) <-names(results_glm2)
  
  additonal_args <- list(
    col.names = c("Total n",
                  "n", "n\nResponder", "Responder Rate\n(%)",
                  "n", "n\nResponder", "Responder Rate\n(%)",
                  "Odds Ratio", "95% CI"),
    format = "xx"
  )
  
  # rname <- rownames(X)[1]
  # x <- split(X, 1:nrow(X))[[1]]
  # resolve the result data.frame to rtable 
  last_header <- "ALL"
  rrow_collection <- Filter(
    function(x)!is.null(x),
    unlist(
      Map(function(x, rname) {
        
        i <- regexpr(".", rname, fixed = TRUE)
        header_row_name <- c(substr(rname, 1, i-1), substring(rname, i+1))
        
        is_new_category <- header_row_name[1] != last_header
        last_header <<- header_row_name[1]
        
        list(
          if (is_new_category) rrow() else NULL,
          if (is_new_category) rrow(last_header) else NULL,
             rrow(
              row.name = header_row_name[2],
              x$resp_ref_n + x$resp_comp_n, # total n
              x$resp_ref_n,
              x$resp_ref_event,
              rcell(x$resp_ref_event / x$resp_ref_n * 100, format = "xx.x"),
              x$resp_comp_n,
              x$resp_comp_event,
              rcell(x$resp_comp_event / x$resp_comp_n * 100, format = "xx.x"),
              if (x$glm_or > 999.99 & !is.na(x$glm_or)) {
                rcell(">999.99", format = "xx")
                } else {
                rcell(x$glm_or, format = "xx.xx")
                },
              rcell(c(x$glm_lcl, x$glm_ucl), format = "(xx.xx, xx.xx)"),
              indent = if (header_row_name[1] == "ALL") 0 else 1
            )
        )
      }, split(X, 1:nrow(X)), rownames(X)),
      recursive = FALSE)
  )
  
  tbl <- do.call(rtable, c(additonal_args, rrow_collection))
  
  tbl
 # Viewer(tbl)
}


#' glm_results(data_for_value)
#' data = data_for_value 
glm_results <- function(data){
  
  #Response Rate
  resp_n <- setNames(table(data$arm), c("resp_ref_n", "resp_comp_n"))

  tbl_freq <- table(data$event,data$arm)
  resp_ref_event <- tbl_freq[rownames(tbl_freq)=="TRUE",colnames(tbl_freq)==levels(data$arm)[1]]
  resp_comp_event <- tbl_freq[rownames(tbl_freq)=="TRUE",colnames(tbl_freq)==levels(data$arm)[2]]
  if (length(resp_ref_event)==0) resp_ref_event = 0
  if (length(resp_comp_event)==0) resp_comp_event = 0
  
  #Logistic Model
  if (length(levels(factor(data$arm))) == 2){
     glm_model <- glm(event ~ arm, family=binomial(link='logit'), data = data)
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
}

