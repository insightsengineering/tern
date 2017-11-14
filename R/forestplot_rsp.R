#' Response Forest Plot Table
#'
#' @param response Tumor Response data
#' @param event is boolean, \code{TRUE} if responder, \code{FALSE} if non-responder
#' @param group_data data frame with one column per sub-group variable
#' @param arm vector with arm information
#' @param covariates set to NULL; currently not available for multivariate survival analysis
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
#' \dontrun{
#' library(atezo.data)
#' library(dplyr) 
#' library(grid)
#' library(teal.oncology)
#' library(forcats)
#' '%needs%' <- teal.oncology:::'%needs%'
#' ARS <- ars(com.roche.cdt30019.go29436.re)
#' ASL <- asl(com.roche.cdt30019.go29436.re)
#' 
#' ARS_f <- ARS %>% filter(PARAMCD == "OVRSPI") %>% 
#'                  filter(ITTWTFL == "Y") %>% 
#'                  filter(ARM %in% c("DUMMY A", "DUMMY C")) %>%
#'                  select(c("USUBJID", "STUDYID", "SEX", "ICLEVEL", "TC3IC3", "ARM", "AVAL", "AVALC"))
#' ASL_f <- ASL %>% filter(ITTWTFL == "Y") %>% filter(ARM %in% c("DUMMY A", "DUMMY C"))
#' 
#' group_data <- ARS_f[c("USUBJID", "STUDYID", "ICLEVEL", "TC3IC3")]
#' names(group_data) <- labels_over_names(group_data)
#' 
#' head(group_data)
#' 
#' arm <- fct_relevel(ARS_f$ARM, "DUMMY C")
#' 
#' tbl <- forest_rsp(
#'           response = ARS_f$AVAL,
#'           event = ARS_f$AVALC %in% c("CR","PR"),
#'           arm = arm, 
#'           group_data = group_data[, -c(1,2), drop=FALSE]
#' )
#' Viewer(tbl)
#' 
#' }
#' 
#' 
#'   
forest_rsp <- function(response, event,
                         arm, group_data, covariates = NULL) {
  
  # argument checking
  n <- length(response)
  if (length(event) != n) stop("event has wrong length")
  if (length(arm) != n) stop("arm has wrong length")
  if (!is.data.frame(group_data)) stop("group_data is expected to be a data.frame")
  if (nrow(group_data) != n) stop("group_data has wrong number of rows")
  
  
  glm_data <- data.frame(response, event,arm)

  # var = names(group_data)[1]
  # split data into a tree for data
  # where each leaf is a data.frame with 
  # the data to compute the survival analysis with
  data_list <- c(
    list(ALL = list(ALL = glm_data)),
    lapply(group_data, function(var) {
      sub_data <- lapply(setNames(unique(var[var != ""]), unique(var[var != ""])), function(value) {
         glm_data[var == value , , drop= FALSE] 
      })
      sub_data[order(names(sub_data),decreasing = F)]
    })
  )
  
  #varname=data_list$TC3IC3
  #data_for_value = varname[1]
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
            rcell(x$glm_or, format = "xx.xx"),
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

# Forest plot using grid
#' Forest plot (table + graph)
#' 
#' @param x rtable from forest_rsp function
#' @param arm.ref string used to label reference arm
#' @param arm.comp string used to label comparable arm
#' @param padx gap between two columns
#' @param cex multiplier applied to overall fontsize
#' 
#' @author Yuyao Song (songy24), \email{yuyao.song@roche.com}
#' 
#' @import grid
#' 
#' @export
#' 
#' @examples 
#' 
#' \dontrun{
#' library(atezo.data)
#' library(dplyr) 
#' library(grid)
#' library(teal.oncology)
#' library(forcats)
#' '%needs%' <- teal.oncology:::'%needs%'
#' ARS <- ars(com.roche.cdt30019.go29436.re)
#' ASL <- asl(com.roche.cdt30019.go29436.re)
#' 
#' ARS_f <- ARS %>% filter(PARAMCD == "OVRSPI") %>% 
#'                  filter(ITTWTFL == "Y") %>% 
#'                  filter(ARM %in% c("DUMMY A", "DUMMY C")) %>%
#'                  select(c("USUBJID", "STUDYID", "SEX", "ICLEVEL", "TC3IC3", "ARM", "AVAL", "AVALC"))
#' ASL_f <- ASL %>% filter(ITTWTFL == "Y") %>% filter(ARM %in% c("DUMMY A", "DUMMY C"))
#' 
#' group_data <- ARS_f[c("USUBJID", "STUDYID", "ICLEVEL", "TC3IC3")]
#' names(group_data) <- labels_over_names(group_data)
#' 
#' head(group_data)
#' 
#' arm <- fct_relevel(ARS_f$ARM, "DUMMY C")
#' 
#' tbl <- forest_rsp(
#'           response = ARS_f$AVAL,
#'           event = ARS_f$AVALC %in% c("CR","PR"),
#'           arm = arm, 
#'           group_data = group_data[, -c(1,2), drop=FALSE]
#' )
#' 
#' forest_rsp_plot(tbl, levels(arm)[1], levels(arm)[2])
#' 
#' }
forest_rsp_plot <- function(x, arm.ref = "Reference", arm.comp = "Treatment", padx = unit(1.5, "lines"), cex = 1) {


  
  vp <- vpTree(
    parent = viewport(
      name = "forestplot",
      layout = grid.layout(
        nrow = 1, ncol = 11,
        widths = unit.c(
          stringWidth("Baseline Risk Factors  ") + 1 * padx,
          stringWidth("xxxxx") + 2 * padx,
          stringWidth("xxxxx") + 2 * padx,
          stringWidth("xxxxx") + 2 * padx,
          stringWidth("xx.xx") + 2 * padx,
          stringWidth("xxxxx") + 2 * padx,
          stringWidth("xxxxx") + 2 * padx,
          stringWidth("xx.xx") + 2 * padx,
          stringWidth("xx.xx") + 2 * padx,
          stringWidth("xx.xx - xx.xx") + 2 * padx,
          unit(1, "null")
        )
      ),
      gp = gpar(cex = cex)
    ),
    children = vpList(
      viewport(name = "col_1", layout.pos.col=1, layout.pos.row=1),
      viewport(name = "col_2", layout.pos.col=2, layout.pos.row=1),
      viewport(name = "col_3", layout.pos.col=3, layout.pos.row=1),
      viewport(name = "col_4", layout.pos.col=4, layout.pos.row=1),
      viewport(name = "col_5", layout.pos.col=5, layout.pos.row=1),
      viewport(name = "col_6", layout.pos.col=6, layout.pos.row=1),
      viewport(name = "col_7", layout.pos.col=7, layout.pos.row=1),
      viewport(name = "col_8", layout.pos.col=8, layout.pos.row=1),
      viewport(name = "col_9", layout.pos.col=9, layout.pos.row=1),
      viewport(name = "col_10", layout.pos.col=10, layout.pos.row=1),
      dataViewport(name = "col_11", layout.pos.col=11, layout.pos.row=1,
                   xData = c(-2,2), yData = c(0,1))
    )
  )
  
  grid.newpage()
  
  pushViewport(plotViewport(margins = c(3,2,1,2)))
  
  pushViewport(vp)
  
  # grid.ls(viewports = TRUE)
  seekViewport("forestplot")
  
  # need once: mid-line OR = 1
  grid.xaxis(at = c(log(0.1), log(0.5), log(1), log(2), log(5), log(10)), label = c(0.1, 0.5, 1, 2, 5, 10), vp = vpPath("col_11"))
  grid.lines(x = unit(c(0,0), "native"), y = unit(c(0,1-2/nrow(x)), "npc"), vp = vpPath("col_11"),
             gp = gpar(lty = 2))  
  
  # Add Header
  draw_header(2, nrow(x), "Baseline Risk Factors","Total n", "n", "n\nResponder", "Responder\nRate (%)", "n", "n\nResponder", "Responder\nRate (%)", "Odds\nRatio", "95%\nCI", arm.ref,arm.comp)
  
  # Add table contents
  for (i in 1:nrow(x)){
    if (!is.null(x[i,1])) {
      draw_row(i+4, nrow(x), row.names(x)[i], x[i, 1], x[i, 2], x[i, 3], round(x[i, 4], 1), x[i, 5], x[i, 6], 
               round(x[i, 7], 1), round(x[i, 8], 2), paste("(", paste(round(x[i, 9],2), collapse = ", "), ")", sep = ""), c(log(abs(x[i,8])), log(abs(x[i,9]))), TRUE)
    } else if (is.null(x[i,1]) & row.names(x)[i] != "") {
      draw_row(i+4, nrow(x), row.names(x)[i], "", "", "", "", "", "", "", "", "", c(NA, NA, NA), FALSE, 2)
    } else {
      draw_row(i+4, nrow(x), "", "", "", "", "", "", "", "", "", "", c(NA, NA, NA), FALSE,2)
    }
  }
}


#' glm_results(data_for_value)
#' data = data_for_value 
glm_results <- function(data){
  
  #Response Rate
  
  tbl_arm <- table(data$arm)
  resp_ref_n <- tbl_arm[names(tbl_arm)==levels(data$arm)[1]]
  resp_comp_n <- tbl_arm[names(tbl_arm)==levels(data$arm)[2]]
  if (is.na(resp_ref_n)) resp_ref_n = 0
  if (is.na(resp_comp_n)) resp_comp_n = 0
  
  tbl_freq <- table(data$event,data$arm)
  resp_ref_event <- tbl_freq[rownames(tbl_freq)=="TRUE",colnames(tbl_freq)==levels(data$arm)[1]]
  resp_comp_event <- tbl_freq[rownames(tbl_freq)=="TRUE",colnames(tbl_freq)==levels(data$arm)[2]]
  if (length(resp_ref_event)==0) resp_ref_event = 0
  if (length(resp_comp_event)==0) resp_comp_event = 0
  
  #Logistic Model
  if (length(levels(as.factor(as.character(data$arm)))) == 2){
  glm_model <- glm(event ~ arm, family=binomial(link='logit'), data = data)
  glm_sum  <- summary(glm_model )
  glm_or   <- exp(glm_sum$coefficient[2,1])
  glm_lcl  <- exp(confint(glm_model)[2,1])
  glm_ucl  <- exp(confint(glm_model)[2,2])
  glm_pval <- glm_sum$coefficients[2,4]
  
  resp_table <- data.frame(resp_ref_n, resp_comp_n, 
                           resp_ref_event, resp_comp_event, 
                           glm_or, glm_lcl, glm_ucl, glm_pval)
  }else {
  resp_table <- data.frame(resp_ref_n, resp_comp_n, 
                             resp_ref_event, resp_comp_event, 
                             glm_or = NA, glm_lcl = NA, glm_ucl = NA, glm_pval = NA)
  }
}

