
#' alternative tablist
#'
#' @param time_to_event time to event data
#' @param event is boolean, \code{TRUE} if event, \code{FALSE} if time_to_event
#'   is censored
#' @param group_data data frame with one column per grouping
#' @param arm vector with arm information
#' @param arm.ref a character vector defining which arms in arm should be taken 
#'   as the reference
#' @param arm.comp a character vector defining which arms in arm should be taken 
#'   as the comparison
#' 
#' 
#' @importFrom survival survfit Surv coxph
#' @export
#' 
#' @examples 
#' 
#' \dontrun{
#' library(atezo.data)
#' library(dplyr)
#' library(forcats)
#' library(survival)
#' 
#' '%needs%' <- teal.oncology:::'%needs%'
#' 
#' surv_tbl_stream <- get_forest_survival_table(com.roche.cdt30019.go29436.re)
#' Viewer(surv_tbl_stream )
#' 
#' ATE <- ate(com.roche.cdt30019.go29436.re)
#' ASL <- asl(com.roche.cdt30019.go29436.re)
#' 
#' ATE_f <- ATE %>% filter(PARAMCD == "OS") %>% 
#'              filter(ITTWTFL == "Y") %>% 
#'              filter(ARM %in% c("DUMMY A", "DUMMY C")) %>%
#'              select(c("USUBJID", "STUDYID", "AVAL", "CNSR", "ARMCD"))
#' ASL_f <- ASL %>% filter(ITTWTFL == "Y") %>% 
#'              filter(ARM %in% c("DUMMY A", "DUMMY C")) %>% 
#'              select(c("USUBJID", "STUDYID", "SEX", "MLIVER", "TCICLVL2", "AGE4CAT", "BECOG"))
#' 
#'
#' group_data <- left_join(ASL_f, ATE_f %>% select(c("USUBJID", "STUDYID")))
#' names(group_data) <- labels_over_names(group_data)
#' head(group_data)
#' 
#' # Dummy C First (comparison in survfit and glm)
#' arm <- fct_relevel(ATE_f$ARMCD, "C")
#' 
#' # Collapse Factors
#' # fct_collapse(arm, "ARM A/B" = c("ARM A", "ARM B"))
#' # rename a level
#' # arm <- fct_recode(ATE_f$ARM, "Treat ARM" = "DUMMY A")
#' 
#' tbl <- forest_tte(
#'    time_to_event = ATE_f$AVAL,
#'    event = ATE_f$CNSR == 0,
#'    group_data = group_data[, -c(1,2), drop=FALSE],
#'    arm = arm
#' )
#' Viewer(tbl)
#' 
#' compare_rtables(tbl, surv_tbl_stream, comp.attr = FALSE)
#' 
#' forest_tte_plot(tbl, levels(arm)[1], levels(arm)[2])
#' 
#' 
#' 
#' }
#' 
#' # forest_tte(Surv(AVAL ~ I(CNSR != 'N') ~ ARM + SEX, data = ATE))
forest_tte <- function(time_to_event, event, 
                       arm, group_data, covariates = NULL) {
  
  # argument checking
  n <- length(time_to_event)
  if (length(event) != n) stop("event has wrong length")
  if (length(arm) != n) stop("arm has wrong length")
  if (!is.data.frame(group_data)) stop("group_data is expected to be a data.frame")
  if (nrow(group_data) != n) stop("group_data has wrong number of rows")
  if (any(grepl(".", group_data, fixed = TRUE))) stop("no . are allowed in the group_data variable names")
  
  cox_data <- data.frame(
    time_to_event,
    event,
    arm = arm)
  
  # split data into a tree for data
  # where each leaf is a data.frame with 
  # the data to compute the survival analysis with
  data_list <- c(
    list(ALL = list(ALL = cox_data)),
    lapply(group_data, function(var) {
      sub_data <- cbind(cox_data, var)
      sub_data <- subset(sub_data, var != "")
      sub_data$var <- as.factor(as.character(sub_data$var))
      lapply(split(sub_data, sub_data$var), function(x){
        x[,-4]
      })
    })
  )
  
  # varname=data_list$RACE
  # data_for_value = varname[4]
  # apply the survival analysis
  results_survival <- lapply(data_list, function(varname) {
    lapply(varname, function(data_for_value) {
      survival_results(data_for_value)
    })
  })
  
  
  # reduce results into a table
  results_survival2 <- unlist(results_survival, recursive = FALSE)
  X <- Reduce(rbind, results_survival2)
  row.names(X) <-names(results_survival2)

  additonal_args <- list(
    col.names = c("Total n",
                  "n", "events", "Median Events\n(Months)",
                  "n", "events", "Median Events\n(Months)",
                  "Hazard Ratio", "95% Wald\nCI"),
    format = "xx"
  )
  
  # rname <- rownames(X)[3]
  # x <- split(X, 1:nrow(X))[[1]]
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
            x$km_ref_n + x$km_comp_n, # total n
            x$km_ref_n,
            x$km_ref_event,
            rcell(x$km_ref_median, format = "xx.x"),
            x$km_comp_n,
            x$km_comp_event,
            rcell(x$km_comp_median, format = "xx.x"),
            rcell(x$cox_hr, format = "xx.xx"),
            rcell(c(x$cox_lcl, x$cox_ucl), format = "(xx.xx, xx.xx)"),
            indent = if (header_row_name[1] == "ALL") 0 else 1
          )
        )
      }, split(X, 1:nrow(X)), rownames(X)),
      recursive = FALSE)
  )
  
  tbl <- do.call(rtable, c(additonal_args, rrow_collection))
  
  # Viewer(tbl)
  class(tbl) <- c("forest_survival", "forest_table", class(tbl))
  
  tbl
}


#' plot
#' 
#' @import grid
#' 
#' @export
#x <- tbl
forest_tte_plot <- function(x, arm.ref = "Reference", arm.comp = "Treatment",
                            padx = unit(.5, "lines"), cex = 1) {

  vp <- vpTree(
    parent = viewport(
      name = "forestplot",
      layout = grid.layout(
        nrow = 1, ncol = 11,
        widths = unit.c(
          stringWidth("Baseline Risk Factors        ") + 1 * padx,
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

  # need once
  grid.xaxis(at = c(log(0.1), log(0.5), log(1), log(2), log(5), log(10)), label = c(0.1, 0.5, 1, 2, 5, 10), vp = vpPath("col_11"))
  grid.lines(x = unit(c(0,0), "native"), y = unit(c(0,1), "npc"), vp = vpPath("col_11"),
             gp = gpar(lty = 2))  
  
  # Add Header
  draw_header(2, nrow(x), "Baseline Risk Factors","Total n", "n", "Events", "Median\n(Months)", "n", "Events", "Median\n(Months)", "Hazard\nRatio", "95% Wald\nCI", arm.ref,arm.comp)
  
  # Add table contents
  for (i in 1:nrow(x)){
   if (!is.null(x[i,1])) {
     draw_row(i+4, nrow(x), row.names(x)[i], x[i, 1], x[i, 2], x[i, 3], round(x[i, 4], 1), x[i, 5], x[i, 6], 
              round(x[i, 7], 1), round(x[i, 8], 2), paste("(", paste(round(x[i, 9],2), collapse = ", "), ")", sep = ""), c(log(abs(x[i,8])), log(abs(x[i,9]))), TRUE)
   } else if (is.null(x[i,1]) & row.names(x)[i] != "") {
     draw_row(i+4, nrow(x), row.names(x)[i], "", "", "", "", "", "", "", "", "", c(-999, -999), FALSE, 2)
   } else {
     draw_row(i+4, nrow(x), "", "", "", "", "", "", "", "", "", "", c(-999, -999), FALSE,2)
   }
  }
}

draw_header <- function(i,n, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12) {
  ypos <- unit(1 - i/(n+5), "npc")
  grid.text(x11, x = unit(0.5, "native"), y = unit(1 - 1/(n+5), "npc"), vp = vpPath("col_4"), gp = gpar(fontsize = 10 ,fontface = 2))
  grid.text(x12, x = unit(0.5, "native"), y = unit(1 - 1/(n+5), "npc"), vp = vpPath("col_7"), gp = gpar(fontsize = 10 ,fontface = 2))
  grid.text(x1, x = unit(0, "npc"), y = ypos, vp = vpPath("col_1"), just = "left", gp = gpar(fontsize = 10, fontface = 2))
  grid.text(x2, y = ypos, vp = vpPath("col_2"), gp = gpar(fontsize = 10, fontface = 2))
  grid.text(x3, y = ypos, vp = vpPath("col_3"), gp = gpar(fontsize = 10, fontface = 2))
  grid.text(x4, y = ypos, vp = vpPath("col_4"), gp = gpar(fontsize = 10, fontface = 2))
  grid.text(x5, y = ypos, vp = vpPath("col_5"), gp = gpar(fontsize = 10, fontface = 2))
  grid.text(x6, y = ypos, vp = vpPath("col_6"), gp = gpar(fontsize = 10, fontface = 2))
  grid.text(x7, y = ypos, vp = vpPath("col_7"), gp = gpar(fontsize = 10, fontface = 2))
  grid.text(x8, y = ypos, vp = vpPath("col_8"), gp = gpar(fontsize = 10, fontface = 2))
  grid.text(x9, y = ypos, vp = vpPath("col_9"), gp = gpar(fontsize = 10, fontface = 2))
  grid.text(x10, y = ypos, vp = vpPath("col_10"), gp = gpar(fontsize = 10 ,fontface = 2))
  grid.text(paste(x11, "\nBetter"), x = unit(-1, "native"), y = ypos, vp = vpPath("col_11"), gp = gpar(fontsize = 10, fontface = 2))
  grid.text(paste(x12, "\nBetter"), x = unit(1, "native"), y = ypos, vp = vpPath("col_11"), gp = gpar(fontsize = 10, fontface = 2))
  grid.lines(x = unit(c(0,1), "native"), y = unit(c(1-1.25/(n+5),1-1.25/(n+5)), "npc"), vp = vpPath("col_3"), gp = gpar(lty = 1, lwd = 2)) 
  grid.lines(x = unit(c(0,1), "native"), y = unit(c(1-1.25/(n+5),1-1.25/(n+5)), "npc"), vp = vpPath("col_4"), gp = gpar(lty = 1, lwd = 2))
  grid.lines(x = unit(c(0,0.95), "native"), y = unit(c(1-1.25/(n+5),1-1.25/(n+5)), "npc"), vp = vpPath("col_5"), gp = gpar(lty = 1, lwd = 2)) 
  grid.lines(x = unit(c(0,1), "native"), y = unit(c(1-1.25/(n+5),1-1.25/(n+5)), "npc"), vp = vpPath("col_6"), gp = gpar(lty = 1, lwd = 2)) 
  grid.lines(x = unit(c(0,1), "native"), y = unit(c(1-1.25/(n+5),1-1.25/(n+5)), "npc"), vp = vpPath("col_7"), gp = gpar(lty = 1, lwd = 2)) 
  grid.lines(x = unit(c(0,0.95), "native"), y = unit(c(1-1.25/(n+5),1-1.25/(n+5)), "npc"), vp = vpPath("col_8"), gp = gpar(lty = 1, lwd = 2))  
  grid.lines(unit(c(0,1), "npc"), y = unit.c(ypos, ypos) - unit(1/(2*n), "npc"), gp = gpar(col = "black", lty = 1, lwd = 2))

}

draw_row <- function(i,n, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, add_hline = FALSE, fontface = 1) {
  ypos <- unit(1 - i/(n+5), "npc")
  
  indent_x1 <- if (fontface == 1 && x1 != "ALL") 1 else 0
  
  grid.text(x1, x = unit(0, "npc") + unit(indent_x1, "lines"),
            y = ypos, vp = vpPath("col_1"),
            gp = gpar(fontsize = 10 , fontface = fontface),
            just = "left")
  grid.text(x2, y = ypos, vp = vpPath("col_2"), gp = gpar(fontsize = 10 , fontface = fontface))
  grid.text(x3, y = ypos, vp = vpPath("col_3"), gp = gpar(fontsize = 10 , fontface = fontface))
  grid.text(x4, y = ypos, vp = vpPath("col_4"), gp = gpar(fontsize = 10 , fontface = fontface))
  grid.text(x5, y = ypos, vp = vpPath("col_5"), gp = gpar(fontsize = 10 , fontface = fontface))
  grid.text(x6, y = ypos, vp = vpPath("col_6"), gp = gpar(fontsize = 10 , fontface = fontface))
  grid.text(x7, y = ypos, vp = vpPath("col_7"), gp = gpar(fontsize = 10 , fontface = fontface))
  grid.text(x8, y = ypos, vp = vpPath("col_8"), gp = gpar(fontsize = 10 , fontface = fontface))
  grid.text(x9, y = ypos, vp = vpPath("col_9"), gp = gpar(fontsize = 10 , fontface = fontface))
  grid.text(x10, y = ypos, vp = vpPath("col_10"), gp = gpar(fontsize = 10 , fontface = fontface))
 # grid.text(x11, y = ypos, vp = vpPath("col_11"))
  
  
  grid.lines(x = unit(x11[2:3], "native"), y = unit.c(ypos, ypos), vp = vpPath("col_11"), gp =gpar(lwd = 2))  
  grid.circle(x = unit(x11[1], "native"), y = ypos, r = unit(1/4, "lines"), vp = vpPath("col_11"),
              gp = gpar(fill = "blue"))
  
  
  if (add_hline) {
    grid.lines(unit(c(0,1), "npc"), y = unit.c(ypos, ypos) - unit(1/(2*n-2), "npc"), gp = gpar(col = "grey", lty = 1, lwd = 0.3))
  }
  
}

# survival_results(data_for_value[[1]])
# data = data_for_value[[1]]
survival_results <- function(data){
  
  # KM Estimate
  # Three scenarios:
  # 1. two arms
  # 2. ref arm has no records
  # 3. comp arm has no records
  arm_freq <- table(data$arm) 
  if (arm_freq[names(arm_freq) == levels(data$arm)[2]] == 0){
    km_sum <- as.data.frame(t(summary(survfit(Surv(time_to_event,event) ~ arm, data = data))$table))
    km_ref_n <- km_sum[1]
    km_comp_n <- 0
    km_ref_event <- km_sum[4]
    km_comp_event <- 0
    km_ref_median <- km_sum[7]
    km_comp_median <- NA
  } else if (arm_freq[names(arm_freq) == levels(data$arm)[1]] == 0){
    km_sum <- as.data.frame(t(summary(survfit(Surv(time_to_event,event) ~ arm, data = data))$table))
    km_ref_n <- 0
    km_comp_n <- km_sum[1]
    km_ref_event <- 0
    km_comp_event <- km_sum[4]
    km_ref_median <- NA
    km_comp_median <- km_sum[7]
  } else if (arm_freq[names(arm_freq) == levels(data$arm)[2]] * arm_freq[names(arm_freq) == levels(data$arm)[1]] > 0){
    km_sum <- summary(survfit(Surv(time_to_event,event) ~ arm, data = data))$table
    km_ref_n <- km_sum[1, 1]
    km_comp_n <- km_sum[2,1]
    km_ref_event <- km_sum[1, 4]
    km_comp_event <- km_sum[2, 4]
    km_ref_median <- km_sum[1, 7]
    km_comp_median <- km_sum[2, 7]
  } else stop("Invalid Arm Counts")
  
  # Cox Model
  # Three Scenarios: 
  # 1. both arms have events; 
  # 2. at least one of the two arms have no events
  # 3. data has only one arm.
  if (nrow(km_sum) == 2 & km_ref_event * km_comp_event > 0){
    cox_sum  <- summary(coxph(Surv(time_to_event,event) ~ arm, data = data))
    cox_hr   <- cox_sum$conf.int[1]
    cox_lcl  <- cox_sum$conf.int[3]
    cox_ucl  <- cox_sum$conf.int[4]
    cox_pval <- cox_sum$conf.int[5]
  } else {
    cox_hr   <- NA
    cox_lcl  <- NA
    cox_ucl  <- NA
    cox_pval <- NA
  }
  
  surv_table <- data.frame(km_ref_n, km_comp_n, 
                           km_ref_event, km_comp_event, 
                           km_ref_median, km_comp_median, 
                           cox_hr, cox_lcl, cox_ucl)
}

#' Forest Plot Numbers for Survival data with ADAM data structure
#' 
#' @export
#' 
#' @inheritParams forest_tte
#' @param ASL asl data frame
#' @param ATE data frame
#' 
#' @importFrom dplyr %>% filter
#' 
#' @examples 
#' \dontrun{
#' 
#' rm(list = ls())
#' library(atezo.data)
#' library(dplyr)
#' 
#' ATE <- ate(com.roche.cdt30019.go29436.re)
#' ASL <- asl(com.roche.cdt30019.go29436.re)
#' 
#' ASL$BAGED <- ifelse(ASL$BAGE <= median(ASL$BAGE), "<=median", ">median")
#' 
#' forest_tte_ADAM(
#'   ASL, ATE,
#'   groupvar = c("SEX", "BECOG", "COUNTRY"),
#'   arm.ref = "DUMMY A", arm.comp = "DUMMY B"
#' )
#'   
#' }
forest_tte_ADAM <- function(ASL, ATE,
                            outcome = "Overall Survival",
                            groupvar,
                            arm.ref,
                            arm.comp,
                            arm.var = "ARM",
                            time_to_event.var = "AVAL",
                            event.var = "CNSR", negate.event.var = TRUE) {
  
  ATE %needs% c("USUBJID", "STUDYID", "PARAM", time_to_event.var, event.var)
  ASL %needs% c("USUBJID", "STUDYID", groupvar, arm.var)
  
  event <- ATE[[event.var]]
  if (!(is.numeric(event) || is.logical(event))) stop("event var needs to be numeric or boolean")  
  
  ATE_f <- ATE %>% filter(PARAM == outcome)
  
  if (nrow(ATE_f) <= 0) stop("ATE data left after filtering")
  
  group_data <- merge(
    ATE_f[c("USUBJID", "STUDYID")],
    ASL[c("USUBJID", "STUDYID", groupvar)],
    all.x = TRUE, all.y = FALSE
  )
  
  forest_tte(
    time_to_event = ATE_f[[time_to_event.var]],
    event = if(negate.event.var) !ATE_f[[event.var]] else ATE_f[[event.var]],
    arm = ATE_f[[arm.var]], 
    group_data = group_data[, -c(1,2), drop=FALSE],
    arm.ref = arm.ref,
    arm.comp = arm.comp
  )
}

