
#' Time-to-event Forest Plot Table
#'
#' @param time_to_event time to event data
#' @param event is boolean, \code{TRUE} if event, \code{FALSE} if time_to_event
#'   is censored
#' @param group_data data frame with one column per grouping
#' @param arm vector with arm information
#' @param covariates set to NULL; currently not available for multivariate survival analysis
#' 
#' @details
#' Cox PH model is used for hazard ratio calculation
#'  
#' @importFrom survival survfit Surv coxph
#' 
#' @export
#' 
#' @author Yuyao Song (songy24), \email{yuyao.song@roche.com}
#' 
#' @examples 
#' 
#' library(random.cdisc.data)
#' ASL <- radam("ASL")
#' ATE <- radam("ATE", ADSL = ASL)
#' 
#' ATE_f <- ATE %>% filter(PARAMCD == "OS") 
#' 
#' ASL_f <- right_join(ASL %>% select(USUBJID, STUDYID, SEX, RACE, ARM),
#'                         ATE_f %>% select(USUBJID, STUDYID))
#' 
#' tbl <- forest_rsp(
#'   response = ATE_f$AVAL,
#'   event = ATE_f$CNSR == 0,
#'   arm = ASL_f$ARM, 
#'   group_data = ASL_f %>% select("SEX", "RACE")
#' )
#' 
#' tbl
#' 
#' 
#' 
#' \dontrun{
#' library(atezo.data)
#' library(dplyr)
#' library(forcats)
#' library(survival)
#' library(teal.oncology)
#' 
#' '%needs%' <- teal.oncology:::'%needs%'
#' 
#' ATE <- ate(com.roche.cdt30019.go29436.re)
#' ASL <- asl(com.roche.cdt30019.go29436.re)
#' 
#' ASL$temp <- c(rep("A", 200), rep("B", 250), rep(NA,1202-450))
#' 
#' tbl_stream <- get_forest_survival_table(com.roche.cdt30019.go29436.re)
#' Viewer(tbl_stream)
#' 
#' 
#' ATE_f <- ATE %>% filter(PARAMCD == "OS") %>% 
#'              filter(ITTWTFL == "Y") %>% 
#'              filter(ARM %in% c("DUMMY A", "DUMMY C")) %>%
#'              select(c("USUBJID", "STUDYID", "AVAL", "CNSR", "ARMCD"))
#' ASL_f <- ASL %>% filter(ITTWTFL == "Y") %>% 
#'              filter(ARM %in% c("DUMMY A", "DUMMY C")) %>% 
#'              select(c("USUBJID", "STUDYID", "SEX", "MLIVER", "TCICLVL2", "AGE4CAT", "RACE", "temp"))
#' 
#'
#' group_data <- left_join(ASL_f, ATE_f %>% select(c("USUBJID", "STUDYID")))
#' group_data$MLIVER <- factor(group_data$MLIVER, levels = c("Y", "N"), labels = c("Yes", "No"))
#' group_data$TCICLVL2 <- factor(group_data$TCICLVL2, levels = c("TC3 or IC2/3", "TC0/1/2 and IC0/1"), labels = c("TC3 or IC2/3", "TC0/1/2 and IC0/1"))#' 
#' group_data$SEX <- factor(group_data$SEX, levels = c("F", "M"), labels = c("FEMALE", "MALE"))
#' group_data$AGE4CAT <- factor(group_data$AGE4CAT, levels = c("<65", "65 to 74", "75 to 84", ">=85"), labels = c("<65", "65 to 74", "75 to 84", ">=85"))
#' group_data$RACE <- factor(group_data$RACE, levels = c("ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE", "UNKNOWN"), labels = c("ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE", "UNKNOWN"))
#' names(group_data) <- labels_over_names(group_data)
#' head(group_data)
#' 
#' arm <- fct_relevel(ATE_f$ARMCD, "C")
#' 
#' tbl <- forest_tte(
#'    time_to_event = ATE_f$AVAL,
#'    event = ATE_f$CNSR == 0,
#'    group_data = group_data[, -c(1,2), drop=FALSE],
#'    arm = arm
#' )
#' Viewer(tbl)
#' 
#' Viewer(tbl, tbl_stream)
#' 
#' compare_rtables(tbl, tbl_stream, comp.attr = FALSE)
#' 
#' forest_tte_plot(tbl)
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
      #sub_data <- sub_data %>% filter(var != "")
      if ("" %in% levels(sub_data$var)) sub_data$var <- factor(sub_data$var, levels = levels(sub_data$var)[-which(levels(sub_data$var) == "")])
   #   sub_data$var <- as.factor(as.character(sub_data$var))
      sub_data$var <- as.factor(sub_data$var)
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
  
 ## for debugging
 # rtab <- results_survival2[[1]]
 # for (i in 2:length(results_survival2)) {rtab <- rbind(rtab,results_survival2[[i]])}

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

#' Time-to-event Forest Plot
#'
#' @param x time-to-event forest plot table from function \code{forest_tte} 
#' @param arm.ref Label displayed in the forest plot for reference arm
#' @param arm.ref Label displayed in the forest plot for comparison arm
#' @param padx gap between two columns
#' @param cex multiplier applied to overall fontsize
#' 
#' @details
#' This function can only be applied to the rtable output from \code{forest_tte}.
#'  
#' @import grid
#' 
#' @export
#' 
#' @author Yuyao Song (songy24), \email{yuyao.song@roche.com}
#' 
#' @examples 
#' 
#' library(random.cdisc.data)
#' ASL <- radam("ASL")
#' ATE <- radam("ATE", ADSL = ASL)
#' 
#' ATE_f <- ATE %>% filter(PARAMCD == "OS") 
#' 
#' ASL_f <- right_join(ASL %>% select(USUBJID, STUDYID, SEX, RACE, ARM),
#'                         ATE_f %>% select(USUBJID, STUDYID))
#' 
#' tbl <- forest_rsp(
#'   response = ATE_f$AVAL,
#'   event = ATE_f$CNSR == 0,
#'   arm = ASL_f$ARM, 
#'   group_data = ASL_f %>% select("SEX", "RACE")
#' )
#' 
#' forest_tte_plot(tbl)
#' 
#' \dontrun{
#' library(atezo.data)
#' library(dplyr)
#' library(forcats)
#' library(survival)
#' library(teal.oncology)
#' 
#' '%needs%' <- teal.oncology:::'%needs%'
#' 
#' ATE <- ate(com.roche.cdt30019.go29436.re)
#' ASL <- asl(com.roche.cdt30019.go29436.re)
#' 
#' tbl_stream <- get_forest_survival_table(com.roche.cdt30019.go29436.re)
#' Viewer(tbl_stream)
#' 
#' ASL$temp <- c(rep("A", 500), rep("B", 450), rep(NA,1202-950))
#' 
#' 
#' ATE_f <- ATE %>% filter(PARAMCD == "OS") %>% 
#'              filter(ITTWTFL == "Y") %>% 
#'              filter(ARM %in% c("DUMMY A", "DUMMY C")) %>%
#'              select(c("USUBJID", "STUDYID", "AVAL", "CNSR", "ARM"))
#' ASL_f <- ASL %>% filter(ITTWTFL == "Y") %>% 
#'              filter(ARM %in% c("DUMMY A", "DUMMY C")) %>% 
#'              select(c("USUBJID", "STUDYID", "SEX", "MLIVER", "TCICLVL2", "AGE4CAT", "RACE"))
#' 
#'
#' group_data <- left_join(ASL_f, ATE_f %>% select(c("USUBJID", "STUDYID")))
#' group_data$MLIVER <- factor(group_data$MLIVER, levels = c("Y", "N"), labels = c("Yes", "No"))
#' group_data$TCICLVL2 <- factor(group_data$TCICLVL2, levels = c("TC3 or IC2/3", "TC0/1/2 and IC0/1"), labels = c("TC3 or IC2/3", "TC0/1/2 and IC0/1"))#' 
#' group_data$SEX <- factor(group_data$SEX, levels = c("F", "M"), labels = c("FEMALE", "MALE"))
#' group_data$AGE4CAT <- factor(group_data$AGE4CAT, levels = c("<65", "65 to 74", "75 to 84", ">=85"), labels = c("<65", "65 to 74", "75 to 84", ">=85"))
#' group_data$RACE <- factor(group_data$RACE, levels = c("ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE", "UNKNOWN"), labels = c("ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE", "UNKNOWN"))
#' names(group_data) <- labels_over_names(group_data)
#' head(group_data)
#' 
#' arm <- fct_relevel(ATE_f$ARM, "DUMMY C")
#' 
#' tbl <- forest_tte(
#'    time_to_event = ATE_f$AVAL,
#'    event = ATE_f$CNSR == 0,
#'    group_data = group_data[, -c(1,2), drop=FALSE],
#'    arm = arm
#' )
#' Viewer(tbl)
#' 
#' Viewer(tbl, tbl_stream)
#' 
#' compare_rtables(tbl, tbl_stream, comp.attr = FALSE)
#' 
#' library(grid)
#' forest_tte_plot(tbl, levels(arm)[1], levels(arm)[2], padx=unit(0, "lines"))
#' 
#' 
#' }
#' 
#x <- tbl
forest_tte_plot <- function(x, arm.ref = "ReferenceAAA verylongtitle", arm.comp = "TreatmentAAA verylongtitle",
                            padx = unit(0, "lines"), cex = 1) {

  
  rn <- c("Baseline Risk Factors", row.names(x))
  
  
  vp <- vpTree(
    parent = viewport(
      name = "forestplot",
      layout = grid.layout(
        nrow = 1, ncol = 11,
        widths = unit.c(
          stringWidth(rn[which.max(nchar(rn))]) + 1 * padx,
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
      )
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
  pushViewport(viewport(gp = gpar(cex = cex))) 
  
  pushViewport(plotViewport(margins = c(3,2,14,2)))
  
  pushViewport(vp)
  
  # grid.ls(viewports = TRUE)
  seekViewport("forestplot")
  
  arm.ref <- wrap_text(arm.ref, width = unit(4, "cm"), collapse = "\n")
  arm.comp <- wrap_text(arm.comp, width = unit(4, "cm"), collapse = "\n")
  
  # arm.ref <- gsub( " ", "\n", arm.ref)
  # arm.comp <- gsub( " ", "\n", arm.comp)

  # Add Header
  draw_header_tte(x1 = "Baseline Risk Factors",x2 = "Total n", x3 = "n", x4 = "Events", x5 = "Median (months)", x6 = "n", x7 = "Events", x8 = "Median (months)", x9 = "Hazard Ratio", x10 = "95% Wald CI", x11 = arm.ref, x12 = arm.comp)
  
  # need once
  grid.xaxis(at = c(log(0.1), log(0.5), log(1), log(2), log(5), log(10)), label = c(0.1, 0.5, 1, 2, 5, 10), vp = vpPath("col_11"))
  grid.lines(x = unit(c(0,0), "native"), y = unit(c(0, 1), "npc"), vp = vpPath("col_11"),
             gp = gpar(lty = 2))  
  
  
  # Add table contents
  for (i in 1:nrow(x)){
   if (!is.null(x[i,1])) {
     draw_row(i, nrow(x), row.names(x)[i], x[i, 1], x[i, 2], x[i, 3], round(x[i, 4], 1), x[i, 5], x[i, 6], 
              round(x[i, 7], 1), round(x[i, 8], 2), paste("(", paste(round(x[i, 9],2), collapse = ", "), ")", sep = ""), c(log(abs(x[i,8])), log(abs(x[i,9]))), TRUE)
   } else if (is.null(x[i,1]) & row.names(x)[i] != "") {
     draw_row(i, nrow(x), row.names(x)[i], "", "", "", "", "", "", "", "", "", c(NA, NA, NA), FALSE, 2)
   } else {
     draw_row(i, nrow(x), "", "", "", "", "", "", "", "", "", "", c(NA, NA, NA), FALSE,2)
   }
  }
}

draw_header_tte <- function(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12) {
  
  library(stringr)
  
  ypos = unit(1, "npc")+unit(1, "lines")

  grid.text(x11, x = unit(0.5, "native"), y = ypos + unit(7.75 + str_count(x11, "\n"), "lines"), vp = vpPath("col_4"), gp = gpar(fontsize = 10 ,fontface = 2))
  grid.text(x12, x = unit(0.5, "native"), y = ypos + unit(7.75 + str_count(x12, "\n"), "lines"), vp = vpPath("col_7"), gp = gpar(fontsize = 10 ,fontface = 2))
  grid.text(x1, x = unit(0, "npc"), y = ypos + unit(0.5, "lines"), vp = vpPath("col_1"), just = "left", gp = gpar(fontsize = 10, fontface = 2))
  grid.text(x2, y = ypos, vp = vpPath("col_2"), gp = gpar(fontsize = 10, fontface = 2), rot = 90, just = c("left", "center"))
  grid.text(x3, y = ypos, vp = vpPath("col_3"), gp = gpar(fontsize = 10, fontface = 2), rot = 90, just = c("left", "center"))
  grid.text(x4, y = ypos, vp = vpPath("col_4"), gp = gpar(fontsize = 10, fontface = 2), rot = 90, just = c("left", "center"))
  grid.text(x5, y = ypos, vp = vpPath("col_5"), gp = gpar(fontsize = 10, fontface = 2), rot = 90, just = c("left", "center"))
  grid.text(x6, y = ypos, vp = vpPath("col_6"), gp = gpar(fontsize = 10, fontface = 2), rot = 90, just = c("left", "center"))
  grid.text(x7, y = ypos, vp = vpPath("col_7"), gp = gpar(fontsize = 10, fontface = 2), rot = 90, just = c("left", "center"))
  grid.text(x8, y = ypos, vp = vpPath("col_8"), gp = gpar(fontsize = 10, fontface = 2), rot = 90, just = c("left", "center"))
  grid.text(x9, y = ypos, vp = vpPath("col_9"), gp = gpar(fontsize = 10, fontface = 2), rot = 90, just = c("left", "center"))
  grid.text(x10, y = ypos, vp = vpPath("col_10"), gp = gpar(fontsize = 10 ,fontface = 2), rot = 90, just = c("left", "center"))
  grid.text("Better", x = unit(-1.0, "native"), y = ypos + unit(.5, "lines"), vp = vpPath("col_11"), gp = gpar(fontsize = 10, fontface = 2))
  grid.text("Better", x = unit(1.0, "native"), y = ypos + unit(.5, "lines"), vp = vpPath("col_11"), gp = gpar(fontsize = 10, fontface = 2))
  grid.text(x12, x = unit(-1.0, "native"), y = ypos + unit(str_count(x11, "\n") + 1.5, "lines"), vp = vpPath("col_11"), gp = gpar(fontsize = 10, fontface = 2))
  grid.text(x11, x = unit(1.0, "native"), y = ypos + unit(str_count(x12, "\n") + 1.5, "lines"), vp = vpPath("col_11"), gp = gpar(fontsize = 10, fontface = 2))
  grid.lines(x = unit(c(0,1), "native"), y = ypos + unit(5.75, "lines"), vp = vpPath("col_3"), gp = gpar(lty = 1, lwd = 2))
  grid.lines(x = unit(c(0,1), "native"), y = ypos + unit(5.75, "lines"), vp = vpPath("col_4"), gp = gpar(lty = 1, lwd = 2))
  grid.lines(x = unit(c(0,0.9), "native"), y = ypos + unit(5.75, "lines"), vp = vpPath("col_5"), gp = gpar(lty = 1, lwd = 2)) 
  grid.lines(x = unit(c(0.1,1), "native"), y = ypos + unit(5.75, "lines"), vp = vpPath("col_6"), gp = gpar(lty = 1, lwd = 2))
  grid.lines(x = unit(c(0,1), "native"), y = ypos + unit(5.75, "lines"), vp = vpPath("col_7"), gp = gpar(lty = 1, lwd = 2))
  grid.lines(x = unit(c(0,1), "native"), y = ypos + unit(5.75, "lines"), vp = vpPath("col_8"), gp = gpar(lty = 1, lwd = 2))
  grid.lines(unit(c(0,1), "npc"), y = ypos - unit(0.5, "lines"), gp = gpar(col = "black", lty = 1, lwd = 2))

}

draw_row <- function(i,n, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, add_hline = FALSE, fontface = 1) {
  ypos <- unit(1 - i/(n+1), "npc")
  
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


  #Only draw if the CI is within the range of 0.1-10
  if (!any(is.na(x11)) && !is.na(x11[1]) && x11[2] <= log(10) && x11[3] >= log(0.1)){
    if ((is.na(x11[2]) || x11[2] < log(0.1)) && x11[3] <= log(10) ){
      grid.lines(x = unit(c(log(0.1), x11[3]), "native"), 
                 y = unit.c(ypos, ypos), 
                 arrow = arrow(angle = 30, length = unit(0.5, "lines"), ends = "first"), 
                 vp = vpPath("col_11"), 
                 gp =gpar(col = "blue", lwd = 2)) 
    }
    if (x11[2] >= log(0.1) && (is.na(x11[3]) || x11[3] > log(10))){
      grid.lines(x = unit(c(x11[2], log(10)), "native"), 
                 y = unit.c(ypos, ypos), 
                 arrow = arrow(angle = 30, length = unit(0.5, "lines"), ends = "last"), 
                 vp = vpPath("col_11"), 
                 gp =gpar(col = "blue", lwd = 2)) 
    }
    if (x11[2] >= log(0.1) && x11[3] <= log(10) ){
      grid.lines(x = unit(c(x11[2], x11[3]), "native"), 
                 y = unit.c(ypos, ypos), 
                 vp = vpPath("col_11"), 
                 gp =gpar(col = "blue", lwd = 2)) 
    }
    if (x11[2] < log(0.1) && x11[3] > log(10) ){
      grid.lines(x = unit(c(log(0.1), log(10)), "native"), 
                 y = unit.c(ypos, ypos), 
                 arrow = arrow(angle = 30, length = unit(0.5, "lines"), ends = "both"), 
                 vp = vpPath("col_11"), 
                 gp =gpar(col = "blue", lwd = 2)) 
    }
    if (x11[1] >= log(0.1) && x11[1] <= log(10)){
      grid.circle(x = unit(x11[1], "native"),
                  y = ypos, r = unit(1/3.5, "lines"), 
                  vp = vpPath("col_11"),
                  gp = gpar(col = "blue", fill = "blue"))
    }
  }    
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
    km_ref_n <- as.numeric(km_sum[1])
    km_comp_n <- 0
    km_ref_event <- as.numeric(km_sum[4])
    km_comp_event <- 0
    km_ref_median <- as.numeric(km_sum[7])
    km_comp_median <- NA
  } else if (arm_freq[names(arm_freq) == levels(data$arm)[1]] == 0){
    km_sum <- as.data.frame(t(summary(survfit(Surv(time_to_event,event) ~ arm, data = data))$table))
    km_ref_n <- 0
    km_comp_n <- as.numeric(km_sum[1])
    km_ref_event <- 0
    km_comp_event <- as.numeric(km_sum[4])
    km_ref_median <- NA
    km_comp_median <- as.numeric(km_sum[7])
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

