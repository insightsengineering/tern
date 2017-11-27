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
#' library(forcats)
#' library(gridExtra)
#' 
#' ASL <- asl(com.roche.cdpt7722.wo29637.rl)
#' AQS <- aqs(com.roche.cdpt7722.wo29637.rl)
#' 
#' #parammap <- unique(AQS[c("PARAM", "PARAMCD")]) 
#' 
#' ASL_f <- ASL %>% select(STUDYID, USUBJID, ARM, ARMCD)
#' AQS_f <- AQS %>% select(STUDYID, USUBJID, PARAMCD, PARAM, AVAL, AVISIT, AVISITN, ADY, ABLFL, APBFL, CHG, BASE)
#'  
#' ANL <- left_join(ASL_f, AQS_f, by = c("STUDYID", "USUBJID")) %>% filter(PARAMCD == "MDASI23")
#' 
#' df <- chgfbl_data(data = ANL)
#' chgfbl_plot(data = df, ytype = "CHG", errbar = "SE", arm_label = "Treatment Arm", ref_line = c(2, -2))
#' chgfbl_plot(data = df, ytype = "AVAL", errbar = "SD", arm_label = "Treatment Arm", ref_line = 3)
#' 
#' tbl <- chgfbl_table(data=df)
#' Viewer(tbl)
#' 
#' 
#' # now 
#' # chng_data <- ... # you already have this # do not export
#' # change_table(chng_data) # split by arm_name make columns and then combine
#' # change_plot(chng_data) # group_by(arm_name) make plotA
#' 
#' # maybe in future
#' # chng_table <- table...
#' # change_plot(chng_table)
#' # selection for chg_var can be "CHG" or "PCHG"
#' 
chgfbl_data <- function(data,
                        arm_var = "ARM"
                        ) {

  #####################
  # Argument checking #
  #####################
  
  if (any(is.na(data[[arm_var]]))) stop("currently cannot deal with missing values in arm")
  #if (!all(group_by %in% unique(data$PARAMCD))) stop("Selected parameters not found in analysis data")
  
  testdupvars <- c("STUDYID", "USUBJID",arm_var,"PARAMCD", "PARAM", "AVISIT", "AVISITN")
  
  #Remove any duplicated records and filter on param values
  data_f <- data[!duplicated(data[testdupvars]), c(testdupvars, "ABLFL","AVAL", "CHG")]
  data_f$CHG <- ifelse(is.na(data_f$CHG), ifelse(data_f$ABLFL == "Y", 0, NA),data_f$CHG)
  attr(data_f$CHG, "label") <- attr(data_f$AVAL, "label")
  
  #Convert AVAL and CHG from wide to long format
  df <- gather(data_f, type, value, c("AVAL", "CHG")) %>%
    mutate(arm = factor(.[[arm_var]]),
           arm_name = factor(paste(.[[arm_var]], type, sep="_")))
  
  df.sum <- df %>% filter(!is.na(value)) %>%
    group_by(arm_name, arm, type, PARAMCD, PARAM, AVISIT, AVISITN) %>%
    summarise(n = sum(!is.na(value)),
              mean = mean(value, na.rm=T),
              sd   = sd(value, na.rm=T),
              median = median (value, na.rm=T),
              q1   = quantile(value, 0.25, na.rm = T),
              q3   = quantile(value, 0.75, na.rm = T),
              min  = min(value, na.rm=T),
              max  = max(value, na.rm=T))
  
  df.final <- data.frame(df.sum) %>%
    mutate(visit = fct_reorder(factor(AVISIT), AVISITN)) %>%
    select(arm_name, arm, type, n:visit, PARAM)
  
  df.final
} 



############################################################################
# Plotting function based on data from chgfbl_data
############################################################################
## selection for y = c("AVAL", "CHG", "PCHG")
## selection for errbar = c("SE","SD", "95CI")
chgfbl_plot <- function(data,
                        ytype = "CHG",
                        errbar = "SE",
                        arm_label = "Treatment Arms",
                        ref_line = NULL) {
  
  if (errbar == "SD") {
    data$errval = data$sd
  } else if (errbar == "SE") {
    data$errval = data$sd/sqrt(data$n)
  } else if (errbar == "95CI") {
    data$errval = 1.96*data$sd/sqrt(data$n)
  } else {data$errval = NA}
  
  
  visitcd <- data.frame(keywd = character(0), shortkey = character(0), stringsAsFactors = FALSE) %>%
    mapply(c, .,
           c("RANDOMIZATION", "R"), 
           c("CYCLE"       , "C"),
           c("DAY"        , "D"),
           c("FOLLOW-UP"   , "FU"),
           c("SURVIVAL"    , "S"),
           c("END OF TREATMENT", "EOT"),
           c("VISIT", " "), 
           c(" ", "")) %>%
    as.data.frame(.)
  
  shorten_visit <- function(x) {
    for (i in 1:nrow(visitcd)) {
      x <- gsub(visitcd$keywd[i], visitcd$shortkey[i], x)
    }
    x
  }

  plotdat <- data %>% filter(type == ytype) %>%
    mutate(ll = mean - errval,
           ul = mean + errval,
           visitlab = fct_relabel(visit, shorten_visit), 
           arm_short = unlist(lapply(as.character(arm), reflow, limit=15)))
  
  
  if (ytype == "AVAL") {
    ylabel = "Value at Visit"
  } else if (ytype == "CHG") {
    ylabel = "Change from Baseline"
  } else if (ytype == "PCHG") {
    ylabel = "% Change from Baseline"
  }
  
  # move second group .05 to the left and right
  pd <- position_dodge(0.2) 
  
  #make change from baseline plot
  p <- ggplot(plotdat, aes(x = visitlab, y = mean, group = arm, color= arm)) + 
    geom_hline(yintercept = 0, color = "grey75", size = 1) +
    geom_hline(yintercept = ifelse(!is.null(ref_line),ref_line, 0), color = "grey75", linetype = 2, size = 1) +
    geom_line(position=pd, size = 1) + 
    geom_point(position=pd) + 
    geom_errorbar(data = plotdat, aes(ymin = ll, ymax = ul), position=pd) +
    theme_bw() + labs(color = arm_label, x = "Visits", y = ylabel ) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "none", legend.background = element_rect(fill="grey95"),
          legend.direction = "horizontal", legend.title = element_text(face="bold"),
          axis.text.x = element_text(angle = 90))
  
  #display count at each visit table as separate plot
  t <- ggplot(plotdat, aes(x = visitlab, y = fct_rev(factor(arm_short)), label = n, color = arm)) +
    geom_text(size = 2.5) + theme_bw() +
    labs(subtitle = "Number of subjects at each visit")
  t1 <- t + theme(legend.position="none", 
          axis.title.y = element_blank(),
          axis.text.y = element_text(color = rev(unique(ggplot_build(t)$data[[1]]$colour)), face = "bold"),
          axis.text.x = element_blank(), 
          axis.title.x = element_blank(), 
          axis.ticks.x = element_blank(),
          panel.grid = element_blank()) 
  
  #wrap plot and table into grobs, and align left margins
  glist <- lapply(list(plot=p, text=t1), ggplotGrob)
  leftmar <- do.call(unit.pmax, lapply(glist, "[[", "widths"))
  glist.aligned <- lapply(glist, function(x) {
    x$widths <- leftmar
    x
  })
  
  
  #Plot the two grobs using grid.arrange
  grid.newpage()
  do.call(grid.arrange, c(glist.aligned, 
                          list(ncol=1), 
                          list(heights=c(8,length(unique(plotdat$arm))))))
  
}



############################################################################
# Table function based on data from chgfbl_data
############################################################################
chgfbl_table <- function(data) {
  
  df <- data %>%
    filter(!(visit == levels(visit)[1] & type != "AVAL"))
  
  col2type = unique(df$type)[!unique(df$type) %in% "AVAL"]
  
  result_sum <- lapply(split(df, df$visit), function(x) {
    lapply(split(x, x$arm_name), function(xi) {
      list(
        "n"         = rcell(xi$n, format = "xx"),
        "Mean (SD)" = rcell(c(xi$mean, xi$sd), format = "xx.x (xx.x)"),
        "Median"    = rcell(xi$median, format = "xx.x"),
        "IQR"       = rcell(c(xi$q1, xi$q3), format = "xx.xx - xx.xx"),
        "Min - Max" = rcell(c(xi$min, xi$max), format = "xx.xx - xx.xx")
      )
    })
  })
  
  #Helper functions for display with rtable
  lrrow <- function(row.name, l, ...) {
    do.call(rrow, c(list(row.name = row.name, ...), l))
  }
  
  out_sum <- Map(function(x, xname) {
    temp_row <- list_transpose(x)
    temp_block <- Map(lrrow, names(temp_row), temp_row, indent = 1)
    c(list(rrow(xname)),
      temp_block,
      list(rrow()))
  }, result_sum, as.list(names(result_sum)))
  
  
  ##########################################
  # Build output data structure with rtable#
  ##########################################3
  colname.n <- rep(unlist(out_sum[[1]][[2]]), each = 2)
  colname.txt <- names(colname.n) %>%
    vapply(function(x) reflow(x, limit = 15), character(1)) %>%
    sub("_.*$", "", .) %>%
    paste(c("Value at Visit", ifelse(col2type == "CHG", "Change from\nBaseline", 
                                     ifelse(col2type == "PCHG", "% Change from\nBaseline", NA_character_))), sep="\n")
    
  
  #--- Header Section ---#
  out_header <- list(
    col.names = paste0(colname.txt, '\n', "(N=", colname.n, ")"),
    format = "xx")
  
  
  tbl <- do.call(rtable, 
                 c(out_header, 
                   unlist(out_sum, recursive = F))
                 )
  
  tbl
}


### every line in dfi is an cell unit in table 
# dfi <- df.s[[1]]

### and a column in rtables lets make this later
# coli <- apply(dfi, 1, FUN = function(row) {
#   list(
#     rrow("name", indent = 1, ...),
#     ...
#   )
# })
# cbind(col1, col2, col3)



#' Helper function to line break
#' x = "hellO-world abcerewerwere testing "
#' reflow(x)
reflow <- function(x, 
                    delim = " ", 
                    limit = NULL) {
  xsplit <- unlist(strsplit(x, delim))
  ctxt = ""
  n = 0
  
  if (is.null(limit)) {limit <- max(unlist(lapply(xsplit, nchar)))}
  
  for (i in xsplit) {
    if (nchar(i) > limit) {
      ctxt <- ifelse(n, paste0(ctxt, "\n", i, "\n"), paste0(ctxt, i, "\n"))
      n = 0
    } else if ((n + nchar(i)) > limit) {
      ctxt <- paste0(ctxt, "\n", i)
      n = nchar(i)
    } else {
      ctxt <- ifelse(n, paste0(ctxt, delim, i), paste0(ctxt, i))
      n = n + nchar(i)
    }
  }
  
  outtxt <- ifelse(substring(ctxt, nchar(ctxt)) == "\n", substr(ctxt, 1, nchar(ctxt)-1), ctxt)
  
  outtxt
}
