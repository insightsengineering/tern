#' our fast km plot
#' 
#' 
#' @import grid
#' @import survival
#' @importFrom scales col_factor
#' @export
#' 
#' 
#' @examples 
#' 
#' \dontrun{
#' library(atezo.data)
#' library(dplyr)
#' library(forcats)
#' library(survival)
#' ATE <- ate(com.roche.cdt30019.go29436.re)
#' 
#' ANL <- ATE %>%
#'   filter(PARAMCD == "OS") %>%
#'   select(AVAL, CNSR, ARM, SEX, RACE, MLIVER) %>%
#'   mutate(ARM = fct_relevel(ARM, "DUMMY C"))
#' 
#' kmplot(Surv(AVAL, 1-CNSR) ~ ARM, data = ANL)
#' 
#' kmplot(Surv(AVAL, 1-CNSR) ~ ARM, data = ANL, add_coxph = TRUE, add_km = TRUE)
#' 
#' 
#' ## facet by row
#' nplots <- 4
#' # new plot
#' grid.newpage()
#' 
#' # margins
#' pushViewport(plotViewport(margin = c(3, 10, 2, 2)))
#' 
#' # layout
#' pushViewport(viewport(layout = grid.layout(ncol = 1, nrow = 2*nplots-1,
#'    heights = unit(head(rep(c(1, 7), nplots), -1), head(rep(c("null", "lines"), nplots), -1))
#' )))
#' 
#' # now add first plot
#' pushViewport(viewport(layout.pos.row = 1))
#' kmplot(Surv(AVAL, 1-CNSR) ~ ARM, data = ANL, add_coxph = TRUE, add = TRUE)
#' popViewport()
#' 
#' #pushViewport(viewport(layout.pos.row = 2))
#' #grid.rect(gp = gpar(fill = "thistle"))
#' #popViewport()
#' 
#' pushViewport(viewport(layout.pos.row = 3))
#' grid.text("MY TITLE", y = unit(1, "npc") + unit(1, "lines"), gp = gpar(fontface = "bold", fontsize = 16))
#' kmplot(Surv(AVAL, 1-CNSR) ~ ARM, data = ANL, add_coxph = TRUE, add = TRUE)
#' popViewport()
#' 
#' 
#' #survminer::ggsurvplot(fit, risk.table = TRUE, break.time.by = 2)
#' 
#' }
#' 
kmplot <- function(formula_km, data, add_km = TRUE, 
                   add_coxph = TRUE, formula_coxph = formula_km, 
                   info_coxph = "Cox Proportional Model: Unstratified Analysis",
                   add = FALSE, 
                   title = "Kaplan - Meier Plot") {
  
  fit <- survfit(formula_km, data = data)

  
  if (length(fit$strata) > 9) stop("unfortunately we currently do not have more than 9 colors to encode different stratas")
  
  # extract kmplot relevant data
  df <- data.frame(
    time = fit$time,
    surv = fit$surv,
    n.risk = fit$n.risk,
    n.censor = fit$n.censor,
    n.event = fit$n.event,
    std.err = fit$std.err,
    upper = fit$upper,
    lower = fit$lower,
    strata = factor(rep(names(fit$strata), fit$strata), levels = names(fit$strata))
  )
  
  # split by strata
  df_s <- split(df, df$strata)
  
  # get the color pallete
  col_pal <- col_factor("Set1", domain = names(df_s))  
  

  # now do the plotting
  if(!add) {
    grid.newpage()
    pushViewport(plotViewport(margins = c(3, 10, 2, 2)))    
  }
  
  pushViewport(viewport(layout = grid.layout(
    nrow = 3, ncol = 1,
    heights = unit(c(5, 4, 3), c("null", "lines", "null")),
    widths = unit(1, "npc"))))
  
  pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 1))
  
  pushViewport(dataViewport(xData = df$time, yData = c(0,1)))
  grid.xaxis()
  grid.yaxis()
  grid.rect()
  grid.text(title, y = unit(1, "npc") + unit(1, "lines"), gp = gpar(fontface = "bold", fontsize = 16))
  
  Map(function(x, col) {
    grid.lines(
      x = c(0, rep(x$time, each = 2)),
      y = c(rep(c(1, head(x$surv, -1)), each = 2), tail(x$surv, 1)),
      default.units = "native",
      gp = gpar(col = col)
    )
    grid.points( 
      x = x[x$n.censor !=0, "time"],
      y = x[x$n.censor !=0, "surv"],
      pch = 3, 
      size = unit(0.5, "char"),
      gp = gpar(col = col))
    
  }, df_s, col_pal(names(df_s)))
  
  
  grid.text(
    label = "Survival Probability",
    x = unit(-9, "lines"),
    rot = 90
  )
  
  ## add coxph
  if (add_coxph) {
    fitcox <- coxph(formula_coxph, data = data)
    
    sfit <- summary(fitcox)
    
    hr <- sfit$coefficients[, "exp(coef)", drop = FALSE]  
     
    ci <- sfit$conf.int[, c("lower .95", "upper .95"), drop = FALSE]  
     
    pvalues <- sfit$coefficients[, "Pr(>|z|)", drop = FALSE]  
    
  
    info <- cbind(hr, ci, pvalues)
    sinfo <- split(as.data.frame(info), 1:nrow(info))
    
    tbl <- do.call(
      rtable,
      c(
        list(col.names = c("HR", "95% CI of HR", "p-value")),
        lapply(sinfo, function(xi) {
          rrow(
            row.name = rownames(xi),
            rcell(xi$'exp(coef)', format = "xx.xx"),
            rcell(c(xi$`lower .95`, xi$`upper .95`), format = "(xx.xx, xx.xx)"),
            rcell(xi$'Pr(>|z|)', format = "xx.xxx")
          )
        })
      )
    )
    tblstr <- toString(tbl, gap = 2)
    lab <- paste0(info_coxph, "\n", tblstr)
    grid.text(label = lab,
              x = unit(1, "lines"), y = unit(1, "lines"),
              just = c("left", "bottom"),
              gp = gpar(fontfamily = "mono", fontsize = 8)
              )
    

  }
  
  if (add_km){
    kminfo <- summary(fit)$table[ , c("records", "median", "0.95LCL", "0.95UCL")]
    skminfo <- split(as.data.frame(kminfo), 1:nrow(kminfo))
    tblkm <- do.call(
      rtable,
      c(
        list(col.names = c("N", "median", "95% CI for median")),
        lapply(skminfo, function(xi) {
          rrow(
            row.name = rownames(xi),
            rcell(xi$records, format = "xx"),
            rcell(xi$median, format = "xx.xx"),
            rcell(c(xi$`0.95LCL`, xi$`0.95UCL`), format = "(xx.xx, xx.xx)")
          )
        })
      )
    )
    tblstr2 <- toString(tblkm, gap = 1)
     
    grid.text(label = tblstr2,
              x = unit(1, "npc") - stringWidth(tblstr2) - unit(1, "lines"),
              y = unit(1, "npc") -  unit(1, "lines"),
              just = c("left", "top"),
              gp = gpar(fontfamily = "mono", fontsize = 8)
    )
  }
  

  
  
  
  
  ## Number of patients at Risk
  popViewport(2)
  
  pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 2))
  grid.text(label = "No of Patients at Risk",
            x = unit(0.1, "npc"),
            y = unit(0.3, "npc"),
            just = "right")
  popViewport(1)
  
  pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 3))
  
  pushViewport(dataViewport(xData = df$time, yData = c(0,1)))
  grid.xaxis()
  grid.rect()
  
  
  
  
  xpos <- seq(0, 28, by = 2)
  
  Map(function(x, ypos, strata, col) {
    
    n.r <- vapply(xpos, function(xi) {
      if (xi <= min(x$time)){
        i <- head(which(x$time >= xi), 1)
        x$n.risk[i]
      } else if (xi > max(x$time)){
        NA
      } else{
        i <- tail(which(x$time <= xi), 1)
        x$n.risk[i] - x$n.censor[i] - x$n.event[i]
      }
    }, numeric(1))
    
    grid.text(
      label = ifelse(!is.na(n.r), as.character(n.r), " "),
      x = unit(xpos, "native"),
      y = unit(ypos, "npc"),
      gp = gpar(col = col)
    )
    
    grid.text(
      label = strata,
      x = unit(-9, "lines"),
      y = unit(ypos, "npc"),
      just = c("left", "center"),
      gp = gpar(col = col)
    )
    
  }, df_s, 1 - 1:length(df_s)/(length(df_s) + 1), names(df_s), col_pal(names(df_s)))
  
  popViewport(3)
}


