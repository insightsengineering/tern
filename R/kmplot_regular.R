
#' Function to create regular Kaplan Meier Plot
#' 
#' This function allows you to create KM plot with comparison between treatment arms, seperate plots by subpopulations,
#' and also apply stratification analysis
#' 
#' @param data TTE input dataset

kmPlot <- function( data, paramcd = PARAMCD, end.point = "OS",
                    tte.var = "AVAL", cens.var = "CNSR",  evt.ind = 0,
                    trt.var = "ARM", ref.trt = NULL, oth.trt = NULL, trt.miss = FALSE,
                    strata.var = NULL, cox.tie = "efron", conf.int = FALSE, plot.median = FALSE, 
                    plot.nrisk = TRUE, time.interval = 5, nrisk.height = 0.25, size.nrisk = 4, 
                    plot.cens = FALSE, size.cens = 4.5, shape.cens = "+",
                    plot.stats = TRUE, size.stats = 4,
                    xystats.up = c(0.7, 0.8), xyinterval.up = c(0.1, 0.1),
                    xystats.lo = c(0.05, 0.3), xyinterval.lo = c(0.08, 0.1),
                    widget = c('N' = TRUE, 'Median(KM)' = TRUE, '95% CI Median' = TRUE,
                                'p-value' = TRUE, 'Hazard Ratio' = TRUE, '95% CI HR' = TRUE),
                    line.color = NULL, line.type = 1, line.width = 1,
                    xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL, title = "Kaplan-Meier Plot",
                    legend.pos = c(0.9, 0.9) ){
  ### if input data is not tibble, convert to tibble first.
  if (!(is.tbl(data))){
    data <- as_tibble(data)
  }
  paramcd <- enquo(paramcd)
  data <- data %>% filter((!!paramcd) == "OS")
  ### subset input data to include analysis related variables and replace blank cells/"." with NA

   data <- data %>% select( c(tte.var, cens.var, trt.var, strata.var) ) %>%  
            mutate_if(is.factor, as.character) %>% mutate_if(is.character, str_trim) %>%
            mutate_all(funs(na_if(., "") )) %>% mutate_all(funs(na_if(., ".")))

  if (!(trt.miss)){ ### if don't keep missing values as one treatment group
    trt.lev <- data[ , trt.var] %>% filter(!is.na(.)) %>% distinct() %>% arrange_(.dots = trt.var) %>% pull()
  } else {   ### if keep missing values as one treatment group
    data[ , trt.var] <- data[ , trt.var] %>% pull() %>% recode(.missing = "Missing")
    trt.lev1 <- data[ , trt.var] %>% filter(. != "Missing") %>% distinct() %>% arrange_(.dots = trt.var)  %>% pull()
    trt.lev2 <- data[ , trt.var] %>% filter(. == "Missing") %>% distinct() %>% arrange_(.dots = trt.var)  %>% pull()
    trt.lev <- c(trt.lev1, trt.lev2)
  }

  if (is.null(ref.trt)){
    ref.trt <- trt.lev[1]
  }

  if (is.null(oth.trt)){
    oth.trt <- trt.lev[which(trt.lev != ref.trt)]
  }

  data[ , trt.var] <- data[ , trt.var] %>% pull() %>% factor(levels = c(ref.trt, oth.trt))

  ### surv.obj1 is used for plotting and unstratified model
  ### surv.obj2 is used for stratified model

  surv.obj1 <- paste0("Surv(", tte.var,",",cens.var, "==", evt.ind, ") ~ ", "factor(", trt.var, ")")
  surv.fit1 <- do.call(survfit, list(as.formula(surv.obj1), data = data))

  if (is.null(line.color))    line.color <- seq(1, length(trt.lev))
  med.line <- ifelse(plot.median, "hv", "none")

  surv.plot <- ggsurvplot(surv.fit1, data = data, break.time.by = time.interval, conf.int = conf.int,
                          surv.median.line = med.line,
                          risk.table =  plot.nrisk, risk.table.title = "No. of Patients at Risk",
                          risk.table.col = "strata", risk.table.height = nrisk.height,
                          risk.table.fontsize = size.nrisk,
                          censor = plot.cens, censor.size = size.cens, censor.shape = shape.cens,
                          legend = legend.pos, legend.labs = c(ref.trt, oth.trt), legend.title = "",
                          palette = line.color, linetype = line.type, size = line.width)

  ### if modeling with stratification factors
  if (!is.null(strata.var)){
    surv.obj2 <- paste0("Surv(", tte.var,",",cens.var, "==", evt.ind, ") ~ ", "factor(", trt.var,
                        ") + strata(", paste(strata.var, collapse = ","), ")")
  }


  return(surv.plot)
}