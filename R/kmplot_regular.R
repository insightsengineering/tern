
#' Function to create regular Kaplan Meier Plot
#' 
#' This function allows you to create KM plot with comparison between treatment arms, seperate plots by subpopulations,
#' and also apply stratification analysis
#' 
#' 

kmPlot <- function( data,
                    tte.var = "AVAL", cens.var = "CNSR",  evt.ind = 0,
                    trt.var = "ARM", ref.trt = NULL, oth.trt = NULL, trt.miss = FALSE,
                    strata.var = NULL, cox.tie = "efron", conf.int = FALSE, plot.nrisk = TRUE,
                    nrisk.interval = 2, nrisk.height = 0.25,
                    plot.cens = FALSE, plot.stats = TRUE, size.stats = 4,
                    xystats.up = c(0.7, 0.8), xyinterval.up = c(0.1, 0.1),
                    xystats.lo = c(0.05, 0.3), xyinterval.lo = c(0.08, 0.1),
                    widget = c('N' = TRUE, 'Median(KM)' = TRUE, '95% CI Median' = TRUE,
                                'p-value' = TRUE, 'Hazard Ratio' = TRUE, '95% CI HR' = TRUE),
                    line.color = NULL, line.type = 1, line.width = 1,
                    xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL, title = "Kaplan-Meier Plot",
                    legend = F, legend.pos = c(0.9, 0.9) ){
  ### if input data is not tibble, convert to tibble first.
  if (!(is.tbl(data))){
    data <- as_tibble(data)
  }
  ### subset input data to include analysis related variables and replace blank cells/"." with NA
  data <- data %>% select(c(tte.var, cens.var, trt.var, strata.var)) %>%  
            mutate_if(is.factor, as.character) %>% mutate_all(funs(str_trim)) %>%
            mutate_all(funs(na_if(., ""))) %>% mutate_all(funs(na_if(., ".")))
  
  
  
}