split.sub <- function(data, variable, cutoff=NULL){
  #create a temporary binary variable for numerical cutoff 
  data[["temp"]] <- data[[variable]] 
  if(is.numeric(data[[variable]])){
    #if no user-defined cutoff, then median is the default
    if(typeof(cutoff)=="NULL") {
      data[["temp"]] <- ifelse(data[[variable]]<=median(data[[variable]],na.rm=T),"<=Median",">Median")
    }else{
      data[["temp"]] <- ifelse(data[[variable]]<=cutoff,paste("<=",cutoff,sep=""),paste(">",cutoff,sep=""))
    }
    data <- data[!is.na(data[[variable]]),]
  }
  if(!is.numeric(data[[variable]])){
    data <- data[data[[variable]]!="",]
  }
  data <- data[,c("USUBJID","temp")]
  return(split(data,data[["temp"]]))
}


#' Subgroup ATE by the selected ASL variable categories;
#' 
#' 
split.anl <- function (x, data){
  y <- merge(x,data,by = "USUBJID",all.x = T)
  return (y)
}

#' Set up data list
data.list <- function (adsl, groupvar, anl){
  
  dlist <- lapply(setNames(groupvar, groupvar), function(var) {
    lapply(split.sub(adsl, var), split.anl, data = anl)
  })
  
  c(list(ALL = list(ALL = anl)), dlist)
}


#' Create a survival table
#' 
#' @importFrom survival survfit Surv
tab.surv <- function(data, outcome, arm.ref, arm.comp){
  
  # data %needs% c("ARM")
  
  datatemp <- data[data[["ARM"]] %in% c(arm.ref, arm.comp), ]
  
  arm.ref.temp  <- paste(arm.ref,collapse = "/")
  arm.comp.temp <- paste(arm.comp,collapse = "/")
  
  #create a temporary Arm variable  to store ref and comparable arms, useful when combined arm is required;
  datatemp[["armtemp"]] <- ifelse(datatemp[["ARM"]] %in% c(arm.ref.temp ), arm.ref.temp , arm.comp.temp)
  datatemp[["armtemp"]] <- factor(datatemp[["armtemp"]], levels = c(arm.ref.temp , arm.comp.temp))
  
  #Create KM table;
  km     <- survfit(Surv(AVAL,CNSR == 1) ~ armtemp,data = datatemp[datatemp$PARAM == outcome,])
  km.tab <- round(summary(km)$table, 1)
  ref.ne   <- paste(km.tab[1,4], km.tab[1,1], sep = "/")  
  comp.ne  <- paste(km.tab[2,4], km.tab[2,1], sep = "/")      
  mst      <- paste(km.tab[1,7], km.tab[2,7], sep = "/")    
  tab      <- data.frame(ref.ne, comp.ne, mst)
  colnames(tab) <- c(paste(arm.ref.temp, "(Event/N)", sep = ""), 
                     paste(arm.comp.temp, "(Event/N)",sep=""), 
                     paste("MST", "(", arm.ref.temp, "/", arm.comp.temp, ")",sep=""))
  
  tab
  
  #Add Cox-regression Table;
}

#' create the time to event table
#' 
#' @export
#' 
#' @examples 
#' 
#' 
#' \dontrun{
#' library(atezo.data)
#' 
#' ATE <- ate(com.roche.cdt30019.go29436.re)
#' ASL <- asl(com.roche.cdt30019.go29436.re)
#' 
#' tab.list(
#'   adsl = ASL,
#'   data = ATE,
#'   groupvar = c("BAGE","BECOG", "SEX"),
#'   outcome = "Overall Survival",
#'   arm.ref = "DUMMY A",
#'   arm.comp = c("DUMMY B", "DUMMY C")
#' )
#' }
#' 
#' 
#' 
tab.list <- function (adsl, data, groupvar, outcome, arm.ref, arm.comp){
  
  dlist <- data.list(adsl = adsl, anl = data, groupvar = groupvar)
  
  # x <- dlist[[1]]
  surv_out <- lapply(dlist, function(x) {
    # data_subgroup <- x[[1]]
    lapply(x, function(data_subgroup) {
      # kaplan meier summary
      df_km <- tab.surv(data = data_subgroup, outcome = outcome,
                           arm.ref = arm.ref, arm.comp = arm.comp)
      
      # cox model
      arm2 <- factor(ifelse(data_subgroup$ARM == arm.ref, arm.ref, "comparison"))
      
      data_subgroup$armtemp <- relevel(arm2, ref = arm.ref)
      coxm    <- summary( coxph( Surv(AVAL, CNSR == 0 ) ~ armtemp, data = data_subgroup))
      
      HR.95 <- coxm$conf.int[1]
      HR.95lb <- coxm$conf.int[3]
      HR.95ub <- coxm$conf.int[4]
      p.val   <- coxm$coef[5]
      
      cbind(df_km, HR.95, HR.95lb, HR.95ub, p.val)
    })
  })
  
  surv_out2 <- unlist(surv_out, recursive = FALSE)
  
  X <- Reduce(rbind, surv_out2)
  row.names(X) <-names(surv_out2)
  
  X
}


