library(atezo.data)
library(survival)
source("~/teal.oncology/R/functions.R")

ATE <- ate(com.roche.cdt30019.go29436.re)
ASL <- asl(com.roche.cdt30019.go29436.re)


#Subgroup ATE by the selected ASL variable categories;
split.anl <- function (x,data){
  y <- merge(x,data,by = "USUBJID",all.x = T)
  return (y)
}

#Set up data list ;
data.list <- function (adsl = ASL, groupvar, anl){
  dlist           <- NULL
  dlist$ALL       <- list(ALL = ATE)
  for (i in 1:length(groupvar)){
    dlist[[groupvar[i]]] <- lapply(split.sub(adsl, groupvar[i]), split.anl, anl = data)
  }
  return(dlist)
}



tab.surv <- function(data, outcome, arm.ref, arm.comp){
  datatemp <- data[data[["ARM"]]%in%c(arm.ref,arm.comp),]
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
  return (tab)
  #Add Cox-regression Table;
}


tab.list <- function (adsl = ASL, data = ATE, groupvar, outcome, arm.ref, arm.comp){
   dlist <- data.list(adsl = ASL, data = ATE, groupvar = groupvar)
   groupvar <- c("ALL", groupvar)
   tlist <- NULL
   Variable <- NULL
  for (i in 1:length(groupvar)){
    tlist[[i]] <- lapply(dlist[[groupvar[i]]], tab.surv, outcome = outcome, arm.ref = arm.ref, arm.comp = arm.comp)
    tlist[[i]] <- do.call(rbind,tlist[[i]])
    Subgroup   <- row.names(tlist[[i]])
    tlist[[i]] <- cbind(Subgroup, tlist[[i]])
    Variable   <- c(Variable, c(names(dlist[i]),rep("",length(Subgroup)-1)))
  }
  tab <- cbind(Variable, do.call(rbind,tlist))
  row.names(tab) <- NULL
  return (tab)
}

tab.list( adsl = ASL, data = ATE, groupvar = c("BAGE","BECOG", "SEX"), outcome = "Overall Survival", arm.ref = "DUMMY A", arm.comp = c("DUMMY B", "DUMMY C"))
