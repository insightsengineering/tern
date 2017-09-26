split.sub <- function(data,variable,cutoff=NULL){
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