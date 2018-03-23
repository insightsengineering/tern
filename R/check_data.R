
# dots <- list(SL = iris$Sepal.Length, iris = iris, b = iris$Species, aaa = NULL) 
check_same_N <- function(..., omit.NULL = TRUE) {
  
  dots <- list(...)
  
  N_list <- Map(function(x, name) {
    if (is.null(x)) {
      if (omit.NULL) NA_integer_ else stop("arg", name, "is not supposed to be NULL")
    } else if (is.data.frame(x)) {
      nrow(x)
    } else if (is.atomic(x)) {
      length(x)
    } else {
      stop("data structure for ", name, "is currently not supported")
    }
  }, dots, names(dots))
  
  N <- na.omit(unlist(N_list))
  
  if (length(unique(N)) > 1) {
    sel <- which(N != N[1])
    stop("dimension missmatch:", paste(names(N)[sel], collapse = ", "), " do not have N=", N[1])
  }
  
  TRUE
}

check_same_N_l <- function(x) {
  do.call(check_same_N, x)
}


#' Check strata_data
#' 
#' strata_data can be NULL or 
#' 
#' @param x data.frame with valid stratification data
#' @param n number of expected rows
#' 
#' 
check_data_frame <- function(x, allow_missing = FALSE) {

  xname <- deparse(substitute(x))
  
  if (!is.null(x)) {
    if (!is.data.frame(x)) stop(xname, " needs to be either NULL or a data.frame")
    
    if (!allow_missing) {
      is_missing <- vapply(x, function(var) {
        if (is.numeric(var)) {
          any(is.na(var))
        } else {
          any(is.na(var)) || any(var == "")
        }
      }, logical(1))
      
      if (any(is_missing)) stop(xname, " can not have any missing values (NA or '')")
    }
  }  
  
  TRUE 
}


check_col_by <- function(x, min_num_levels = 2) {
  
  if (!is(x, "no_by") && !is.factor(x)) stop("col_by needs to be a factor")
  if (any(is.na(x))) stop("col_by can not have any missing data")
  
  #if (any(table(x)<=0)) stop("data is required for all levels of col_by")
  
  if (!(min_num_levels == 1 && is(x, "no_by"))) {
    if (length(levels(x)) < min_num_levels) stop("col_by is required to have at least", min_num_levels, "levels")
  }

  TRUE
}


check_is_event <- function(x) {
  
  if (!is.logical(x)) stop("is_event needs to be of type logical")
  if (any(is.na(x))) stop("is_event can not have any missing data")
  
  TRUE
}


check_is_factor <- function(x, allow_NA = TRUE) {
  
  if (!is.factor(x)) stop(deparse(substitute(x))," needs to be a factor")
  if (!allow_NA) {
    if (any(is.na(x))) stop(deparse(substitute(x))," cannot have any missing data")
  }
  
  TRUE
}


check_is_numeric <- function(x, allow_NA = TRUE) {
  
  if (!is.numeric(x)) stop(deparse(substitute(x))," needs to be numerical")
  if (!allow_NA) {
    if (any(is.na(x))) stop(deparse(substitute(x))," cannot have any missing data")
  }

  TRUE
}