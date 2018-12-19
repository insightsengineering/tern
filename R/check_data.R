
#' check if elements ... have the same dimension
#' 
#' @param ... data.frames or vectors
#' @param omit.NULL are \code{NULL} elements in \code{...} to be omitted from the check?
#' 
#' @importFrom stats na.omit
#' 
#' @noRd
#' 
#' @examples 
#' \dontrun{
#' dots <- list(SL = iris$Sepal.Length, iris = iris, b = iris$Species, aaa = NULL)  
#' }
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
#' 
#' @noRd
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

# if total is non-null then it can not be in the levels of col_by
check_col_by <- function(col_by, col_N, min_num_levels = 2, total = NULL) {
  
  if (!is(col_by, "no_by") && !is.factor(col_by))
    stop("col_by needs to be a factor")
  
  if (any(is.na(col_by)) || '' %in% levels(col_by))
    stop("col_by can not have any missing data or have a level with an empty string")
  
  if (length(col_N) != nlevels(col_by))
    stop("col_N has not the same length as there are levels in col_by")
  
  if (nlevels(col_by) < min_num_levels)
    stop(paste("at least", min_num_levels, "expected in col_by but got", nlevels(col_by)))

  if (!is.null(total) && total %in% levels(col_by))
    stop("total level cannot exist as a level in col_by")
  
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