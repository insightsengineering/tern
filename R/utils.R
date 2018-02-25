
#' Check if list or data.frame has elements/variables
#' 
#' Checks if list names exist and throws an error otherwise
#' 
#' @param data a \code{data.frame} or named list
#' @param names vector with names
#' 
#' @return \code{TRUE} if all variables exist and an appropriate error if not.
#' 
#' @author Adrian Waddell (waddella), \email{adrian.waddell@roche.com}
#' 
#' @noRd
#' 
#' @examples 
#' \dontrun{
#' # function is not exported
#' `%needs%` <- teal.oncology:::`%needs%`
#' 
#' iris %needs% c("Sepal.Length", "Petal.Width")
#' 
#' iris %needs% "ABC"
#' 
#' }
#' 
`%needs%` <- function(data, names) {
  
  i <- is.na(match(names, names(data)))

  if (any(i)) {
    msg <- if (sum(i) == 1) {
      paste("variable ", names[i], " does not exist")
    } else {
      paste("variables", paste(names[i], collapse = ", "), "do not exist")
    }
    stop(msg)
  }
  
  invisible(TRUE)
}


#' Return Ordered Dataset so that a set of variables match exactly
#' 
#' This function is useful to ensure that two datasets have the same subjects
#' ordered in the same way
#' 
#' @param x data set to reorder
#' @param ref reference data set
#' @param key variables that define a unique patient
#' 
#' @export
#'  
#' @examples 
#' 
#' A <- data.frame(USUBJID=paste0("id-",1:10), STUDYID = "A", stringsAsFactors = FALSE)
#' B <- data.frame(USUBJID=paste0("id-",10:1), STUDYID = "A", stringsAsFactors = FALSE)
#'  
#' reorder_to_match_id(A, B)
#' 
#' \dontrun{
#' C <- data.frame(USUBJID=paste0("id-",1:9), STUDYID = "A")
#' reorder_to_match_id(A, C)
#'
#' D <- B
#' D$STUDYID[3] <- "B"
#' reorder_to_match_id(A, D)
#' 
#' E <- B
#' E$USUBJID[2] <- "id-10"
#' reorder_to_match_id(A, E)
#' }
#' 
#' 
reorder_to_match_id <- function(x, ref, key = c("USUBJID", "STUDYID")) {
  
  if (nrow(x) != nrow(ref)) stop("dimension missmatch")
  if (!all(key %in% names(x))) stop("x has not all keys")
  if (!all(key %in% names(ref))) stop("ref has not all keys")

  if (any(is.na(x[key]))) stop("no missing values allows in x[,key]")
  if (any(is.na(ref[key]))) stop("no missing values allows in ref[,key]")

  if (any(duplicated(ref[, key]))) stop("key is not unique")
  
  x_ord <- do.call(order, x[key])
  ref_ord <- do.call(order, ref[key])
  
  out <- x[x_ord[order(ref_ord)],]
  
  is_same <- unlist(Map(function(v1, v2) {
    all(v1 == v2)
  }, out[key], ref[key]))
  
  if (!all(is_same)) stop("not same ids")
  
  out
}


#' Combine factor levels
#' 
#' 
#' @export
#' 
#' @examples 
#' x <- factor(letters[1:5], levels = letters[5:1])
#' combine_levels(x, levels = c('a', 'b') )
#' 
#' combine_levels(x, c('e', 'b'))
#' 
combine_levels <- function(x, levels, new_level = paste(levels, collapse = "/")) {
  
  if (!is.factor(x)) stop("x is required to be a factor")
  
  if (!all(levels %in% levels(x))) {
    stop("not all levels are part of levels(x)")
  }
  
  lvls <- levels(x)
  
  lvls[lvls %in% levels] <- new_level
  
  levels(x) <- lvls
  
  x
}


#' return a vector with the variable labels and names if variable labels do not exist
#' 
#' 
#' @param df data.frame object
#' 
#' @noRd
#' 
#' @examples 
#' 
#' X <- data.frame(
#'  a = structure(1:4, label = "label for a"),
#'  b = 3:6,
#'  d = structure(letters[1:4], label = "label for d"),
#'  stringsAsFactors = FALSE
#' )
#' 
#' \dontrun{
#' View(X)
#' 
#' names(X)
#' }
#' 
labels_over_names <- function(df) {
  
  as.vector(unlist(Map(function(var, varname) {
    label <- attr(var, "label")
    if (!is.null(label)) label else varname
  }, df, names(df))))
  
}

#' re-attach labels to variables
#' 
#' @param df a data.frame
#' @param labels a named vector with the labels. names are variable names
#' 
#' @noRd
#' 
#' @examples
#' 
#' df <- data.frame(c = c(1, 2), b = c("a", "b"), a = c(3,4))
#' 
#' labels <- setNames(c("a var", "b var"), c("a", "b"))
#' 
#' 
#' X <- add_labels(df, labels)
#' 
#' \dontrun{
#' View(X) 
#' }
#' 
add_labels <- function(df, labels) {
  for (name in names(df)) {
    
    lab <- labels[name]
    if (!is.na(lab[1]) && length(lab) == 1) {
      attr(df[[name]], "label") <- lab
    }
  }
  df
}


start_with_NULL <- function(x) {
  c(list(NULL), x)
}

#' Stack rtables with rbind and add empy rows between tables
#' 
#' @param ... rtbale objects
#' 
#' @export
#' 
#' 
stack_rtables <- function(..., nrow_pad = 1) {
  
  tbls <- Filter(Negate(is.null), list(...))
  
  if (length(tbls) > 0) {
    if (!rtables:::are(tbls, "rtable")) stop("not all objects are of type rtable")
    
    header <- attr(tbls[[1]], "header")
    tbl_with_empty_rows <- rtablel(header = header, replicate(nrow_pad, rrow()))
    
    Reduce(
      function(x, y) rbind(x, tbl_with_empty_rows, y),
      tbls
    )
    
  } else {
    list()
  }
}

#' @export
stack_rtables_l <- function(x) {
  do.call(stack_rtables, x)
}



