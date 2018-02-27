
#' Get Label Attributes of Variables in a \code{data.frame}
#' 
#' Variable labels can be stored as a \code{label} attribute for each variable.
#' This functions returns a named character vector with the variabel labels
#' (empty sting if not specified)
#' 
#' @param x a \code{data.frame} oject
#' @param fill boolean in case the \code{label} attribute does not exist if
#'   \code{TRUE} the variable names is returned, otherwise \code{NA}
#' 
#' @return a named character vector with the variable labels, the names
#'   correspond to the variable names
#' 
#' @export
#' 
#' @examples 
#' x <- iris
#' var_labels(x)
#' var_labels(x) <- paste("label for", names(iris))
#' var_labels(x)    
#' 
var_labels <- function(x, fill = FALSE) {
  
  if (!is(x, "data.frame")) stop("x must be a data.frame")
  
  y <- Map(function(var, name) {
    lbl <- attr(var, "label")
    
    if (is.null(lbl)) {
      if (fill) name else NA_character_
    } else {
      if (!is.character(lbl) && !(length(lbl) == 1)) 
        stop("label for variable ", name, "is not a character string")
      as.vector(lbl)
    }
    
  }, x, names(x))

  labels <- unlist(y, recursive = FALSE, use.names = TRUE) 
  
  if (!is.character(labels)) stop("label extraction failed")
  
  labels
    
}


#' Set Label Attributes of All Variables in a \code{data.frame}
#' 
#' Variable labels can be stored as a \code{label} attribute for each variable.
#' This functions sets all non-missing variable labels in a \code{data.frame}
#' 
#' @inheritParams var_labels
#' 
#' @return modifies the variable labels of \code{x}
#' 
#' @export
#' 
#' @examples
#' x <- iris
#' var_labels(x)
#' var_labels(x) <- paste("label for", names(iris))
#' var_labels(x)
#' 
#' \dontrun{
#' View(x) # in RStudio data viewer labels are displayed
#' }
`var_labels<-` <- function(x, value) {
  if (!is(x, "data.frame")) stop("x must be a data.frame")
  if (!is.character(value)) stop("values must be of type character")
  if (ncol(x) != length(value)) stop("dimension missmatch")
  
  for (j in seq_along(x)) {
    if (!is.na(value[j])) {
      attr(x[[j]], "label") <- value[j]
    }
  }
  x
}

#' Copy and Change Variable Labels of a \code{data.frame}
#' 
#' Relabel a subset of the variables
#' 
#' @inheritParams var_labels<-
#' @param ... name-valeu pairs, where name corresponds to a variable name in
#'   \code{x} and the value to the new variable label
#'   
#' @return a copy of \code{x} with changed labels according to \code{...}
#'  
#' @export
#' 
#' @examples 
#' x <- var_relabel(iris, Sepal.Length = "Sepal Length of iris flower")
#' var_labels(x)
#' 
var_relabel <- function(x, ...) {
  if (!is(x, "data.frame")) stop("x must be a data.frame")
  
  dots <- list(...)
  varnames <- names(dots)
  if (is.null(varnames)) stop("missing variable declarations")
  
  map_varnames <- match(varnames, names(x))
  if (any(is.na(map_varnames)))
    stop("variables: ", paste(varnames[is.na(map_varnames)], collapse = ", "), " not found" )
  if (any(vapply(dots, Negate(is.character), logical(1))))
    stop("all variable labels must be of type character")
    
  for (i in seq_along(map_varnames)) {
    attr(x[[map_varnames[[i]]]], "label") <-  dots[[i]]
  }
  
  x
}

  
  
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

stack_rtables_l <- function(x) {
  do.call(stack_rtables, x)
}

wrap_with <- function(x, left, right, as_list = TRUE) {
  lbls <- paste0(left, x, right)
  if (as_list) as.list(lbls) else lbls
}

all_as_factor <- function(x) {
  if (!is(x, "data.frame")) stop("x needs to be a data.frame")
  
  is_fct <- vapply(x, is.factor, logical(1))
  
  if (!all(is_fct)) {
    for (i in which(!is_fct)) {
      x[[i]] <- structure(as.factor(x[[i]]), label = attr(x[[i]], "label"))
    }
  }
  x
}

#' Remove Shared Variables
#' 
#' Variables are considered shared if they have the same variable name
#' 
#' @param x a data.frame
#' @param y a data.frame
#' @param keep optional, a vector with variable names that should not be removed
#' 
#' @return a data.frame
#' 
#' @export
#' 
#' @examples 
#' 
#' drop_shared_variables(iris, iris[, 1:3])
#' 
drop_shared_variables <- function(x, y, keep) {
  
  if(!is.data.frame(x)) stop("x must be a data.frame")
  if(!is.data.frame(y)) stop("y must be a data.frame")
  
  if (missing(keep)) keep <- character(0)
  
  df <- x[, !(names(x) %in% setdiff(names(y), keep)), drop = FALSE]
  
  for (a in c("md5sum", "source", "access_by", "accessed_on")) {
     attr(df, a) <- attr(x, a)
  }
  
  df
}

na_as_level <- function(x, na_level = "NA") {
  if (!is.factor(x)) stop("x is required to be a factor")
  
  if (any(is.na(x))) {
    if (na_level %in% levels(x)) stop(na_level, " can not be a level of x")
    levels(x) <- c(levels(x), "NA")
    x[is.na(x)] <- "NA"
  }
  x
}

as.global <- function(...) {
  
  dots <- substitute(list(...))[-1]
  names <- sapply(dots, deparse)
  
  args <- list(...)
  
  ge <- globalenv()
  
  Map(function(x, name) {
    ge[[name]] <- x
  }, args, names)
  
}
