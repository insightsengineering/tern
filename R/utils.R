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
#' # function is not exported
#' `%needs%` <- tern:::`%needs%`
#'
#' iris %needs% c("Sepal.Length", "Petal.Width")
#'
#' \dontrun{
#' iris %needs% "ABC"
#' }
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
reorder_to_match_id <- function(x, ref, key = c("USUBJID", "STUDYID")) {
  stopifnot(
    nrow(x) == nrow(ref),
    all(key %in% names(x)),
    all(key %in% names(ref)),
    !any(is.na(x[key])),
    !any(is.na(ref[key])),
    !any(duplicated(ref[, key]))
  )

  x_ord <- do.call(order, x[key])
  ref_ord <- do.call(order, ref[key])

  out <- x[x_ord[order(ref_ord)], ]

  is_same <- unlist(Map(function(v1, v2) {
    all(v1 == v2)
  }, out[key], ref[key]))

  if (!all(is_same)) {
    stop("not same ids")
  }

  out
}


#' Combine Factor Levels
#'
#' @param x factor
#' @param levels level names to be combined
#' @param new_level name of new level
#'
#' @return a factor
#'
#' @export
#'
#' @examples
#' x <- factor(letters[1:5], levels = letters[5:1])
#' combine_levels(x, levels = c('a', 'b') )
#'
#' combine_levels(x, c('e', 'b'))
combine_levels <- function(x, levels, new_level = paste(levels, collapse = "/")) {
  stopifnot(
    is.factor(x),
    all(levels %in% levels(x))
  )

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
#' X <- add_labels(df, labels)
#'
#' \dontrun{
#' View(X)
#' }
add_labels <- function(df, labels) {
  for (name in names(df)) {
    lab <- labels[name]
    if (!is.na(lab[1]) && length(lab) == 1) {
      attr(df[[name]], "label") <- lab
    }
  }
  df
}


start_with_null <- function(x) {
  c(list(NULL), x)
}


#' Calculate and Stack Tables
#'
#'
#' @param funs list of functions or vector with function names to create the
#'   tables
#' @param ... union of named arguments for all the functions defined in
#'   \code{funs}
#' @param nrow_pad number of empty rows between tables
#'
#' @noRd
#'
#' @author Adrian Waddell
#'
#' @examples
#' t_tbl1 <- function(x, by, na.rm = TRUE) rtabulate(x, by, mean, format = "xx.xx")
#' t_tbl2 <- function(x, by) rtabulate(x, by, sd, format = "xx.xx")
#' t_tbl3 <- function(x, by) rtabulate(x, by, range, format = "xx.xx - xx.xx")
#'
#'
#' t_tbl <- function(x, by, na.rm) {
#'   compound_table(
#'     funs = c("t_tbl1", "t_tbl2", "t_tbl3"),
#'     x = iris$Sepal.Length,
#'     by = iris$Species,
#'     na.rm = FALSE
#'   )
#' }
compound_table <- function(funs, ..., nrow_pad = 1) {
  dots <- list(...)

  tbls <- lapply(funs, function(fun) {

    f <- match.fun(fun)

    do.call(f, dots[names(dots) %in% names(formals(f))])

  })

  rbindl_rtables(tbls, gap = nrow_pad)
}

wrap_with <- function(x, left, right, as_list = TRUE) {
  lbls <- paste0(left, x, right)
  if (as_list) {
    as.list(lbls)
  } else {
    lbls
  }
}

#' check if all elements in x are factors
#'
#' @param x data.frame
#'
#' @importFrom methods is
#'
#' @noRd
all_as_factor <- function(x) {
  stopifnot(is.data.frame(x))

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
#' drop_shared_variables(iris, iris[, 1:3])
drop_shared_variables <- function(x, y, keep) {
  stopifnot(
    is.data.frame(x),
    is.data.frame(y)
  )

  if (missing(keep)) {
    keep <- character(0)
  }

  df <- x[, !(names(x) %in% setdiff(names(y), keep)), drop = FALSE]

  for (a in c("md5sum", "source", "access_by", "accessed_on")) {
    attr(df, a) <- attr(x, a)
  }

  df
}

na_as_level <- function(x, na_level = "NA") {
  stopifnot(is.factor(x))

  if (any(is.na(x))) {
    if (na_level %in% levels(x)) {
      stop(na_level, " can not be a level of x")
    }
    levels(x) <- c(levels(x), "NA")
    x[is.na(x)] <- "NA"
  }
  x
}

as.global <- function(...) {
  dots <- substitute(list(...))[-1]
  names <- sapply(dots, deparse)

  args <- list(...)

  ge <- globalenv() # nolint

  Map(function(x, name) {
    ge[[name]] <- x
  }, args, names)
}

#' Helper functions to re-format and reflow long arm/grouping labels by inserting
#' line breaks
#'
#' @param x input single string
#' @param delim delimiter, default is space
#' @param limit number of characters allowed before inserting line break,
#'   default is the maximum length of longest word
#'
#' @noRd
#'
#' @author Chendi Liao (liaoc10), \email{chendi.liao@roche.com}
#'
#' @examples
#' x = "hellO-world abcerewerwere testing "
#' reflow(x)
#' reflow(x, delim = "-")
#' reflow(x, limit= 9)
reflow <- function(x,
                   delim = " ",
                   limit = NULL) {

  xsplit <- unlist(strsplit(x, delim))
  ctxt <- ""
  n <- 0

  if (is.null(limit)) {
    limit <- max(unlist(lapply(xsplit, nchar)))
  }

  for (i in xsplit) {
    if (nchar(i) > limit) {
      ctxt <- ifelse(n, paste0(ctxt, "\n", i, "\n"), paste0(ctxt, i, "\n"))
      n <- 0
    } else if ((n + nchar(i)) > limit) {
      ctxt <- paste0(ctxt, "\n", i)
      n <- nchar(i)
    } else {
      ctxt <- ifelse(n, paste0(ctxt, delim, i), paste0(ctxt, i))
      n <- n + nchar(i)
    }
  }

  outtxt <- ifelse(substring(ctxt, nchar(ctxt)) == "\n", substr(ctxt, 1, nchar(ctxt) - 1), ctxt)

  outtxt
}


to_n <- function(x, n) {
  if (is.null(x)) {
    NULL
  } else if (length(x) == 1) {
    rep(x, n)
  } else if (length(x) == n) {
    x
  } else {
    stop("dimension missmatch")
  }
}


#' Duplicate an object to create total group
#'
#' Used to duplicate data in order to create a total column in a table.
#'
#' @param x an object to dispatch on
#' @param ... arguments passed on to methods
#'
#' @noRd
add_total <- function(x, ...) {
  UseMethod("add_total", x)
}

#' Duplicate vector
#'
#' @inheritParams add_total
#' @param x a vector
#' @param col_by a factor the same length as \code{x}
#' @param total_level a label that is used as a new factor level in the duplicated \code{col_by}
#' @param col_N a numeric vector with length equal to the number of levels of \code{col_by}.
#' It is used to build the row in the table header with (N=xx).
#'
#' @details
#' Returns a list of duplicated \code{x}, \code{col_by} and \code{col_N}
#'
#' @noRd
#'
#' @examples
#' # factor
#' add_total(x=iris$Species, col_by = iris$Species, total_level = "All")
#' # numeric
#' add_total(x=iris$Sepal.Length, col_by = iris$Species, total_level = "All")
add_total.default <- function(x, col_by, total_level = "All", col_N = table(col_by)) { # nolint
  stopifnot(
    is.atomic(x),
    !total_level %in% levels(col_by),
    length(x) == length(col_by)
  )

  col_N <- c(col_N, sum(col_N)) # nolint
  names(col_N)[length(col_N)] <- total_level

  cb_levels <- c(levels(col_by), total_level)
  col_by <- factor(c(as.character(col_by), rep(total_level, length(x))), cb_levels)

  list(
    x = rep(x, 2),
    col_by = col_by,
    col_N = col_N
  )
}

#' Duplicate data frame
#'
#' @inheritParams add_total.default
#' @param x a data frame
#'
#' @details
#' Returns a list of duplicated \code{x}, \code{col_by} and \code{col_N}
#'
#' @noRd
#'
#' @examples
#' add_total(x=iris[, c("Sepal.Length", "Sepal.Width")], col_by = iris$Species, total_level = "All")
add_total.data.frame <- function(x, col_by, total_level = "All", col_N = table(col_by)) { # nolint

  vl <- var_labels(x)
  y <- rbind(x, x)
  var_labels(y) <- vl

  cb_levels <- c(levels(col_by), total_level)
  col_by <- factor(c(as.character(col_by), rep(total_level, nrow(x))), cb_levels)

  col_N <- c(col_N, sum(col_N)) # nolint
  names(col_N)[length(col_N)] <- total_level

  list(
    x = y,
    col_by = col_by,
    col_N = col_N
  )
}

has_no_na <- function(x) {
  !any(is.na(x))
}
