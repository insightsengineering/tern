# STREAM AE tables ----

#' @include utils.R
NULL

#' Events by Highest Grade Table
#'
#' This function summarizes number of unique subjects by highest grade and events term(s).
#' Events \code{terms} can be one level term or two level terms (one higher level and one lower level).
#' An implementation example is to apply \code{t_events_per_term_grade_id} on Adverse Event Data
#' to create Adverse Events by Highest \code{NCI CTCAE} grade table
#' (\code{AET04}, \href{http://bioportal.roche.com/stream_doc/2_05/um/report_outputs_aet04.html}{STREAM2.x},
#' \href{https://rochewiki.roche.com/confluence/pages/viewpage.action?pageId=294027501}{STREAM1.17}).
#'
#' @inheritParams argument_convention
#' @param terms character or factor vector, or dataframe to represent events information;
#'   Currently \code{terms} can only be a vector or dataframe with 1 or 2 columns.
#'   For \code{terms} with 2 columns, 1st column should represent higher level term and 2nd
#'   column should be lower level term.
#' @param id vector of subject identifier. Length of \code{id} must be the same as the
#'   length or number of rows of \code{terms}.
#' @param grade grade of adverse event as numeric or factor.
#' @param col_N numeric vector with information of the number of patients in the
#'   levels of \code{col_by}. This is useful if there are patients that have no
#'   adverse events can be accounted for with this argument.
#' @param grade_levels an ordered character or factor vector used for naming rows for each
#'   level of grade. If \code{grade} is a factor, \code{grade_levels} will overwrite the
#'   level orders in \code{grade}
#' @param any_grade string to specify the row name which counts any occurrence,
#'   it is named \code{Any Grade} by default
#' @param event_type string to specify the type of event that is summarized, \code{event} by default.
#'   Only displayed when \code{terms} has 2 columns.
#' @param missing_term_action Specify how the missing terms should be handled.
#'   Either "nocode" (by default) or "ignore". "nocode" means to code as
#'   "No Coding Available"; "ignore" means not to count the missing terms.
#'
#' @details
#' \code{t_events_per_term_grade_id} includes percentages based on the total number of subjects
#' in the column heading (i.e. \code{"N=nnn"}). \code{col_N} can be explicitly specified to
#' get N for percentage calculation from either events dataset or additional dataset like
#' subject level dataset. See the example.
#'
#' Multiple events within a subject of the same term (if \code{terms} is one level) or lower level term
#' (if \code{terms} is two levels) are counted once using the
#'  greatest intensity reported.
#'
#' \code{t_events_per_term_grade_id} doesn't deal with data with missing grade. Impute or filter missing
#' values before using \code{t_events_per_term_grade_id}.
#'
#' \code{t_events_per_term_grade_id} orders data by "All Patients" column from the most commonly
#'  reported higher level term to the least frequent one. Within each group of higher level term,
#'  it sorts by decreasing frequency of lower level term. It brakes ties using \code{terms} names in alphabetical order.
#'
#' \code{t_events_per_term_grade_id} fills in \code{col_by} and \code{grade} with \code{0} value
#' in case there was no events reported for particular \code{col_by} and/or
#' \code{grade} category. Use \code{grade_levels} to modify the range of desired
#' grades.
#'
#' @return an \code{\link{rtable}} object.
#'
#' @export
#'
#' @seealso \code{\link{t_max_grade_per_id}}, \code{\link{t_events_per_term_id}}
#'
#' @examples
#' library(dplyr)
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(10, seed = 1)
#' ADAE <- radae(ADSL, 4L, seed = 2)
#'
#' t_events_per_term_grade_id(
#'   terms = ADAE$AEDECOD,
#'   id = ADAE$USUBJID,
#'   grade = ADAE$AETOXGR,
#'   col_by = ADAE$ARM,
#'   col_N = table(ADSL$ARM),
#'   grade_levels = 1:5
#' )
#'
#' ADAE$AEDECOD <- as.character(ADAE$AEDECOD)
#' ADAE$AEBODSYS <- as.character(ADAE$AEBODSYS)
#' ADAE$AEDECOD[c(1,5)] = ""
#' ADAE$AEBODSYS[c(2,6)] = NA
#' ADAE <- ADAE %>%
#'   mutate(grade_category = case_when(
#'     AETOXGR %in% c(1,2) ~ "Mild",
#'     AETOXGR %in% c(3,4) ~ "Moderate",
#'     AETOXGR %in% c(5) ~ "Severe") %>% as.factor) %>%
#'   var_relabel(
#'     AEBODSYS = "MedDRA System Organ Class",
#'     AEDECOD = "MedDRA Preferred Term",
#'     AETOXGR = "Analysis Toxicity Grade",
#'     grade_category = "Insensity")
#'
#' levels(ADAE$grade_category) = c("Mild", "Moderate", "Severe")
#'
#' t_events_per_term_grade_id(
#'   terms = ADAE$AEDECOD,
#'   id = ADAE$USUBJID,
#'   grade = ADAE$AETOXGR,
#'   col_by = ADAE$ARM,
#'   col_N = table(ADSL$ARM),
#'   total = "All Patients",
#'   grade_levels = 1:6
#' )
#'
#' t_events_per_term_grade_id(
#'   terms = ADAE %>% select(AEBODSYS, AEDECOD),
#'   id = ADAE$USUBJID,
#'   grade = ADAE$AETOXGR,
#'   col_by = ADAE$ARM,
#'   col_N = table(ADSL$ARM),
#'   grade_levels = 1:5,
#'   event_type = "adverse events",
#'   missing_term_action = "ignore"
#' )
#'
#' tbls <- t_events_per_term_grade_id(
#'   terms = ADAE %>% select(AEBODSYS, AEDECOD),
#'   id = ADAE$USUBJID,
#'   grade = ADAE$grade_category,
#'   col_by = ADAE$ARM,
#'   col_N = table(ADSL$ARM),
#'   total = "All Patients",
#'   event_type = "adverse events",
#'   any_grade = "Any Severity",
#'   table_tree = TRUE
#' )
#' summary(tbls)
#' to_rtable(tbls)
t_events_per_term_grade_id <- function(terms,
                                       id,
                                       grade,
                                       col_by,
                                       col_N = NULL, # nolint
                                       total = NULL,
                                       grade_levels = NULL,
                                       event_type = "event",
                                       missing_term_action = "nocode",
                                       any_grade = "Any Grade",
                                       table_tree = FALSE) {
  if (is.atomic(terms)) {
    terms <- list(terms)
  }
  stopifnot(is.list(terms))
  stopifnot(is.null(total) || is_character_single(total))
  stopifnot(!is.null(missing_term_action) && missing_term_action %in% c("nocode", "ignore"))

  grade_label <- label(grade) %||% deparse(substitute(grade))
  col_by_label <- label(col_by) %||% deparse(substitute(col_by))

  grade <- with_label(grade, label = grade_label)

  if (missing_term_action == "nocode") {
    terms <- lapply(terms, function(x) {
      x <- as.character(x)
      x[x %in% c("", " ", NA)] <- "No Coding Available"
      return(x)
    })
  } else if (missing_term_action == "ignore") {
    missing_terms <- apply(
      sapply(terms, function(x) x %in% c("", " ", NA)),
      1,
      any
    )

    terms <- lapply(terms, function(x) {
      x[!missing_terms]
    })
    id <- id[!missing_terms]
    grade <-  with_label(grade[!missing_terms], label = grade_label)
    col_by <- with_label(col_by[!missing_terms], label = col_by_label)
  }

  col_by <- col_by_to_matrix(col_by, x = id)
  col_N <- col_N %||% get_N(col_by) #nolintr
  if (!is.null(total)) {
    col_by <- by_add_total(col_by, label = total)
    col_N <- col_N_add_total(col_N) #nolintr
    total <- NULL
  }

  # First create a tree to distribute information splitting by terms, we will then use rapply_tree to operate based on
  # this info.

  terms <- lapply(terms, as_factor_keep_attributes) # removes class attributes
  tree_data <- rsplit_to_tree(
    list(grade = grade, id = id, col_by = col_by),
    by_lst = terms
  )
  tree_data@name <- paste0("- Any ", event_type, " -")
  tree_data@format_data$children_indent <- 0

  tree <- rapply_tree(
    tree_data,
    function(name, content, ...) {
      list(name = name, content = t_max_grade_per_id(
        grade = content$grade,
        id = content$id,
        col_by = content$col_by,
        col_N = col_N,
        total = total,
        grade_levels = grade_levels,
        any_grade = any_grade
      ))
    }
  )

  tree <- rsort_tree(
    tree,
    function(node) {
      # we sort by the number of events and ignore the first column because it is the overall grade level
      order(vapply(
        node@children,
        function(child) {
          tbl <- child@content
          -sum(vapply(tbl[[1]][2:ncol(tbl)], `[`, numeric(1), 1)) #- to reverse order
        },
        numeric(1)
      ))
    }
  )

  # make the leaf node names invisible and add this name as the first row.name of the leaf content
  tree <- full_apply_at_depth(
    tree,
    function(node) {
      name <- node@name
      node@name <- invisible_node_name(name)
      row.names(node@content) <- c(name, row.names(node@content)[-1])
      node
    },
    depth = length(terms)
  )

  tree <- full_apply_at_depth(
    tree,
    function(node) {
      row.names(node@content) <- c("- Overall -", row.names(node@content)[-1])
      node
    },
    depth = length(terms) - 1
  )

  # change formatting when the terms is not recursive (list length >= 2)
  if (length(terms) == 1) {
    tree@name <- invisible_node_name(tree@name)
    tree@format_data <- node_format_data(content_indent = 0)
  } else {
    row.names(tree@content) <- c("- Overall -", row.names(tree@content)[-1])
  }

  #tree@format_data[["left_header"]] <- do.call(rheader, lapply(terms, label))

  if (table_tree) {
    tree
  } else {
    to_rtable(tree)
  }
}

# Elementary Tables Used for AE tables ----

#' Tabulate maximum grade per id by \code{col_by}
#'
#' \code{t_max_grade_per_id} is used for deriving adverse events tables, these are returned
#' as nested lists.
#'
#' @inheritParams argument_convention
#' @param grade a numeric or character vector with grade levels.
#' @param id a vector with id values
#' @param grade_levels an ordered character or factor vector used for naming rows for each
#'   level of grade. If \code{grade} is a factor, \code{grade_levels} will overwrite the
#'   level orders in \code{grade}
#' @param any_grade add a row that counts any occurrence, it is named \code{Any
#'   Grade} by default
#'
#' @importFrom stats aggregate
#' @export
#'
#' @seealso \code{\link{t_max_grade_per_id}}, \code{\link{t_events_per_term_grade_id}}
#'
#' @examples
#' t_max_grade_per_id(
#'   grade = factor(c(1,2,3)),
#'   id = c(1,1,1),
#'   col_by = factor(rep("A", 3)),
#'   col_N = 3
#' )
#'
#' id <- c(1,1,2,2,3,3,3,4)
#' grade <- factor(c("a","b","c","b","a","a","b","c"))
#' t_max_grade_per_id(
#'   grade = grade,
#'   id = id,
#'   col_by = factor(LETTERS[id]),
#'   grade_levels = factor(c("d","c","b","a"), levels = c("d","c","b","a")),
#'   col_N = c(4,3,5,3),
#'   any_grade = "Any Class"
#' )
#'
#' t_max_grade_per_id(
#'   grade = factor(c(1,2,3,2,1,1,2,3)),
#'   id = id,
#'   col_by = factor(LETTERS[id]),
#'   col_N = c(4,3,5,4)
#' )
#'
#' \dontrun{
#' # throws an error because each id can only have one col_by
#' # t_max_grade_per_id(
#'   #   grade = factor(c(1,2,3)),
#'   #   id = c(1,2,2),
#'   #   col_by = factor(LETTERS[1:3]),
#'   #   col_N = c(15, 10, 12)
#'   # )
#' }
#'
#' \dontrun{
#' # throws an error because grade NA is not in grade_levels
#' # t_max_grade_per_id(
#' #   grade =  factor(c(1,2,NA)),
#' #   id = c(1,2,3),
#' #   col_by = factor(LETTERS[1:3]),
#' #   col_N = c(15, 10, 12)
#' # )
#' }
t_max_grade_per_id <- function(grade,
                               id,
                               col_by,
                               col_N = NULL, # nolint
                               total = NULL,
                               grade_levels = NULL,
                               any_grade = "Any Grade") {
  grade_label <- label(grade) %||% deparse(substitute(grade))

  stopifnot(is.null(total) || is_character_single(total))
  stopifnot(!any(duplicated(grade_levels)))
  stopifnot(!any(is.na(grade)))

  grade_levels <- grade_levels %||% levels(as.factor(grade))

  if (length(setdiff(as.factor(grade), grade_levels)) > 0) {
    stop(paste0("grades exist that are not in grade_levels: ", setdiff(as.factor(grade), grade_levels)))
  }

  grade <- factor(grade, levels = levels(as.factor(grade_levels)), ordered = TRUE)

  stopifnot(!any(is.na(id)))
  col_by <- col_by_to_matrix(col_by, grade)
  col_N <- col_N %||% get_N(col_by) #nolintr
  if (!is.null(total)) {
    col_by <- by_add_total(col_by, label = total)
    col_N <- col_N_add_total(col_N) #nolintr
    total <- NULL
  }

  check_col_by(grade, col_by, col_N, min_num_levels = 1)
  check_id(id, col_by)

  df <- data.frame(grade, id, stringsAsFactors = FALSE)

  df_max <- do.call(rbind, unname(Map(
    function(rows, col_by_name) {
      if (sum(rows) == 0) {
        # rows is logical
        # aggregate does not work when rows is all FALSE
        data.frame(grade = 1, id = 1, col_by = 1)[c(), ] # get an empty data frame
      } else {
        data.frame(
          aggregate(grade ~ id, FUN = max, drop = TRUE, data = df[rows, ], na.rm = TRUE),
          col_by = col_by_name
        )
      }
    },
    col_by,
    colnames(col_by)
  )))
  df_max$col_by <- factor(df_max$col_by, colnames(col_by))

  # when col_by is a factor and not a general matrix, the equivalent call is
  # col_by <- col_by_to_factor(col_by) #nolintr
  # df_max <- aggregate(grade ~ id + col_by, FUN = max, drop = TRUE, data = df, na.rm = TRUE) #nolintr

  df_max$grade <- factor(df_max$grade, levels = grade_levels)
  df_max_no_na <- na.omit(df_max)

  tbl_any <- if (!is.null(any_grade)) {
    any_grade <- paste0("- ", any_grade, " -")
    rtabulate(
      x = df_max_no_na,
      row_by = by_all(any_grade),
      col_by = as.factor(df_max_no_na$col_by),
      FUN = count_perc_col_N,
      format = "xx (xx.x%)",
      col_wise_args = list(n_i = col_N)
    )
  } else {
    NULL
  }

  tbl_x <- rtabulate(
    x = df_max_no_na,
    row_by  = df_max_no_na$grade,
    col_by  = df_max_no_na$col_by,
    FUN = count_perc_col_N,
    format = "xx (xx.x%)",
    col_wise_args = list(n_i = col_N)
  )

  tbl <- rbind(tbl_any, tbl_x)
  tbl <- header_add_N(tbl, col_N)

  row_names_as_col(tbl, c("", grade_label))
}

#' checks that each patient appears in at most one col_by column (possibly
#' several times as AVAL corresponds to several measures and there are >= 1 rows per patient)
#'
#' @param id patient id
#' @param col_by columns indicating where patient is, (this function checks that the patient only
#'   appears in one column, ignoring the "by_all" / total column)
#' @examples
#' library(random.cdisc.data)
#' ADSL <- radsl(10, seed = 1)
#' ADAE <- radae(ADSL, 4L, seed = 2)
#' tern:::check_id(id = ADAE$USUBJID, col_by = ADAE$ARM)
#'
#' @importFrom dplyr group_by summarise_all select
#' @importFrom magrittr %>%
check_id <- function(id, col_by) {
  col_by <- col_by_to_matrix(col_by)
  # remove total column if present
  col_by <- col_by[, !vapply(col_by, all, logical(1)), drop = FALSE]
  if (ncol(col_by) == 0) {
    return(invisible(NULL))
  }
  # for each id, count number of appearances in each column of col_by, then check
  # that each id appears in at most one column of col_by (possibly several times)
  ids_in_at_most_one_col_by <- all(rowSums((
    data.frame(id = id, col_by) %>%
      group_by(.data$id) %>%
      summarise_all(sum) %>%
      select(-id)
  ) > 0) <= 1)
  if (!ids_in_at_most_one_col_by) {
    stop("Patient appears in multiple col_by columns/ARMs (excluding total column)")
  }

  invisible(NULL)
}
