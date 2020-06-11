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
#' @md
#' @inheritParams argument_convention
#' @param terms (\code{character} or \code{factor} vector, or \code{data.frame})\cr
#'   Represents events information.
#'   Multi-level nesting is possible when \code{terms} is a \code{data.frame} and columns should
#'   be ordered with the first column specifying the highest level term and the last column the
#'   lowest level term.
#' @param id (vector)\cr
#'   contains subject identifier. Length of \code{id} must be the same as the
#'   length or number of rows of \code{terms}.
#' @param grade (vector)\cr
#'   contains grade of adverse event.
#'   For factors, it is assumed that intensity corresponds to the order of
#'   the factor levels, from lowest to worst level.
#'   If that is not the case, you have to preprocess using `levels(grade) <- new_levels`
#'   beforehand.
#'   The resulting table is then  ordered in the same fashion. To instead display
#'   it in descending maximum grade order, you need to apply some postprocessing.
#' @param col_N numeric vector with information of the number of patients in the
#'   levels of \code{col_by}. This is useful if there are patients that have no
#'   adverse events can be accounted for with this argument.
#' @param any_grade string to specify the row name which counts any occurrence,
#'   it is named \code{Any Grade} by default
#' @param event_type (\code{character} value) to specify the type of event that is summarized, \code{event} by default.
#'   Only displayed when \code{terms} has 2 columns.
#'
#' @details
#' \code{t_events_per_term_grade_id} includes percentages based on the total number of subjects
#' in the column heading (i.e. \code{"N=nnn"}). \code{col_N} can be explicitly specified to
#' get N for percentage calculation from either events dataset or additional dataset like
#' subject level dataset. See the example.
#'
#' Multiple occurrences of the same event in an individual \code{id} are counted only once when counting
#' number of subjects.
#'
#' \code{t_events_per_term_grade_id} doesn't deal with data with missing grade. Impute or filter missing
#' values with \code{sas_na} and \code{explicit_na} before using \code{t_events_per_term_grade_id}.
#'
#' \code{t_events_per_term_grade_id} orders data by overall frequency across all columns from the most commonly
#'  reported higher level term to the least frequent one. Within each group of higher level term,
#'  it sorts by decreasing frequency of lower level term. It brakes ties using \code{terms} names in
#'  alphabetical order.
#'
#' \code{t_events_per_term_grade_id} fills in \code{col_by} and \code{grade} with \code{0} value
#' in case there was no events reported for particular \code{col_by} and/or
#' \code{grade} category.
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
#' ADSL <- radsl(cached = TRUE)
#' ADAE <- radae(ADSL, 4L, seed = 2)
#'
#' t_events_per_term_grade_id(
#'   terms = ADAE$AEDECOD,
#'   id = ADAE$USUBJID,
#'   grade = ADAE$AETOXGR,
#'   col_by = ADAE$ARM,
#'   col_N = table(ADSL$ARM)
#' )
#'
#' # don't prune zero rows
#' t_events_per_term_grade_id(
#'   terms = ADAE$AEDECOD,
#'   id = ADAE$USUBJID,
#'   grade = ADAE$AETOXGR,
#'   col_by = ADAE$ARM,
#'   col_N = table(ADSL$ARM),
#'   prune_zero_rows = FALSE
#' )
#'
#' # Introducing imperfect data and demo how to preprocess
#' ADAE$AEDECOD <- as.character(ADAE$AEDECOD)
#' ADAE$AEBODSYS <- as.character(ADAE$AEBODSYS)
#' ADAE$AEDECOD[c(1,5)] = ""
#' ADAE$AEBODSYS[c(2,6)] = " "
#'
#' ADAE <- ADAE %>%
#'   mutate(
#'    grade_category = case_when(
#'      AETOXGR %in% c(1,2) ~ "Mild",
#'      AETOXGR %in% c(3,4) ~ "Moderate",
#'      AETOXGR %in% c(5) ~ "Severe"
#'    ),
#'    grade_category = factor(grade_category, levels=c("Mild", "Moderate", "Severe")),
#'    # Preprocess imperfect data
#'    AEDECOD = AEDECOD %>% sas_na %>% explicit_na(label = "Not Coded"),
#'    AEBODSYS = AEBODSYS %>% sas_na %>% explicit_na(label = "Not Coded"),
#'   ) %>%
#'   var_relabel(
#'     AEBODSYS = "MedDRA System Organ Class",
#'     AEDECOD = "MedDRA Preferred Term",
#'     AETOXGR = "Analysis Toxicity Grade",
#'     grade_category = "Insensity")
#'
#'
#' t_events_per_term_grade_id(
#'   terms = ADAE$AEDECOD,
#'   id = ADAE$USUBJID,
#'   grade = ADAE$AETOXGR,
#'   col_by = ADAE$ARM,
#'   col_N = table(ADSL$ARM),
#'   total = "All Patients"
#' )
#'
#' t_events_per_term_grade_id(
#'   terms = ADAE$AEDECOD,
#'   id = ADAE$USUBJID,
#'   grade = ADAE$grade_category,
#'   col_by = ADAE$ARM,
#'   col_N = table(ADSL$ARM),
#'   total = "All Patients"
#' )
#'
#' t_events_per_term_grade_id(
#'   terms = ADAE %>% select(AEBODSYS, AEDECOD),
#'   id = ADAE$USUBJID,
#'   grade = ADAE$AETOXGR,
#'   col_by = ADAE$ARM,
#'   col_N = table(ADSL$ARM),
#'   event_type = "adverse events"
#' )
#'
#' levels(ADAE$grade_category) <- c("Severe", "Moderate", "Mild")
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
                                       event_type = "event",
                                       any_grade = "Any Grade",
                                       prune_zero_rows = TRUE,
                                       table_tree = FALSE) {


  terms <- argfix_events_terms(terms)

  stopifnot(is.null(total) || is_character_single(total))
  stopifnot(is_logical_single(prune_zero_rows))

  grade_label <- label(grade) %||% deparse(substitute(grade))

  grade <- with_label(grade, label = grade_label)

  col_by <- col_by_to_matrix(col_by, x = id)
  col_N <- col_N %||% get_N(col_by) #nolintr
  if (!is.null(total)) {
    col_by <- by_add_total(col_by, label = total)
    col_N <- col_N_add_total(col_N) #nolintr
    total <- NULL
  }

  # First create a tree to distribute information splitting by terms, we will then use rapply_tree to operate based on
  # this info.

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
        any_grade = any_grade,
        prune_zero_rows = prune_zero_rows
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
#' @param grade `factor` grade of adverse event.
#'   It is assumed that intensity corresponds to the order of the factor levels.
#' @param id a vector with id values
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
#'   grade = factor(c(1, 2, 3)),
#'   id = c(1,1,1),
#'   col_by = factor(rep("A", 3)),
#'   col_N = 3
#' )
#'
#' # don't prune zero rows
#' t_max_grade_per_id(
#'   grade = factor(c(1, 2, 3)),
#'   id = c(1,1,1),
#'   col_by = factor(rep("A", 3)),
#'   col_N = 3,
#'   prune_zero_rows = FALSE
#' )
#'
#' id <- c(1,1,2,2,3,3,3,4)
#' grade <- factor(c("a", "b", "c", "b", "a", "a", "b", "c"))
#' levels(grade) <- c("d", "c", "b", "a")
#' t_max_grade_per_id(
#'   grade = grade,
#'   id = id,
#'   col_by = factor(LETTERS[id]),
#'   col_N = c(4, 3, 5, 3),
#'   any_grade = "Any Class"
#' )
#'
#' t_max_grade_per_id(
#'   grade = factor(c(1, 2, 3, 2, 1, 1, 2, 3)),
#'   id = id,
#'   col_by = factor(LETTERS[id]),
#'   col_N = c(4, 3, 5, 4)
#' )
#'
#' \dontrun{
#' # throws an error because each id can only have one col_by
#' # t_max_grade_per_id(
#'   #   grade = factor(c(1, 2, 3)),
#'   #   id = c(1, 2, 2),
#'   #   col_by = factor(LETTERS[1:3]),
#'   #   col_N = c(15, 10, 12)
#'   # )
#' }
#'
#' \dontrun{
#' # throws an error because grade should not be NA
#' # t_max_grade_per_id(
#' #   grade =  factor(c(1, 2, NA)),
#' #   id = c(1, 2, 3),
#' #   col_by = factor(LETTERS[1:3]),
#' #   col_N = c(15, 10, 12)
#' # )
#' }
t_max_grade_per_id <- function(grade,
                               id,
                               col_by,
                               col_N = NULL, # nolint
                               total = NULL,
                               any_grade = "Any Grade",
                               prune_zero_rows = TRUE) {
  grade_label <- label(grade) %||% deparse(substitute(grade))

  stopifnot(is.null(total) || is_character_single(total))
  stopifnot(is.factor(grade))
  stopifnot(!any(is.na(grade)))
  stopifnot(is_logical_single(prune_zero_rows))

  grade <- factor(grade, levels = levels(grade), ordered = TRUE) # needed to take maximum
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

  df_max$grade <- factor(df_max$grade, levels = levels(grade))
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

  if (prune_zero_rows) {
    # remove attributes of rcell to get content
    remove_attributes <- function(x) {
      attributes(x) <- NULL
      return(x)
    }
    # lapply on rtables loops over rows
    # select rows which are not all equal to 0
    tbl_x <- tbl_x[
      vapply(tbl_x, function(row) {
        any(vapply(lapply(row, remove_attributes), function(x) !identical(x, 0), logical(1)))
      }, logical(1))
      ]
  }

  tbl <- rbind(tbl_any, tbl_x)
  tbl <- header_add_N(tbl, col_N)

  row_names_as_col(tbl, c("", grade_label))
}

#' checks that each patient appears in at most one \code{col_by} column (possibly
#' several times as \code{AVAL} corresponds to several measures and there are >= 1 rows per patient)
#'
#' @param id patient id
#' @param col_by columns indicating where patient is, (this function checks that the patient only
#'   appears in one column, ignoring the "by_all" / total column)
#'
#' @importFrom dplyr group_by summarise_all select
#' @importFrom magrittr %>%
#'
#' @examples
#' library(random.cdisc.data)
#' ADSL <- radsl(cached = TRUE)
#' ADAE <- radae(ADSL, 4L, seed = 2)
#' tern:::check_id(id = ADAE$USUBJID, col_by = ADAE$ARM)
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
