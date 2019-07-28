# STREAM AE tables ----

# todo: move to utils
as_factor_keep_attributes <- function(x) {
  stopifnot(is.atomic(x))
  x_attrs <- attributes(x)
  x_attrs <- x_attrs[names(x_attrs) != "class"]
  do.call(structure, c(list(.Data = as.factor(x)), attributes(x)))
}

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
#'   \code{var_relabel} is used as the character string used as a label in the column header
#'   for each term.
#' @param id vector of subject identifier. Length of \code{id} must be the same as the
#'   length or number of rows of \code{terms}.
#' @param grade grade of adverse event as numeric. \code{var_relabel} is used as the character
#'   string used as a label in the column header for each grade.
#' @param col_N numeric vector with information of the number of patients in the
#'   levels of \code{col_by}. This is useful if there are patients that have no
#'   adverse events can be accounted for with this argument.
#' @param grade_levels numeric, ordered values of possible of grades in a form
#'   of \code{x:y}, default is \code{1:5}.
#'
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
#' \code{t_events_per_term_grade_id} doesn't deal with data with any non-complete records (has \code{NA}),
#' e.g. if any terms are missing. Impute missing values before using \code{t_events_per_term_grade_id}.
#'
#' \code{t_events_per_term_grade_id} orders data by "All Patients" column from the most commonly
#'  reported higher level term to the least frequent one. Within each group of higher level term,
#'  it sorts by decreasing frequency of lower level term. It brakes ties using \code{terms} names in alphabetical order.
#'
#' \code{t_events_per_term_grade_id} fills in \code{col_by} and \code{grade} with \code{0} value
#' in case there was no events reported for particular \code{col_by} and/or
#' \code{grade} category. Use \code{grade_levels} to modify the range of existing
#' grades. If data does not have any records with \code{grade} 5 and the intent
#' is to show only grades 1-4 rows then use \code{grade_levels = 1:4}.
#'
#' @return an \code{\link{rtable}} object.
#'
#' @export
#'
#' @template author_manukyae
#' @template author_waddella
#' @template author_zhanc107
#' @template author_wangh107
#'
#' @seealso \code{\link{t_max_grade_per_id}}, \code{\link{lt_events_per_term_grade_id_1}},
#'   \code{\link{lt_events_per_term_grade_id_2}}, \code{\link{t_events_per_term_id}}
#'
#' @examples
#' library(dplyr)
#' library(random.cdisc.data)
#' library(purrr)
#'
#' ADSL <- radsl(10, seed = 1)
#' ADAE <- radae(ADSL, 4, seed = 2)
#'
#' t_events_per_term_grade_id(
#'   terms = as_factor_keep_attributes(ADAE$AEDECOD),
#'   id = ADAE$USUBJID,
#'   grade = ADAE$AETOXGR,
#'   col_by = ADAE$ARM,
#'   col_N = table(ADSL$ARM),
#'   grade_levels = 1:5
#' )
#'
#' t_events_per_term_grade_id(
#'   terms = as_factor_keep_attributes(ADAE$AEDECOD), #todo: keep label or fix it right away in ADAE to be a factor
#'   id = ADAE$USUBJID,
#'   grade = ADAE$AETOXGR,
#'   col_by = ADAE$ARM %>% by_add_total("All Patients"),
#'   col_N = col_N_add_total(table(ADSL$ARM)),
#'   grade_levels = 1:5
#' )
#'
#' t_events_per_term_grade_id(
#'   terms = ADAE %>% select(AEBODSYS, AEDECOD) %>% map(as_factor_keep_attributes), #todo: fix in ADAE to be a factor
#'   id = ADAE$USUBJID,
#'   grade = ADAE$AETOXGR,
#'   col_by = ADAE$ARM %>% by_add_total("All Patients"),
#'   col_N = col_N_add_total(table(ADSL$ARM)),
#'   grade_levels = 1:5
#' )
t_events_per_term_grade_id <- function(terms,
                                       id,
                                       grade,
                                       col_by,
                                       col_N = NULL, # nolint
                                       grade_levels = 1:5,
                                       table_tree = FALSE) {
  if (is.atomic(terms)) {
    terms <- list(terms)
  }
  stopifnot(is.list(terms))

  terms_header <- vapply(terms, label, character(1))
  #todo: assign them to header row

  tree <- rsplit_to_tree(
    list(grade = grade, id = id, col_by = col_by),
    terms
  )
  tree@name <- "- Any adverse events -"
  tree@format_data$children_indent <- 0

  tree <- rapply_tree(
    tree,
    function(name, content, path) {
      list(name = name, content = t_max_grade_per_id(
        grade = content$grade,
        id = content$id,
        col_by = content$col_by,
        col_N = col_N,
        grade_levels = grade_levels,
        any_grade = "-Any Grade-"
      ))
    }
  )

  tree <- rsort_tree(
    tree,
    function(node) {
      # we sort by the numberbecause the first column is the overall grade level
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
#' @param grade a numeric vector with grade values
#' @param id a vector with id values
#' @param grade_levels a numeric vector used for naming rows for each level of
#'   grade
#' @param any_grade add a row that counts any occurrence, it is named \code{-Any
#'   Grade-} by default
#'
#' @importFrom stats aggregate
#' @export
#'
#' @template author_waddella
#' @template author_zhanc107
#' @template author_wangh107
#' @template author_qit3
#'
#' @seealso \code{\link{t_max_grade_per_id}}, \code{\link{t_events_per_term_grade_id}},
#'   \code{\link{lt_events_per_term_grade_id_2}}, \code{\link{lt_events_per_term_grade_id_1}}
#'
#' @examples
#' t_max_grade_per_id(
#'   grade =  c(1,2,3),
#'   id = c(1,1,1),
#'   col_by = factor(rep("A", 3)),
#'   col_N = 3
#' )
#'
#' id <- c(1,1,2,2,3,3,3,4)
#' t_max_grade_per_id(
#'   grade =  c(1,2,3,2,1,1,2,3),
#'   id = id,
#'   col_by = factor(LETTERS[id]),
#'   col_N = c(4,3,5,3)
#' )
#'
#' t_max_grade_per_id(
#'   grade =  c(1,2,3,2,1,1,2,3),
#'   id = id,
#'   col_by = factor(LETTERS[id]),
#'   col_N = c(4,3,5,4)
#' )
#'
#' \dontrun{
#' # throws an error because each id can only have one col_by
#' t_max_grade_per_id(
#'   grade =  c(1,2,3),
#'   id = c(1,2,2),
#'   col_by = factor(LETTERS[1:3]),
#'   col_N = c(15, 10, 12)
#' )
#' }
#'
#' \dontrun{
#' # throws an error because grade NA is not in grade_levels
#' t_max_grade_per_id(
#'   grade =  c(1,2,NA),
#'   id = c(1,2,3),
#'   col_by = factor(LETTERS[1:3]),
#'   col_N = c(15, 10, 12)
#' )
#' }
t_max_grade_per_id <- function(grade,
                               id,
                               col_by,
                               col_N = NULL, # nolint
                               grade_levels = NULL,
                               any_grade = "-Any Grade-") {
  stopifnot(is.numeric(grade))
  stopifnot(!any(is.na(id)))
  col_by <- col_by_to_matrix(col_by, grade)
  col_N <- col_N %||% get_N(col_by)
  check_col_by(grade, col_by, col_N, min_num_levels = 1)
  check_id(id, col_by)

  # # todo: replace by check that accounts for total column, at beginning of this function
  # if (any(duplicated(df_max$id))) {
  #   stop("every id can only have one col_by")
  # }

  grade_levels <- grade_levels %||% seq(1, max(grade, na.rm = TRUE))
  if (length(setdiff(grade, grade_levels)) > 0) {
    stop("grades exist that are not in grade_levels")
  }

  df <- data.frame(grade, id, stringsAsFactors = FALSE)

  df_max <- do.call(rbind, unname(Map(
    function(rows, col_by_name) {
      if (sum(rows) == 0) { # rows is logical
        # aggregate does not work when rows is all FALSE
        data.frame(grade = 1, id = 1, col_by = 1)[c(), ] # get an empty data frame
      } else {
        data.frame(
          aggregate(grade ~ id, FUN = max, drop = TRUE, data = df[rows,], na.rm = TRUE),
          col_by = col_by_name
        )
      }
    },
    col_by,
    names(col_by)
  )))
  df_max$col_by <- factor(df_max$col_by, names(col_by))
  # when col_by is a factor and not a general matrix, the equivalent call is
  # col_by <- col_by_to_factor(col_by)
  # df_max <- aggregate(grade ~ id + col_by, FUN = max, drop = TRUE, data = df, na.rm = TRUE)

  df_max$grade <- factor(df_max$grade, levels = grade_levels)
  df_max_no_na <- na.omit(df_max) # todo: why not do before

  tbl_any <- if (!is.null(any_grade)) {
    # TODO: Heng why do we allow this (na.omit)?
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
  header_add_N(tbl, col_N)
  # rownames_as_col(
  #   header_add_N(tbl, col_N),
  #   rownames_header = rheader(rrow(""), rrow(label(grade) %||% deparse(substitute(grade))))
  # )
}

# #todo: move to rtables
# todo: unfortunately not working
# rownames_as_col <- function(x, rownames_header) {
#   # moves row.names to a separate column
#   stopifnot(is(x, "rtable"))
#   browser()
#   cbind_rtables(rtablel(rownames_header, lapply(attr(x, "names"), function(x) rrowl(row.name = NULL, x))), x)
# }

#' checks that each patient appears in at most one col_by column (possibly
#' several times as AVAL corresponds to several measures and there are >= 1 rows per patient)
#' @examples
#' check_id(id = ADAE$USUBJID, col_by = ADAE$ARM)
#' @importFrom dplyr group_by summarise_all select
#' @importFrom magrittr %>%
check_id <- function(id, col_by) {
  col_by_old <- col_by
  col_by <- col_by_to_matrix(col_by)
  # remove total column if present
  col_by <- col_by[, !vapply(col_by, all, logical(1))]
  if (ncol(col_by) == 0) {
    return(invisible(NULL))
  }
  # for each id, count number of appearances in each column of col_by, then check
  # that each id appears in at most one column of col_by (possibly several times)
  ids_in_at_most_one_col_by <- all(rowSums((
    data.frame(id = id, col_by) %>% group_by(.data$id) %>% summarise_all(sum) %>% select(-id)
  ) > 0) <= 1)
  if (!ids_in_at_most_one_col_by) {
    stop("Patient appears in multiple col_by columns/ARMs (excluding total column)")
  }

  invisible(NULL)
}
