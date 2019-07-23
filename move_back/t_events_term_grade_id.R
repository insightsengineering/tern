# STREAM AE tables ----

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
#'
#' ADSL <- radsl(10, seed = 1)
#' ADAE <- radae(ADSL, 4, seed = 2)
#'
#' t_events_per_term_grade_id(
#'   terms = ADAE$AEDECOD,
#'   id = ADAE$USUBJID,
#'   grade = ADAE$AETOXGR,
#'   col_by = ADAE$ARM,
#'   col_N = table(ADSL$ARM),
#'   total = "All Patients",
#'   grade_levels = 1:5
#' )
#'
#' t_events_per_term_grade_id(
#'   terms = ADAE %>% select(AEBODSYS, AEDECOD),
#'   id = ADAE$USUBJID,
#'   grade = ADAE$AETOXGR,
#'   col_by = ADAE$ARM,
#'   col_N = table(ADSL$ARM),
#'   total = "All Patients",
#'   grade_levels = 1:5
#' )
#'
#'
#'
#' # Table Trees
#' tbls <- t_events_per_term_grade_id(
#'   terms = ADAE$AEDECOD,
#'   id = ADAE$USUBJID,
#'   grade = ADAE$AETOXGR,
#'   col_by = ADAE$ARM,
#'   col_N = table(ADSL$ARM),
#'   total = "All Patients",
#'   grade_levels = 1:5,
#'   table_tree = TRUE
#' )
#' summary(tbls)
#' rbindl_rtables(tbls, gap = 1) # because term is already a rowname
#'
#' tbls <- t_events_per_term_grade_id(
#'   terms = ADAE %>% select(AEBODSYS, AEDECOD),
#'   id = ADAE$USUBJID,
#'   grade = ADAE$AETOXGR,
#'   col_by = ADAE$ARM,
#'   col_N = table(ADSL$ARM),
#'   total = "All Patients",
#'   grade_levels = 1:5,
#'   table_tree = TRUE
#' )
#' summary(tbls)
#' rbind_table_tree(lapply(tbls, rbindl_rtables, gap = 1))
#'
t_events_per_term_grade_id <- function(terms,
                                       id,
                                       grade,
                                       col_by,
                                       col_N, # nolint
                                       total = "All Patients",
                                       grade_levels = 1:5,
                                       table_tree = FALSE) {
  stopifnot(!is.null(terms))

  if (is.atomic(terms)) {
    terms <- data.frame(term = terms, stringsAsFactors = FALSE)
  } else if (!is.data.frame(terms)) {
    stop("terms needs to be either a vector or a data.frame")
  }

  irow <- ncol(terms)
  sort_index <-  if (is.null(total)) {
    # because the first column is the grade level
    function(tbli) sum(vapply(tbli[[irow]][2:ncol(tbli)], `[`, numeric(1), 1))
  } else {
    c(irow, nlevels(col_by) + 2, 1)
  }

  tbls <- if (ncol(terms) == 1) {
    l_tbls <- lt_events_per_term_grade_id_1(
      term = terms[[1]],
      id = id,
      grade = grade,
      col_by = col_by,
      col_N = col_N,
      total = total,
      grade_levels = grade_levels
    )

    order_tbls <- order_rtables(l_tbls, indices = sort_index, decreasing = TRUE)
    l_tbls[c(1, setdiff(order_tbls, 1))]

  } else if (ncol(terms) == 2) {

    l_tbls <- lt_events_per_term_grade_id_2(
      terms = terms,
      id = id,
      grade = grade,
      col_by = col_by,
      col_N = col_N,
      total = total,
      grade_levels = grade_levels
    )
    # l_tbls is of type list(all = list(tbl_overallm), cl_1 = list(tbl_overall, tbl_term_a, ...), ...)

    # sort tables by term frequency
    l_s_terms <- lapply(l_tbls, function(l_tbls_i) {
      order_tbls_i <- order_rtables(l_tbls_i, indices = sort_index, decreasing = TRUE)
      l_tbls_i[c(1, setdiff(order_tbls_i, 1))]
    })

    order_class <- order_rtables(lapply(l_s_terms, `[[`, 1), indices = sort_index, decreasing = TRUE)

    l_tbls_sorted <- l_s_terms[c(1, setdiff(order_class, 1))]

    l_tbls_sorted
  } else {
    stop("currently one or two terms are summported")
  }


  if (table_tree) {
    table_tree(tbls)
  } else {
    if (ncol(terms) == 1) {
      rbindl_rtables(tbls, gap = 1)
    } else {
      rbind_table_tree(lapply(tbls, rbindl_rtables,  gap = 1))
    }
  }

}

# Create Nested Lists of Tables that Compose Events tables ----

#' List of Events Terms Tables by Highest Grade
#'
#' \code{lt_events_per_term_grade_id_2} returns a nested list of events tables by max
#' grade (\code{\link{t_max_grade_per_id}}).
#'
#' @inheritParams t_events_per_term_grade_id
#'
#' @details
#' \if{html}{
#'
#' The data is split and table functions are applied to leaf nodes as follows:
#'
#' \figure{lt_events_per_term_grade_id_2.png}{options: alt="lt_events_per_term_grade_id_2 layout"}
#' }
#'
#' @return \code{rtable}
#'
#' @template author_waddella
#' @template author_zhanc107
#' @template author_wangh107
#' @template author_qit3
#'
#' @seealso \code{\link{t_max_grade_per_id}}, \code{\link{t_events_per_term_grade_id}},
#'   \code{\link{lt_events_per_term_grade_id_1}}, \code{\link{t_events_per_term_id}}
#'
lt_events_per_term_grade_id_2 <- function(terms,
                                          id,
                                          grade,
                                          col_by,
                                          col_N, # nolint
                                          total = "All Patients",
                                          grade_levels) {

  # check argument validity and consitency
  check_col_by(col_by, col_N, min_num_levels = 1, total)

  stopifnot(
    !any("- Overall -" %in% terms),
    !any(terms == "", na.rm = TRUE),
    is.data.frame(terms) && ncol(terms) == 2
  )

  class_label <- label(terms[[1]])
  term_label <- label(terms[[2]])
  grade_label <- label(grade)

  if (is.null(term_label)) {
    class_label <- deparse(substitute(class))
  }
  if (is.null(term_label)) {
    term_label <- deparse(substitute(term))
  }
  if (is.null(grade_label)) {
    grade_label <- deparse(substitute(grade))
  }

  # data prep
  df <- data.frame(
    class = terms[[1]],
    term = terms[[2]],
    id = id,
    grade = grade,
    col_by = col_by,
    stringsAsFactors = FALSE
  )

  if (any(is.na(df))) {
    stop("partial missing data in rows of [class, term, grade] is currently not supported")
  }

  # adding All Patients
  if (!is.null(total)) {
    .t <- add_total(x = df, col_by = col_by, total_level = total, col_N = col_N)
    col_N <- .t$col_N # nolint
    df <- data.frame(class = .t$x$class, term = .t$x$term, id = paste(.t$x$id, "-", .t$col_by),
                     grade = .t$x$grade,
                     col_by = .t$col_by, stringsAsFactors = FALSE)
  }

  # start tabulatings

  # create nested list with data used for creating the sub tables
  df_class_term <- c(
    list("- Any adverse events -" = list("- Overall -" = df)),
    lapply(split(df, df$class), function(x) {
      c(
        list("- Overall -" = x),
        split(x, x$term)
      )
    })
  )

  tbl_header <- rheader(
    rrowl(class_label, c(list(NULL), as.list(levels(df$col_by)))),
    rrowl(term_label, c(list(rcell(grade_label, format = "xx")), as.list(col_N)), format = "(N=xx)", indent = 1)
  )

  # now create the tables
  Map(function(df_terms, class_name) {
    Map(function(df_term, term_name) {

      tbl_raw <- t_max_grade_per_id(
        grade = df_term$grade,
        id = df_term$id,
        col_by = df_term$col_by,
        col_N = col_N,
        grade_levels = grade_levels,
        any_grade = "- Any Grade -"
      )

      # move rownames to column
      tbl <- row_names_as_col(tbl_raw)
      row.names(tbl)[1] <- term_name

      tbl

    }, df_terms, names(df_terms))
  }, df_class_term, names(df_class_term))

}

#' List of Events Terms Tables By Highest Grade (One Level Term only)
#'
#' \code{lt_events_per_term_grade_id_1} returns a nested list of events tables by max
#' grade (\code{\link{t_max_grade_per_id}}).
#'
#' @inheritParams t_events_per_term_grade_id
#' @param term term information as character or factor, however factor levels
#'   are not repeated by class, only terms with count > 1 are listed per class.
#'   \code{var_relabel} is used as the character string used as a label in the
#'   column header for each term.
#'
#' @details
#' \if{html}{
#'
#' The data is split and table functions are applied to leaf nodes as follows:
#'
#' \figure{lt_events_per_term_grade_id_1.png}{options: alt="lt_events_per_term_grade_id_1 layout"}
#' }
#'
#' @template author_waddella
#' @template author_zhanc107
#' @template author_wangh107
#' @template author_qit3
#'
#' @seealso \code{\link{t_max_grade_per_id}}, \code{\link{t_events_per_term_grade_id}},
#'   \code{\link{lt_events_per_term_grade_id_2}}, \code{\link{t_events_per_term_id}}
#'
#' @examples
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(10, seed = 1)
#' ADAE <- radae(ADSL, 4, seed = 2)
#'
#' l_tbls <- tern:::lt_events_per_term_grade_id_1(
#'   term = ADAE$AEDECOD,
#'   id = ADAE$USUBJID,
#'   grade = ADAE$AETOXGR,
#'   col_by = ADAE$ARM,
#'   col_N = table(ADSL$ARM),
#'   total = "All Patients",
#'   grade_levels = 1:5
#' )
#'
#' rbindl_rtables(l_tbls, gap = 1)
lt_events_per_term_grade_id_1 <- function(term,
                                          id,
                                          grade,
                                          col_by,
                                          col_N, # nolint
                                          total = "All Patients",
                                          grade_levels) {

  check_col_by(col_by, col_N, min_num_levels = 1, total)

  stopifnot(
    !any("All Patients" %in% col_by),
    !any(term == "", na.rm = TRUE)
  )

  term_label <- label(term)
  grade_label <- label(grade)

  if (is.null(term_label)) {
    term_label <- deparse(substitute(term))
  }
  if (is.null(grade_label)) {
    grade_label <- deparse(substitute(grade))
  }

  df <- data.frame(
    term = term,
    id = id,
    grade = grade,
    col_by = col_by,
    stringsAsFactors = FALSE
  )

  if (any(is.na(df))) {
    stop("partial missing data in rows of [class, grade] is currently not supported")
  }


  if (!is.null(total)) {
    .t <- add_total(x = df, col_by = col_by, total_level = total, col_N = col_N)
    col_N <- .t$col_N # nolint
    df <- data.frame(term = .t$x$term, id = paste(.t$x$id, "-", .t$col_by),
                     grade = .t$x$grade,
                     col_by = .t$col_by, stringsAsFactors = FALSE)
  }

  # start tabulating
  df_terms <- c(
    list("- Overall -" = df),
    split(df, df$term)
  )

  tbl_header <- rheader(
    rrowl("", c("", levels(df$col_by))),
    rrowl(term_label, c(list(rcell(grade_label, format = NULL)), setNames(as.list(col_N), NULL)), format = "(N=xx)")
  )

  Map(function(df_term, term_i) {

    tbl_raw <- t_max_grade_per_id(
      grade = df_term$grade,
      id = df_term$id,
      col_by = df_term$col_by,
      col_N = col_N,
      grade_levels = grade_levels,
      any_grade = "- Any Grade -"
    )

    tbl <- row_names_as_col(tbl_raw)
    row.names(tbl)[1] <- term_i
    header(tbl) <- tbl_header

    tbl

  }, df_terms, names(df_terms))

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
#'
t_max_grade_per_id <- function(grade,
                               id,
                               col_by,
                               col_N, # nolint
                               grade_levels = NULL,
                               any_grade = "-Any Grade-") {

  check_col_by(col_by, col_N, 1)
  stopifnot(is.numeric(grade))
  stopifnot(has_no_na(id))

  if (is.null(grade_levels)) {
    grade_levels <- seq(1, max(grade, na.rm = TRUE))
  }

  if (length(setdiff(grade, grade_levels)) > 0) {
    stop("grades exist that are not in grade_levels")
  }

  df <- data.frame(grade, id, col_by, stringsAsFactors = FALSE)

  df_max <- aggregate(grade ~ id + col_by, FUN = max, drop = TRUE, data = df, na.rm = TRUE)

  if (any(duplicated(df_max$id))) {
    stop("every id can only have one col_by")
  }

  df_max$fct_grade <- factor(df_max$grade, levels = grade_levels)

  tbl_any <- if (!is.null(any_grade)) {
    # TODO: Heng why do we allow this (na.omit)?
    df_no_na <- na.omit(df)
    df_no_na_id <- df_no_na[!duplicated(df_no_na$id), ]
    rtabulate(
      x = df_no_na_id,
      row_by = no_by(any_grade),
      col_by = df_no_na_id$col_by,
      FUN = count_perc_col_N,
      format = "xx (xx.x%)",
      col_wise_args = list(n_i = col_N)
    )
  } else {
    NULL
  }

  df_max_no_na <- na.omit(df_max)
  tbl_x <- rtabulate(
    x = df_max_no_na,
    row_by  = df_max_no_na$fct_grade,
    col_by  = df_max_no_na$col_by,
    FUN = count_perc_col_N,
    format = "xx (xx.x%)",
    col_wise_args = list(n_i = col_N)
  )

  tbl <- rbind(tbl_any, tbl_x)

  header_add_N(tbl, col_N)

}
