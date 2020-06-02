#' Abnormality Table
#'
#' Summarize number of patients with abnormalities by grade (e.g. High, Low, Abnormal).
#' Denominator for each abnormal grade is defined as number of subjects with
#' non-missing grade (if specified, excluding subjects whose baseline grade is
#' the same as this abnormal grade); numerator is defined as number of subjects
#' with the specified abnormal grade (if specified, excluding subjects whose
#' baseline grade is the same as this abnormal grade).
#'
#' Input data should include all post-baseline records (repeated and
#' unscheduled). Baseline and screening records should be excluded.
#'
#' @inheritParams argument_convention
#' @inheritParams t_summary_by
#' @inheritParams t_el_abnormality
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(random.cdisc.data)
#'
#' # example 1
#' ANL1 <- tibble(
#'   USUBJID = c("ID1", "ID1", "ID2", "ID2", "ID3", "ID3"),
#'   ARM = factor(rep("ARM A", 6)),
#'   PARAMCD = factor(rep("X", 6), levels = c("X", "Y")),
#'   ANRIND = c("N", "H", "N", "N", "N", "H"),
#'   BNRIND = c("N", "N", "L", "L", "N", "N"),
#' )
#'
#' ANL2 <- tibble(
#'   USUBJID = c("ID1", "ID1", "ID2", "ID2", "ID3", "ID3", "ID4", "ID5"),
#'   ARM = factor(rep("ARM A", 8)),
#'   PARAMCD = factor(rep("Y", 8), levels = c("X", "Y")),
#'   ANRIND = c("N", "L", "L", "N", "H", "L", "N", "H"),
#'   BNRIND = c("N", "N", "L", "L", "N", "N", "H", "")
#' )
#'
#' ANL <- rbind(ANL1, ANL2) %>%
#'   var_relabel(
#'     USUBJID = "Subject ID",
#'     ARM = "Treatment Arm",
#'     PARAMCD = "Parameter Code",
#'     ANRIND = "Normal Range Indicator",
#'     BNRIND = "Baseline Indicator"
#'   )
#'
#' t_abnormality(
#'   grade = sas_na(ANL$ANRIND),
#'   abnormal = c("L", "H"),
#'   baseline = sas_na(ANL$BNRIND),
#'   id = ANL$USUBJID,
#'   exclude_base_abn = TRUE,
#'   row_by = nested_by(ANL[,c("PARAMCD")]),
#'   col_by = ANL$ARM,
#'   col_N = 6,
#'   total = NULL,
#'   table_tree = FALSE
#' )
#'
#' # example 2
#' ADSL <- radsl(cached = TRUE)
#' ADLB0 <- radlb(cached = TRUE)
#' ADLB_labels <- var_labels(ADLB0)
#' ADLB_base <- ADLB0 %>%
#'  dplyr::filter(ABLFL == "Y") %>%
#'  dplyr::select(USUBJID, PARAM, ANRIND) %>%
#'  dplyr::rename(BNRIND = ANRIND)
#'
#' ADLB <- merge(ADLB0, ADLB_base, by = c("USUBJID", "PARAM")) %>%
#'  dplyr::filter(ABLFL != "Y" & ABLFL2 != "Y")
#'
#' ADLB$ANRIND <- as.character(ADLB$ANRIND)
#' ADLB$ANRIND[c(1,10,500)] <- " "
#' ADLB$ANRIND <- ADLB$ANRIND %>% sas_na() %>% as.factor()
#' var_labels(ADLB) <- c(ADLB_labels, BNRIND = "Baseline Grade")
#'
#' tbl2 <- t_abnormality(
#'   grade = ADLB$ANRIND,
#'   abnormal = c("LOW", "HIGH"),
#'   baseline = ADLB$BNRIND,
#'   id = ADLB$USUBJID,
#'   row_by = nested_by(ADLB[, c("PARAM", "AVISIT")]),
#'   col_by = by_all("All Patients"),
#'   col_N = nrow(ADSL),
#'   table_tree = FALSE
#' )
#'
#' header_row.names(tbl2) <- c("", paste0(ADLB_labels["PARAM"], "/", ADLB_labels["AVISIT"]))
#'
#' Viewer(tbl2)
#'

t_abnormality <- function(grade,
                          abnormal = c("L", "H"),
                          baseline,
                          id,
                          exclude_base_abn = FALSE,
                          col_by,
                          row_by = NULL,
                          col_N = NULL, #nolint
                          total = NULL,
                          table_tree = FALSE) {

  stopifnot(is.null(total) || is_character_single(total))

  if (!is_nested_by(row_by)) {
    row_by <- nested_by(list(row_by))
  }
  stopifnot(is.list(row_by))

  col_by <- col_by_to_matrix(col_by, grade)
  col_N <- col_N %||% get_N(col_by) #nolintr
  if (!is.null(total)) {
    col_by <- by_add_total(col_by, label = total)
    col_N <- col_N_add_total(col_N) #nolintr
    total <- NULL
  }

  abnormal <- as.character(abnormal)

  stopifnot(is_logical_single(exclude_base_abn),
            is.atomic(grade),
            is.atomic(baseline),
            is_character_vector(abnormal)
  )

  do.call(check_same_n, c(list(grade = grade,  baseline = baseline, id = id, col_by = col_by), row_by))

  grade_label <- label(grade) %||% deparse(substitute(grade))
  x <- data.frame(grade = grade, baseline = baseline, id = id, stringsAsFactors = FALSE)

  tree_data <- rsplit_to_tree(
    list(x = x, col_by = col_by),
    by_lst = row_by,
    drop_empty_levels = TRUE
  )

  tree <- rapply_tree(
    tree_data,
    function(name, content, path, is_leaf, ...) {
      # only compute for leaf nodes
      if (is_leaf) {
        xi <- tree_data@children[[1]]@content$x
        col_by_i <- tree_data@children[[1]]@content$col_by

        xi <- content$x
        col_by_i <- content$col_by

        tbl <- t_el_abnormality(
          grade =  xi$grade,
          abnormal = abnormal,
          baseline = xi$baseline,
          id = xi$id,
          exclude_base_abn = exclude_base_abn,
          col_by = col_by_i,
          col_N = col_N,
          total = total
        )

        content <- row_names_as_col(tbl, c("", grade_label))

        list(name = name,
             content = content)

      } else {

        list(name = name,
             content = NULL)

      }
    } # end function
  ) # end rapply_tree

  tree@name <- invisible_node_name(tree@name)

  # Adjust display by bring grade levels up to the same position as last element in row_by
  tree <- full_apply_at_depth(
    tree,
    function(node) {
      name <- node@name
      node@name <- invisible_node_name(name)

      if (is_rtable(node@content)) {
        row.names(node@content) <- c(name, row.names(node@content)[-1])
      }

      node

    },

    depth = length(row_by)
  )

  if (table_tree) {
    tree
  } else {
    to_rtable(tree)
  }
}

#' Count abnormality numerator and denominator for each grade level
#'
#' @inheritParams argument_convention
#' @param grade a vector specifying the grades. Fill blank with NA using
#'   \code{sas_na()}.
#' @param abnormal a vector specifying the unique values that
#'   represent abnormality in the order of display.
#' @param baseline a vector with length of \code{nrow(grade)} specifying
#'   the baseline grade for each record. Fill blank with NA using \code{sas_na()}.
#' @param id a vector of subject identifier with length of \code{nrow(grade)}
#' @param exclude_base_abn boolean to specify whether to exclude subjects with
#'   baseline abnormality in denominator. Default is \code{FALSE}.
#'
#' @export
#'
#' @return an \code{rtable}
#'
#' @examples
#' library(dplyr)
#'
#' ANL <- tibble(
#'   USUBJID = c("ID1", "ID1", "ID2", "ID2", "ID3", "ID3"),
#'   ANRIND = c("N", "L", "N", "N", "N", "H"),
#'   BNRIND = c("N", "N", "L", "L", "N", ""),
#'   ARM = factor(rep("ARM A", 6)),
#' )
#'
#' t_el_abnormality(
#'   grade = sas_na(ANL$ANRIND),
#'   abnormal = c("H", "L"),
#'   baseline = sas_na(ANL$BNRIND),
#'   id = ANL$USUBJID,
#'   col_by = ANL$ARM,
#'   col_N = table(ANL$ARM),
#'   exclude_base_abn = TRUE
#' )
#'

t_el_abnormality <- function(grade,
                             abnormal = c("L", "H"),
                             baseline,
                             id,
                             exclude_base_abn = FALSE,
                             col_by,
                             col_N = NULL, #nolint
                             total = NULL) {

  stopifnot(is.null(total) || is_character_single(total))

  col_by <- col_by_to_matrix(col_by, grade)
  col_N <- col_N %||% get_N(col_by) #nolintr
  if (!is.null(total)) {
    col_by <- by_add_total(col_by, label = total)
    col_N <- col_N_add_total(col_N) #nolintr
    total <- NULL
  }

  stopifnot(is_logical_single(exclude_base_abn),
            is.atomic(grade),
            is.atomic(baseline),
            is_character_vector(abnormal)
  )

  check_same_n(grade = grade,  baseline = baseline, id = id, col_by = col_by)

  x <- data.frame(grade = grade, baseline = baseline, id = id)

  check_col_by(x, col_by, col_N, min_num_levels = 1)

  # format_factor_perc displays three numbers in the format of xx/xx (xx.xx%)
  format_factor_perc <- function(x, output) {
    if (x[1] > 0 & x[2] > 0) {
      paste0(x[1], "/", x[2], " (", round(x[3], 3) * 100, "%)")
    } else if (x[1] == 0) {
      paste0(x[1], "/", x[2])
    } else {
      "-"
    }
  }

  tbl_header <- by_header(col_by)

  tbl <- rtablel(
    header = tbl_header,
    Map(function(grade_level) {
      rrowl(grade_level,
            lapply(col_by, function(rows) {
              xi <- x[rows, ]
              denominator <- xi[!is.na(xi$grade), c("id")] %>%
                unique() %>%
                count_n()
              numerator <- xi[xi$grade == grade_level, c("id")] %>%
                unique() %>%
                count_n()

              if (exclude_base_abn) {
                # If Among Subjects Without Abnormality at Baseline, exclude
                # subjects whose baseline is the same as the specified abnormal grade
                denominator <- do.call(setdiff,
                                       list(xi[!is.na(xi$grade), c("id")],
                                            xi[!is.na(xi$grade) & xi$baseline == grade_level, c("id")])
                ) %>%
                  unique() %>%
                  count_n()

                numerator <- do.call(setdiff,
                                     list(xi[xi$grade == grade_level, c("id")],
                                          xi[!is.na(xi$grade) & xi$baseline == grade_level, c("id")])
                ) %>%
                  unique() %>%
                  count_n()

              } # end of if (exclude_base_abn)

              return(c(numerator, denominator, numerator/denominator)) #nolint
            }), #end lapply a list of vectors
            format = format_factor_perc
      ) # end rrowl
    }, abnormal) # end Map
  ) # end rtablel

  header_add_N(tbl, col_N)

}
