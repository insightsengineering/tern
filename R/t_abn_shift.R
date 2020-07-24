#' Abnormality Shift from Baseline Table
#'
#' Summarize the number of patients at the specified visit with the abnormality (e.g. High, Low),
#' among patients with non-missing value at that visit and a baseline value.
#'
#' Input data should include all post-baseline records (repeated and
#' unscheduled). Baseline and screening records should be excluded.
#'
#' @inheritParams argument_convention
#' @inheritParams t_summary_by
#' @inheritParams t_el_abn_shift
#'
#' @importFrom rlang `%||%`
#' @importFrom rtables col_by_to_matrix  by_add_total
#' @importFrom utils.nest is_character_single is_character_vector
#' @export
#'
#' @examples
#' library(dplyr)
#' library(random.cdisc.data)
#' ADSL <- radsl(cached = TRUE)
#' ADLB <- radlb(cached = TRUE) %>%
#'   dplyr::filter(ABLFL != "Y") %>%
#'   dplyr::filter(AVISIT != "SCREENING") %>%
#'   dplyr::mutate(AVISIT = droplevels(AVISIT))
#'
#' ADLB_f <- ADLB %>% dplyr::filter(PARAMCD == "CRP")
#'
#' tbl1 <- t_abn_shift(
#'   rri_ana = ADLB_f$ANRIND,
#'   rri_base = ADLB_f$BNRIND,
#'   value_ana = ADLB_f$AVAL,
#'   value_base = ADLB_f$BASE,
#'   abnormal = c("LOW", "HIGH"),
#'   id = ADLB_f$USUBJID,
#'   row_by = ADLB_f[,  "AVISIT"],
#'   col_by = ADLB_f$ARM,
#'   col_N = table(ADSL$ARM),
#'   table_tree = FALSE
#' )
#' \dontrun{
#' Viewer(tbl1)
#' }
#'
#' tbl2 <- t_abn_shift(
#'   rri_ana = ADLB$ANRIND,
#'   rri_base = ADLB$BNRIND,
#'   value_ana = ADLB$AVAL,
#'   value_base = ADLB$BASE,
#'   abnormal = c("LOW", "HIGH"),
#'   id = ADLB$USUBJID,
#'   row_by = ADLB[, c("PARAM", "AVISIT")],
#'   col_by = ADLB$ARM,
#'   col_N = table(ADSL$ARM),
#'   total = "All Patients",
#'   table_tree = TRUE
#' )
#' \dontrun{
#' Viewer(to_rtable(tbl2))
#' }
#'
#'

t_abn_shift <- function(rri_ana,
                        rri_base,
                        value_ana,
                        value_base,
                        abnormal = c("LOW", "HIGH"),
                        id,
                        col_by,
                        row_by,
                        col_N = NULL, #nolint
                        total = NULL,
                        table_tree = FALSE) {
  stopifnot(is.null(total) || is_character_single(total))
  if (!is_nested_by(row_by)) {
    check_row_by(row_by, rri_ana)
  }

  if (!is.list(row_by)) {
    row_by <- list(row_by)
  }
  stopifnot(is.list(row_by))

  col_by <- col_by_to_matrix(col_by, rri_ana)
  col_N <- col_N %||% get_N(col_by) #nolintr
  if (!is.null(total)) {
    col_by <- by_add_total(col_by, label = total)
    col_N <- col_N_add_total(col_N) #nolintr
    total <- NULL
  }

  stopifnot(
    is.atomic(rri_ana),
    is.atomic(rri_base),
    is.atomic(value_ana),
    is.atomic(value_base),
    is_character_vector(abnormal)
  )
  check_same_n(
    rri_ana = rri_ana,
    rri_base = rri_base,
    value_ana = value_ana,
    value_base = value_base,
    id = id,
    col_by = col_by
  )
  x <- data.frame(
    rri_ana = rri_ana,
    rri_base = rri_base,
    value_ana = value_ana,
    value_base = value_base,
    id = id,
    row_by = row_by,
    stringsAsFactors = FALSE
  )
  tree_data <- rsplit_to_tree(
    list(x = x,   col_by = col_by),
    by_lst =  row_by
  )
  tree <- rapply_tree(
    tree_data,
    function(name, content, path, is_leaf, ...) {
      # only compute for leaf nodes
      if (is_leaf) {
        xi <- content$x
        col_by_i <- content$col_by
        tbl <- t_el_abn_shift(
          rri_ana = xi$rri_ana,
          rri_base = xi$rri_base,
          value_ana = xi$value_ana,
          value_base = xi$value_base,
          id = xi$id,
          abnormal = abnormal,
          col_by = col_by_i,
          col_N = col_N,
          total = total
        )
        list(name = name,
             content = tbl)

      } else {
        list(name = name,
             content = NULL)
      }
    }
  )
  tree@name <- invisible_node_name(tree@name)
  if (table_tree) {
    return(tree)
  } else {
    return(to_rtable(tree))
  }
}

#' Count abnormality shift from baseline for each abnormality level
#'
#' This is the elementary function used in function [t_abn_shift], where the calculation could be repeated \cr
#' for multiple post-baseline visits and multiple laboratory tests.
#'
#' @inheritParams argument_convention
#' @md
#' @param rri_ana a vector specifying the Reference Range Indicator (e.g. High/Low/Normal). \cr
#'   Fill blank with NA using [sas_na()].
#' @param rri_base a vector specifying the Reference Range Indicator (e.g. High/Low/Normal) at baseline \cr
#'   for the corresponding records in `rri_ana`. Fill blank with NA using [sas_na()].
#' @param value_ana a vector of numeric measurement.
#' @param value_base a vector of numeric measurement at baseline \cr
#'   for the corresponding records in `value_ana`.
#' @param abnormal a vector specifying the unique values that
#'   represent abnormality in the order of display.
#' @param id a vector of subject identifier with length of `rri_ana`.
#'
#' @importFrom rlang `%||%`
#' @importFrom rtables col_by_to_matrix  by_add_total
#' @importFrom utils.nest is_character_single is_character_vector
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
#'   BNRIND = c("N", "N", "L", "L", "", ""),
#'   AVAL = c(1.5, 0.4, 1.8, 1.9, 2.4, 4.6),
#'   BASE = c(1.2, 1.2, 0.5, 0.5, NA, NA),
#'   ARM = factor(rep("ARM A", 6)),
#' )
#'
#' t_el_abn_shift(
#'   rri_ana = sas_na(ANL$ANRIND),
#'   rri_base = sas_na(ANL$BNRIND),
#'   value_ana = ANL$AVAL,
#'   value_base = ANL$BASE,
#'   abnormal = c("H", "L"),
#'   id = ANL$USUBJID,
#'   col_by = ANL$ARM,
#'   col_N = table(ANL$ARM),
#'   total = "All Patients"
#' )
#'
#' t_el_abn_shift(
#'   rri_ana = sas_na(ANL$ANRIND),
#'   rri_base = sas_na(ANL$BNRIND),
#'   value_ana = ANL$AVAL,
#'   value_base = ANL$BASE,
#'   abnormal = c("H", "L"),
#'   id = ANL$USUBJID,
#'   col_by = by_all("All Patients"),
#'   col_N = nrow(ANL)
#' )
#'

t_el_abn_shift <- function(rri_ana,
                           rri_base,
                           value_ana,
                           value_base,
                           abnormal = c("HIGH", "LOW"),
                           id,
                           col_by,
                           col_N = NULL, #nolintr
                           total = NULL) {

  stopifnot(is.null(total) || is_character_single(total))
  col_by <- col_by_to_matrix(col_by, rri_ana)
  if (!is.null(total)) {
    col_by <- by_add_total(col_by, label = total)
    col_N <- col_N_add_total(col_N) #nolintr
    total <- NULL
  }
  x <- data.frame(
    rri_ana = rri_ana,
    rri_base = rri_base,
    value_ana = value_ana,
    value_base = value_base,
    id = id,
    stringsAsFactors = FALSE
  )

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

  tbl_list <- Map(function(abn) {
    base_status <- c(paste("NOT", abn), abn, "Total")
    sum_col <- lapply(col_by, function(rows) {
      xi <- x[rows, ]
      xi_base_non_abn <- subset(xi, !is.na(value_base) & rri_base != abn & !is.na(value_ana))
      xi_base_abn <- subset(xi, !is.na(value_ana) & rri_base == abn)
      deno_base_non_abn <- length(unique(xi_base_non_abn[["id"]]))
      nume_base_non_abn <- length(unique(subset(xi_base_non_abn, rri_ana == abn)[["id"]]))
      deno_base_abn <- length(unique(xi_base_abn[["id"]]))
      nume_base_abn <- length(unique(subset(xi_base_abn, rri_ana == abn)[["id"]]))
      deno_tot <- length(unique(subset(xi, !is.na(value_ana))[["id"]]))
      nume_tot <- length(unique(subset(xi, rri_ana == abn)[["id"]]))
      chk <- list(
        c(nume_base_non_abn, deno_base_non_abn, nume_base_non_abn / deno_base_non_abn),
        c(nume_base_abn, deno_base_abn, nume_base_abn / deno_base_abn),
        c(nume_tot, deno_tot, nume_tot / deno_tot)
      )
      names(chk) <- base_status
      chk
    })
    tbl <- rtablel(
      header = tbl_header,
      Filter(function(nl) !is.null(nl),
             lapply(base_status, function(x) {
               cells <- lapply(sum_col, function(y) y[[x]])
               cell_2 <- lapply(cells, function(i) i[2])
               if (any(cell_2 != 0)){
                 rrowl(x, cells, format = format_factor_perc)
               }
             }))
    )
    tbl <- header_add_N(tbl, col_N)
    tbl <- row_names_as_col(tbl, c("", "Baseline Status"))
    if (dim(tbl)[1] > 0){
      row.names(tbl) <- c(abn, rep("", dim(tbl)[1] - 1))
      tbl
    } else {
      NULL
    }
  }, abnormal)

  el_table <- do.call("rbind", tbl_list)
  return(el_table)
}
