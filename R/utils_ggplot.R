#' Convert `rtable` to `ggplot` object
#'
#' Given a [rtables::rtable()] object, performs basic conversion to a `ggplot` object built using
#' functions from the `ggplot2` package.
#'
#' @param tbl (`rtable`)\cr a [rtables::rtable()] object.
#' @param fontsize (`numeric`)\cr font size.
#' @param colwidths (`vector` of `numeric`)\cr a vector of column widths. Each element's position in
#'   `colwidths` corresponds to the column of `tbl` in the same position. If `NULL`, column widths
#'   are calculated according to maximum number of characters per column.
#' @param first_col_padding (`numeric`)\cr additional padding to use when calculating spacing between
#'   the first (label) column and the second column of `tbl`. Defaults to 0.
#'
#' @return a `ggplot` object.
#'
#' @examples
#' dta <- data.frame(
#'   USUBJID = rep(1:6, each = 3),
#'   PARAMCD = rep("lab", 6 * 3),
#'   AVISIT  = rep(paste0("V", 1:3), 6),
#'   ARM     = rep(LETTERS[1:3], rep(6, 3)),
#'   AVAL    = c(9:1, rep(NA, 9))
#' )
#'
#' lyt <- basic_table() %>%
#'   split_cols_by(var = "ARM") %>%
#'   split_rows_by(var = "AVISIT") %>%
#'   analyze_vars(vars = "AVAL")
#'
#' tbl <- build_table(lyt, df = dta)
#'
#' rtable2gg(tbl)
#'
#' rtable2gg(tbl, fontsize = 5, colwidths = c(2, 1, 1, 1))
#'
#' @export
rtable2gg <- function(tbl, fontsize = 4, colwidths = NULL, first_col_padding = 0) {
  mat <- matrix_form(tbl)
  mat_strings <- mf_strings(mat)
  mat_aligns <- mf_aligns(mat)
  mat_indent <- mf_rinfo(mat)$indent
  nlines_hdr <- mf_nlheader(mat)
  shared_hdr_rows <- which(apply(mf_display(mat), 1, function(x) (any(!x))))

  tbl_df <- data.frame(mat_strings)
  body_rows <- seq(nlines_hdr + 1, nrow(tbl_df))
  mat_aligns <- apply(mat_aligns, 1:2, function(x) if (x == "left") 0 else if (x == "center") 0.5 else 1)

  # Apply indentation in first column
  tbl_df[body_rows, 1] <- sapply(body_rows, function(i) {
    ind_i <- mat_indent[i - nlines_hdr] * 4
    if (ind_i > 0) paste0(paste(rep(" ", ind_i), collapse = ""), tbl_df[i, 1]) else tbl_df[i, 1]
  })

  # Get column widths
  if (is.null(colwidths)) {
    colwidths <- apply(tbl_df, 2, function(x) max(nchar(x))) + 1
  }
  tot_width <- sum(colwidths) + first_col_padding

  if (length(shared_hdr_rows) > 0) {
    tbl_df <- tbl_df[-shared_hdr_rows, ]
    mat_aligns <- mat_aligns[-shared_hdr_rows, ]
  }

  res <- ggplot(data = tbl_df) +
    theme_void() +
    scale_x_continuous(limits = c(0, tot_width)) +
    scale_y_continuous(limits = c(0, nrow(mat_strings))) +
    geom_segment(aes(
      x = 0, xend = tot_width,
      y = nrow(mat_strings) - nlines_hdr + 0.5, yend = nrow(mat_strings) - nlines_hdr + 0.5
    ))

  # If header content spans multiple columns, center over these columns
  if (length(shared_hdr_rows) > 0) {
    mat_strings[shared_hdr_rows, ] <- trimws(mat_strings[shared_hdr_rows, ])
    for (hr in shared_hdr_rows) {
      hdr_lbls <- unique(mat_strings[hr, ])
      hdr_lbls <- hdr_lbls[nzchar(hdr_lbls)]
      for (hl in hdr_lbls) {
        which_cols <- which(mat_strings[hr, ] == hl)
        line_pos <- c(
          sum(colwidths[1:(which_cols[1] - 1)]) + 1 + first_col_padding,
          sum(colwidths[1:max(which_cols)]) - 1 + first_col_padding
        )
        lbl_pos <- mean(line_pos)
        res <- res +
          geom_text(
            x = lbl_pos,
            y = nrow(mat_strings) + 1 - hr,
            label = hl,
            size = fontsize
          ) +
          geom_segment(
            x = line_pos[1],
            xend = line_pos[2],
            y = nrow(mat_strings) - hr + 0.5,
            yend = nrow(mat_strings) - hr + 0.5
          )
      }
    }
  }

  # Add table columns
  for (i in seq_len(ncol(tbl_df))) {
    res <- res + geom_text(
      x = if (i == 1) 0 else sum(colwidths[1:i]) - 0.5 * colwidths[i] + first_col_padding,
      y = rev(seq_len(nrow(tbl_df))),
      label = tbl_df[, i],
      hjust = mat_aligns[, i],
      size = fontsize
    )
  }

  res
}
