#' Horizontal Waterfall Plot
#'
#' This basic waterfall plot visualizes a quantity `height` ordered by value with some markup.
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param height (`numeric``)\cr vector containing values to be plotted as the waterfall bars.
#' @param id (`character`)\cr vector containing IDs to use as the x-axis label for the waterfall bars.
#' @param col (`character`)\cr colors.
#' @param col_var (`factor`, `character` or `NULL`)\cr categorical variable for bar coloring. `NULL` by default.
#' @param xlab (`character`)\cr x label. Default is `"ID"`.
#' @param ylab (`character`)\cr y label. Default is `"Value"`.
#' @param title (`character`)\cr text to be displayed as plot title.
#' @param col_legend_title (`character`)\cr text to be displayed as legend title.
#'
#' @return A `ggplot` waterfall plot.
#'
#' @examples
#' library(dplyr)
#' library(nestcolor)
#'
#' g_waterfall(height = c(3, 5, -1), id = letters[1:3])
#'
#' g_waterfall(
#'   height = c(3, 5, -1),
#'   id = letters[1:3],
#'   col_var = letters[1:3]
#' )
#'
#' adsl_f <- tern_ex_adsl %>%
#'   select(USUBJID, STUDYID, ARM, ARMCD, SEX)
#'
#' adrs_f <- tern_ex_adrs %>%
#'   filter(PARAMCD == "OVRINV") %>%
#'   mutate(pchg = rnorm(n(), 10, 50))
#'
#' adrs_f <- head(adrs_f, 30)
#' adrs_f <- adrs_f[!duplicated(adrs_f$USUBJID), ]
#' head(adrs_f)
#'
#' g_waterfall(
#'   height = adrs_f$pchg,
#'   id = adrs_f$USUBJID,
#'   col_var = adrs_f$AVALC
#' )
#'
#' g_waterfall(
#'   height = adrs_f$pchg,
#'   id = paste("asdfdsfdsfsd", adrs_f$USUBJID),
#'   col_var = adrs_f$SEX
#' )
#'
#' g_waterfall(
#'   height = adrs_f$pchg,
#'   id = paste("asdfdsfdsfsd", adrs_f$USUBJID),
#'   xlab = "ID",
#'   ylab = "Percentage Change",
#'   title = "Waterfall plot"
#' )
#'
#' @export
g_waterfall <- function(height,
                        id,
                        col_var = NULL,
                        col = getOption("ggplot2.discrete.colour"),
                        xlab = NULL,
                        ylab = NULL,
                        col_legend_title = NULL,
                        title = NULL) {
  if (!is.null(col_var)) {
    check_same_n(height = height, id = id, col_var = col_var)
  } else {
    check_same_n(height = height, id = id)
  }

  checkmate::assert_multi_class(col_var, c("character", "factor"), null.ok = TRUE)
  checkmate::assert_character(col, null.ok = TRUE)

  xlabel <- deparse(substitute(id))
  ylabel <- deparse(substitute(height))

  col_label <- if (!missing(col_var)) {
    deparse(substitute(col_var))
  }

  xlab <- if (is.null(xlab)) xlabel else xlab
  ylab <- if (is.null(ylab)) ylabel else ylab
  col_legend_title <- if (is.null(col_legend_title)) col_label else col_legend_title

  plot_data <- data.frame(
    height = height,
    id = as.character(id),
    col_var = if (is.null(col_var)) "x" else to_n(col_var, length(height)),
    stringsAsFactors = FALSE
  )

  plot_data_ord <- plot_data[order(plot_data$height, decreasing = TRUE), ]

  p <- ggplot2::ggplot(plot_data_ord, ggplot2::aes(x = factor(id, levels = id), y = height)) +
    ggplot2::geom_col() +
    ggplot2::geom_text(
      label = format(plot_data_ord$height, digits = 2),
      vjust = ifelse(plot_data_ord$height >= 0, -0.5, 1.5)
    ) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 0, vjust = .5))

  if (!is.null(col_var)) {
    p <- p +
      ggplot2::aes(fill = col_var) +
      ggplot2::labs(fill = col_legend_title) +
      ggplot2::theme(
        legend.position = "bottom",
        legend.background = ggplot2::element_blank(),
        legend.title = ggplot2::element_text(face = "bold"),
        legend.box.background = ggplot2::element_rect(colour = "black")
      )
  }

  if (!is.null(col)) {
    p <- p +
      ggplot2::scale_fill_manual(values = col)
  }

  if (!is.null(title)) {
    p <- p +
      ggplot2::labs(title = title) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
  }

  p
}
