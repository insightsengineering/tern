#' Horizontal Waterfall Plot
#'
#' This basic waterfall plot visualizes a quantity \code{height} ordered by value with some
#' markup
#'
#' @param height  numeric vector to be plotted as the waterfall bars
#' @param id vector of IDs used as the x-axis label for the waterfall bars
#' @param col vector of a categorical variable for bar coloring
#' @param xlab x label. Default is \code{ID}.
#' @param ylab y label. Default is \code{Value}.
#' @param title A string to be displayed as plot title.
#' @param col_legend_title A string to be displayed as legend title.
#'
#' @template author_song24
#'
#' @importFrom ggplot2 ggplot aes theme geom_col geom_text labs xlab ylab
#'   element_text element_blank element_rect scale_fill_manual
#'
#' @export
#'
#' @examples
#' g_waterfall(height = c(3,5,-1), id = letters[1:3])
#'
#' g_waterfall(height = c(3,5,-1), id = letters[1:3], col = c("red", "green", "red"))
#'
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' ADSL <- cadsl
#' ADSL_f <- ADSL %>%
#'   select(USUBJID, STUDYID, ARM, ARMCD, SEX)
#'
#' ADRS <- cadrs
#' ADRS_f <- subset(ADRS, PARAMCD == "OVRINV") %>%
#'   mutate(pchg = rnorm(1200, 10, 50))
#'
#' ADRS_f <- head(ADRS_f, 30)
#' ADRS_f <- ADRS_f[!duplicated(ADRS_f$USUBJID),]
#' head(ADRS_f)
#'
#' g_waterfall(
#'   height = ADRS_f$pchg,
#'   id = ADRS_f$USUBJID,
#'   col = ADRS_f$AVALC
#' )
#'
#' g_waterfall(
#'   height = ADRS_f$pchg,
#'   id = ADRS_f$USUBJID,
#'   col = ADRS_f$AVALC
#' )
#'
#' g_waterfall(
#'   height = ADRS_f$pchg,
#'   id = paste("asdfdsfdsfsd",ADRS_f$USUBJID),
#'   col = ADRS_f$SEX
#' )
#'
#' g_waterfall(
#'   height = ADRS_f$pchg,
#'   id = paste("asdfdsfdsfsd",ADRS_f$USUBJID),
#'   xlab = "ID",
#'   ylab = "Percentage Change",
#'   title = "Waterfall plot"
#' )
g_waterfall <- function(height,
                        id,
                        col = NULL,
                        xlab = NULL,
                        ylab = NULL,
                        col_legend_title = NULL,
                        title = NULL) {

  if (!is.null(col)) {
    check_same_n(height = height, id = id, col = col)
  } else {
    check_same_n(height = height, id = id)
  }

  xlabel <- deparse(substitute(id))
  ylabel <- deparse(substitute(height))

  col_label <- if (!missing(col)) {
    deparse(substitute(col))
  }

  xlab <- if (is.null(xlab)) xlabel else xlab
  ylab <- if (is.null(ylab)) ylabel else ylab
  col_legend_title <- if (is.null(col_legend_title)) col_label else col_legend_title

  plot_data <- data.frame(
    height = height,
    id = as.character(id),
    col = if (is.null(col)) "x" else to_n(col, length(height)),
    stringsAsFactors = FALSE
  )

  plot_data_ord <- plot_data[order(plot_data$height, decreasing = TRUE), ]

  p <- ggplot(plot_data_ord, aes(x = factor(id, levels = id), y = height)) +
    geom_col() +
    geom_text(label = format(plot_data_ord$height, digits = 2),
              vjust = ifelse(plot_data_ord$height >= 0, -0.5, 1.5)) +
    xlab(xlab) +
    ylab(ylab) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = .5))

  if (!is.null(col)) {
    p <- p +
      aes(fill = col) +
      labs(fill = col_legend_title) +
      theme(
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.title = element_text(face = "bold"),
        legend.box.background = element_rect(colour = "black")
      )
  }

  if (!is.null(title)) {
    p <- p +
      labs(title = title) +
      theme(plot.title = element_text(face = "bold"))
  }

  p
}
