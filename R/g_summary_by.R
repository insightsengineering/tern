#' Change From Baseline Plot
#'
#' Change from baseline is plotted.
#'
#' @param x x coordinates
#' @param y y coordinates
#' @param group factor with group information
#' @param y_lower lower bound of error bar, if \code{NULL} no error bar is displayed
#' @param y_upper upper bound of error bar, if \code{NULL} no error bar is displayed
#' @param n_visit for each element of \code{x}
#' @param y_refline draw horizontal reference lines
#' @param y_range y limits
#' @param xlab xlabel
#' @param ylab ylabel
#' @param nlab n_visit label
#' @param title title
#' @param fontsize font size of annotation text
#' @param draw boolean should plot be drawn
#' @param newpage boolean should plot be drawn on newpage
#'
#' @import ggplot2
#' @importFrom gridExtra grid.arrange
#'
#' @template author_liaoc10
#'
#' @noRd
#'
#' @examples
#'
#' ANL <- expand.grid(
#'   VISIT = paste0("visit ", 1:10),
#'   ARM = c("ARM A", "ARM B")
#' )
#' ANL$ARM <- factor(ANL$ARM)
#' ANL$VISIT <- factor(ANL$VISIT)
#' ANL$MEAN <- rnorm(nrow(ANL), 10, 2)
#' ANL$SD <- rnorm(nrow(ANL), 0, 1)
#' ANL$MEDIAN <- sample(c(1:20), nrow(ANL), replace = TRUE)
#' ANL$Q1 <- ANL$MEDIAN - 2.5
#' ANL$Q3 <- ANL$MEDIAN + 2.5
#' ANL$MIN <- ANL$MEDIAN - 5
#' ANL$MAX <- ANL$MEDIAN + 5
#' ANL$NVIS <- sample(c(50:100), nrow(ANL), replace = TRUE)
#'
#' g_summary_by(
#'  x = ANL$VISIT, y = ANL$MEDIAN, group = ANL$ARM,
#'  y_lower = ANL$Q1, y_upper = ANL$Q3, n_visit = ANL$NVIS,
#'  xlab = "Visit", ylab = "Mean Result", nlab = "Number of subject at each visit",
#'  title = "Plot of mean and IQR"
#' )
g_summary_by <- function(x, # nousage # nolint
                         y,
                         group,
                         y_lower,
                         y_upper,
                         n_visit,
                         y_refline,
                         y_range,
                         xlab,
                         ylab,
                         nlab,
                         title,
                         fontsize = 16,
                         draw = TRUE,
                         newpage = TRUE) {

  # Argument check
  check_same_n(
    x = x,
    y = y,
    group = group,
    y_lower = y_lower,
    y_upper = y_upper,
    n_visit = n_visit,
    omit_null = FALSE
  )
  check_col_by_factor(x, group, table(group), 1)

  check_is_numeric(y)
  # allow y_lower y_upper n_visit y_refline y_range to be NULL?
  # x can be either factor or numeric?

  # Data for plotting
  plotdat <- data.frame(x, y, group, y_lower, y_upper, n_visit)

  # move second group .2 to the left and right
  pd <- position_dodge(0.2)

  # make change from baseline plot
  p <- ggplot(plotdat, aes(x = x, y = y, group = group, color = group)) +
    geom_line(position = pd, size = 1) +
    geom_point(position = pd) +
    geom_errorbar(data = plotdat, aes(ymin = y_lower, ymax = y_upper), position = pd) +
    theme_bw() +
    labs(x = xlab, y = ylab, title = title) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      legend.background = element_rect(fill = "grey95"),
      legend.direction = "horizontal",
      legend.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 90, vjust = 0.5),
      text = element_text(size = fontsize)
    )

  # display count at each visit table as separate plot
  t <- ggplot(
    plotdat,
    aes(
      x = x,
      y = factor(group, levels = rev(levels(group))),
      label = n_visit,
      color = group
    )
  ) +
    geom_text(aes(angle = 0), size = fontsize * 0.3) + theme_bw() +
    labs(subtitle = nlab)

  t1 <- t + theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.text.y = element_text(color = rev(unique(ggplot_build(t)$data[[1]]$colour)), face = "bold"),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid = element_blank(),
    text = element_text(size = fontsize))

  # wrap plot and table into grobs, and align left margins
  glist <- lapply(list(plot = p, text = t1), ggplotGrob)
  leftmar <- do.call(unit.pmax, lapply(glist, "[[", "widths"))
  glist_aligned <- lapply(glist, function(x) {
    x$widths <- leftmar
    x
  })

  # Plot the two grobs using grid.arrange
  if (newpage) {
    grid.newpage()
  }
  do.call(grid.arrange, c(glist_aligned,
                          list(ncol = 1),
                          list(heights = c(8, length(unique(plotdat$group))))))

}
