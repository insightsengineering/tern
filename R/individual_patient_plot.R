#' Individual Patient Plots
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Line plot(s) displaying trend in patients' parameter values over time is rendered.
#' Patients' individual baseline values can be added to the plot(s) as reference.
#' Depending on user preference, renders a single graphic or compiles a list of graphics
#' that show trends in individual's parameter values over time.
#'
#' @inheritParams argument_convention
#' @param xvar (`string`)\cr time point variable to be plotted on x-axis.
#' @param yvar (`string`)\cr continuous analysis variable to be plotted on y-axis.
#' @param xlab (`string`)\cr plot label for x-axis.
#' @param ylab (`string`)\cr plot label for y-axis.
#' @param id_var (`string`)\cr variable used as patient identifier.
#' @param title (`string`)\cr title for plot.
#' @param subtitle (`string`)\cr subtitle for plot.
#' @param add_baseline_hline (`flag`)\cr adds horizontal line at baseline y-value on
#' plot when TRUE.
#' @param yvar_baseline (`string`)\cr variable with baseline values only.
#' Ignored when `add_baseline_hline` is FALSE.
#' @param ggtheme (`theme`)\cr optional graphical theme function as provided
#' by `ggplot2` to control outlook of plot. Use `ggplot2::theme()` to tweak the display.
#' @param plotting_choices (`character`)\cr specifies options for displaying
#' plots. Must be one of "all_in_one", "split_by_max_obs", "separate_by_obs".
#' @param max_obs_per_plot (`count`)\cr Number of observations to be plotted on one
#' plot. Ignored when `plotting_choices` is not "separate_by_obs".
#' @param caption (`character` scalar) \cr optional caption below the plot.
#' @param col (`character`)\cr lines colors.
#' @return a `ggplot` object or a list of `ggplot` objects.
#'
#' @examples
#' library(scda)
#' library(dplyr)
#'
#' # Select a small sample of data to plot.
#' adlb <- synthetic_cdisc_data("latest")$adlb %>%
#'   filter(PARAMCD == "ALT", !(AVISIT %in% c("SCREENING", "BASELINE"))) %>%
#'   slice(1:36)
#'
#' plot_list <- g_ipp(
#'   df = adlb,
#'   xvar = "AVISIT",
#'   yvar = "AVAL",
#'   xlab = "Visit",
#'   ylab = "SGOT/ALT (U/L)",
#'   title = "Individual Patient Plots",
#'   add_baseline_hline = TRUE,
#'   plotting_choices = "split_by_max_obs",
#'   max_obs_per_plot = 5
#' )
#' plot_list
#'
#' @export
g_ipp <- function(df,
                  xvar,
                  yvar,
                  xlab,
                  ylab,
                  id_var = "USUBJID",
                  title = "Individual Patient Plots",
                  subtitle = "",
                  caption = NULL,
                  add_baseline_hline = FALSE,
                  yvar_baseline = "BASE",
                  ggtheme = h_set_nest_theme(10),
                  plotting_choices = c("all_in_one", "split_by_max_obs", "separate_by_obs"),
                  max_obs_per_plot = 4,
                  col = getOption("tern.color")) {
  checkmate::assert_count(max_obs_per_plot)
  checkmate::assert_subset(plotting_choices, c("all_in_one", "split_by_max_obs", "separate_by_obs"))
  checkmate::assert_character(col)

  plotting_choices <- match.arg(plotting_choices)

  if (plotting_choices == "all_in_one") {
    p <- h_g_ipp(
      df = df,
      xvar = xvar,
      yvar = yvar,
      xlab = xlab,
      ylab = ylab,
      id_var = id_var,
      title = title,
      subtitle = subtitle,
      caption = caption,
      add_baseline_hline = add_baseline_hline,
      yvar_baseline = yvar_baseline,
      ggtheme = ggtheme,
      col = col
    )

    return(p)
  } else if (plotting_choices == "split_by_max_obs") {
    id_vec <- unique(df[[id_var]])
    id_list <- split(
      id_vec,
      rep(1:ceiling(length(id_vec) / max_obs_per_plot),
        each = max_obs_per_plot,
        length.out = length(id_vec)
      )
    )

    df_list <- list()
    plot_list <- list()

    for (i in seq_along(id_list)) {
      df_list[[i]] <- df[df[[id_var]] %in% id_list[[i]], ]

      plots <- h_g_ipp(
        df = df_list[[i]],
        xvar = xvar,
        yvar = yvar,
        xlab = xlab,
        ylab = ylab,
        id_var = id_var,
        title = title,
        subtitle = subtitle,
        caption = caption,
        add_baseline_hline = add_baseline_hline,
        yvar_baseline = yvar_baseline,
        ggtheme = ggtheme,
        col = col
      )

      plot_list[[i]] <- plots
    }
    return(plot_list)
  } else {
    ind_df <- split(df, df[[id_var]])
    plot_list <- lapply(
      ind_df,
      function(x) {
        h_g_ipp(
          df = x,
          xvar = xvar,
          yvar = yvar,
          xlab = xlab,
          ylab = ylab,
          id_var = id_var,
          title = title,
          subtitle = subtitle,
          caption = caption,
          add_baseline_hline = add_baseline_hline,
          yvar_baseline = yvar_baseline,
          ggtheme = ggtheme,
          col = col
        )
      }
    )

    return(plot_list)
  }
}

#' Helper Function to Set a `ggplot` Graphical Theme
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Function that sets a `ggplot` graphical theme to control the outlook of a plot.
#'
#' @param font_size (`number`)\cr text font size.
#'
#' @export
h_set_nest_theme <- function(font_size) {
  ggplot2::theme(
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(colour = "grey", fill = NA, size = 1),
    legend.position = "bottom",
    legend.background = ggplot2::element_blank(),
    legend.box.background = ggplot2::element_rect(colour = "grey", fill = NA, size = 1),
    legend.direction = "horizontal",
    legend.title = ggplot2::element_text(face = "bold"),
    text = ggplot2::element_text(size = font_size),
    plot.caption = ggplot2::element_text(hjust = 0)
  )
}

#' Helper Function To Create Simple Line Plot over Time
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Function that generates a simple line plot displaying parameter trends over time.
#'
#' @inheritParams argument_convention
#' @inheritParams g_ipp
#'
#' @examples
#' library(scda)
#' library(dplyr)
#'
#' # Select a small sample of data to plot.
#' adlb <- synthetic_cdisc_data("latest")$adlb %>%
#'   filter(PARAMCD == "ALT", !(AVISIT %in% c("SCREENING", "BASELINE"))) %>%
#'   slice(1:36)
#'
#' p <- h_g_ipp(
#'   df = adlb,
#'   xvar = "AVISIT",
#'   yvar = "AVAL",
#'   xlab = "Visit",
#'   id_var = "USUBJID",
#'   ylab = "SGOT/ALT (U/L)",
#'   add_baseline_hline = TRUE
#' )
#' p
#'
#' @export
h_g_ipp <- function(df,
                    xvar,
                    yvar,
                    xlab,
                    ylab,
                    id_var,
                    title = "Individual Patient Plots",
                    subtitle = "",
                    caption = NULL,
                    add_baseline_hline = FALSE,
                    yvar_baseline = "BASE",
                    ggtheme = h_set_nest_theme(10),
                    col = getOption("tern.color")) {
  checkmate::assert_string(xvar)
  checkmate::assert_string(yvar)
  checkmate::assert_string(yvar_baseline)
  checkmate::assert_string(id_var)
  checkmate::assert_string(xlab)
  checkmate::assert_string(ylab)
  checkmate::assert_string(title)
  checkmate::assert_string(subtitle)
  checkmate::assert_subset(c(xvar, yvar, yvar_baseline, id_var), colnames(df))
  checkmate::assert_data_frame(df)
  checkmate::assert_flag(add_baseline_hline)
  checkmate::assert_character(col)

  p <- ggplot2::ggplot(
    data = df,
    mapping = ggplot2::aes_string(
      x = xvar,
      y = yvar,
      group = id_var,
      colour = id_var
    )
  ) +
    ggplot2::geom_line(size = 0.4) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      title = title,
      subtitle = subtitle,
      caption = caption
    ) +
    ggtheme

  if (add_baseline_hline) {
    baseline_df <- df[, c(id_var, yvar_baseline)]
    baseline_df <- unique(baseline_df)

    p <- p +
      ggplot2::geom_hline(
        data = baseline_df,
        mapping = ggplot2::aes_string(
          yintercept = yvar_baseline,
          colour = id_var
        ),
        linetype = "dotdash",
        size = 0.4
      ) +
      ggplot2::geom_text(
        data = baseline_df,
        mapping = ggplot2::aes_string(
          x = 1,
          y = yvar_baseline,
          label = id_var,
          colour = id_var
        ),
        nudge_y = 0.025 * (max(df[, yvar], na.rm = TRUE) - min(df[, yvar], na.rm = TRUE)),
        vjust = "right",
        size = 2
      ) +
      ggplot2::scale_color_manual(values = col)
  }
  p
}
