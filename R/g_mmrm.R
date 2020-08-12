# Functions for MMRM plots.

#' Plot the LS means of an MMRM model.
#'
#' This function summarizes adjusted \code{lsmeans} and standard error, as well as conducts
#' comparisons between groups' adjusted \code{lsmeans}, where the first level of the group
#' is the reference level.
#'
#' @param object the MMRM model result produced by \code{\link{s_mmrm}}.
#' @param select character vector to select one or both of "estimates" and "contrasts" plots.
#' @param titles character vector with elements \code{estimates} and \code{contrasts} containing the plot titles.
#' @param ylab string with the y axis label.
#' @param width width of the error bars.
#' @param show_pval should the p-values for the differences of LS means vs. control be displayed (not default)? Note
#'   that this is an experimental feature and will be further improved.
#'
#' @return a \code{ggplot2} plot
#'
#' @details If variable labels are available in the original data set, then these are used. Otherwise
#'   the variable names themselves are used for annotating the plot.
#'
#' @export
#' @import ggplot2
#'
#' @seealso \code{\link{t_mmrm_lsmeans}}
#'
#' @examples
#' library(dplyr)
#' library(random.cdisc.data)
#'
#' adsl <- radsl(cached = TRUE)
#' adqs <- radqs(cached = TRUE)
#' adqs_f <- adqs %>%
#'   dplyr::filter(PARAMCD=="FKSI-FWB" & !AVISIT %in% c("BASELINE")) %>%
#'   droplevels() %>%
#'   dplyr::mutate(ARM = factor(ARM, levels = c("B: Placebo", "A: Drug X", "C: Combination"))) %>%
#'   dplyr::mutate(AVISITN = rank(AVISITN) %>% as.factor() %>% as.numeric() %>% as.factor())
#' var_labels(adqs_f) <- var_labels(adqs)
#'
#' mmrm_results <- s_mmrm(
#'   vars = list(
#'     response = "AVAL",
#'     covariates = c("STRATA1", "BMRKR2"),
#'     id = "USUBJID",
#'     arm = "ARM",
#'     visit = "AVISIT"
#'   ),
#'   data = adqs_f,
#'   cor_struct = "random-quadratic",
#'   weights_emmeans = "equal"
#' )
#' g_mmrm_lsmeans(mmrm_results)
#' g_mmrm_lsmeans(mmrm_results, select = "estimates")
#' g_mmrm_lsmeans(
#'   mmrm_results,
#'   select = "contrasts",
#'   titles = c(contrasts = "Contrasts of FKSI-FWB means"),
#'   show_pval = TRUE,
#'   width = 0.8
#' )
g_mmrm_lsmeans <-
  function(object,
           select = c("estimates", "contrasts"),
           titles = c(
             estimates = paste("Adjusted mean of", object$labels$response, "by treatment at visits"),
             contrasts = paste0(
               "Differences of ", object$labels$response, " adjusted means vs. control ('", object$ref_level, "')"
             )
           ),
           ylab = paste0("Estimates with ", round(object$conf_level * 100), "% CIs"),
           width = 0.6,
           show_pval = TRUE) {

  stopifnot(is(object, "mmrm"))
  select <- match.arg(select, several.ok = TRUE)
  stopifnot(is.character(titles), all(select %in% names(titles)))
  stopifnot(is.character(ylab), identical(length(ylab), 1L))
  stopifnot(is.numeric(width), identical(length(width), 1L))
  stopifnot(is.logical(show_pval), identical(length(show_pval), 1L))

  # Get relevant subsets of the estimates and contrasts data frames.
  v <- object$vars
  estimates <- object$lsmeans$estimates[, c(v$arm, v$visit, "estimate", "lower_cl", "upper_cl")]
  estimates$p_value <- NA
  contrasts <- object$lsmeans$contrasts[, c(v$arm, v$visit, "estimate", "lower_cl", "upper_cl", "p_value")]
  contrasts[[v$arm]] <- factor(contrasts[[v$arm]], levels = levels(estimates[[v$arm]]))


  plot_data <- if (identical(select, "estimates")) {
    cbind(estimates, type = "estimates")
  } else if (identical(select, "contrasts")) {
    cbind(contrasts, type = "contrasts")
  } else {
    rbind(
      cbind(estimates, type = "estimates"),
      cbind(contrasts, type = "contrasts")
    )
  }

  pd <- position_dodge2(width, preserve = "total", padding = .2)
  result <- ggplot(
    plot_data,
    aes_string(
      x = v$visit,
      y = "estimate",
      colour = v$arm,
      group = v$arm,
      ymin = "lower_cl",
      ymax = "upper_cl"
    )
  ) +
    geom_errorbar(width = width, position = pd) +
    geom_point(position = pd) +
    expand_limits(x = 0) +
    scale_color_discrete(
      name = object$labels$arm,
      drop = FALSE  # To ensure same colors for only contrasts plot.
    ) +
    ylab(ylab) +
    xlab(object$labels$visit) +
    facet_wrap(
      ~ type,
      nrow = length(select),
      scales = "free_y",  # Since estimates and contrasts need to have different y scales.
      labeller = as_labeller(titles)
    )
  if ("contrasts" %in% select) {
    result <- result +
      geom_hline(
        data = data.frame(type = "contrasts", height = 0),
        aes_string(yintercept = "height"),
        colour = "black"
      )
    if (show_pval) {
      pval_data <- plot_data[plot_data$type == "contrasts", ]
      pval_data$y_pval <- ifelse(
        as.numeric(pval_data[[v$arm]]) %% 2,
        pval_data$upper_cl,
        pval_data$lower_cl
      )
      pval_data$vjust <- ifelse(
        as.numeric(pval_data[[v$arm]]) %% 2,
        -0.1,
        +1.1
      )
      pval_data$y_total <- pval_data$y_pval + pval_data$vjust
      pval_data$label <- ifelse(
        pval_data$p_value < 0.0001,
        "<0.0001",
        sprintf("%.4f", pval_data$p_value)
      )
      result <- result +
        geom_text(
          data = pval_data,
          mapping = aes_string(y = "y_pval", vjust = "vjust", label = "label"),
          position = pd,
          show.legend = FALSE
        ) +
        coord_cartesian(clip = "off")
    }
  }
  return(result)
}
