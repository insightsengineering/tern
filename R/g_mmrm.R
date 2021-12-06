#' MMRM Plots
#'
#' This summarizes available plotting functions for MMRM.
#'
#' @seealso [g_mmrm_diagnostic], [g_mmrm_lsmeans]
#' @name g_mmrm
NULL

#' Diagnostic Plots for MMRM
#'
#' This function produces diagnostic plots.
#'
#' @param object (`mmrm`)\cr model result produced by \code{\link{fit_mmrm}}.
#' @param type (`string`)\cr specifying the type of diagnostic plot to be produced:
#'   \describe{
#'     \item{fit-residual}{A fitted vs residuals plot, grouped by visits. This allows to see if there is remaining
#'       structure in the residuals that might be captured by adding additional covariates to the model.}
#'     \item{q-q-residual}{A Q-Q plot for the residuals (i.e. sorted standardized residuals vs. normal quantiles),
#'       grouped by visits. Observations with an absolute standardized residual above \code{z_threshold} will be
#'       labeled.}
#'   }
#' @param z_threshold (`numeric`)\cr optional number indicating the normal quantile threshold for the Q-Q plot.
#'
#' @return a \code{ggplot2} plot
#'
#' @details Here we use marginal fitted values and residuals. That is, only the fixed effects are used
#'   to estimate fitted values, and the difference of those fitted values vs. the observed data are
#'   the residuals. This is in line with the MMRM philosophy where random effects are only used to
#'   model the marginal residual distribution.
#'
#' @export
#'
#' @seealso \code{\link{g_mmrm_lsmeans}} for plotting the LS means and contrasts.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(scda)
#'
#' adsl <- synthetic_cdisc_data("latest")$adsl
#' adqs <- synthetic_cdisc_data("latest")$adqs
#' adqs_f <- adqs %>%
#'   filter(PARAMCD=="FKSI-FWB" & !AVISIT %in% c("BASELINE")) %>%
#'   droplevels() %>%
#'   mutate(ARM = factor(ARM, levels = c("B: Placebo", "A: Drug X", "C: Combination"))) %>%
#'   mutate(AVISITN = rank(AVISITN) %>% as.factor() %>% as.numeric() %>% as.factor())
#'
#' mmrm_results <- fit_mmrm(
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
#' g_mmrm_diagnostic(mmrm_results)
#' g_mmrm_diagnostic(mmrm_results, type = "q-q-residual")
#'}
g_mmrm_diagnostic <- function(
  object,
  type = c("fit-residual", "q-q-residual"),
  z_threshold = NULL
) {
  stopifnot(methods::is(object, "mmrm"))
  type <- match.arg(type)
  stopifnot(is.null(z_threshold) || (is.numeric(z_threshold) && z_threshold > 0))

  model <- object$fit
  vars <- object$vars
  amended_data <- object$fit@frame
  amended_data$.fitted <- stats::predict(
    model,
    re.form = NA  # Don't include random effects. We want marginal fitted values.
  )
  amended_data$.resid <- amended_data[[vars$response]] - amended_data$.fitted

  result <- if (type == "fit-residual") {
    amended_data_smooth <- suppressWarnings(tryCatch({
      get_smooths(amended_data, x = ".fitted", y = ".resid", groups = vars$visit, level = 0.95)
    }, error = function(msg) {
      message(
        paste(
          "Warning: Data not amenable to the Locally Weighted Scatterplot Smoothing.",
          "\nSmooth line will not be displayed in the fit-residual plot."
        )
      )
    }
    )
  )
    tmp <- ggplot2::ggplot(amended_data, aes_string(x = ".fitted", y = ".resid")) +
      ggplot2::geom_point(colour = "blue", alpha = 0.3) +
      ggplot2::facet_grid(stats::as.formula(paste(". ~", vars$visit)), scales = "free_x") +
      ggplot2::geom_hline(yintercept = 0)
    if (!is.null(amended_data_smooth)) {
      tmp <- tmp + ggplot2::geom_line(
        data = amended_data_smooth,
        ggplot2::aes_string(x = "x", y = "y", group = vars$visit),
        color = "red",
        size = 1.4
      ) +
        ggplot2::geom_ribbon(
        data = amended_data_smooth,
        ggplot2::aes_string(
          x = "x",
          y = NULL,
          ymin = "ylow",
          ymax = "yhigh",
          group = vars$visit
        ),
        alpha = 0.4,
        color = "light grey"
      )
    }
    tmp <- tmp +
      xlab("Fitted values") +
      ylab("Residuals")

  } else if (type == "q-q-residual") {
    # We use visit specific standard deviation of marginal residuals for scaling residuals.
    all_sigmas <- sqrt(diag(object$cov_estimate))
    amended_data$.sigma <- all_sigmas[amended_data[[vars$visit]]]
    amended_data$.scaled_resid <- amended_data$.resid / amended_data$.sigma

    # For each visit, calculate x and y coordinates of the specific Q-Q-plot.
    plot_data <- split(amended_data, amended_data[[vars$visit]]) %>%
      lapply(function(data) {
        res <- data.frame(
          x = stats::qnorm(stats::ppoints(data$.scaled_resid)),
          y = sort(data$.scaled_resid)
        )
        res[[vars$visit]] <- data[[vars$visit]]  # Note that these are all the same.
        res
      }) %>%
      do.call(rbind, .)
    tmp <- ggplot2::ggplot(plot_data, aes_string(x = "x", y = "y")) +
      ggplot2::geom_point(colour = "blue", alpha = 0.3) +
      ggplot2::xlab("Standard normal quantiles") +
      ggplot2::ylab("Standardized residuals") +
      ggplot2::geom_abline(intercept = 0, slope = 1) +
      ggplot2::facet_grid(stats::as.formula(paste(". ~", vars$visit)))
    if (!is.null(z_threshold)) {
      label_data <- plot_data
      label_data$label <- ifelse(
        abs(plot_data$y) > z_threshold,
        as.character(amended_data[[vars$id]]),
        ""
      )
      tmp <- tmp +
        ggplot2::geom_text(
          ggplot2::aes_string(x = "x", y = "y", label = "label"),
          data = label_data,
          hjust = "inward",
          size = 2
        ) +
        ggplot2::coord_cartesian(clip = "off")
    }
    tmp
  }
  return(result)
}

#' Plot LS means for MMRM
#'
#' This function summarizes adjusted \code{lsmeans} and standard error, as well as conducts
#' comparisons between groups' adjusted \code{lsmeans}, where the first level of the group
#' is the reference level.
#'
#' @param object (`mmrm`)\cr model result produced by \code{\link{fit_mmrm}}.
#' @param select (`character`)\cr to select one or both of "estimates" and "contrasts" plots.
#' Note "contrasts" option is not applicable to model summaries excluding an arm variable.
#' @param titles (`character`)\cr with elements \code{estimates} and \code{contrasts} containing the plot titles.
#' @param ylab (`string`)\cr with the y axis label.
#' @param width (`numeric`)\cr width of the error bars.
#' @param show_pval (`flag`)\cr should the p-values for the differences of
#' LS means vs. control be displayed (not default)?
#'
#' @return a \code{ggplot2} plot
#'
#' @details If variable labels are available in the original data set, then these are used. Otherwise
#'   the variable names themselves are used for annotating the plot.
#' @details Contrast plot is not going to be returned if treatment is not
#' considered in the `mmrm` object considered in `object` argument,
#' no matter if `select` argument contains `contrasts` value.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(scda)
#' library(rtables)
#'
#' adsl <- synthetic_cdisc_data("latest")$adsl
#' adqs <- synthetic_cdisc_data("latest")$adqs
#' adqs_f <- adqs %>%
#'   filter(PARAMCD=="FKSI-FWB" & !AVISIT %in% c("BASELINE")) %>%
#'   droplevels() %>%
#'   mutate(ARM = factor(ARM, levels = c("B: Placebo", "A: Drug X", "C: Combination"))) %>%
#'   mutate(AVISITN = rank(AVISITN) %>% as.factor() %>% as.numeric() %>% as.factor())
#' var_labels(adqs_f) <- var_labels(adqs)
#'
#' mmrm_results <- fit_mmrm(
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
#'
#'adqs_f2 <- adqs_f %>%
#'  filter(ARMCD == "ARM A")
#'
#'mmrm_results_no_arm <- fit_mmrm(
#'   vars = list(
#'     response = "AVAL",
#'     covariates = c("STRATA1", "BMRKR2"),
#'     id = "USUBJID",
#'     visit = "AVISIT"
#'   ),
#'   data = adqs_f2,
#'   cor_struct = "random-quadratic",
#'   weights_emmeans = "equal"
#' )
#'
#' g_mmrm_lsmeans(mmrm_results_no_arm, select = "estimates")
#' g_mmrm_lsmeans(
#'   mmrm_results_no_arm,
#'   select = c("estimates", "contrasts"),
#'   titles = c(estimates = "Adjusted mean of FKSI-FWB",
#'   contrasts = "it will not be created"),
#'   show_pval = TRUE,
#'   width = 0.8
#' )
#'
#' g_mmrm_lsmeans(
#'   mmrm_results_no_arm,
#'   select = c("estimates"),
#'   titles = c(estimates = "Adjusted mean of FKSI-FWB"),
#'   show_pval = TRUE,
#'   width = 0.8
#' )
#'}
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

    stopifnot(methods::is(object, "mmrm"))
    select <- match.arg(select, several.ok = TRUE)
    if (is.null(object$vars$arm)) {
      select <- "estimates"
      arms <- FALSE
      if (identical(names(titles), "contrasts"))
      names(titles) <- "estimates"
      titles <- titles
    } else {
      arms <- TRUE
    }
    stopifnot(is.character(titles), all(select %in% names(titles)))
    stopifnot(is.character(ylab), identical(length(ylab), 1L))
    stopifnot(is.numeric(width), identical(length(width), 1L))
    stopifnot(is.logical(show_pval), identical(length(show_pval), 1L))
    if (isFALSE(arms)) {
      stopifnot(sum(duplicated(object$lsmeans$estimates$AVISIT)) == 0L)
    }

    # Get relevant subsets of the estimates and contrasts data frames.
    v <- object$vars
    if (arms) {
      estimates <- object$lsmeans$estimates[, c(v$arm, v$visit, "estimate", "lower_cl", "upper_cl")]
      contrasts <- object$lsmeans$contrasts[, c(v$arm, v$visit, "estimate", "lower_cl", "upper_cl", "p_value")]
      contrasts[[v$arm]] <- factor(contrasts[[v$arm]], levels = levels(estimates[[v$arm]]))
    } else {
      estimates <- object$lsmeans$estimates[, c(v$visit, "estimate", "lower_cl", "upper_cl")]
    }
    estimates$p_value <- NA

    plot_data <- if (identical(select, "estimates") || isFALSE(arms)) {
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

    result <- ggplot2::ggplot(
      plot_data,
      aes_string(
        x = v$visit,
        y = "estimate",
        colour = if (arms) v$arm else NULL,
        group = if (arms) v$arm else NULL,
        ymin = "lower_cl",
        ymax = "upper_cl"
      )
    ) +
      ggplot2::geom_errorbar(width = width, position = pd) +
      ggplot2::geom_point(position = pd) +
      ggplot2::expand_limits(x = 0) +
      ggplot2::scale_color_discrete(
        name = if (arms) object$labels$arm else NULL,
        drop = FALSE  # To ensure same colors for only contrasts plot.
      ) +
      ggplot2::ylab(ylab) +
      ggplot2::xlab(object$labels$visit) +
      ggplot2::facet_wrap(
        ~ type,
        nrow = length(select),
        scales = "free_y",  # Since estimates and contrasts need to have different y scales.
        labeller = as_labeller(titles)
      )
    if ("contrasts" %in% select) {
      result <- result +
        ggplot2::geom_hline(
          data = data.frame(type = "contrasts", height = 0),
          ggplot2::aes_string(yintercept = "height"),
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
          ggplot2::geom_text(
            data = pval_data,
            mapping = aes_string(y = "y_pval", vjust = "vjust", label = "label"),
            position = pd,
            show.legend = FALSE
          ) +
          ggplot2::coord_cartesian(clip = "off")
      }
    }
    return(result)
  }
