#' Time to Event Table
#'
#' The time to event table summarizes time to event data with different models
#' as described in the details section.
#'
#' @inheritParams argument_convention
#' @inheritParams s_coxph_pairwise
#' @param event_descr (\code{factor} vector) that partitions the the events into earliest contributing event. The
#'   variable name can be provided unquoted in which case it is looked up in \code{data} and then the calling
#'   environment.
#' @param time_points (\code{numeric} vector) with times displayed in the time point analysis, if \code{NULL} this
#'   section of the table will be omitted
#' @param time_unit (\code{character} value) with the unit of the \code{tte} argument
#' @param conf_level (\code{numeric} value or named \code{numeric} vector) either a single number or a named vector of
#'   confidence levels where the names are \code{survfit}, \code{coxph}, and \code{ztest}
#' @param conf_type_survfit (one of \code{"plain"} (default), \code{"none"}, \code{"log"},  \code{"log-log"}). Used in
#'   \code{conf.type} in \code{\link[survival]{survfit}}
#' @param probs_survfit (\code{numeric} vector of length two) to specify the quantiles in \code{\link[stats]{quantile}}
#' @param ties_coxph passed as argument \code{ties} to \code{\link{coxph}}
#' @param pval_method_coxph passed as argument \code{pval_method} to function \code{\link{s_coxph_pairwise}}
#'
#' @details
#' The time to event section is derived from a Kaplan-Meier estimator for the
#' \code{formula} argument with the strata special dropped.
#'
#' The stratified and unstratified analysis is evaluated pair-wise (reference to
#' comparison) and \code{\link[survival]{coxph}} is used to get the p-value, calculate the hazard ratio
#' and confidence interval.
#'
#' The time point analysis is based on the Kaplan-Meier estimator.
#'
#' @template return_rtable
#'
#' @importFrom stats terms quantile pchisq qnorm pnorm
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#' ADSL <- radsl(cached = TRUE)
#'
#' ADTTE <- radtte(cached = TRUE)
#' ADTTE_f <- subset(ADTTE, PARAMCD == "EFS" & BMEASIFL == "Y")
#'
#' t_tte(
#'   formula = Surv(AVAL, !CNSR) ~ arm(ARM),
#'   data = ADTTE_f,
#'   col_N = table(ADSL$ARM)
#' )
#'
#' t_tte(
#'   formula = Surv(AVAL, !CNSR) ~ arm(ARM),
#'   data = ADTTE_f,
#'   col_N = table(ADSL$ARM),
#'   event_descr = factor(ADTTE_f$EVNTDESC)
#' )
#'
#' t_tte(
#'   formula = Surv(AVAL, !CNSR) ~ arm(ARM),
#'   data = ADTTE_f,
#'   col_N = table(ADSL$ARM),
#'   event_descr = factor(ADTTE_f$EVNTDESC),
#'   time_points = c(6, 12, 360),
#'   time_unit = "month",
#' )
#'
#' t_tte(
#'   formula = Surv(AVAL, !CNSR) ~ arm(ARM),
#'   data = ADTTE_f,
#'   col_N = table(ADSL$ARM),
#'   event_descr = factor(ADTTE_f$EVNTDESC),
#'   time_points = c(6, 12, 360),
#'   conf_level = c(survfit = 0.91, coxph = 0.95, ztest = 0.99)
#' )
#'
#' t_tte(
#'   formula = Surv(AVAL, !CNSR) ~ arm(ARM) + strata(SEX),
#'   data = ADTTE_f,
#'   col_N = table(ADSL$ARM),
#'   event_descr = factor(ADTTE_f$EVNTDESC),
#'   time_points = c(6, 12, 24),
#'   time_unit = "month",
#'   pval_method_coxph = "wald",
#'   conf_type_survfit = "log-log",
#'   ties_coxph = "exact"
#' )
#'
#' # table_tree = TRUE
#' tbl3 <- t_tte(
#'   formula = Surv(AVAL, !CNSR) ~ arm(ARM) + strata(SEX),
#'   data = ADTTE_f,
#'   col_N = table(ADSL$ARM),
#'   event_descr = factor(ADTTE_f$EVNTDESC),
#'   time_points = c(6, 2000),
#'   time_unit = "month",
#'   table_tree = TRUE
#' )
#' summary(tbl3)
#'
#' to_rtable(tbl3)
t_tte <- function(formula,
                  data,
                  col_N, # nolint
                  event_descr = NULL,
                  time_points = NULL,
                  time_unit = "month",
                  conf_level = 0.95,
                  conf_type_survfit = c("plain", "none", "log", "log-log"),
                  probs_survfit = c(0.25, 0.75),
                  pval_method_coxph =  c("log-rank", "wald",  "likelihood"),
                  ties_coxph = c("efron", "breslow", "exact"),
                  table_tree = FALSE) {

  cl <- match.call()

  conf_type_survfit <- match.arg(conf_type_survfit)
  pval_method_coxph <- match.arg(pval_method_coxph)
  ties_coxph <- match.arg(ties_coxph)
  stopifnot(is.data.frame(data), length(probs_survfit) == 2)
  conf_level <- check_conf_level(conf_level, c("survfit", "coxph", "ztest"))
  check_numeric_range(probs_survfit, 0, 1)


  # extracted data
  tm <- t_tte_items(formula, cl, data, parent.frame())

  tte <- tm$tte
  is_event <- as.logical(tm$event)
  arm <- tm$arm

  # Argument Checking

  if (length(tte) != nrow(data)) {
    stop("some of the following variable contain missing values:\n   ",
         sub("^list", "", deparse(attr(terms(formula), "variables"))),
         "\nmissing data in for the survival analysis is currently disabled")
  }

  check_same_n(is_event = is_event, event_descr = event_descr, arm = arm)
  col_N <- col_N %||% table(arm) #nolint
  check_col_by_factor(tte, arm, col_N, 2)
  if (!is.null(event_descr) && !is.factor(event_descr)) {
    stop("event_descr is required to be a factor")
  }
  if (!is.null(time_points) && !is.numeric(time_points)) {
    stop("time_points is required to be numeric")
  }

  # Calculate elements of the table

  header <- rheader(rrowl("", levels(arm)))

  # Event Table
  # ###########

  tbl_event <- node(
    invisible_node_name("Patient events"),
    content = NULL,
    children = list(
      node(
        invisible_node_name("Patients with event"),
        rtabulate(
          is_event,
          arm,
          positives_and_proportion,
          format = "xx.xx (xx.xx%)",
          row.name = "Patients with event (%)"
        ),
        children = compact(list(
          if (!is.null(event_descr)) {
            tbl <- rtabulate(droplevels(factor(event_descr)[is_event]), arm[is_event], length)
            header(tbl) <- header
            node(
              "Earliest Contributing Event",
              tbl
            )
          } else {
            NULL
          }
        )),
        format_data = list(gap_to_children = 0, children_indent = 1)
      ),
      node(
        invisible_node_name("Patients without events"),
        rtabulate(!is_event, arm, positives_and_proportion, format = "xx.xx (xx.xx%)",
                  row.name = "Patients without event (%)")
      )
    ),
    format_data = list(children_gap = 0)
  )

  # Time to Event
  # #############
  f <- tm$formula_nostrata
  environment(f) <- environment()

  surv_km_fit <- survfit(
    formula = f,
    data = data,
    conf.int = conf_level["survfit"],
    conf.type = conf_type_survfit
  )

  srv_tbl <- summary(surv_km_fit)$table
  med <- as.list(srv_tbl[, "median"])
  ci <- Map(
    function(x, y) c(x, y),
    srv_tbl[, paste0(conf_level["survfit"], "LCL")],
    srv_tbl[, paste0(conf_level["survfit"], "UCL")]
  )

  srv_qt_tbl <- quantile(surv_km_fit, probs = probs_survfit)$quantile

  qnt <- Map(function(x, y) c(x, y), srv_qt_tbl[, 1], srv_qt_tbl[, 2])

  rng_c <- lapply(split(data.frame(tte, is_event), arm), function(x) {
    range(x$tte[!x$is_event], na.rm = TRUE)
  })
  rng_e <- lapply(split(data.frame(tte, is_event), arm), function(x) {
    range(x$tte[x$is_event], na.rm = TRUE)
  })

  tbl_tte <- node(
    paste0("Time to Event (", time_unit, "s)"),
    rtable(
      header = header,
      rrowl("Median", med, format = "xx.xx"),
      rrowl(paste0(conf_level["survfit"] * 100, "% CI"), ci, indent = 1, format = "(xx.x, xx.x)"),
      rrowl(paste0(probs_survfit[1] * 100, "% and ", probs_survfit[2] * 100, "%-ile"), qnt, format = "xx.x, xx.x"),
      rrowl("Range (censored)", rng_c, format = "xx.x to xx.x"),
      rrowl("Range (event)", rng_e, format = "xx.x to xx.x")
    )
  )

  # Unstratified (and Stratified) Analysis
  # #####################


  coxph_values <- s_coxph_pairwise(formula, data, conf_level["coxph"], pval_method_coxph, ties = ties_coxph)

  # this function is reused for stratified analysis
  pval_label <- paste0("p-value (", capitalize(pval_method_coxph), ")")

  tbl_coxph <- lapply(c("unstratified", "stratified"), function(sel_strat) {
    if (is.null(tm$formula_strata) && sel_strat == "stratified") {
      NULL
    } else {
      # first column is empty
      pval <- start_with_null(
        lapply(coxph_values[-1], function(x) x[[sel_strat]]$pvalue)
      )
      hr <- start_with_null(
        lapply(coxph_values[-1], function(x) x[[sel_strat]]$hr)
      )
      hr_ci <- start_with_null(
        lapply(coxph_values[-1], function(x) x[[sel_strat]]$hr_ci)
      )
      coxph_node <-  node(
        paste0(capitalize(sel_strat), " Analysis"),
        content = NULL,
        children = list(
          node(
            invisible_node_name("p-value"),
            rtable(
              header = header,
              rrowl(pval_label, pval, format = "xx.xxxx")
            )
          ),
          node(
            invisible_node_name("Hazard Ratio"),
            rtable(
              header = header,
              rrowl("Hazard Ratio", hr, format = "xx.xxxx"),
              rrowl(paste0(conf_level["coxph"] * 100, "% CI"), hr_ci, indent = 1, format = "(xx.xxxx, xx.xxxx)")
            )
          )
        )
      )
      coxph_node
    }
  })


  # Time Point Analysis
  # ###################

  tbl_timepoints <- if (is.null(time_points)) {
    NULL
  } else {

    tp <- try(summary(surv_km_fit, times = time_points), silent = TRUE)

    tp_rtables <- if (is(tp, "try-error")) {
      stop("Not yet implemented")
      list(
        rtable(
          header = header,
          rrow(paste("time points: ", paste(time_points, collapse = ", ")), indent = 1),
          rrow("-- no data", indent = 2)
        )
      )

    } else {

      df_tp <- as.data.frame(tp[c("time", "n.risk", "surv", "lower", "upper", "strata", "std.err")])
      s_df_tp <- split(df_tp, factor(df_tp$time, levels = time_points), drop = FALSE)

      Map(function(dfi, time_point) {
        tbl <- if (nrow(dfi) <= 1) {
          rtable(
            header = header,
            rrow(if (nrow(dfi) == 0) "-- no data" else "-- not enough data", indent = 2)
          )
        } else {

          if (!all(dfi$time == time_point)) {
            stop("time points do not match")
          }

          # z-test
          d <- dfi$surv[-1] - dfi$surv[1]
          sd <- sqrt(dfi$std.err[-1]^2 + dfi$std.err[1]^2)
          qs <- qnorm(conf_level["ztest"] + c(1, -1) * (1 - conf_level["ztest"]) / 2)
          l_ci <- Map(function(di, si) di + qs * si, d, sd)

          pval <- 2 * (1 - pnorm(abs(d) / sd))

          rtable(
            header = header,
            rrowl("Patients remaining at risk", dfi$n.risk, format = "xx", indent = 2),
            rrowl("Event Free Rate (%)", dfi$surv, format = "xx.xx%", indent = 2),
            rrowl(
              paste0(conf_level["survfit"] * 100, "% CI"),  as.data.frame(t(dfi[c("lower", "upper")] * 100)),
              format = "(xx.xx, xx.xx)", indent = 3
            ),
            rrowl("Difference in Event Free Rate", c(list(NULL), as.list(d * 100)), format = "xx.xx", indent = 2),
            rrowl(
              paste0(conf_level["ztest"] * 100, "% CI"), c(list(NULL), lapply(l_ci, function(x) 100 * x)),
              format = "(xx.xx, xx.xx)", indent = 3
            ),
            rrowl("p-value (Z-test)", c(list(NULL), as.list(pval)), format = "xx.xxxx", indent = 2)
          )
        }
        node(
          paste(time_point, if (time_point == 1) time_unit else paste0(time_unit, "s")),
          tbl
        )
      }, s_df_tp, time_points)
    }

    node(
      "Time Point Analysis",
      content = NULL,
      children = tp_rtables
    )
  }

  ## Now Stack Tables together
  tree <- invisible_node(
    children = compact(c(
      tbl_event,
      tbl_tte,
      tbl_coxph[[1]],
      tbl_coxph[[2]],
      tbl_timepoints
    ))
  )
  tree <- rapply_tree(tree, function(name, content, ...) {
    if (is_non_empty_rtable(content)) {
      content <- header_add_N(content, col_N)
      list(name = name, content = content)
    } else {
      list(name = name, content = content)
    }
  })

  if (table_tree) {
    tree
  } else {
    to_rtable(tree)
  }
}

t_tte_items <- function(formula, cl, data, env) {
  # extract information
  mf <- cl
  mt <- terms(formula, specials = c("arm", "strata", "cluster", "tt"), data = data)
  if (!all(all.vars(attr(mt, "variables")) %in% names(data))) {
    stop("All formula variables must appear in 'data'")
  }

  irsp <- attr(mt, "response")
  istr <- attr(mt, "specials")$strata
  iarm <- attr(mt, "specials")$arm

  if (is.null(irsp) | is.null(iarm)) {
    stop("formula must include a response and arm")
  }

  if (is.null(istr)) {
    uf <- formula
    f <- NULL
  } else {
    uf <- drop_special(mt, "strata")
    f <- formula
  }

  m <- match(c("formula", "data", "weights"), names(mf), 0L)

  mf <- mf[c(1L, m)]
  mf[[1L]] <- quote(stats::model.frame)
  mf$na.action <- quote(stats::na.omit) # nolint

  mf <- eval(mf, env)

  if (!inherits(mf[, irsp], "Surv")) {
    stop("Response is not a 'Surv' object")
  }
  if (attr(mf[, irsp], "type") != "right") {
    stop("Response is not a right-censored 'Surv' object")
  }

  list(
    tte = mf[, irsp][, "time"],
    event = mf[, irsp][, "status"],
    arm = mf[, iarm],
    formula_strata = f,
    formula_nostrata = uf,
    model_frame  = mf
  )

}
