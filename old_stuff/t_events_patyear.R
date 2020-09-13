#' event rate adjusted per person-years at Risk
#'
#' Table for event rate adjusted for Patient-Years
#'
#' @inheritParams s_events_patyear
#' @param event_type  named (\code{character} value) to specify the type of event that is summarized
#' @export
#' @examples
#' library(dplyr)
#' library(random.cdisc.data)
#' ADSL <- radsl(cached = TRUE)
#' ADAETTE <- radaette(cached = TRUE)
#' ANL <- ADAETTE %>%
#'    dplyr::filter(PARAM == "Time to first occurrence of any adverse event")
#' tbl1 <- t_events_patyear(
#'    events = !(ANL$CNSR),
#'    time_in_years = ANL$AVAL,
#'    col_by = ANL$ARM,
#'    col_N = table(ADSL$ARM),
#'    conf_level = 0.95,
#'    conf_method = "normal",
#'    lambda = 100
#' )
#'
#' ANL <- ANL %>% dplyr::filter(SEX %in% c("F", "M"))
#' tbl2 <- t_events_patyear(
#'    events = !(ANL$CNSR),
#'    time_in_years = ANL$AVAL,
#'    col_by = ANL$ARM,
#'    col_N = NULL,
#'    total = "Total Patients",
#'    conf_level = 0.9,
#'    conf_method = "byar",
#'    lambda = 1000,
#'    event_type = c("AE" = "adverse event")
#' )
#'
#'
t_events_patyear <- function(events,
                             time_in_years,
                             col_by,
                             col_N = NULL, # nolint
                             total = NULL,
                             conf_level = 0.95,
                             conf_method = c("normal", "exact", "byar"),
                             lambda = 100,
                             event_type = "events") {

  if (is.null(names(event_type))) names(event_type) <- event_type

  s_results <- s_events_patyear(
    events = events,
    time_in_years = time_in_years,
    col_by = col_by,
    col_N = col_N,
    total = total,
    conf_level = conf_level,
    conf_method = conf_method,
    lambda = lambda
  )

  events_by_col <- s_results$events_by_col
  times_by_col <- s_results$times_by_col
  rate_by_col <- s_results$rate_by_col
  ci_by_col <- s_results$ci_by_col
  col_N <- s_results$col_N  #nolintr


  tbl <- rtable(
    header = rheader(rrowl(NULL, names(events_by_col))),
    rrowl(
      "Total patient-years at risk", times_by_col, format = "xx.x"
    ),
    rrowl(
      paste0("Number of ", event_type, " observed"), events_by_col
      ),
    rrowl(
      paste0(names(event_type), " rate per ", lambda, " patient-years"),
      rate_by_col, format = "xx.xx"
    ),
    rrowl(
      paste0(conf_level * 100, "% CI"), ci_by_col, format = "(xx.xx, xx.xx)"
    )
  )

  header_add_N(tbl, col_N) #nolintr
}

#' event rate adjusted per person-years at Risk
#'
#' Table for event rate adjusted for Patient-Years
#'
#' @param events (\code{logical} vector) \code{TRUE} if record is an event
#' @param time_in_years (\code{numeric} vector) time at risk in year
#' @inheritParams argument_convention
#' @param lambda a numeric rate factor, e.g. 100 for 100 person-years
#' @inheritParams ci_patyear
#'
#' @export
#' @examples
#' library(dplyr)
#' library(random.cdisc.data)
#' ADSL <- radsl(cached = TRUE)
#' ADAETTE <- radaette(cached = TRUE)
#' ANL <- ADAETTE %>%
#'    dplyr::filter(PARAM == "Time to first occurrence of any adverse event")
#' s_patyear <- s_events_patyear(
#'    events = !(ANL$CNSR),
#'    time_in_years = ANL$AVAL,
#'    col_by = ANL$ARM,
#'    col_N = table(ADSL$ARM),
#'    total = "All Patients",
#'    conf_level = 0.95,
#'    conf_method = "normal",
#'    lambda = 100
#' )
#'
#' s_patyear$events_by_col
#'

s_events_patyear <- function(events,
                             time_in_years,
                             col_by,
                             col_N = NULL, # nolint
                             total = NULL,
                             conf_level = 0.95,
                             conf_method = c("normal", "exact", "byar"),
                             lambda = 100) {
  stopifnot(all(!is.na(events)))
  stopifnot(is_logical_vector(events))
  stopifnot(is_numeric_vector(time_in_years))
  stopifnot(is.null(total) || is_character_single(total))
  col_by <- col_by_to_matrix(col_by, events)
  col_N <- col_N %||% get_N(col_by) #nolintr
  if (!is.null(total)) {
    col_by <- by_add_total(col_by, label = total)
    col_N <- col_N_add_total(col_N) #nolintr
    total <- NULL
  }
  check_col_by(events, col_by, col_N, min_num_levels = 1) #nolintr

  conf_method <- match.arg(conf_method)

  events_by_col <- lapply(col_by, function(rows) {
    sum(events[rows])
  })

  times_by_col <- lapply(col_by, function(rows) {
    sum(time_in_years[rows])
  })

  rate_by_col <-  Map(function(event, time) {
    event / time * lambda
  }, events_by_col, times_by_col)

  ci_by_col <- Map(function(event, time) {
    ci_patyear(
      event_number = event,
      person_year = time,
      conf_level = conf_level,
      conf_method = conf_method
    ) * lambda
  }, events_by_col, times_by_col)

  list(events_by_col = events_by_col,
       times_by_col = times_by_col,
       rate_by_col = rate_by_col,
       ci_by_col = ci_by_col,
       col_N = col_N #nolintr
       )



}


#' confidence interval for event rate per person-years
#'
#' applicable methods for computing confidence intervals for event rate over person-years at risk
#'
#' @details
#'   Confidence interval methods for  observed Y events over
#'    total of T person-years at risk  at a given \eqn{\alpha} :
#'   \enumerate{
#'   \item {Normal approximation confidence interval}
#'      {\deqn{LB = Y/T - Z(1- \alpha/2)*sqrt(Y/(T^2))}}
#'      {\deqn{UB = Y/T + Z(1- \alpha/2)*sqrt(Y/(T^2))}}
#'   \item {Exact confidence interval}
#'      {\deqn{LB = chisq(p = \alpha/2, df = 2Y)/(2T)}}
#'      {\deqn{UB= chisq(p = 1- \alpha/2, df = 2(Y + 1))/(2T)}}
#'   \item {Byar's confidence interval}
#'      {\deqn{LB = (Y + 0.5)(1- 1/(9(Y + 0.5)) - Z(1- \alpha/2)*sqrt(1/(Y + 0.5))/3)^3/T}}
#'      {\deqn{LB= (Y + 0.5)(1- 1/(9(Y + 0.5)) + Z(1- \alpha/2)*sqrt(1/(Y + 0.5))/3)^3/T}}
#' }
#'
#' @param event_number an integer
#' @param person_year  a number
#' @param conf_level confidence level, default is 0.95
#' @param conf_method confidence method (\code{"normal", "exact", "byar"}),
#'    see details of \code{\link{ci_patyear}}
#'
#' @export
#' @importFrom stats qchisq
#' @examples
#' ci_patyear(event_number = 10,
#'            person_year = 400,
#'            conf_level = 0.95,
#'            conf_method = "exact")
#'
#'

ci_patyear <- function(event_number,
                       person_year,
                       conf_level = 0.95,
                       conf_method = c("normal", "exact", "byar")) {
  stopifnot(is_numeric_single(event_number))
  stopifnot(is_numeric_single(person_year))
  check_numeric_range(conf_level, 0, 1)
  alpha <- 1 - conf_level
  q_norm <- qnorm(1 - alpha / 2)
  conf_method <- match.arg(conf_method)
  if (conf_method == "normal") {
    rate <- event_number / person_year
    ase <- sqrt(rate / person_year)
    lcl <- rate - q_norm * ase
    ucl <- rate + q_norm * ase
  } else if (conf_method == "exact") {
    lcl <- qchisq(p = (alpha) / 2, df = 2 * event_number) / (2 * person_year)
    ucl <- qchisq(p = 1 - (alpha) / 2, df = 2 * event_number + 2) / (2 * person_year)
  } else if (conf_method == "byar") {
    seg_1 <- event_number + 0.5
    seg_2 <- 1 - 1 / (9 * (event_number + 0.5))
    seg_3 <- q_norm * sqrt(1 / (event_number + 0.5)) / 3
    lcl <- seg_1 * ((seg_2 - seg_3)^3) / person_year
    ucl <- seg_1 * ((seg_2 + seg_3)^3) / person_year
  }
  c(lcl, ucl)
}
