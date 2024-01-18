#' Bland Altman analysis
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Functions of bland altman method to assess the agreement between two numerical vectors.
#'
#' @inheritParams argument_convention
#' @param y ('numeric')\cr vector of numbers we want to analyze, which we want to compare with x.
#'
#' @name bland_altman
#' @examples
#' x <- seq(1, 60, 5)
#' y <- seq(5, 50, 4)
#' conf_level <- 0.9
#' # Derive statistics that are needed for Bland Altman plot
#' s_bland_altman(x, y, conf_level = conf_level)
#' # Create a Bland Altman plot
#' g_bland_altman(x, y, conf_level = conf_level)
NULL

#' @describeIn bland_altman
#'
#' @export
s_bland_altman <- function(x, y, conf_level = 0.95) {
  checkmate::assert_numeric(x, min.len = 1, any.missing = TRUE)
  checkmate::assert_numeric(y, len = length(x), any.missing = TRUE)
  checkmate::assert_numeric(conf_level, lower = 0, upper = 1, any.missing = TRUE)

  alpha <- 1 - conf_level

  ind <- complete.cases(x, y) # use only pairwise complete observations, and check if x and y have the same length
  x <- x[ind]
  y <- y[ind]
  n <- sum(ind) # number of 'observations'

  if (n == 0) {
    stop("there is no valid paired data")
  }

  difference <- x - y # vector of differences
  average <- (x + y) / 2 # vector of means
  difference_mean <- mean(difference) # mean difference
  difference_sd <- sd(difference) # SD of differences
  al <- qnorm(1 - alpha / 2) * difference_sd
  upper_agreement_limit <- difference_mean + al # agreement limits
  lower_agreement_limit <- difference_mean - al


  difference_se <- difference_sd / sqrt(n) # standard error of the mean
  al_se <- difference_sd * sqrt(3) / sqrt(n) # standard error of the agreement limit
  tvalue <- qt(1 - alpha / 2, n - 1) # t value for 95% CI calculation
  difference_mean_ci <- difference_se * tvalue
  al_ci <- al_se * tvalue
  upper_agreement_limit_ci <- c(upper_agreement_limit - al_ci, upper_agreement_limit + al_ci)
  lower_agreement_limit_ci <- c(lower_agreement_limit - al_ci, lower_agreement_limit + al_ci)


  list(
    df = data.frame(average, difference),
    difference_mean = difference_mean,
    ci_mean = difference_mean + c(-1, 1) * difference_mean_ci,
    difference_sd = difference_sd,
    difference_se = difference_se,
    upper_agreement_limit = upper_agreement_limit,
    lower_agreement_limit = lower_agreement_limit,
    agreement_limit_se = al_se,
    upper_agreement_limit_ci = upper_agreement_limit_ci,
    lower_agreement_limit_ci = lower_agreement_limit_ci,
    t_value = tvalue,
    n = n
  )
}

#' @describeIn bland_altman
#'
#' @export
g_bland_altman <- function(x, y, conf_level = 0.95) {
  result_tem <- s_bland_altman(x, y, conf_level = conf_level)
  xpos <- max(result_tem$df$average) * 0.9 + min(result_tem$df$average) * 0.1
  yrange <- diff(range(result_tem$df$difference))

  p <- ggplot(result_tem$df) +
    geom_point(aes(x = average, y = difference), color = "blue") +
    geom_hline(yintercept = result_tem$difference_mean, color = "blue", linetype = 1) +
    geom_hline(yintercept = 0, color = "blue", linetype = 2) +
    geom_hline(yintercept = result_tem$lower_agreement_limit, color = "red", linetype = 2) +
    geom_hline(yintercept = result_tem$upper_agreement_limit, color = "red", linetype = 2) +
    annotate("text", x = xpos, y = result_tem$lower_agreement_limit + 0.03 * yrange,
             label = "lower limits of agreement", color = "red") +
    annotate("text", x = xpos, y = result_tem$upper_agreement_limit + 0.03 * yrange,
             label = "upper limits of agreement", color = "red") +
    annotate("text", x = xpos, y = result_tem$difference_mean + 0.03 * yrange,
             label = "mean of difference between two measures", color = "blue") +
    annotate("text", x = xpos, y = result_tem$lower_agreement_limit - 0.03 * yrange,
             label = sprintf("%.2f", result_tem$lower_agreement_limit), color = "red") +
    annotate("text", x = xpos, y = result_tem$upper_agreement_limit - 0.03 * yrange,
             label = sprintf("%.2f", result_tem$upper_agreement_limit), color = "red") +
    annotate("text", x = xpos, y = result_tem$difference_mean - 0.03 * yrange,
             label = sprintf("%.2f", result_tem$difference_meanm), color = "blue") +
    xlab("Average of two measures") +
    ylab("Difference between two measures")

  return(p)
}
