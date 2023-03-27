s_bland_altman_cj <- function(x, y, conf_level = 0.95, group = NULL){

  alpha <- 1 - conf_level

  ind <- complete.cases(x, y) # use only pairwise complete observations, and check if x and y have the same length
  x <- x[ind]
  y <- y[ind]
  group <- group[ind]

  difference <- x - y                                 # vector of differences
  average <- (x + y) / 2                              # vector of means
  difference_mean <- mean(difference)                 # mean difference
  difference_sd <- sd(difference)                     # SD of differences
  al <- qnorm(1 - alpha / 2) * difference_sd
  upper_agreement_limit <- difference_mean + al       # agreement limits
  lower_agreement_limit <- difference_mean - al
  n <- length(difference)                             # number of 'observations'

  difference_se <- difference_sd / sqrt(n)            # standard error of the mean
  al_se <- difference_sd * sqrt(3) / sqrt(n)          # standard error of the agreement limit
  tvalue <- qt(1 - alpha / 2, n - 1)                  # t value for 95% CI calculation
  difference_mean_ci <- difference_se * tvalue
  al_ci <- al_se * tvalue
  upper_agreement_limit_ci <- c(upper_agreement_limit - al_ci, upper_agreement_limit + al_ci)
  lower_agreement_limit_ci <- c(lower_agreement_limit - al_ci, lower_agreement_limit + al_ci)


  list(
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
