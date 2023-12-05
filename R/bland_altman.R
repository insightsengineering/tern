
#' Bland Altman analysis
#'
#' @description 'r lifecycle::badge("stable")'
#'
#' @inheritParams argument_convention
#' @param y ('numeric')\cr vector of numbers we want to analyze.
#'
#' @name s_bland_altman
#' @details
#' https://doi.org/10.1016/S0140-6736(86)90837-8
#' @references
#' @article{bland1986statistical,
# title={Statistical methods for assessing agreement between two methods of clinical measurement},
# author={Bland, J Martin and Altman, DouglasG},
# journal={The lancet},
# volume={327},
# number={8476},
# pages={307--310},
# year={1986},
# publisher={Elsevier}
# }
# https://cran.r-project.org/web/packages/Rdpack/vignettes/Inserting_bibtex_references.pdf

#' @examples
#' x <- seq(1, 60, 5)
#' y <- seq(5, 50, 4)
#' conf_level <- 0.9
#' s_bland_altman(x, y, conf_level = conf_level)


s_bland_altman <- function(x, y, conf_level = 0.95){
  checkmate::assert_numeric(x, min.len = 1, any.missing = TRUE)
  checkmate::assert_numeric(y, len = length(x), any.missing = TRUE)
  checkmate::assert_numeric(conf_level, lower = 0, upper = 1, any.missing = TRUE)

  alpha <- 1 - conf_level

  ind <- complete.cases(x, y) # use only pairwise complete observations, and check if x and y have the same length
  x <- x[ind]
  y <- y[ind]
  n <- length(n)                                      # number of 'observations'

  if(n ==0){
    stop("there is no valid paired data")
  }

  difference <- x - y                                 # vector of differences
  average <- (x + y) / 2                              # vector of means
  difference_mean <- mean(difference)                 # mean difference
  difference_sd <- sd(difference)                     # SD of differences
  al <- qnorm(1 - alpha / 2) * difference_sd
  upper_agreement_limit <- difference_mean + al       # agreement limits
  lower_agreement_limit <- difference_mean - al


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
