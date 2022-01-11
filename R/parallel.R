# Parallel processing helper functions.

#' Get an approximate number of free cores.
#'
#' @return the approximate number of free cores, which is an integer between 1 and one less than
#'   the total cores.
#'
#' @details This uses the maximum load average at 1, 5 and 15 minutes on Linux and Mac
#'   machines to approximate the number of busy cores. For Windows, the load percentage is
#'   multiplied with the total number of cores.
#'   We then subtract this from the number of all detected cores. One additional core
#'   is not used for extra safety.
#'
#' @noRd
get_free_cores <- function() {
  all_cores <- parallel::detectCores(all.tests = TRUE)
  busy_cores <-
    if (.Platform$OS.type == "windows") {
      load_percent_string <- system("wmic cpu get loadpercentage", intern = TRUE)
      # This gives e.g.: c("LoadPercentage", "10", "")
      # So we just take the number here.
      load_percent <- as.integer(min(load_percent_string[2L], 100))
      stopifnot(!is.na(load_percent), load_percent >= 0, load_percent <= 100)
      ceiling(all_cores * load_percent / 100)
    } else if (.Platform$OS.type == "unix") {
      uptime_string <- system("uptime", intern = TRUE)
      # This gives e.g.:
      # "11:00  up  1:57, 3 users, load averages: 2.71 2.64 2.62"
      # Here we just want the last three numbers.
      uptime_split <- strsplit(uptime_string, split = ",|\\s")[[1]] # Split at comma or white space.
      uptime_split <- uptime_split[uptime_split != ""]
      load_averages <- as.numeric(utils::tail(uptime_split, 3))
      ceiling(max(load_averages))
    }
  stopifnot(!is.na(all_cores), !is.na(busy_cores))
  # For safety, we subtract 1 more core from all cores.
  free_cores <- as.integer(max(1, all_cores - busy_cores - 1))
  return(free_cores)
}
