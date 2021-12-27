#' Helper Function to create a map dataframe that can be used in `trim_levels_to_map` split function.
#'
#' Helper Function to create a map dataframe from the input dataset, which can be used as an argument in the
#' `trim_levels_to_map` split function. Based on different method, the map is constructed differently.
#'
#' @inheritParams argument_convention
#' @param abnormal (`named list`)\cr identifying the abnormal range level(s) in `df`. Based on the levels of abnormality
#' of the input dataset, it can be something like `list(Low = "LOW LOW", High = "HIGH HIGH")` or
#' `abnormal = list(Low = "LOW", High = "HIGH"))`
#' @param method (`string`)\cr indicates how the returned map will be constructed. Can be either `"default"` or
#' `"range"`.
#' If method is `"default"`, the returned map will only have the abnormal directions that are observed in the `df`, and
#' records with all normal values will be excluded to avoid error in creating layout.
#' If method is `"range"`, the returned map will be based on the rule that at least one observation with low range > 0
#' for low direction and at least one observation with high range is not missing for high direction.
#'
#' @export
#'
#'
#' @examples
#'
#' library(scda)
#' adlb <- synthetic_cdisc_data("latest")$adlb
#' adlb <- df_explicit_na(adlb)
#'
#' h_map_for_count_abnormal(
#'   df = adlb,
#'   variables = list(anl = "ANRIND", split_rows = c("LBCAT", "PARAM")),
#'   abnormal = list(low = c("LOW"), high = c("HIGH")),
#'   method = "default",
#'   na_level = "<Missing>"
#' )
#'
#'
#' df <- data.frame(
#'   USUBJID = c(rep("1", 4), rep("2", 4), rep("3", 4)),
#'   AVISIT = c(
#'     rep("WEEK 1", 2),
#'     rep("WEEK 2", 2),
#'     rep("WEEK 1", 2),
#'     rep("WEEK 2", 2),
#'     rep("WEEK 1", 2),
#'     rep("WEEK 2", 2)
#'   ),
#'   PARAM = rep(c("ALT", "CPR"), 6),
#'   ANRIND = c(
#'     "NORMAL", "NORMAL", "LOW",
#'     "HIGH", "LOW", "LOW", "HIGH", "HIGH", rep("NORMAL", 4)
#'   ),
#'   ANRLO = rep(5, 12),
#'   ANRHI = rep(20, 12)
#' )
#' df$ANRIND <- factor(df$ANRIND, levels = c("LOW", "HIGH", "NORMAL"))
#' h_map_for_count_abnormal(
#'   df = df,
#'   variables = list(
#'     anl = "ANRIND",
#'     split_rows = c("PARAM"),
#'     range_low = "ANRLO",
#'     range_high = "ANRHI"
#'   ),
#'   abnormal = list(low = c("LOW"), high = c("HIGH")),
#'   method = "range",
#'   na_level = "<Missing>"
#' )
h_map_for_count_abnormal <- function(df,
                                     variables = list(
                                       anl = "ANRIND",
                                       split_rows = c("PARAM"),
                                       range_low = "ANRLO",
                                       range_high = "ANRHI"
                                     ),
                                     abnormal = list(low = c("LOW", "LOW LOW"), high = c("HIGH", "HIGH HIGH")),
                                     method = c("default", "range"),
                                     na_level = "<Missing>") {
  method <- match.arg(method)
  assertthat::assert_that(
    "anl" %in% names(variables),
    "split_rows" %in% names(variables),
    is_variables(variables),
    utils.nest::is_character_list(abnormal, min_length = 2, max_length = 2),
    is_df_with_factors(df, list(val = variables$anl)),
    is_df_with_no_na_level(
      df,
      variables = list(anl = variables$anl, split_rows = variables$split_rows), na_level = na_level
    ),
    !any(is.na(df[variables$split_rows])),
    is_factor_no_na(df[[variables$anl]])
  )

  # Drop usued levels from df as they are not supposed to be in the final map
  df <- droplevels(df)

  normal_value <- setdiff(levels(df[[variables$anl]]), unlist(abnormal))
  assertthat::assert_that(
    # Based on the understanding of clinical data, there should only be one level of normal which is "NORMAL"
    length(normal_value) == 1
  )

  # Default method will only have what is observed in the df, and records with all normal values will be excluded to
  # avoid error in layout building.
  if (method == "default") {
    df_abnormal <- subset(df, df[[variables$anl]] %in% unlist(abnormal))
    map <- unique(df_abnormal[c(variables$split_rows, variables$anl)])
    map_normal <- unique(subset(map, select = variables$split_rows))
    map_normal[[variables$anl]] <- normal_value
    map <- rbind(map, map_normal)
  } else if (method == "range") {
    # range method follows the rule that at least one observation with ANRLO > 0 for low
    # direction and at least one observation with ANRHI is not missing for high direction.
    assertthat::assert_that(
      is_df_with_variables(df, variables = list(range_low = variables$range_low, range_high = variables$range_high)),
      "range_low" %in% names(variables),
      "range_high" %in% names(variables),
      "LOW" %in% toupper(names(abnormal)),
      "HIGH" %in% toupper(names(abnormal))
    )

    # Define low direction of map
    df_low <- subset(df, df[[variables$range_low]] > 0)
    map_low <- unique(df_low[variables$split_rows])
    low_levels <- unname(unlist(abnormal[toupper(names(abnormal)) == "LOW"]))
    low_levels_df <- as.data.frame(low_levels)
    colnames(low_levels_df) <- variables$anl
    low_levels_df <- do.call("rbind", replicate(nrow(map_low), low_levels_df, simplify = FALSE))
    rownames(map_low) <- NULL # Just to avoid strange row index in case upstream functions changed
    map_low <- map_low[rep(seq_len(nrow(map_low)), each = length(low_levels)), , drop = FALSE] # nolint
    map_low <- cbind(map_low, low_levels_df)

    # Define high direction of map
    df_high <- subset(df, df[[variables$range_high]] != na_level | !is.na(df[[variables$range_high]]))
    map_high <- unique(df_high[variables$split_rows])
    high_levels <- unname(unlist(abnormal[toupper(names(abnormal)) == "HIGH"]))
    high_levels_df <- as.data.frame(high_levels)
    colnames(high_levels_df) <- variables$anl
    high_levels_df <- do.call("rbind", replicate(nrow(map_high), high_levels_df, simplify = FALSE))
    rownames(map_high) <- NULL
    map_high <- map_high[rep(seq_len(nrow(map_high)), each = length(high_levels)), , drop = FALSE] # nolint
    map_high <- cbind(map_high, high_levels_df)

    # Define normal of map
    map_normal <- unique(rbind(map_low, map_high)[variables$split_rows])
    map_normal[variables$anl] <- normal_value

    map <- rbind(map_low, map_high, map_normal)
  }

  # map should be all characters
  map <- data.frame(lapply(map, as.character), stringsAsFactors = FALSE)

  # sort the map final output by split_rows variables
  for (i in rev(seq_len(length(variables$split_rows)))) {
    map <- map[order(map[[i]]), ]
  }
  map
}
