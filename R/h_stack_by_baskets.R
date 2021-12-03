#' Helper Function to create a new `SMQ` variable in ADAE by stacking
#' `SMQ` and/or `CQ` records.
#'
#' Helper Function to create a new `SMQ` variable in ADAE that
#' consists of all adverse events belonging to selected
#' Standardized/Customized queries.
#' The new dataset will only contain records of the adverse events
#' belonging to any of the selected baskets.
#'
#' @inheritParams argument_convention
#' @param baskets (`character`)\cr variable names of the selected Standardized/Customized queries.
#' @param smq_varlabel (`string`)\cr a label for the new variable created.
#' @param keys (`character`)\cr names of the key variables to be returned
#' along with the new variable created.
#' @param aag_summary (`data frame`)\cr containing the SMQ baskets
#' and the levels of interest for the final SMQ variable. This is useful when
#' there are some levels of interest that are not observed in the `df` dataset.
#' The two columns of this dataset should be named `basket` and `basket_name`.
#'
#' @export
#'
#' @examples
#'
#' library(scda)
#'
#' adae <- synthetic_cdisc_data("latest")$adae[1:20 , ] %>% df_explicit_na()
#' h_stack_by_baskets(df = adae)
#'
#' aag <- data.frame(
#' NAMVAR = c("CQ01NAM", "CQ02NAM", "SMQ01NAM", "SMQ02NAM"),
#' REFNAME = c("D.2.1.5.3/A.1.1.1.1 AESI", "X.9.9.9.9/Y.8.8.8.8 AESI",
#'  "C.1.1.1.3/B.2.2.3.1 AESI", "C.1.1.1.3/B.3.3.3.3 AESI"),
#' SCOPE = c("", "", "BROAD", "BROAD"),
#' stringsAsFactors = FALSE
#'  )
#'
#' basket_name <- character(nrow(aag))
#' cq_pos <- grep("^(CQ).+NAM$", aag$NAMVAR)
#' smq_pos <- grep("^(SMQ).+NAM$", aag$NAMVAR)
#' basket_name[cq_pos] <- aag$REFNAME[cq_pos]
#' basket_name[smq_pos] <- paste0(
#' aag$REFNAME[smq_pos], "(", aag$SCOPE[smq_pos], ")"
#' )
#'
#' aag_summary <- data.frame(
#' basket = aag$NAMVAR,
#' basket_name = basket_name,
#' stringsAsFactors = TRUE
#' )
#'
#' result <- h_stack_by_baskets(df = adae, aag_summary = aag_summary)
#' all(levels(aag_summary$basket_name) %in% levels(result$SMQ))
#'
#' h_stack_by_baskets(
#' df = adae,
#' aag_summary = NULL,
#' keys = c("STUDYID", "USUBJID", "AEDECOD", "ARM"),
#' baskets = "SMQ01NAM"
#' )
#'
h_stack_by_baskets <- function(df,
                               baskets = grep("^(SMQ|CQ).+NAM$", names(df), value = TRUE),
                               smq_varlabel = "Standardized MedDRA Query",
                               keys = c("STUDYID", "USUBJID", "ASTDTM", "AEDECOD", "AESEQ"),
                               aag_summary = NULL,
                               na_level = "<Missing>") {

  #Use of df_explicit_na() in case the user has not previously used
  df <- df_explicit_na(df, na_level = na_level)

  smq_nam <- baskets[startsWith(baskets, "SMQ")]
  # SC corresponding to NAM
  smq_sc <- gsub(pattern = "NAM", replacement = "SC", x = smq_nam, fixed = TRUE)
  smq <- setNames(smq_sc, smq_nam)

  assertthat::assert_that(
    is.character(baskets),
    is.string(smq_varlabel),
    is.data.frame(df),
    all(startsWith(baskets, "SMQ") | startsWith(baskets, "CQ")),
    all(endsWith(baskets, "NAM")),
    all(baskets %in% names(df)),
    all(keys %in% names(df)),
    all(smq_sc %in% names(df)),
    is.string(na_level),
    sum(baskets %in% names(df)) > 0
  )

  if (!is.null(aag_summary)) {
    assertthat::assert_that(
    is_df_with_variables(
      df = aag_summary,
      variables = list(val = c("basket", "basket_name"))
      )
    )
    #Warning in case there is no match between `aag_summary$basket` and `baskets` argument.
    #Honestly, I think those should completely match. Target baskets should be the same.
    if (length(intersect(baskets, unique(aag_summary$basket))) == 0) {
      warning("There are 0 baskets in common between aag_summary$basket and `baskets` argument.")
    }
  }

  var_labels <- c(var_labels(df[, keys]), smq_varlabel)

  #convert `na_level` records from baskets to NA for the later loop and from wide to long steps
  df[, c(baskets, smq_sc)][df[, c(baskets, smq_sc)] == na_level] <- NA

  if (all(is.na(df[, baskets]))) { #in case there is no level for the target baskets
    df_long <- df[-seq_len(nrow(df)), keys] # we just need an empty dataframe keeping all factor levels
  } else {

    # Concatenate SMQxxxNAM with corresponding SMQxxxSC
    df_cnct <- df[, c(keys, baskets[startsWith(baskets, "CQ")])]

    for (nam in names(smq)) {

      sc <- smq[nam] # SMQxxxSC corresponding to SMQxxxNAM
      nam_notna <- !is.na(df[[nam]])
      new_colname <- paste(nam, sc, sep = "_")
      df_cnct[nam_notna, new_colname] <- paste0(df[[nam]], "(", df[[sc]], ")")[nam_notna]
    }

      df_cnct$unique_id <- seq(1, nrow(df_cnct))
      var_cols <- names(df_cnct)[!(names(df_cnct) %in% c(keys, "unique_id"))]
      # have to convert df_cnct from tibble to dataframe
      # as it throws a warning otherwise about rownames.
      # tibble do not support rownames and reshape creates rownames

      df_long <- stats::reshape(
        data = as.data.frame(df_cnct),
        varying = var_cols,
        v.names = "SMQ",
        idvar = names(df_cnct)[names(df_cnct) %in% c(keys, "unique_id")],
        direction = "long",
        new.row.names = seq(prod(length(var_cols), nrow(df_cnct)))
        )

      df_long <- df_long[!is.na(df_long[, "SMQ"]), !(names(df_long) %in% c("time", "unique_id"))]
      df_long$SMQ <- as.factor(df_long$SMQ) #nolint
    }

  smq_levels <- setdiff(levels(df_long[["SMQ"]]), na_level)

  if (!is.null(aag_summary)) {
    #A warning in case there is no match between df and aag_summary records
    if (length(intersect(smq_levels, unique(aag_summary$basket_name))) == 0) {
      warning("There are 0 basket levels in common between aag_summary$basket_name and df.")
    }
    df_long[["SMQ"]] <- factor(
      df_long[["SMQ"]],
      levels = sort(
        c(
          smq_levels,
          setdiff(unique(aag_summary$basket_name), smq_levels)
        )
      )
    )
  } else {

    all_na_basket_flag <- vapply(df[, baskets], function(x) {
      all(is.na(x))
    }, FUN.VALUE = logical(1))
    all_na_basket <- baskets[all_na_basket_flag]

    df_long[["SMQ"]] <- factor(
      df_long[["SMQ"]],
      levels = sort(c(smq_levels, all_na_basket))
    )
  }
  var_labels(df_long) <- var_labels
  tibble::tibble(df_long)
}
