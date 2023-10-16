#' Split Function to Place Reference Group Facet Last
#'
#' Place reference group facet last during post-processing stage in a custom split function.
#'
#' @param splret result of the core split
#' @param spl split object
#' @param fulldf data.frame of incoming data to be split
#'
#' @export
#'
#' @seealso [rtables::make_split_fun()]
#'
#' @examples
#' library(dplyr)
#'
#' # Define custom split function
#' ref_last <- make_split_fun(post = list(ref_group_last))
#'
#' dat <- data.frame(
#'   x = factor(letters[1:5], levels = letters[5:1]),
#'   y = 1:5
#' )
#'
#' # With rtables layout functions
#' basic_table() %>%
#'   split_cols_by("x", ref_group = "c", split_fun = ref_last) %>%
#'   analyze("y") %>%
#'   build_table(dat)
#'
#' # With tern layout funcitons
#' adtte_f <- tern_ex_adtte %>%
#'   filter(PARAMCD == "OS") %>%
#'   mutate(
#'     AVAL = day2month(AVAL),
#'     is_event = CNSR == 0
#'   )
#'
#' basic_table() %>%
#'   split_cols_by(var = "ARMCD", ref_group = "ARM B", split_fun = ref_last) %>%
#'   add_colcounts() %>%
#'   surv_time(
#'     vars = "AVAL",
#'     var_labels = "Survival Time (Months)",
#'     is_event = "is_event",
#'   ) %>%
#' build_table(df = adtte_f)
#'
ref_group_last <- function(splret, spl, fulldf){

  if(!"ref_group_value" %in% slotNames(spl)) {
    stop("Reference group is undefined.")
  }

  spl_var <- spl@payload
  init_lvls <- levels(fulldf[[spl_var]])

  if(!all(names(splret$values) %in% init_lvls)) {
    stop("This split function does not work with combination facets.")
  }

  ref_group_pos <- which(init_lvls == spl@ref_group_value)
  reord_lvls <- c(init_lvls[-ref_group_pos], init_lvls[ref_group_pos])
  ord <- match(reord_lvls, names(splret$values))

  make_split_result(splret$values[ord],
                    splret$datasplit[ord],
                    splret$labels[ord])
}
