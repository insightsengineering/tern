arm_x <- "A: Drug X"
arm_y <- "B: Placebo"

#' Split Function to Configure Risk Difference Column
#'
#' @export
add_risk_diff <- function(arm_x,
                          arm_y,
                          col_label = "Risk Difference (%) (95% CI)") {
  combodf <- tribble(
    ~valname, ~label, ~levelcombo, ~exargs,
    paste("riskdiff", arm_x, arm_y, sep = "_"), col_label, c(arm_x, arm_y), list()
  )
  add_combo_levels(combodf)
}
