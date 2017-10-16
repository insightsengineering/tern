

#' Combine Arms to Baseline and Comparison Arms
#' 
#' Combine groups in arms to reference and comparison groups in order to use it
#' in \code{\link[survival]{survfit}} and \code{\link{glm}}.
#' 
#' @param arm vector with arm information
#' @param arm.ref values in arm that are combined to the reference group
#' @param arm.comp values in arm to be used as the comparison group(s)
#' @param arm.comp.combine Boolean value, \code{TRUE} if want all non-ref arms to be combined into one comparison group, 
#'                       \code{FALSE} if want each of the non-ref arms to be a separate comparison group
#' 
#' @export
#' 
#' @author songy24 liaoc10
#' 
#' @examples 
#' 
#' combine_arm(arm = LETTERS[1:4], arm.ref = c("A", "B"), arm.comp.combine = T)
#' combine_arm(arm = LETTERS[1:4], arm.ref = c("A", "B"), arm.comp.combine = F)
#' 
#' combine_arm(arm = LETTERS[1:4], arm.ref = c("A", "B"), arm.comp = "C", arm.comp.combine = T)
#' combine_arm(arm = LETTERS[1:4], arm.ref = c("A", "B"), arm.comp = "C", arm.comp.combine = F)
#' 
#' combine_arm(arm = LETTERS[1:4], arm.ref = c("A", "B", "R"), arm.comp = "C")
#' 
combine_arm <- function(arm, arm.ref, arm.comp = setdiff(arm, arm.ref), arm.comp.combine = T) {
  
  if (!all(arm.ref %in% arm)) stop("not all arms in arm.ref are in arm")
  if (!all(arm.comp %in% arm)) stop("not all arms in arm.comp are in arm")
  
  name_arm_ref <- paste(arm.ref, collapse = "/")
  name_arm_comp <- paste(arm.comp, collapse = "/")
  
  arm2 <- vapply(arm, function(x) {
    if (is.na(x) || x == "") {
      NA_character_
    } else if (x %in% arm.ref) {
      name_arm_ref
    } else if (x %in% arm.comp & isTRUE(arm.comp.combine)) {
      name_arm_comp
    } else if (x %in% arm.comp & !isTRUE(arm.comp.combine)) {
      x
    } else {
      NA_character_
    }
  }, character(1))
  
  if (arm.comp.combine == TRUE) {
    factor(as.vector(arm2), levels = c(name_arm_ref, name_arm_comp))
  } else if (arm.comp.combine == FALSE) {
    factor(as.vector(arm2), levels = c(name_arm_ref, arm.comp))
  }
  
}