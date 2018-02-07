

#' Table of Variable Summaries
#' 
#' Similiar as the demographic table in STREAM
#' 
#' @inheritParams rtables::rtabulate.numeric
#' @param data data frame
#' @param total if not \code{NULL} then it must be a string and an addition
#'   column will be added with the overall summaries
#'   
#' @details
#' Give a detailed description of what this function does.
#' 
#' @export
#' 
#' @author Adrian Waddell \email{adrian.waddell@roche.com} and Xiao Yu Mo (mox5), \email{xiao_yu.mo@roche.com}
#' 
#' @examples 
#' 
#' library(tern)
#' library(random.cdisc.data)
#' library(forcats)
#' 
#' ASL <- radam("ASL")
#' 
#' # control the label
#' attr(ASL$BAGE, "label") <- "Baseline Age of patient"
#' attr(ASL$SEX, "label")
#' 
#' # control categorical order
#' ASL$SEX <- fct_relevel(ASL$SEX, "M", "F", "UNDEFINED")
#' 
#' # control arm order
#' ASL$ARM <- fct_relevel(ASL$ARM, "ARM B", "ARM A")
#' 
#' t_summarize_variables(ASL %>% select(SEX, BAGE), col_by = ASL$ARM, total = "All Patients")
#' 
#' t_summarize_variables(iris %>% select(-Species), col_by  = iris$Species)
#' 
#' t_summarize_variables(iris %>% select(-Species), col_by  = no_by("All Species"))
#' 
#' \dontrun{
#' library(tern)
#' library(atezo.data)
#' library(dplyr)
#' 
#' ASL <- asl(com.roche.cdt30019.go29436.re)
#' tbl_expected <- get_demographic_table(com.roche.cdt30019.go29436.re)
#' Viewer(tbl_expected)
#' 
#' # order factor levels if they exist
#' of <- function(x, ...) {
#'   lvls <- if (is.factor(x)) levels(x) else unique(x)
#'   olvls <- intersect(unlist(list(...)), lvls)
#'   factor(as.character(x), levels = c(olvls, setdiff(lvls, olvls)))
#' }
#' add_lbl <- function(lbl, x) {
#'   structure(x, label = lbl)
#' }
#' lbl <- function(x) {
#'   attr(x, "label")
#' }
#' 
#' ADSL <- ASL %>%
#'  filter(ITTWTFL == 'Y') %>%
#'  mutate(ARM = of(ARM, "DUMMY C", "DUMMY B", "DUMMY A")) %>%
#'  mutate(SEX = add_lbl(lbl(SEX), recode_factor(SEX, M = "MALE", F = "FEMALE")))
#' 
#' tbl <- t_summarize_variables(
#'    data = subset(ADSL, select = toupper(c("sex", "mliver", "tciclvl2", "bage", "age4cat",
#'      "ethnic", "race", "bwt", "tobhx", "hist", "EGFRMUT",
#'      "alkmut", "krasmut", "becog"))),
#'    col_by = ADSL$ARM,
#'    total = "All Patients"
#' )
#' Viewer(tbl)
#' 
#' compare_rtables(tbl, tbl_expected)
#' 
#' # if all is good then
#' tbl <- tbl_expected
#' compare_rtables(tbl, tbl_expected)
#' 
#' }
#' 
t_summarize_variables <- function(data, col_by, total = NULL) {
  
  if (!is.data.frame(data)) stop("data is expected to be a data frame")
  if (!is.no_by(col_by) && nrow(data) != length(col_by)) stop("dimension missmatch col_by")
  
  if (!is.no_by(col_by) && !is.factor(col_by)) col_by <- as.factor(col_by) 
  
  if (!is.null(total) && !is.no_by(col_by)) { ## add total column
    
    if (length(total) != 1) stop("total must be either NULL or a single string")
    if (total %in% col_by) stop("total cannot be an level in col_by")

    n <- nrow(data)
    data <- rbind(data, data)
    col_by <- factor(c(as.character(col_by), rep(total, n)), levels = c(levels(col_by), total))
  }
  
  rtables_vars <- Map(function(var, varname) {
    tbl_summary <- if (is.numeric(var)) {
      rbind(
        rtabulate(var, col_by, length, row.name = "n", indent = 1),
        rtabulate(var, col_by, function(x) c(mean(x), sd(x)), format = "xx.xx (xx.xx)", row.name = "Mean (SD)", indent = 1),
        rtabulate(var, col_by, median, row.name = "Median", indent = 1),
        rtabulate(var, col_by, range, format = "xx.xx - xx.xx", row.name = "Min - Max", indent = 1)
      )
    } else {
      # treat as factor
      rtabulate(
        x = factor(var),
        col_by = col_by,
        FUN = function(x_cell, x_row, x_col) {
          if (length(x_col) > 0) length(x_cell) * c(1, 1/length(x_col)) else rcell("-", format = "xx")
        },
        row_col_data_args = TRUE,
        format = "xx (xx.xx%)",
        indent = 1
      )
    }
    
    # Add label row
    label <- attr(var, "label")
    if (is.null(label)) label <- varname
    
    rbind(
      rtable(header = names(tbl_summary), rrow(label)),
      tbl_summary
    )
  }, data, names(data))
  
  # now add empty rows
  Reduce(function(x,y) {rbind(x, rtable(header = names(x), rrow()), y)}, rtables_vars)
}
