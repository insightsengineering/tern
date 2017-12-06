

#' Create Demographics Table from ADSL data
#' 
#' Similiar as in STREAM
#' 
#' @param data data frame,
#' @param arm_var variable to split by columnn
#' @param all.patients boolean, add column with all patients
#' @param group_by_vars variables to be summarized
#' 
#' @details
#' Give a detailed description of what this function does.
#' 
#' @export
#' 
#' @author Xiao Yu Mo (mox5), \email{xiao_yu.mo@roche.com}
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
#' demographic_table(ASL, group_by_vars = c("SEX", "BAGE"))
#' 
#' 
#' \dontrun{
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
#' 
#' 
#' tbl <- demographic_table(
#'    data = ADSL,
#'    arm_var = "ARM",
#'    all.patients = TRUE,
#'    group_by_vars = toupper(c("sex", "mliver", "tciclvl2", "bage", "age4cat",
#'      "ethnic", "race", "bwt", "tobhx", "hist", "EGFRMUT",
#'      "alkmut", "krasmut", "becog"))
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
#' `%needs%` <- teal.oncology:::`%needs%`
#' 
#' 
demographic_table <- function(data,
                              arm_var = "ARM",
                              all.patients = TRUE,
                              group_by_vars = c("SEX", "AGE", "RACE", "ETHNIC")) {
  
  data %needs% c(arm_var, group_by_vars)
  
  arm <- factor(data[[arm_var]])
  if (any(is.na(arm))) stop("currently cannot deal with missing values in arm")
  
  if (all.patients) {
    if ("All Patients" %in% names(arm)) stop("if argument all.patients = TRUE then there cannot be an arm 'All Patients'")
    n <- nrow(data)
    data <- rbind(data, data)
    arm <- factor(c(as.character(arm), rep("All Patients", n)), levels = c(levels(arm), "All Patients"))
  }
  
  
  ## get a list with each variables tht we want to summarize
  
  var_collection <- data[group_by_vars]
  type <- lapply(var_collection, function(var) if(is.numeric(var)) "numeric" else "categorical")
  
  var_lables <- unlist(Map(function(var, varname) {
    label <- attr(var, "label")
    if (is.null(label)) varname else label
  }, var_collection, names(var_collection)))
    
  
  ## x <- var_collection[[1]]
  var_col_info <- Map(function(x, xtype) {
    if (xtype == "numeric") {
      ## then return the n, mean median, and range
      df <- data.frame(x = x, arm = arm) %>% filter(!is.na(x))
      
      lapply(split(df, df$arm), function(dfi) {
        xii <- dfi$x
        
        #lapply(c(length, mean, sd, median, range), function(fun)fun(xii))
        list(
          "n" = rcell(length(xii), format = "xx"),
          "Mean (SD)" = rcell(c(mean(xii), sd(xii)), format = "xx.x (xx.x)"),
          "Median" = rcell(median(xii), format = "xx.x"),
          "Min - Max" = rcell(range(xii), format = "xx.xx - xx.xx")
        )
      })
    } else if (xtype == "categorical") {
      ## categorical count
      df <- data.frame(x = factor(x), arm = arm) %>% filter(!is.na(x))
      
      lapply(split(df, df$arm), function(dfi) {
        xii <- dfi$x
        n <- length(xii)
        c(
          list(
            "n" = rcell(n, format = "xx")
          ),
          lapply(split(dfi, dfi$x), function(dfii) {
            nii <- nrow(dfii)
            rcell(c(nii, nii/n), format = "xx (xx.xx%)")
          })
        )
      })
    } else {
      stop("unknown type ", xtype)
    }
  }, var_collection, type)
  
  var_row_info <- lapply(var_col_info, list_transpose)
  
  n <- vapply(var_row_info[[1]][[1]], as.vector, numeric(1))
  
  ## create the rtable object
  first.row <- TRUE
  
  do.call(rtable, c(
    list(
      col.names = paste0(levels(arm), "\n", paste0("(N=",n, ")")),
      format = "xx"
    ),
    Filter(function(x)!is.null(x), unlist(Map(function(rows_for_var, var, label) {
      c(
        list(
          if (first.row) {first.row <<- FALSE; NULL} else  rrow(),
          rrow(label)
        ),
        Map(function(ri, category) {
          do.call(rrow, c(list(row.name = category, indent = 1), ri))
        }, rows_for_var, names(rows_for_var))
      )
    }, var_row_info, names(var_row_info), var_lables), recursive = FALSE))
  ))
}
