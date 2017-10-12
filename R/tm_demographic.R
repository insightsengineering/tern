

#' Create Demographics Table from ADSL data
#' 
#' Similiar as in STREAM
#' 
#' @param ADSL ADSL dataset with the following variables: USUBJID, STUDYID, AGE,
#'   and SEX
#' 
#' @details
#' Give a detailed description of what this function does.
#' 
#' @export
#' 
#' @author Xiao Yu Mo (mox5), \email{xiao_yu.mo@roche.com}
#' 
#' @examples 
#' n <- 100
#' ASL <- data.frame(
#'    USUBJID = paste0("id-", 1:n),
#'    STUDYID = "study 1",
#'    AGE = 40 + rnorm(n, 0, 20),
#'    SEX = sample(c("M", "F", "UNDEFINED", NA), n, replace = TRUE),
#'    stringAsFactors = FALSE
#' )
#' 
#' demographic_table(ASL)
#' 
#' \dontrun{
#' library(atezo.data)
#' library(dplyr)
#' 
#' ASL <- asl(com.roche.cdt30019.go29436.re)
#' tbl_expected <- get_demographic_table(com.roche.cdt30019.go29436.re)
#' Viewer(tbl_expected)
#' 
#' 
#' ADSL <- ASL %>% filter(ITTFL == 'Y') 
#' 
#' tbl <- demographic_table(
#'    data = ADSL,
#'    arm = ADSL$ARM,
#'    all.patients = TRUE
#' )
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
                              arm,
                              all.patients = TRUE,
                              group_by = c("SEX", "AGE", "RACE", "ETHNIC")) {
  
  data %needs% c("USUBJID", "STUDYID", group_by)
  
  if (nrow(data) != length(arm)) stop("dimension of arm and data missmatch")
  
  if (any("All Patients" %in% arm)) stop("All Patients is not a valid arm name")
  
  if (all.patients) {
    n <- nrow(data)
    data <- rbind(data, data)
    arm <- c(arm, rep("All Patients", n))
  }
  
  ## get a list with each variables tht we want to summarize
  
  X <- data[group_by]

  ## xi <- X[[1]]
  row_info <- lapply(X, function(xi) {
    if (!is.numeric(xi)) {
      ## then return the n, mean median, and range
      df <- data.frame(xi, arm)
      xi_split <- split(df, arm)
      c(
        list(
          n = vapply(xi_split, nrow, numeric(1))
        ),
        lapply(split(df, df$xi), function(xii) vapply(split(xii, xii$arm), nrow, numeric(1)))
      )

    } else {
      ## categorical count
      
    }
  })
  
  first.row <- TRUE
  rrow_collection <- unlist(lapply(row_info, function(ri) {
    l <- list(
      if (first.row) NULL else rrow(),
      rrow("Sex", ...),
      rrow("n", ..., indent = 1)
    )
    first.row <<- FALSE
    l
  }), recursive = FALSE)
  
  
  do.call(rtable, c(list(col.names = ...), rrow_collection))
  
  
  tbl
}
