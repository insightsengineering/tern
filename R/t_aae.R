
#' Adverse Events Table
#' 
#' 
#' 
#' @author Edgar
#' 
#' @export
#' 
#' @examples 
#' \dontrun{
#' devtools::install_github("Rpackages/rgda", host = "https://github.roche.com/api/v3")
#' 
#' 
#' AAE <- rgda::get_data("BCE", "/opt/BIOSTAT/prod/s28363v/libraries/xaae.sas7bdat")
#' ASL <- rgda::get_data("BCE", "/opt/BIOSTAT/prod/s28363v/libraries/asl.sas7bdat")
#' 
#' }
#' 
#' 
#' 
t_aae <- function() {
  
  ## ARM always needs to be a factor
  
  ## do not use AAE and ASL as arguments
  ## think of cdisc independent arguments
  
  df_tot_at_lease_one_ae <- merge(ASL[, c("USUBJID", "STUDYID")], AAE[, ...]) # aae USUBJID has NA
  
  tbl_at_lease_one_ae <- rtabulate(df_tot_at_lease_one_ae$"<var with NA>", function(x) {
    sum(x, na.rm = TRUE) * c(1, 1/length(x))
  }, row.name = "...")
  
  ## 
  tbl_n_ae <- rtabulate(AAE$USUBJID, col_by = AAE$ARM, length)

  
  ## Now create a list of tables, one table per term
  ## for now lets just create class - term structure
  ## we can look later into generalizing this to any depth
  df <- AAE[, c("USUBJID", "AESOC", "ARM", "AETERM")]
  
  # use factor levels for ordering
  df.s <- split(df, AAE$AESOC)
  
  lapply(df.s, function(df_i) {
    rtabulate(df_i, col_var = "ARM", )
  })

    
}