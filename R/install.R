

#' Matrix with dependency info
#' 
#' @export
#' 
#' @return a data.frame with a column for each dependent package and the rows
#'   correspond to recommended version combinations
#' 
#' 
#' 
#' @seealso \code{\link{install_pkg}}
#' 
#' @noRd
#' 
#' 
recomended_version_combinations <- function() {
  x <- matrix(
    c(
      "v0.0.1",     "v0.0.1",    "v0.0.1"
    ),
    dimnames = list(NULL, c("tern", "rtables", "random.cdisc.data")),
    byrow = TRUE,
    ncol = 3
  )
  
  as.data.frame(x, stringsAsFactors = FALSE)
}

#' Install a particular version of tern with the recommended versions of terns
#' dependencies
#' 
#' For production use one should install \code{tern} using this function so that
#' matching versions of tern and \code{random.cdisc.data} are installed. For a
#' first time installation use the instructions on
#' \url{GitHub repository README.md}{http://github.roche.com/Rpackages/tern}.
#' 
#' @param ref either \code{NULL}, \code{'master'}, \code{'devel'}, or a version
#'   tag (i.e. \code{'v0.0.1'}). If \code{NULL} then the current installed
#'   version is used.
#' @param dependencies one of \code{'all'}, \code{'depends'} or \code{'none'}
#' @param eval execute the installation. This is only useful to install
#'   \code{ref='master'}, \code{'devel'}, or a version the preceeds the current
#'   installed version
#' 
#' 
#' @details 
#' Currently the R packages \code{tern}, \code{rtables},
#' \code{random.cdisc.data}, \code{teal.oncology}, and \code{teal} are evolving
#' rapidly and we currently do not guarantee backwards compatibility. Therefore
#' \code{tern} and \code{teal.oncology} will provide the
#' \code{install_<pkg>} functions which provide install instructions for
#' recommended package versions.
#' 
#' 
#' @export
#' 
#' @examples 
#' install_tern(eval = FALSE)
#' install_tern('master', 'depends', eval = FALSE)
#' install_tern('devel', 'none', eval = FALSE)
install_tern <- function(ref = NULL, dependencies = c('all', 'depends', 'none'), eval = TRUE) {
  
  dependencies <- match.arg(dependencies)
  
  if (is.null(ref)) {
    ref <- paste0("v", packageDescription("tern")[['Version']])
  }
  
  if (!grepl("^v", ref) && !(ref %in% c('devel', 'master'))) stop("ref needs to be either a version, NULL, 'master', or 'devel'")
  
  if (grepl("^v", ref)) {
    rec_versions <- recomended_version_combinations()
    
    i <- match(ref, rec_versions$tern)
    
    if (is.na(i)) stop('no recommended version combination for tern ', ref, ' is  stored in the current installed package tern')
    
    ref_rtables <- rec_versions$rtables[i]
    ref_rcd <- rec_versions$random.cdisc.data[i]
    
  } else {
    ref_rtables <- ref
    ref_rcd <- ref
  }
  
  cl_all <- bquote({
    devtools::install_github("Rpackages/random.cdisc.data", ref = .(ref_rcd), host='https://github.roche.com/api/v3')
    devtools::install_github("Roche/rtables", ref = .(ref_rtables))
    devtools::install_github("Rpackages/tern", ref = .(ref), host='https://github.roche.com/api/v3')
  })

  
  cl <- switch(
    dependencies,
    all = cl_all,
    depends = cl_all[c(1,3,4)],
    none = cl_all[c(1, 4)]
  )
  
  if (eval) {
    eval(cl)
    invisible(cl)
  } else {
    cl
  }

}
  
  
  