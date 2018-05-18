.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste(
    "\n\nThis commit of tern is not valid. Install tern version 0.5.0 with:",
    '  devtools::install_github("Rpackages/tern", "v0.5.0", host = "https://github.roche.com/api/v3")',
    sep = "\n"
  ))
}

