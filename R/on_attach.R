.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste(
    "\nThis is a beta version for tern v0.5.1. Install the current stable tern v0.5.0 with:",
    '  devtools::install_github("Rpackages/tern", "v0.5.0", host = "https://github.roche.com/api/v3")',
    sep = "\n"
  ))
}
