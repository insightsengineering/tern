.onAttach <- function(libname, pkgname) { # nolint
  return()
  packageStartupMessage(paste(
    "\nThis is a beta version for tern v0.6.1. Install the current stable tern v0.6.0 with:",
    '  devtools::install_github("NEST/tern", "v0.6.0", host = "https://github.roche.com/api/v3")',
    sep = "\n"
  ))
}
