#' .onAttach
#'
#' Verifies that the package is running with RStudio open. RStudio needed for
#' secure password input.
#'

.onAttach <- function(libname, pkgname) {
  #rstudioapi::verifyAvailable(version_needed = NULL)
}

