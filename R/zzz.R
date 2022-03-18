
.onLoad <- function(libname, pkgname) {
  if (! requireNamespace("rjd3toolkit", quietly=T)) stop("Loading rjd3 libraries failed")
}
