
DATE_MIN<-NULL
DATE_MAX<-NULL


.onLoad <- function(libname, pkgname) {

  DATE_MIN<<-dateOf(1,1,1)
  DATE_MAX<<-dateOf(9999, 12, 31)

}

