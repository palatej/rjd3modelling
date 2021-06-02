#' @include utils.R
NULL

j2r_ldt<-function(ldt){
  if (is.jnull(ldt))
    return (NULL)
  dt<-.jcall(ldt, "Ljava/time/LocalDate;", "toLocalDate")
  return (as.Date(.jcall(dt, "S", "toString")))
}

j2r_dt<-function(dt){
  if (is.jnull(dt))
    return (NULL)
  return (as.Date(.jcall(dt, "S", "toString")))
}

r2j_dt<-function(dt){
  jdt<-.jnew("java/lang/String", as.character(dt))
  return (.jcall("java/time/LocalDate", "Ljava/time/LocalDate;", "parse", .jcast(jdt, "java/lang/CharSequence")))
}

r2j_ldt<-function(dt){
  jdt<-r2j_dt(dt)
  return (.jcall(jdt, "Ljava/time/LocalDateTime;", "atStartOfDay"))
}


jd2r_parameters <- function(jparams){
  if (is.jnull(jparams))
    return(NULL)
  param<-.jcastToArray(jparams)
  len <- length(param)
  if (len==0)
    return (NULL)
  param_name <- deparse(substitute(jparams))
  Type <- sapply(param, function(x) .jcall(.jcall(x, "Ldemetra/data/ParameterType;", "getType"), "S", "name"))
  Value <- sapply(param, function(x) .jcall(x, "D", "getValue"))
  data_param <- data.frame(Type = Type, Value = Value)
  rownames(data_param) <- sprintf("%s(%i)",
                                  param_name,
                                  1:len)
  data_param
}

jdomain<-function(period, start, end){
  if (period == 0)return (.jnull("demetra/timeseries/TsDomain"))
  n<-period*(end[1]-start[1])+end[2]-start[2]
  jdom<-.jcall("demetra/timeseries/r/TsUtility", "Ldemetra/timeseries/TsDomain;", "of"
               , as.integer(period), as.integer(start[1]), as.integer(start[2]), as.integer(n))
  return (jdom)
}
