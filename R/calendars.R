#' @include utils.R
NULL

#' Create a calendar
#'
#'
#'
#' @examples
#' belgiumCalendar<-calendar.new()
#' calendar.fixedday(belgiumCalendar, 7, 21)
#' calendar.holiday(belgiumCalendar, "NEWYEAR")
#' calendar.holiday(belgiumCalendar, "CHRISTMAS")
#' calendar.holiday(belgiumCalendar, "MAYDAY")
#' calendar.holiday(belgiumCalendar, "EASTERMONDAY")
#' calendar.holiday(belgiumCalendar, "WHITMONDAY")
#' calendar.holiday(belgiumCalendar, "ASSUMPTION")
#' calendar.holiday(belgiumCalendar, "ALLSAINTDAY")
#' calendar.holiday(belgiumCalendar, "ARMISTICE")
#' M<-td(12, c(1980,1), 120, c(1,1,1,1,2,3,0), contrasts = FALSE)
#'
#' H<-htd(belgiumCalendar, 12, c(1980,1), 120, c(1,1,1,1,1,2,0), contrasts = FALSE)
#'
#' MC<-td(4, c(1980,1), 120, c(1,1,1,1,1,2,0), contrasts = TRUE)
#' HC<-htd(belgiumCalendar, 4, c(1980,1), 120, c(1,1,1,1,1,2,0), contrasts = TRUE)
#'
#' C12<-longTermMean(belgiumCalendar, 12)
#' C4<-longTermMean(belgiumCalendar, 4)
#'
#' C12bis<-longTermMean(belgiumCalendar, 12, c(1,1,1,1,1,2,0))
#' C4bis<-longTermMean(belgiumCalendar, 4, c(1,1,1,1,1,2,0))
#'
#' print(C12)
#' print(C12bis)
#' @export
calendar.new<-function(){
  return (jd3.Calendar$new())
}



validityPeriod<-function(start, end){
  vp<-jd3.ValidityPeriod$new()
  if (is.null(start)) {
    pstart=DATE_MIN
  }else{
    pstart=parseDate(start)
  }
  if (is.null(end)){
    pend=DATE_MAX
  }else{
    pend=parseDate(end)
  }
  vp$start<-pstart
  vp$end<-pend
  return (vp)
}
#' @importFrom stats is.mts ts
length_ts <- function(s){
  if(is.mts(s)){
    nrow(s)
  }else{
    length(s)
  }
}
#' Add fixed day to a calendar
#'
#' @param calendar The calendar.
#' @param month,day the month and the day of the fixed day to add.
#' @param weight weight associated to the holiday.
#' @param start,end Validity period of the holiday in the format \code{"YYYY-MM-DD"}.
#'
#'
#' @examples
#' calendar <-calendar.new()
#' calendar.fixedday(calendar, 1, 1) # add New-Year
#' calendar.fixedday(calendar, 12, 24) # add Christmas
#' @export
calendar.fixedday<-function(calendar, month, day, weight=1, start=NULL, end=NULL){
  fd<-jd3.FixedDay$new()
  fd$month<-month
  fd$day<-day
  fd$weight<-weight
  fd$validity<-validityPeriod(start, end)
  n<-1+length(calendar$fixed_days)
  calendar$fixed_days[[n]]<-fd
}

#' Add fixed week day to a calendar
#'
#' @inheritParams calendar.fixedday
#' @param month the month of the day to add: from 1 (January) to 12 (December).
#' @param week the number of the week of the day to add: from 1 (first week of the month) to 5.
#' @param dayofweek the day of the week: from 1 (Monday) to 7 (Sunday).
#'
#'
#' @export
calendar.fixedweekday<-function(calendar, month, week,
                                dayofweek, weight=1, start=NULL, end=NULL){
  fd<-jd3.FixedWeekDay$new()
  fd$month<-month
  fd$position <- week
  fd$weekday <- dayofweek
  fd$weight<-weight
  fd$validity<-validityPeriod(start, end)
  n<-1+length(calendar$fixed_week_days)
  calendar$fixed_week_days[[n]]<-fd
}

#' Add Easter related day to a calendar
#'
#' @inheritParams calendar.fixedday
#' @param offset The position of the holiday in relation to the Easter Sunday, measured in days (can be positive or negative).
#' @param julian boolean indicating if julian calendar must be used.
#'
#' @export
#' @examples
#' calendar <- calendar.new()
#' calendar.easter(calendar, 1) # add Easter Monday
#' calendar.easter(calendar, -2) # add Easter Good Friday
calendar.easter<-function(calendar, offset, julian=FALSE, weight=1, start=NULL, end=NULL){
  ed<-jd3.EasterRelatedDay$new()
  ed$offset<-offset
  ed$julian<-julian
  ed$weight<-weight
  ed$validity<-validityPeriod(start, end)
  n<-1+length(calendar$easter_related_days)
  calendar$easter_related_days[[n]]<-ed
}

#' Add specific holiday to a calendar
#'
#' @inheritParams calendar.easter
#' @param offset The position of the holiday in relation to the selected pre-specified holiday measured in days (can be positive or negative).
#' By default \code{offset = 0}.
#' @param event the event to add (see details).
#'
#' @details Possible values :
#'
#' \tabular{ll}{
#' NEWYEAR        \tab Fixed holiday, falls on January, 1.                                                  \cr
#' SHROVEMONDAY   \tab Moving holiday, falls on Monday before Ash Wednesday (48 days before Easter Sunday). \cr
#' SHROVETUESDAY  \tab Moving holiday, falls on Tuesday before Ash Wednesday (47 days before Easter Sunday).\cr
#' ASHWEDNESDAY   \tab Moving holiday, occurring 46 days before Easter Sunday.                              \cr
#' MAUNDYTHURSDAY \tab Moving holiday, falls on the Thursday before Easter.                                 \cr
#' GOODFRIDAY     \tab Moving holiday, falls on the Friday before Easter.                                   \cr
#' EASTER         \tab Moving holiday, varies between March, 22 and April, 25.                              \cr
#' EASTERMONDAY   \tab Moving holiday, falls on the day after Easter.                                       \cr
#' ASCENSION      \tab Moving holiday, celebrated on Thursday, 39 days after Easter.                        \cr
#' PENTECOST      \tab Moving holiday, celebrated 49 days after Easter Sunday.                              \cr
#' WHITMONDAY     \tab Moving holiday, falling on the day after Pentecost.                                  \cr
#' CORPUSCHRISTI  \tab Moving holiday, celebrated 60 days after Easter Sunday.                              \cr
#' JULIANEASTER   \tab                                                                                      \cr
#' MAYDAY         \tab Fixed holiday, falls on May, 1.                                                      \cr
#' ASSUMPTION     \tab Fixed holiday, falls on August, 15.                                                  \cr
#' HALLOWEEN      \tab Fixed holiday, falls on October, 31.                                                 \cr
#' ALLSAINTDAY    \tab Fixed holiday, falls on November, 1.                                                 \cr
#' ARMISTICE      \tab Fixed holiday, falls on November, 11.                                                \cr
#' CHRISTMAS      \tab Fixed holiday, falls on December, 25.
#' }
#'
#'
#' @export
#' @examples
#' calendar <- calendar.new()
#' calendar.holiday(calendar, "EASTERMONDAY") # add Easter Monday
calendar.holiday<-function(calendar, event, offset=0, weight=1, start=NULL, end=NULL){
  pd<-jd3.PrespecifiedHoliday$new()
  pd$event<-.JD3_ENV$enum_of(jd3.CalendarEvent, event, "HOLIDAY")
  pd$offset<-offset
  pd$weight<-weight
  pd$validity<-validityPeriod(start, end)
  n<-1+length(calendar$prespecified_holidays)
  calendar$prespecified_holidays[[n]]<-pd
}

p2jd_calendar<-function(pcalendar){
  bytes<-pcalendar$serialize(NULL)
  jcal<-.jcall("demetra/calendar/r/Calendars", "Ldemetra/timeseries/calendars/Calendar;",
               "calendarOf", bytes)
  return (jcal)
}
group_names <- function(x, contrasts = TRUE){
  if(!is.matrix(x))
    return(x)
  col_names <- seq_len(ncol(x)) - !contrasts #if !constrast then it starts from 0
  colnames(x) <- sprintf("group-%i", col_names)
  x
}

#' Usual trading days variables
#'
#' @param frequency Annual frequency. Should be a divisor of 12.
#' @param start Array with the first year and the first period (for instance \code{c(1980, 1)}).
#' @param length Length of the variables
#' @param groups Groups of days. The length of the array must be 7. It indicates to what group each week day
#' belongs. The first item corresponds to Mondays and the last one to Sundays. The group used for contrasts (usually Sundays) is identified by 0.
#' The other groups are identified by 1, 2,... n (<= 6). For instance, usual trading days are defined by c(1,2,3,4,5,6,0),
#' week days by c(1,1,1,1,1,0,0), week days, Saturdays, Sundays by c(1,1,1,1,1,2,0) etc...
#' @param contrasts If true, the variables are defined by contrasts with the 0-group. Otherwise, raw number of days are provided
#'
#' @return The variables corresponding to each group, starting with the 0-group (\code{contrasts = FALSE})
#' or the 1-group (\code{contrasts = TRUE}).
#' @export
#'
#' @examples
td<-function(frequency, start, length, groups=c(1,2,3,4,5,6,0), contrasts=TRUE){
  jdom<-tsdomain_r2jd(frequency, start[1], start[2], length)
  igroups<-as.integer(groups)
  jm<-.jcall("demetra/modelling/r/Variables", "Ldemetra/math/matrices/MatrixType;",
             "td", jdom, igroups, contrasts)
  data <- matrix_jd2r(jm)
  data <- group_names(data, contrasts = contrasts)
  return (ts(data, start = start, frequency = frequency))
}

#' @param s time series used to get the dates for the trading days variables.
#' @export
#' @rdname td
td.forTs<-function(s, groups=c(1,2,3,4,5,6,0), contrasts=TRUE){
  if (! is.ts(s)) stop("s should be a time series")
  return (td(frequency(s), start(s), length_ts(s), groups, contrasts))
}

#' Calendar specific trading days variables
#'
#' @inheritParams td
#' @param calendar The calendar.
#' @param meanCorrection boolean indicating if the regressors are corrected for long-term term.
#' By default the correction is done if \code{contrasts = TRUE}.
#'
#' @return The variables corresponding to each group, starting with the 0-group (\code{contrasts = FALSE})
#' or the 1-group (\code{contrasts = TRUE}).
#' @export
#'
#' @examples
htd<-function(calendar,frequency, start, length, groups=c(1,2,3,4,5,6,0), contrasts=TRUE,
              meanCorrection = contrasts){
  jdom<-tsdomain_r2jd(frequency, start[1], start[2], length)
  jcal<-p2jd_calendar(calendar)
  r.Variables <- J("demetra/modelling/r/Variables")
  # jm<-.jcall("demetra/modelling/r/Variables", "Ldemetra/math/matrices/MatrixType;",
  #            "htd", jcal, jdom, as.integer(groups), contrasts, meanCorrection)
  jm <- r.Variables$htd(jcal, jdom, as.integer(groups), contrasts, meanCorrection)
  return <- matrix_jd2r(jm)
  return <- group_names(return, contrasts = contrasts)
  return (ts(return, start = start, frequency = frequency))
}

#' @param s The time series.
#' @export
#' @rdname htd
htd.forTs<-function(s, calendar, groups = c(1,2,3,4,5,6,0), contrasts = TRUE){
  if (! is.ts(s)) stop("s should be a time series")
  return (htd(calendar, frequency(s), start(s), length_ts(s), groups, contrasts))
}


#' Gets the days corresponding to the holidays
#'
#' @param calendar The calendar
#' @param start First day of the calendar in the format \code{"YYYY-MM-DD"}.
#' @param length Length of the calendar.
#' @param nonworking Indexes of non working days (Monday=1, Sunday=7).
#' @param type Adjustment type when a holiday falls a week-end: \code{"NextWorkingDay"},
#' \code{"PreviousWorkingDay"},
#' \code{"Skip"} (holidays corresponding to non working days are simply skipped in the matrix),
#' \code{"All"} (holidays are always put in the matrix, even if they correspond to a non working day).
#'
#' @returns A matrix where each column is associated to a holiday (in the order of creation of the holiday) and each row to a date.
#'
#' @examples
#' belgiumCalendar<-calendar.new()
#' calendar.fixedday(belgiumCalendar, 7, 21)
#' calendar.holiday(belgiumCalendar, "NEWYEAR")
#' calendar.holiday(belgiumCalendar, "CHRISTMAS")
#' calendar.holiday(belgiumCalendar, "CHRISTMAS", offset=1, weight=.5)
#' calendar.holiday(belgiumCalendar, "MAYDAY")
#' calendar.holiday(belgiumCalendar, "EASTERMONDAY")
#' calendar.holiday(belgiumCalendar, "WHITMONDAY")
#' calendar.holiday(belgiumCalendar, "ASSUMPTION")
#' calendar.holiday(belgiumCalendar, "ALLSAINTDAY")
#' calendar.holiday(belgiumCalendar, "ARMISTICE")
#' q<-holidays(belgiumCalendar, "2021-01-01", 365.25*10, type="NextWorkingDay")
#' plot(apply(q,1, max))
#' @export
holidays<-function(calendar, start, length, nonworking=c(6,7), type=c("Skip", "All", "NextWorkingDay", "PreviousWorkingDay")){
  type<-match.arg(type)
  jcal<-p2jd_calendar(calendar)
  jm<-.jcall("demetra/calendar/r/Calendars", "Ldemetra/math/matrices/MatrixType;",
             "holidays", jcal, as.character(start), as.integer(length), .jarray(as.integer(nonworking)), type)
  res <- matrix_jd2r(jm)
  rownames(res) <- as.character(seq(as.Date(start), length.out = nrow(res), by="days"))
  return (res)

}

#' Long-term means of a calendar
#'
#' @inheritParams htd
#'
#' @return The long term means corresponding to each group/period, starting with the 0-group
#' @export
#'
#' @examples
longTermMean<-function(calendar,frequency,groups=c(1,2,3,4,5,6,0)){
  jcal<-p2jd_calendar(calendar)
  jm<-.jcall("demetra/calendar/r/Calendars", "Ldemetra/math/matrices/MatrixType;",
             "longTermMean", jcal, as.integer(frequency), as.integer(groups))
  res <- matrix_jd2r(jm)
  return (group_names(res, contrasts = FALSE))
}

#' Compute Easter days between two years
#'
#' @param year0,year1 years.
#' @inheritParams calendar.easter
#'
#' @export
#'
#' @examples
#' easter.dates(2020, 2021)
easter.dates<-function(year0, year1, julian = FALSE){
  dates<-.jcall("demetra/calendar/r/Calendars", "[S", "easter", as.integer(year0), as.integer(year1), as.logical(julian))
  return (sapply(dates, as.Date))
}

#' Stock Trading days
#'
#' @inheritParams td
#' @param w indicates day of the month when inventories and other stock are reported (to denote the last day of the month enter 31).
#' @export
stock.td<-function(frequency, start, length, w = 31){
  jdom <- tsdomain_r2jd(frequency, start[1], start[2], length)
  r.Variables <- J("demetra/modelling/r/Variables")
  data <-r.Variables$stockTradingDays(jdom, as.integer(w))
  data <- matrix_jd2r(data)
  colnames(data) <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  return (ts(data, frequency = frequency, start= start))
}
