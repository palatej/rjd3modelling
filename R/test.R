GaliciaCalendar <- calendar.new()

calendar.fixedday(GaliciaCalendar, month = 3, day = 19, start="1995-03-19", end="1995-04-19")
calendar.fixedday(GaliciaCalendar, month = 3, day = 19, start="1998-01-01", end="1999-12-31")

calendar.fixedday(GaliciaCalendar, month = 6, day = 24, start="2013-01-01", end="2013-12-31")
calendar.fixedday(GaliciaCalendar, month = 6, day = 24, start="2016-01-01", end="2016-12-31")
calendar.fixedday(GaliciaCalendar, month = 6, day = 24, start="2020-01-01", end="2020-12-31")

#calendar.singledate(GaliciaCalendar, "1995-03-19")
#calendar.singledate(GaliciaCalendar, "1998-03-19")
#calendar.singledate(GaliciaCalendar, "1999-03-19")

#calendar.singledate(GaliciaCalendar, "2013-06-24")
#calendar.singledate(GaliciaCalendar, "2016-06-24")
#calendar.singledate(GaliciaCalendar, "2020-06-24")


Days_off<-holidays(GaliciaCalendar, "1960-01-01", 365.25*140, nonworking=7, type="All")
Days_off["1960-03-19",] # It is considering the days off defined for 1995 and 1998
Days_off["1960-06-24",] # It is considering the days off defined for 2013, 2016 and 2020
