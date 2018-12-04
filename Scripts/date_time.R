library(lubridate)
library(tidyverse)


###########################################################################
## time between sets
###########################################################################

# identify variable type
sapply (all_sets.date, class) 
sapply (all_sets, class) 

## code example
#t2 = as.POSIXct(all_sets[1,7], format = "%m/%d/%Y %H:%M:%S", tz = Sys.timezone())
t1 = mdy_hms(all_sets[1,7], tz = Sys.timezone())
t2 = mdy_hms(all_sets[2,7], tz = Sys.timezone())
difftime(t2,t1, units = "min")
sapply(t2, class)

## date time
all_sets.date <- all_sets
all_sets.date$SET_BEGIN_DATETIME<- mdy_hms(all_sets.date$SET_BEGIN_DATETIME, tz = "")

## remove nas?? careful..if do diff after this then any after na will be inflated
# sum(is.na(all_sets.date$SET_BEGIN_DATETIME))
# which(is.na(all_sets.date$SET_BEGIN_DATETIME))
# all_sets.date <- all_sets.date[-which(is.na(all_sets.date$SET_BEGIN_DATETIME)),]

## diff in rows, NAs for 1st sets and extreme values from missing set 1s
## might not be the best way, some sets with really huge lags that appear real
all_sets.date$set.lag <- c(NA, diff(all_sets.date$SET_BEGIN_DATETIME, lag=1, difference=1))
all_sets.date[which(all_sets.date$SET_NUM == 1), "set.lag"] <- NA
all_sets.date[which(all_sets.date$set.lag > 20160), "set.lag"] <- NA ## arbitrarily setting to 2 weeks
all_sets.date[which(all_sets.date$set.lag < 0), "set.lag"] <- NA
## days column
all_sets.date$set.lag.days <- all_sets.date$set.lag / 60 / 24

## check out min and max easily
View(arrange(all_sets.date, set.lag))


    
# histo of set lag
all_sets.date %>%
  filter(DECLARED_TRIP == 'D') %>%
  ggplot(aes(set.lag)) + geom_histogram(binwidth = 1000)




## split date/time into new columns
all_years$SET_BEGIN_DATE <- as.Date(sapply(strsplit(all_years$SET_BEGIN_DATETIME, " "), "[", 1), "%m/%d/%Y")
# all_years$SET_BEGIN_TIME <- sapply(strsplit(all_years$SET_BEGIN_DATETIME, " "), "[", 2)
# all_years$HAUL_BEGIN_DATE <- sapply(strsplit(all_years$HAUL_BEGIN_DATETIME, " "), "[", 1)
# all_years$HAUL_BEGIN_TIME <- sapply(strsplit(all_years$HAUL_BEGIN_DATETIME, " "), "[", 2)



library(fields)
sets_lag <- sets_alldata %>%
  select(1:20) %>%
  mutate(PREV_LAT = lag(HAUL_END_LAT)) %>%
  mutate(PREV_LON = lag(HAUL_END_LON))


# first column lon, second lat
rdist.earth.vec(sets_endhaul, sets_beginset, miles = F, R = 6371)


sets_alldata <- sets_alldata %>%
  mutate(LAG_DIST = rdist.earth.vec(matrix(c(sets_lag$SET_BEGIN_LON, sets_lag$SET_BEGIN_LAT), ncol=2), matrix(c(lag(HAUL_END_LON), lag(HAUL_END_LAT)), ncol=2), miles = F, R = 6371))




