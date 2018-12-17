#############################################################################
# Finding near vessels, computing averages eg cpue
# Joseph Fader
# December, 2018


library(tidyverse)
library(lubridate)
library(chron)
library(ecodist)
library(feather)

sets_deep_all <- read_feather("Data/sets_deep_all.feather")


#############################################################################
### 
#############################################################################
# 1. function to filter by time and distance
# 2. apply by row
# filter by sets within time window (1 day either side?)
# 2. filter by sets within distance x
# 3. include current set

sapply(sets_deep_all, class)

testday <- mdy(sets_deep_all$HAUL_BEGIN_DATE[1000])

testday - days(3)

filter_func <- function(df, haulday){
    temp <- filter(df, mdy(HAUL_BEGIN_DATE) >= testday - days(3) & mdy(HAUL_BEGIN_DATE) <= testday + days(3))
    View(temp)
    mean(temp$CPUE)
  }

filter_func(sets_deep_all, testday)

sets_deep_all %>% 
  mutate(HAUL_BEGIN_DATE)
mutate(DP_LAG_NUM = ifelse(SET_NUM == (lag(SET_NUM) + 1), DP_LAG_NUM, NA))

sets_alldata <- sets_alldata %>%
  mutate(LAG_DIST = rdist.earth.vec(matrix(c(SET_BEGIN_LON, SET_BEGIN_LAT), ncol=2), matrix(c(lag(HAUL_END_LON), lag(HAUL_END_LAT)), ncol=2), miles = F, R = 6371))
sets_alldata[which(sets_alldata$SET_NUM == 1), "LAG_DIST"] <- NA






