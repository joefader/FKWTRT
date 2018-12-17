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

# create column in df to accept averaged values
sets_deep_all <- sets_deep_all %>% mutate(avg_cpue = 0)

# make test dataset to play with
sets_deep_test <- sets_deep_all %>% select(-c(SOAK:NONTARGET)) %>% filter(YEAR == 2010 & MONTH == 5)


# filtering function, finds all sets within specified space/time and puts in temp file
filter_func <- function(x){
    sets_deep_test %>%
      filter(mdy(HAUL_BEGIN_DATE) >= mdy(x["HAUL_BEGIN_DATE"]) - days(1) &
                     mdy(HAUL_BEGIN_DATE) <= mdy(x["HAUL_BEGIN_DATE"]) + days(1)) %>%
      summarise(mean(CPUE, na.rm=T)) %>% as_vector()
  }

sets_deep_test[, "avg_cpue"] <- apply(sets_deep_test, 1, filter_func)



#### replicate for distance
filter_dist <- function(x){
  sets_deep_test %>%
    filter(mdy(HAUL_BEGIN_DATE) >= mdy(x["HAUL_BEGIN_DATE"]) - days(1) &
             mdy(HAUL_BEGIN_DATE) <= mdy(x["HAUL_BEGIN_DATE"]) + days(1)) %>%
    rdist.earth.vec(matrix(c(HAUL_BEGIN_LON, HAUL_BEGIN_LAT), ncol=2),
                    matrix(c(x["HAUL_BEGIN_LON"], x['HAUL_BEGIN_LAT']), ncol=2), 
                           miles = F, R = 6371) 
    #summarise(mean(CPUE, na.rm=T)) %>% as_vector()
}

sets_deep_test[, "avg_cpue"] <- apply(sets_deep_test, 1, filter_dist)

rdist.earth

rdist.earth
#
sets_alldata <- sets_alldata %>%
  mutate(LAG_DIST = rdist.earth.vec(matrix(c(SET_BEGIN_LON, SET_BEGIN_LAT), ncol=2), matrix(c(lag(HAUL_END_LON), lag(HAUL_END_LAT)), ncol=2), miles = F, R = 6371))
sets_alldata[which(sets_alldata$SET_NUM == 1), "LAG_DIST"] <- NA






