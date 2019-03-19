#############################################################################
# Finding near vessels, computing averages eg cpue
# Joseph Fader
# December, 2018


library(tidyverse)
library(lubridate)
#library(chron) - blocks days function in lubridate
library(feather)
library(fields)

sets_deep_all <- read_feather("Data/sets_deep_all.feather")


#############################################################################
### 
#############################################################################
# 1. function to filter by time and distance
# 2. apply by row


sapply(sets_deep_all, class)

## make sure adding date works..lubridate
# testday <- mdy(sets_deep_all$HAUL_BEGIN_DATE[1000])
# testday - days(3)

# create column in df to accept averaged values -- not needed, apply will add
# sets_deep_all <- sets_deep_all %>% mutate(avg_cpue = 0) %>% mutate(avg_cpue = 0)

## make test dataset to play with
# sets_deep_test <- sets_deep_all %>% select(-c(SOAK:NONTARGET)) %>% filter(YEAR == 2010 & MONTH ==5)

### filtering function: makes matrix of rows coordinates,
### puts all sets within specified space/time in temp file, and
### averages specified value (cpue here)

filter_dist <- function(x){
  coordinates <- matrix(c(as.numeric(x["HAUL_BEGIN_LON"]), as.numeric(x["HAUL_BEGIN_LAT"])), ncol=2)
  sets_deep_all %>%
    filter(mdy(HAUL_BEGIN_DATE) >= mdy(x["HAUL_BEGIN_DATE"]) - days(1) &
             mdy(HAUL_BEGIN_DATE) <= mdy(x["HAUL_BEGIN_DATE"]) + days(1)) %>%
    filter(rdist.earth.vec(coordinates,
                           matrix(c(HAUL_BEGIN_LON, HAUL_BEGIN_LAT), ncol=2),
                           miles = F, R = 6371) <= 100) %>% 
   summarise(mean(CPUE, na.rm=T)) %>% as_vector()
}

### assign value for each row to corresponding row in df
sets_deep_all[, "cpue_avg_3d_100k"] <- apply(sets_deep_all, 1, filter_dist)
sets_deep_all[, "cpue_avg_1d_100k"] <- apply(sets_deep_all, 1, filter_dist)

### same function as above but gives 'n' for how many were averaged
filter_n <- function(x){
  coordinates <- matrix(c(as.numeric(x["HAUL_BEGIN_LON"]), as.numeric(x["HAUL_BEGIN_LAT"])), ncol=2)
  sets_deep_all %>%
    filter(mdy(HAUL_BEGIN_DATE) >= mdy(x["HAUL_BEGIN_DATE"]) - days(1) &
             mdy(HAUL_BEGIN_DATE) <= mdy(x["HAUL_BEGIN_DATE"]) + days(1)) %>%
    filter(rdist.earth.vec(coordinates,
                           matrix(c(HAUL_BEGIN_LON, HAUL_BEGIN_LAT), ncol=2),
                           miles = F, R = 6371) <= 100) %>% 
    summarise(n()) %>% as_vector()
}

sets_deep_all[, "num_vessels_3d_100k"] <- apply(sets_deep_all, 1, filter_n)
sets_deep_all[, "num_vessels_1d_100k"] <- apply(sets_deep_all, 1, filter_n)


View(sets_deep_all[,90:102])

write_feather(sets_deep_all, "Data/sets_deep_all.feather")
sets_deep_all <- read_feather("Data/sets_deep_all.feather")

