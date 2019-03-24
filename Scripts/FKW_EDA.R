#############################################################################
# EDA on FKW
# Joseph Fader
# MArch, 2019

library(tidyverse)
library(feather)
library(lubridate)
library(fields)



# depredation on previous sets when FKW caught

sets_deep_all %>% 
  filter(MM_any == 1) %>% 
  group_by(FKW) %>% 
  summarize(
    n(),
    sum(DP_LAG1, na.rm = T),
    sum(DP_LAG2, na.rm = T),
    mean(DP_LAG_NUM, na.rm = T),
    mean(cpue_avg_1d_100k, na.rm = T)
    )

## appears not anymore likely to have depredation around when fkw caught..
sets_deep_fish %>% 
  filter(MM_any == 1) %>% 
  group_by(FKW) %>% 
  summarize(
    n(),
    mean(BET_avgL_1d_100k, na.rm = T),
    sum(MM_any_1d_b4_100k),
    mean((MM_any_1d_b4_100k - 1) / (n_vessels_1d_100k - 1), na.rm = T)
  )


# filtering function - to see if fkw sets have dp/fkws nearby also 

#####

### makes matrix of rows coordinates,  puts all sets within specified space/time in temp file,
###  and averages specified value

nearby_depred <- function(x){
  coordinates <- matrix(c(as.numeric(x["HAUL_BEGIN_LON"]), as.numeric(x["HAUL_BEGIN_LAT"])), ncol=2)
  sets_deep_fish %>%
    filter(mdy(HAUL_BEGIN_DATE) >= mdy(x["HAUL_BEGIN_DATE"]) - days(1) &
             mdy(HAUL_BEGIN_DATE) <= mdy(x["HAUL_BEGIN_DATE"]) + days(0)) %>%
    filter(rdist.earth.vec(coordinates,
                           matrix(c(HAUL_BEGIN_LON, HAUL_BEGIN_LAT), ncol=2),
                           miles = F, R = 6371) <= 100) %>% 
  summarise(sum(MM_any, na.rm = T)) %>% as_vector()
  #summarise(n()) %>% as_vector()
}

### assign value for each row to corresponding row in df
sets_deep_fish[, "MM_any_1d_b4_100k"] <- apply(sets_deep_fish, 1, nearby_depred)
sets_deep_fish[, "n_vessels_1d_100k"] <- apply(sets_deep_fish, 1, nearby_depred)
sets_deep_fish[, "n_vessels_3d_100k"] <- apply(sets_deep_fish, 1, nearby_depred)




## what do boats do after depredation?







