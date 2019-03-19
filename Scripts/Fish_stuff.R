
library(tidyverse)
library(lubridate)
library(feather)
library(fields)

## depredation patterns around FKW bycatch

## summaries of SETS w/ FKWs
sets_deep_all %>% 
  filter(FKW == 1) %>% 
  summarize(
    n(),
    sum(as.numeric(as.character(MM_YN))),
    sum(as.numeric(MM_sum)),
    sum(as.numeric(DP_LAG1), na.rm = T)
  )


## summaries of TRIPS w/ FKW - rate of depred on trip, num caught, etc
sets_deep_all %>% 
  group_by(TRIP_ID) %>% 
  summarize(
    MM_YN = as.numeric(ifelse(1 %in% MM_YN, 1, 0)),
    FKW_YN = as.numeric(ifelse(1 %in% FKW, 1, 0)),
    FKWsum = sum(as.numeric(as.character(FKW))),
    MMrate = sum(MM_sum)/sum(NUM_HKS_SET)            # depredation / hks set on whole trip
  ) %>% 
  group_by(FKW_YN) %>% 
  summarize(
      MM = sum(MM_YN),
      total = n(),
      avg = MM/total,                # percent trips with depredation
      FKWmax = max(FKWsum),
      DPrate = mean(MMrate)               # average dp rate at trip level
  )



## add up total tuna lost, average weight?

catch_allyears_fish <- read_feather( "Data/catch_allyears_depred_losses.feather")
cat(paste(shQuote(colnames(catch_allyears_fish[,-c(1:50)]), type="cmd"), collapse=", "))

catch_allyears_fish <- catch_allyears_fish %>% 
  select(c("TRIP_ID", "VESSEL_ID", "DECLARED_TRIP_TYPE_CODE", "SET_NUM", "SET_BEGIN_TIME", "YEAR",
           "SET_BEGIN_DATETIME", "SET_BEGIN_LAT", "SET_BEGIN_LON", "SET_END_DATETIME", "SET_END_LAT", "SET_END_LON",
           "HAUL_BEGIN_DATETIME", "HAUL_BEGIN_DATE", "HAUL_BEGIN_TIME", "HAUL_BEGIN_LAT", "HAUL_BEGIN_LON", "HAUL_END_DATETIME", "HAUL_END_LAT", "HAUL_END_LON", 
           "NUM_HKS_SET", "NUM_FLTS", "HKS_PER_FLT", "SPECIES_CODE", "SPECIES_COMMON_NAME", 
           "FLT_NUM", "HK_NUM", "KEPT_RETURN_CODE_VAL", "DAMAGE_CODE", "DAMAGE_CODE_VAL",
           "MEAS1_TYPE_CODE", "MEAS1_TYPE_CODE_VAL", "MEAS1", "MEAS2_TYPE_CODE", "MEAS2_TYPE_CODE_VAL", "MEAS2", "MEAS3_TYPE_CODE", "MEAS3_TYPE_CODE_VAL", "MEAS3", 
           "MM", "SP", "KEPT", "TYPE", "FKW_caught", "SFPW_caught", "RISS_caught", "SWO", "BET", "YFT", "TUNA", "MAHI", "WAHOO", "BILLFISH", "NONTARGET"))

catch_allyears %>% filter(DECLARED_TRIP_TYPE_CODE == 'D') %>%
  count(SPECIES_COMMON_NAME, sort = T)


## list most caught/depredated species
View(catch_allyears %>%
       #filter(MM == 1) %>% 
       filter(DECLARED_TRIP_TYPE_CODE == 'D') %>%
       group_by(SPECIES_COMMON_NAME) %>%
       count(sort = 'T'))

## summaries of size..
View(catch_allyears_fish %>%
  filter(DECLARED_TRIP_TYPE_CODE == "D") %>% 
  filter(MEAS1_TYPE_CODE == "FL" | MEAS1_TYPE_CODE == "EF") %>% 
  filter(KEPT == 1, TYPE == "FISH") %>% 
  group_by(SPECIES_COMMON_NAME, MEAS1_TYPE_CODE) %>% 
  summarise(
    n(),
    mean(MEAS1),
    sd(MEAS1)
  ))

#####
#  collapse catch data to set data 
#####
sets_deep_fish <- catch_allyears_fish %>% 
  filter(DECLARED_TRIP_TYPE_CODE == 'D') %>% 
  group_by(TRIP_ID, SET_NUM) %>%
  summarise(
    VESSEL_ID = first(VESSEL_ID),
    DECLARED_TRIP = first(DECLARED_TRIP_TYPE_CODE),
    SET_BEGIN_DATETIME = first(SET_BEGIN_DATETIME),
    SET_END_DATETIME = first(SET_END_DATETIME),
    SET_BEGIN_LAT = first(SET_BEGIN_LAT),
    SET_BEGIN_LON = first(SET_BEGIN_LON),
    SET_END_LAT = first(SET_END_LAT),
    SET_END_LON = first(SET_END_LON),
    HAUL_BEGIN_DATETIME = first(HAUL_BEGIN_DATETIME),
    HAUL_BEGIN_DATE = first(HAUL_BEGIN_DATE),
    HAUL_END_DATETIME = first(HAUL_END_DATETIME),
    HAUL_BEGIN_LAT = first(HAUL_BEGIN_LAT),
    HAUL_BEGIN_LON = first(HAUL_BEGIN_LON),
    HAUL_END_LAT = first(HAUL_END_LAT),
    HAUL_END_LON = first(HAUL_END_LON),
    SOAK = as.numeric(HAUL_END_DATETIME - SET_END_DATETIME),
    
    # other gear chars
    NUM_FLTS = first(NUM_FLTS),
    NUM_HKS_SET = first(NUM_HKS_SET),
    HKS_PER_FLT = first(HKS_PER_FLT),

    # catch
    NUM_CAUGHT = n(),
    KEPT_n = sum(KEPT),
    
    FISH = sum(TYPE == 'FISH'),
    SHARKS = sum(TYPE == 'SHARK'),
    ODONT = sum(TYPE == 'ODONT'),
    TURTLES = sum(TYPE == 'TURTLE'),
    SWO = sum(SWO),
    BET_nall = sum(BET[KEPT == 1 | MM ==1]),                         ## n of kept or MM BET
    BET_avgL = 
      mean(MEAS1[BET == 1 & MEAS1_TYPE_CODE == 'FL']),               ## average fl of BET
    BET_n_meas = 
      sum(BET == 1 & MEAS1_TYPE_CODE == 'FL'),                       ## n of measured BET
    YFT_nall = sum(YFT[KEPT == 1 | MM ==1]),
    YFT_avgL = 
      mean(MEAS1[YFT == 1 & MEAS1_TYPE_CODE == 'FL']),
    YFT_n_meas = 
      sum(YFT == 1 & MEAS1_TYPE_CODE == 'FL'),
    TUNA_nall =                               
      sum(TUNA[(BET == 1 | YFT == 1) & (KEPT == 1 | MM == 1)]),      ## just BE and YF tuna
    TUNA_avgL = 
      mean(MEAS1[(BET == 1 | YFT == 1) & MEAS1_TYPE_CODE == 'FL']),
    TUNA_n_meas = 
      sum((BET == 1 | YFT == 1) & MEAS1_TYPE_CODE == 'FL'),
    MAHI_nall = sum(MAHI[KEPT == 1 | MM == 1]),                      ## n of kept or MM mahi
    MAHI_avgL = 
      mean(MEAS1[MAHI == 1 & MEAS1_TYPE_CODE == 'FL']),              ## average fl of mahi
    MAHI_n_meas = 
      sum(MAHI == 1 & MEAS1_TYPE_CODE == 'FL'),                      ## n of measured BET
    
    WAHOO_n = sum(WAHOO),
    BILLFISH_n = sum(BILLFISH),
    NONTARGET_n = sum(NONTARGET),
    
    CPUE = (FISH/NUM_HKS_SET)*1000,
    CPUE_FLT = (FISH/(NUM_FLTS)),
    ## depredation
    # mammals
    MM_YN = as.factor(ifelse('Marine mammal damage' %in% DAMAGE_CODE_VAL, 1, 0)),
    MM_sum = sum(MM),
    MM_TUNA = sum(TUNA[MM == 1]),
    MM_MAHI = sum(MAHI[MM == 1]),
    MM_BILL = sum(BILLFISH[MM == 1]),
    DPUE = (MM_sum/NUM_HKS_SET) *1000,
    DP = (MM_sum/KEPT_n),
    
    # sharks
    SP_any = as.factor(ifelse('SB' %in% DAMAGE_CODE | 'ST' %in% DAMAGE_CODE | 'SH' %in% DAMAGE_CODE, 1, 0)),
    SP_sum = sum(SP),
    
    # bycatch
    FKW = as.factor(ifelse('Whale, False Killer' %in% SPECIES_COMMON_NAME, 1, 0)),
    FKW_sum = sum(FKW_caught),
    SFPW = as.factor(ifelse('Whale, Short-finned Pilot' %in% SPECIES_COMMON_NAME, 1, 0)),
    SFPW_sum = sum(SFPW_caught),
    RISS = as.factor(ifelse('Dolphin, Risso\'s' %in% SPECIES_COMMON_NAME, 1, 0)),
    RISS_sum = sum(RISS_caught)
  ) %>%
  filter(!is.na(SET_BEGIN_DATETIME),!is.na(SET_END_DATETIME),!is.na(HAUL_BEGIN_DATETIME),!is.na(HAUL_END_DATETIME)) %>%
  filter(!is.na(HAUL_BEGIN_LAT), !is.na(HAUL_BEGIN_LON))

## fixing date as in original sets df - filtering function wasn't working with it as above..
sets_deep_fish <- as.data.frame(sets_deep_fish)
sets_deep_fish$HAUL_BEGIN_DATE <- as.character(sets_deep_fish$HAUL_BEGIN_DATETIME, tz="")
sets_deep_fish$HAUL_BEGIN_DATE <- sapply(strsplit(sets_deep_fish$HAUL_BEGIN_DATE, " "), "[", 1) #, "%m/%d/%Y")
sets_deep_fish$HAUL_BEGIN_DATE <- as.Date(sets_deep_fish$HAUL_BEGIN_DATE)
sets_deep_fish$HAUL_BEGIN_DATE <- format(sets_deep_fish$HAUL_BEGIN_DATE, "%m/%d/%Y")
sets_deep_fish$HAUL_BEGIN_DATE <- as.character.Date(sets_deep_fish$HAUL_BEGIN_DATE)

#####

# filtering function

#####

### makes matrix of rows coordinates,  puts all sets within specified space/time in temp file,
###  and averages specified value

nearby_catch <- function(x){
  coordinates <- matrix(c(as.numeric(x["HAUL_BEGIN_LON"]), as.numeric(x["HAUL_BEGIN_LAT"])), ncol=2)
  sets_deep_fish %>%
    filter(mdy(HAUL_BEGIN_DATE) >= mdy(x["HAUL_BEGIN_DATE"]) - days(1) &
             mdy(HAUL_BEGIN_DATE) <= mdy(x["HAUL_BEGIN_DATE"]) + days(1)) %>%
    filter(rdist.earth.vec(coordinates,
                           matrix(c(HAUL_BEGIN_LON, HAUL_BEGIN_LAT), ncol=2),
                           miles = F, R = 6371) <= 100) %>% 
    summarise(sum(BET_avgL*BET_n_meas, na.rm = T)/sum(BET_n_meas, na.rm = T)) %>% as_vector()
    #summarise(sum(BET_n_meas, na.rm=T)) %>% as_vector()
    #summarise(sum(YFT_avgL*YFT_n_meas, na.rm = T)/sum(YFT_n_meas, na.rm = T)) %>% as_vector()
    #summarise(sum(YFT_n_meas, na.rm=T)) %>% as_vector()
    #summarise(sum(MAHI_avgL*MAHI_n_meas, na.rm = T)/sum(MAHI_n_meas, na.rm = T)) %>% as_vector()
    #summarise(sum(MAHI_n_meas, na.rm=T)) %>% as_vector()
}

#(sum(BET_avgL*BET_n_meas)/sum(BET_n_meas))
### assign value for each row to corresponding row in df
sets_deep_fish[, "BET_avgL_1d_100k"] <- apply(sets_deep_fish, 1, nearby_catch)
sets_deep_fish[, "BET_n_1d_100k"] <- apply(sets_deep_fish, 1, nearby_catch)
sets_deep_fish[, "BET_avgL_3d_200k"] <- apply(sets_deep_fish, 1, nearby_catch)
sets_deep_fish[, "BET_n_3d_200k"] <- apply(sets_deep_fish, 1, nearby_catch)

?apply

## diff in BET size
sets_deep_fish %>% 
  filter(DECLARED_TRIP =='D') %>%  
  ggplot(aes(x = BET_avgL_1d_100k, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 10 )


sets_deep_fish %>% 
  summarise(
    
  )












