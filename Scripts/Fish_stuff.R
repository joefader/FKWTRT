
library(tidyverse)
library(lubridate)
library(feather)
library(fields)
library(reshape2)

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
       filter(MM == 1) %>% 
       filter(YEAR >= 2004) %>% 
       filter(DECLARED_TRIP_TYPE_CODE == 'D') %>%
       group_by(SPECIES_COMMON_NAME) %>%
       count(sort = 'T')) 


## summaries of size..
View(catch_allyears_fish %>%
  filter(DECLARED_TRIP_TYPE_CODE == "D") %>% 
  filter(MEAS1_TYPE_CODE == "FL" | MEAS1_TYPE_CODE == "EF") %>% 
  #filter(KEPT == 1, TYPE == "FISH") %>% 
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
    
    WAHOO_nall = sum(WAHOO[KEPT == 1 | MM == 1]),
    WAHOO_avgL = 
      mean(MEAS1[WAHOO == 1 & MEAS1_TYPE_CODE == 'FL']),
    WAHOO_n_meas = 
      sum(WAHOO == 1 & MEAS1_TYPE_CODE == 'FL'),
    BILLFISH_nall = sum(BILLFISH[KEPT == 1 | MM ==1]),
    BILLFISH_avgL = 
      mean(MEAS1[BILLFISH == 1 & MEAS1_TYPE_CODE == 'EF']),
    BILLFISH_n_meas = 
      sum(BILLFISH == 1 & MEAS1_TYPE_CODE == 'EF'),
    NONTARGET_n = sum(NONTARGET),
    
    CPUE = (FISH/NUM_HKS_SET)*1000,
    CPUE_FLT = (FISH/(NUM_FLTS)),
    ## depredation
    # mammals
    MM_YN = as.factor(ifelse('Marine mammal damage' %in% DAMAGE_CODE_VAL, 1, 0)),
    MM_any = as.factor(ifelse('Marine mammal damage' %in% DAMAGE_CODE_VAL | 'Whale, False Killer' %in% SPECIES_COMMON_NAME, 1, 0)),
    MM_sum = sum(MM),
    MM_BET = sum(BET[(
      SPECIES_COMMON_NAME == 'Tuna, Bigeye') & (MM == 1)]),
    MM_YFT = sum(YFT[(
      SPECIES_COMMON_NAME == 'Tuna, Yellowfin') & (MM == 1)]),
    MM_UNID_TUNA = sum(TUNA[(
      SPECIES_COMMON_NAME == 'Tuna, Unidentified') & (MM == 1)]),
    MM_MAHI = sum(MAHI[MM == 1]),
    MM_BILL = sum(BILLFISH[MM == 1]),
    MM_WAHOO = sum(WAHOO[MM == 1]),
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

sets_deep_fish[is.na(sets_deep_fish) ] <- NA


## fixing date as in original sets df - filtering function wasn't working with it as above..
sets_deep_fish <- as.data.frame(sets_deep_fish)
sets_deep_fish$HAUL_BEGIN_DATE <- as.character(sets_deep_fish$HAUL_BEGIN_DATETIME, tz="")
sets_deep_fish$HAUL_BEGIN_DATE <- sapply(strsplit(sets_deep_fish$HAUL_BEGIN_DATE, " "), "[", 1) #, "%m/%d/%Y")
sets_deep_fish$HAUL_BEGIN_DATE <- as.Date(sets_deep_fish$HAUL_BEGIN_DATE)
sets_deep_fish$HAUL_BEGIN_DATE <- format(sets_deep_fish$HAUL_BEGIN_DATE, "%m/%d/%Y")
sets_deep_fish$HAUL_BEGIN_DATE <- as.character.Date(sets_deep_fish$HAUL_BEGIN_DATE)

## time stuff..
sets_deep_fish$MONTH <- as.numeric(format(sets_deep_fish$SET_BEGIN_DATETIME, "%m"))
sets_deep_fish$YEAR <- as.numeric(format(sets_deep_fish$SET_BEGIN_DATETIME, "%Y"))


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
    #summarise(sum(BET_avgL*BET_n_meas, na.rm = T)/sum(BET_n_meas, na.rm = T)) %>% as_vector()
    #summarise(sum(BET_n_meas, na.rm=T)) %>% as_vector()
    #summarise(sum(YFT_avgL*YFT_n_meas, na.rm = T)/sum(YFT_n_meas, na.rm = T)) %>% as_vector()
    #summarise(sum(YFT_n_meas, na.rm=T)) %>% as_vector()
    #summarise(sum(MAHI_avgL*MAHI_n_meas, na.rm = T)/sum(MAHI_n_meas, na.rm = T)) %>% as_vector()
    #summarise(sum(MAHI_n_meas, na.rm=T)) %>% as_vector()
    summarise(n()) %>% as_vector()
}

#(sum(BET_avgL*BET_n_meas)/sum(BET_n_meas))
### assign value for each row to corresponding row in df
sets_deep_fish[, "BET_avgL_1d_100k"] <- apply(sets_deep_fish, 1, nearby_catch)
sets_deep_fish[, "BET_n_1d_100k"] <- apply(sets_deep_fish, 1, nearby_catch)
sets_deep_fish[, "YFT_avgL_1d_100k"] <- apply(sets_deep_fish, 1, nearby_catch)
sets_deep_fish[, "YFT_n_1d_100k"] <- apply(sets_deep_fish, 1, nearby_catch)
sets_deep_fish[, "MAHI_avgL_1d_100k"] <- apply(sets_deep_fish, 1, nearby_catch)
sets_deep_fish[, "MAHI_n_1d_100k"] <- apply(sets_deep_fish, 1, nearby_catch)
sets_deep_fish[, "num_vessels_1d_100k"] <- apply(sets_deep_fish, 1, nearby_catch)

sets_deep_fish[, "BET_avgL_3d_200k"] <- apply(sets_deep_fish, 1, nearby_catch)
sets_deep_fish[, "BET_n_3d_200k"] <- apply(sets_deep_fish, 1, nearby_catch)

?apply


## making a backup copy of 'filtered' stuff in case 
## need to make changes to original fish/sets df -- must join back to main df
sets_deep_fish_filterhold <- sets_deep_fish %>% 
  select(c("TRIP_ID", "VESSEL_ID", "SET_NUM", "BET_avgL_1d_100k":"num_vessels_1d_100k"))

sets_deep_fish <- left_join(sets_deep_fish, sets_deep_fish_filterhold2, by=c("TRIP_ID", "VESSEL_ID", "SET_NUM"))

sets_deep_fish[is.na(sets_deep_fish) ] <- NA


## diff in BET size
sets_deep_fish %>% 
  filter(DECLARED_TRIP =='D') %>%  
  #ggplot(aes(x = BET_avgL_1d_100k, y = ..density.., colour = MM_YN)) +
  ggplot(aes(x = BET_avgL_1d_100k)) +
  geom_histogram()
#  geom_freqpoly(binwidth = 10 )

2.77562E-05*(114^2.93652)*5000*5/1000/14*2204*5
## estimating biomass lost
## 
# BET LWR = 2.77562E-05*BET_avgL_1d_100k^2.93652
# YFT LWR = 3.16534E-05*YFT_n_1d_100k^2.88938
# WAHOO LWR = 1.4157E-07*FL^3.3034
# MAHI LWR = 1.0693E-05*MAHI_avgL_1d_100k^2.9337         ## female LWR
# MAHI LWR = 8.0856E-06*MAHI_avgL_1d_100k^3.0157         ## male LWR - males bigger, not pooled in Ushiyama, could find sex ratio but for now just using fems to be conservative 

## if no fish nearby (filter estimates NA), subsituting avg for species across all sets
## doing by set, if use catch data frame gives same answer within a thousandth..
sets_deep_fish %>% 
  #group_by(YEAR) %>% 
  summarise(
    sum(BET_avgL*BET_n_meas, na.rm = T)/sum(BET_n_meas, na.rm = T),
    sum(YFT_avgL*YFT_n_meas, na.rm = T)/sum(YFT_n_meas, na.rm = T),
    sum(MAHI_avgL*MAHI_n_meas, na.rm = T)/sum(MAHI_n_meas, na.rm = T)
  )

## replace Nas from filtered stuff with averages
sets_deep_fish <- sets_deep_fish %>% 
  replace_na(list(
    BET_avgL_1d_100k = 112.4046, 
    YFT_avgL_1d_100k = 108.9714,
    MAHI_avgL_1d_100k = 85.89522))

## add up depredated catch by month and year
vessel_by_mo <- sets_deep_fish %>% 
  filter(YEAR >= 2004) %>% 
  group_by(YEAR) %>% 
  summarise(
    BET = sum((2.77562E-05*BET_avgL_1d_100k^2.93652)*MM_BET, na.rm=T)*5 / 1000,
    YFT = sum((3.16534E-05*YFT_avgL_1d_100k^2.88938)*MM_YFT, na.rm=T)*5 / 1000,
    UNID_TUNA = sum(
      ((YFT_n_1d_100k / (BET_n_1d_100k + YFT_n_1d_100k)) * MM_UNID_TUNA *
         (3.16534E-05*YFT_n_1d_100k^2.88938)) + 
      ((BET_n_1d_100k / (BET_n_1d_100k + YFT_n_1d_100k)) * MM_UNID_TUNA *
         (2.77562E-05*BET_avgL_1d_100k^2.93652)), na.rm = T)*5 / 1000,
    MM_TUNA_MT = sum(BET + YFT + UNID_TUNA),
    MAHI = sum((1.0693E-05*MAHI_avgL_1d_100k^2.9337)*MM_MAHI, na.rm=T)*5 / 1000,
    tots = sum(BET + YFT + UNID_TUNA + MAHI) 
    ) %>%
  summarise(
    mean(MM_TUNA_MT),
    sd(MM_TUNA_MT),
    min(MM_TUNA_MT),
    max(MM_TUNA_MT)
  )
  


#########################
# auction data
#####

## data acquired from POP website scrubbed using Parse Hub. 2012-2018 seem clean and straightforward,
## 2011 and earlier have duplicate days and other inconsistencies that dont always make sense. Could 
## delete duplicates or otherwise clean, but for now considering 2012 and later only


POP_2012 <- read_csv("Data/POP_auction/POP_2012.csv")
POP_2013 <- read_csv("Data/POP_auction/POP_2013.csv")
POP_2014 <- read_csv("Data/POP_auction/POP_2014.csv")
POP_2015 <- read_csv("Data/POP_auction/POP_2015.csv")
POP_2016 <- read_csv("Data/POP_auction/POP_2016.csv")
POP_2017 <- read_csv("Data/POP_auction/POP_2017.csv")
POP_2018 <- read_csv("Data/POP_auction/POP_2018.csv")

POP_all <- rbind(POP_2012, POP_2013, POP_2014, POP_2015, POP_2016, POP_2017, POP_2018)

## collects each day on single line, separated by | bc commas in dataset already
POP_all_csv <- POP_all %>%
  group_by(day) %>%
  summarise(field=paste(field, collapse='|'))

POP_all_csv$all <- paste(POP_all_csv$day, "|", POP_all_csv$field)    # add delim bt day and others
POP_all_csv <- POP_all_csv %>% select(all)                            # get rid of extra cols
write.table(POP_all_csv, 
          "Data/POP_auction/POP_all_full.csv", quote = F, sep = "|")   # write csv, get rid of quotes
max(count.fields("Data/POP_auction/POP_all_full.csv", sep = '|'))      # max number of cols in df
POP_all_full <- read.table("Data/POP_auction/POP_all_full.csv",       # fill allows empty cols
                            fill = T, header = F, sep = "|", 
                            col.names = c("ID", "day", 
                            "row1", "type1a", "count1a", "weight1a", "price1a", "type1b", "count1b", "weight1b", "price1b",
                            "row2", "type2a", "count2a", "weight2a", "price2a", "type2b", "count2b", "weight2b", "price2b",
                            "row3", "type3a", "count3a", "weight3a", "price3a", "type3b", "count3b", "weight3b", "price3b",
                            "row4", "type4a", "count4a", "weight4a", "price4a", "type4b", "count4b", "weight4b", "price4b",
                            "row5", "type5a", "count5a", "weight5a", "price5a", "type5b", "count5b", "weight5b", "price5b",
                            "row6", "type6a", "count6a", "weight6a", "price6a", "type6b", "count6b", "weight6b", "price6b",
                            "row7", "type7a", "count7a", "weight7a", "price7a", "type7b", "count7b", "weight7b", "price7b",
                            "row8", "type8a", "count8a", "weight8a", "price8a", "type8b", "count8b", "weight8b", "price8b",
                            "row9", "type9a", "count9a", "weight9a", "price9a", "type9b", "count9b", "weight9b", "price9b",
                            "row10", "type10a", "count10a", "weight10a", "price10a", "type10b", "count10b", "weight10b", "price10b",
                            "row11", "type11a", "count11a", "weight11a", "price11a", "type11b", "count11b", "weight11b", "price11b",
                            "row12", "type12a", "count12a", "weight12a", "price12a", "type12b", "count12b", "weight12b", "price12b"), 
                            
                            colClasses = c(rep("character", 110)))

POP_all_full <- POP_all_full[-1,-1]         # drop useless 1st row and col

#View(POP_all_full[,c(1,90:109)])           # viewing extra cols, only one day with 12 rows but looks fine
#################
#########################
## found a few more errors/duplicates
#########################
# Mar 22, 2012 - extra misc rows, deleted the 6th row
# May 04, 2012 - two tunas in first row, spread out with misc/zeros
# may 8, 2012 - two exact days, deleted one in 2012 csv
# jun 7, 2012 - 3 rows 1st, 5 rows 2nd, changed to 8 total rows
# Jul 18, 2012 - duplicate misc row - deleted
# Aug 31, 2012 - two pages of 3 rows, changed 2nd to 4, 5, 6
# Oct 21, 2012 - row 4 just tuna, throws off data frame - added misc for row 4 but put zeros

# Jan 15, 2013 - row 1 w 2 misc, labeled one as row 2
# Jan 17, 2013 - 2 row 2 misc but none in row 3, moved 1 to row 3
# Jan 18, 2013 - misassigned row for row 1, moved misc up from 2 to 1
# Jan 21, 2013 - 4 rows of tuna then jumps to 6th for misc, just lumped misc with 4th tuna
# Apr 25, 2013 - rows 6 and 7 only misc, deleting
# Jun 07, 2013 - duplicate days, deleted one
# Jul 11, 2013 - extra misc row, deleted
# Jul 29, 2013 - misc row 6 instead of 4, changed
# Oct 23, 2013 - 2 tuna in first row, combine with avgs

# oct 12, 2015 - different, 5 rows first day 4 rows 2nd, changed 2nd day rows to 6-9
################
## converting to tidy format
################

# converting to tidy format now..
cols <- c("day", "row", "type", "count", "weight", "price")       # col names for tidy dataset
POP_all_1a <- POP_all_full[,1:6] %>% setNames(.,cols)             # 
POP_all_1b <- POP_all_full[,c(1:2, 7:10)] %>% setNames(.,cols)

POP_all_2a <- POP_all_full[,c(1, 11:15)] %>% setNames(.,cols)
POP_all_2b <- POP_all_full[,c(1, c(11,16:19))] %>% setNames(.,cols)

POP_all_3a <- POP_all_full[,c(1, 20:24)] %>% setNames(.,cols)
POP_all_3b <- POP_all_full[,c(1, c(20, 25:28))] %>% setNames(.,cols)

POP_all_4a <- POP_all_full[,c(1, 29:33)] %>% setNames(.,cols)
POP_all_4b <- POP_all_full[,c(1, c(29, 34:37))] %>% setNames(.,cols)

POP_all_5a <- POP_all_full[,c(1, 38:42)] %>% setNames(.,cols)
POP_all_5b <- POP_all_full[,c(1, c(38, 43:46))] %>% setNames(.,cols)

POP_all_6a <- POP_all_full[,c(1, 47:51)] %>% setNames(.,cols)
POP_all_6b <- POP_all_full[,c(1, c(47, 52:55))] %>% setNames(.,cols)

POP_all_7a <- POP_all_full[,c(1, 56:60)] %>% setNames(.,cols)
POP_all_7b <- POP_all_full[,c(1, c(56, 61:64))] %>% setNames(.,cols)

POP_all_8a <- POP_all_full[,c(1, 65:69)] %>% setNames(.,cols)
POP_all_8b <- POP_all_full[,c(1, c(65, 70:73))] %>% setNames(.,cols)

POP_all_9a <- POP_all_full[,c(1, 74:78)] %>% setNames(.,cols)
POP_all_9b <- POP_all_full[,c(1, c(74, 79:82))] %>% setNames(.,cols)

POP_all_10a <- POP_all_full[,c(1, 83:87)] %>% setNames(.,cols)
POP_all_10b <- POP_all_full[,c(1, c(83, 88:91))] %>% setNames(.,cols)

POP_all_11a <- POP_all_full[,c(1, 92:96)] %>% setNames(.,cols)
POP_all_11b <- POP_all_full[,c(1, c(92, 97:100))] %>% setNames(.,cols)

POP_all_12a <- POP_all_full[,c(1, 101:105)] %>% setNames(.,cols)
POP_all_12b <- POP_all_full[,c(1, c(101, 106:109))] %>% setNames(.,cols)

POP_all_tidy <- rbind(POP_all_1a, POP_all_1b, POP_all_2a, POP_all_2b, POP_all_3a, POP_all_3b,
                      POP_all_4a, POP_all_4b, POP_all_5a, POP_all_5b, POP_all_6a, POP_all_6b,
                      POP_all_7a, POP_all_7b, POP_all_8a, POP_all_8b, POP_all_9a, POP_all_9b,
                      POP_all_10a, POP_all_10b, POP_all_11a, POP_all_11b, POP_all_12a, POP_all_12b) %>% 
                na_if(., "") %>% 
                drop_na()

summary(POP_all_tidy)

# fixin up the vars
POP_all_tidy$day <- dmy(POP_all_tidy$day)
POP_all_tidy$MONTH <- as.numeric(format(POP_all_tidy$day, "%m"))
POP_all_tidy$YEAR <- as.numeric(format(POP_all_tidy$day, "%Y"))

POP_all_tidy$row <- as.integer(POP_all_tidy$row)
POP_all_tidy$count <- as.integer(gsub("," ,"", POP_all_tidy$count))

POP_all_tidy$weight <- as.numeric(gsub(",", "", POP_all_tidy$weight))
POP_all_tidy$price <- as.numeric(gsub("\\$", "", POP_all_tidy$price))

####################
##add it up
####################

write_feather(POP_all_tidy, "Data/POP_auction/POP_all_tidy.feather")
POP_all_tidy <- read_feather("Data/POP_auction/POP_all_tidy.feather")


sales_by_mo <- POP_all_tidy %>% 
  filter(type == 'Tuna', YEAR < 2018) %>% 
  group_by(YEAR, MONTH) %>% 
  summarise(
    pounds_auction = sum(weight),       # metric tons by month
    dollars_auction = sum(weight * price),
    avg_perlb = dollars_auction / pounds_auction
  )


depred_costs <- left_join(sales_by_mo, vessel_by_mo, by = c("YEAR", "MONTH"))

depred_costs <- depred_costs %>% 
  mutate(MM_TUNA_LBS = MM_TUNA_MT * 2204.62)  %>%    # convert to lbs
  mutate(MM_TUNA_cost = MM_TUNA_LBS * avg_perlb) 

depred_costs %>% 
  group_by(YEAR) %>% 
  summarise(
    cost_fleet = sum(MM_TUNA_cost),
    cost_per_boat = cost_fleet / 153
) %>% 
  summarise(
    fleet_mean = mean(cost_fleet), 
    sd = sd(cost_fleet),
    boat_mean = fleet_mean / 153
  )


vessel_by_mo %>% 
  filter(YEAR >= 2012) %>% 
  summarize(
    
  )
  







