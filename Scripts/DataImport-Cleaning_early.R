#############################################################################
# Data import and cleaning of HI PLL observer collected data for FKWTRT analysis
# Joseph Fader
# January 4, 2018

###headers
##sub steps
#notes
library(tidyverse)
library(lubridate)
library(chron)
library(feather)


#############################################################################
### get data into R

## import catch datasets into R objects - sourced from raw csv files in project data folder - original xlsx files backed up separately

LL2003 <- read.csv("Data/raw-csvs/LL2003.csv")
LL2004 <- read.csv("Data/raw-csvs/LL2004.csv")

## combine into single data frame - raw file so do not need to re-import indiv years
# DO NOT DELETE OR MODIFY 'all_years_catch_raw' ----- now 'raw_catch_allyears

# note warning messages for 'invalid factor levels'
catch_early <- rbind(LL2003, LL2004)
## remove single years from R environment - do after step below - making working copy
rm(LL2003, LL2004, LL2005, LL2006, LL2007, LL2008, LL2009, LL2010, LL2011, LL2012, LL2013, LL2014, LL2015, LL2016, LL2017)

write.csv(catch_allyears_raw, "Data/catch_allyears_raw.csv")
write.csv(catch_early, "Data/catch_early_raw.csv")

rm(catch_allyears_raw)
write_feather(catch_allyears_raw, "Data/catch_allyears_raw.feather")
write_feather(catch_early, "Data/catch_early_raw.feather")

#############################################################################
#################### TO START OVER - START HERE ##################
####################################################################################

## create working copy from the raw data frame
catch_allyears <- read.csv("Data/catch_allyears_raw.csv")
catch_early <- read_feather("Data/catch_early_raw.feather")
####################################################################################
### check and set variable types
####################################################################################

sapply(catch_allyears, class)
summary(catch_allyears)

## convert certain factors to numeric (num hooks, light sticks)
catch_early$NUM_HKS_SET <- as.numeric(catch_early$NUM_HKS_SET)
catch_early$NUM_LITE_DEVICES <- as.numeric(catch_early$NUM_LITE_DEVICES)

## convert character to factor
catch_early$EXPR_TRIP_TYPE_CODE <- as.factor((catch_early$EXPR_TRIP_TYPE_CODE))

summary(catch_early$EXPR_TRIP_TYPE_CODE)

## here as.character.Date - alternative to lubridate/posix
catch_early$SET_BEGIN_DATETIME <- as.character.Date(catch_early$SET_BEGIN_DATETIME)
catch_early$SET_END_DATETIME <- as.character.Date(catch_early$SET_END_DATETIME)
catch_early$HAUL_BEGIN_DATETIME <- as.character(catch_early$HAUL_BEGIN_DATETIME)
catch_early$HAUL_END_DATETIME <- as.character.Date(catch_early$HAUL_END_DATETIME)

## split date/time into new columns...only works if as.character.Date
catch_early$SET_BEGIN_TIME <- sapply(strsplit(catch_early$SET_BEGIN_DATETIME, " "), "[", 2)
catch_early$HAUL_BEGIN_DATE <- as.Date(sapply(strsplit(catch_early$HAUL_BEGIN_DATETIME, " "), "[", 1), "%m/%d/%Y")
catch_early$HAUL_BEGIN_TIME <- sapply(strsplit(catch_early$HAUL_BEGIN_DATETIME, " "), "[", 2)

## convert date time
# using lubridate which stores as POSIX
catch_early$SET_BEGIN_DATETIME <- mdy_hms(catch_early$SET_BEGIN_DATETIME, tz="")
catch_early$SET_END_DATETIME <- mdy_hms(catch_early$SET_END_DATETIME, tz="")
catch_early$HAUL_BEGIN_DATETIME <- mdy_hms(catch_early$HAUL_BEGIN_DATETIME, tz="")
catch_early$HAUL_END_DATETIME <- mdy_hms(catch_early$HAUL_END_DATETIME, tz="")
#catch_allyears$SET_BEGIN_TIME <- hms(catch_allyears$SET_BEGIN_DATETIME, tz="")
catch_early$HAUL_BEGIN_DATE <- mdy(catch_early$HAUL_BEGIN_DATE, tz="")
#catch_allyears$HAUL_BEGIN_TIME <- hms(catch_allyears$HAUL_BEGIN_TIME, tz="")

####################################################################################
### new variables
####################################################################################

## indicator for MM damage
catch_early <- mutate(catch_early, MM = ifelse(DAMAGE_CODE_VAL == 'Marine mammal damage', 1, 0))
## indicator for Kept
catch_early <- mutate(catch_early, KEPT = ifelse(KEPT_RETURN_CODE_VAL == 'Kept', 1, 0))

## code sps type - so can sum all fish and other catch categories
catch_early <- mutate(catch_early, TYPE = ifelse(grepl('Tuna', SPECIES_COMMON_NAME) | grepl('Fish', SPECIES_COMMON_NAME) |
                                                       grepl('fish', SPECIES_COMMON_NAME) | grepl('Pomfret', SPECIES_COMMON_NAME) |
                                                       grepl('Marlin', SPECIES_COMMON_NAME) | grepl('Mola', SPECIES_COMMON_NAME) |
                                                       grepl('Barracuda', SPECIES_COMMON_NAME) | grepl('Escolar', SPECIES_COMMON_NAME) |
                                                       grepl('Mackerel', SPECIES_COMMON_NAME) | grepl('Jack', SPECIES_COMMON_NAME) |
                                                       grepl('Puffer', SPECIES_COMMON_NAME) | grepl('Dolphinfish', SPECIES_COMMON_NAME) |
                                                       SPECIES_COMMON_NAME == 'Opah' | SPECIES_COMMON_NAME == 'Wahoo' |
                                                       SPECIES_COMMON_NAME == 'Louvar' | SPECIES_COMMON_NAME == 'Yellowtail', 'FISH',
                                                ifelse(SPECIES_COMMON_NAME == 'Whale, False Killer' | SPECIES_COMMON_NAME == 'Whale, Short-finned Pilot' |
                                                       SPECIES_COMMON_NAME == 'Dolphin, Risso\'s' | SPECIES_COMMON_NAME == 'Whale, Pygmy Killer' |
                                                       grepl('Dolphin', SPECIES_COMMON_NAME), 'ODONT',
                                                ifelse(grepl('Shark', SPECIES_COMMON_NAME), 'SHARK',
                                                ifelse(grepl('Turtle', SPECIES_COMMON_NAME), 'TURTLE',
                                                       'OTHER')))))
                    

## MM bycatch
unique(catch_early$SPECIES_COMMON_NAME)
catch_early <- mutate(catch_early, FKW_caught = ifelse(SPECIES_COMMON_NAME == 'Whale, False Killer', 1, 0))
catch_early <- mutate(catch_early, SFPW_caught = ifelse(SPECIES_COMMON_NAME == 'Whale, Short-finned Pilot', 1, 0))
catch_early <- mutate(catch_early, RISS_caught = ifelse(SPECIES_COMMON_NAME == 'Dolphin, Risso\'s', 1, 0))

## target species
# note including all not just 'kept' fish

## SWO
catch_early <- mutate(catch_early, SWO = ifelse(SPECIES_COMMON_NAME == 'Swordfish', 1, 0))
## BET
catch_early <- mutate(catch_early, BET = ifelse(SPECIES_COMMON_NAME == 'Tuna, Bigeye', 1, 0))
## all tunas
catch_early <- mutate(catch_early, TUNA = ifelse(grepl('Tuna', SPECIES_COMMON_NAME), 1, 0))
## mahi
catch_early <- mutate(catch_early, MAHI = ifelse(grepl('Dolphinfish', SPECIES_COMMON_NAME), 1, 0))

# ## hooks
# catch_allyears <- mutate(catch_allyears, HK_TYPE =ifelse(is.na(HKS_PERCENTAGE_1) | HKS_PERCENTAGE_1 == 100, HK_TYPE_CODE_VAL_1,
#                                                   ifelse(HKS_PERCENTAGE_1 > HKS_PERCENTAGE_2 & HKS_PERCENTAGE_1 > HKS_PERCENTAGE_3 &
#                                                       HKS_PERCENTAGE_1 > HKS_PERCENTAGE_4, HK_TYPE_CODE_VAL_1,
#                                                   ifelse(HKS_PERCENTAGE_2 > HKS_PERCENTAGE_1 & HKS_PERCENTAGE_2 > HKS_PERCENTAGE_3 &
#                                                       HKS_PERCENTAGE_2 > HKS_PERCENTAGE_4, HK_TYPE_CODE_VAL_2,
#                                                   ifelse(HKS_PERCENTAGE_3 > HKS_PERCENTAGE_1 & HKS_PERCENTAGE_3 > HKS_PERCENTAGE_2 &
#                                                       HKS_PERCENTAGE_3 > HKS_PERCENTAGE_4, HK_TYPE_CODE_VAL_3,
#                                                   ifelse(HKS_PERCENTAGE_4 > HKS_PERCENTAGE_1 & HKS_PERCENTAGE_4 > HKS_PERCENTAGE_2 &
#                                                       HKS_PERCENTAGE_4 > HKS_PERCENTAGE_3, HK_TYPE_CODE_VAL_4,
#                                                       NA))))))
# catch_allyears <- mutate(catch_allyears, HK_PCT =ifelse(HKS_PERCENTAGE_1 == 100, HKS_PERCENTAGE_1,
#                                                         ifelse(HKS_PERCENTAGE_1 > HKS_PERCENTAGE_2 & HKS_PERCENTAGE_1 > HKS_PERCENTAGE_3 &
#                                                           HKS_PERCENTAGE_1 > HKS_PERCENTAGE_4, HKS_PERCENTAGE_1,
#                                                         ifelse(HKS_PERCENTAGE_2 > HKS_PERCENTAGE_1 & HKS_PERCENTAGE_2 > HKS_PERCENTAGE_3 &
#                                                                  HKS_PERCENTAGE_2 > HKS_PERCENTAGE_4, HKS_PERCENTAGE_2,
#                                                                ifelse(HKS_PERCENTAGE_3 > HKS_PERCENTAGE_1 & HKS_PERCENTAGE_3 > HKS_PERCENTAGE_2 &
#                                                                         HKS_PERCENTAGE_3 > HKS_PERCENTAGE_4, HKS_PERCENTAGE_3,
#                                                                       ifelse(HKS_PERCENTAGE_4 > HKS_PERCENTAGE_1 & HKS_PERCENTAGE_4 > HKS_PERCENTAGE_2 &
#                                                                                HKS_PERCENTAGE_4 > HKS_PERCENTAGE_3, HKS_PERCENTAGE_4,
#                                                                              NA))))))
#         
# hks <- catch_allyears %>%
#   select(contains("HK"))
# lowpercent <- hks %>%
#   filter(HKS_PERCENTAGE_1 < 20)

####################################################################################
### filter out undeclared sets, invalid trips, experimental trips
####################################################################################

# only using 2004-2017, 6-22-2004 was start of declaring deep vs shallow
# not sure what to do with undeclared sets in 03-04, but for now filtering out any undeclared sets
# note including all except invalid trips, might not be appropriate to include unfinalized versions in analysis

# all sets pre 6/22/04 should be deep sets, filtering for these although recording MM damage started in August 03
catch_deep_early <- catch_early %>%
  filter(DECLARED_TRIP_TYPE_CODE != 'S' | is.na(DECLARED_TRIP_TYPE_CODE)) %>%
  filter(TRIP_APPROVAL_STATUS != 'INVALID') %>%
  filter(HAUL_BEGIN_DATE > "2003-08-20") #earliest found MM damagae is 2003-08-24 07:07:00 -  wont filter by date, leaving in for now
summary(catch_early)
unique(catch_deep_early$DECLARED_TRIP_TYPE_CODE)


## write out prepped catch file
write.csv(catch_allyears, "Data/catch_allyears_prepd.csv")
summary(catch_allyears)
rm(catch_allyears)
## feather version conserves data types
write_feather(catch_allyears, "Data/catch_allyears_prepd.feather")

####################################################################################
#################### START HERE for prepped catch data ##################
####################################################################################

catch_allyears <- read_feather("Data/catch_allyears_prepd.feather")
catch_allyears <- read.csv("Data/catch_allyears_prepd.csv", header = TRUE)
summary(catch_allyears)
catch_allyears <- as.data.frame(catch_allyears)

####################################################################################
### sets - aggregate catch data to set level
####################################################################################

sets_deep_early <- group_by(catch_deep_early, TRIP_ID, SET_NUM) %>%
  summarise(
    VESSEL_ID = first(VESSEL_ID),
    DECLARED_TRIP = first(DECLARED_TRIP_TYPE_CODE),
    #EXPR_TRIP_TYPE_CODE = first(EXPR_TRIP_TYPE_CODE),
    SET_BEGIN_DATETIME = first(SET_BEGIN_DATETIME),
    SET_BEGIN_TIME = first(SET_BEGIN_TIME),
    SET_END_DATETIME = first(SET_END_DATETIME),
    SET_BEGIN_LAT = first(SET_BEGIN_LAT),
    SET_BEGIN_LON = first(SET_BEGIN_LON),
    SET_END_LAT = first(SET_END_LAT),
    SET_END_LON = first(SET_END_LON),
    HAUL_BEGIN_DATETIME = first(HAUL_BEGIN_DATETIME),
    HAUL_END_DATETIME = first(HAUL_END_DATETIME),
    HAUL_BEGIN_DATE = first(HAUL_BEGIN_DATE),
    HAUL_BEGIN_TIME = first(HAUL_BEGIN_TIME),
    HAUL_BEGIN_LAT = first(HAUL_BEGIN_LAT),
    HAUL_BEGIN_LON = first(HAUL_BEGIN_LON),
    HAUL_END_LAT = first(HAUL_END_LAT),
    HAUL_END_LON = first(HAUL_END_LON),
    SOAK = as.numeric(HAUL_END_DATETIME - SET_END_DATETIME),
    
    # hooks, leave out for now
    #HOOK_TYPE = first(HK_TYPE_CODE_VAL_1),
    #HOOK_SZ = first(SZ_HKS_1),
  
    # other gear chars
    NUM_FLTS = first(NUM_FLTS),
    NUM_HKS_SET = first(NUM_HKS_SET),
    HKS_PER_FLT = first(HKS_PER_FLT),
    
    # length/depth
    FLTLN_LEN = first(FLTLN_LEN),
    BRNCHLN_LEN = first(BRNCHLN_LEN),
    LDR_LEN = first(LDR_LEN),
    MIN_LEN = sum(FLTLN_LEN + BRNCHLN_LEN + LDR_LEN),
    
    # bait etc
    BLUEDYE_YN = first(DS_BLUEDYED_YN),
    BLUEDYE = ifelse(BLUEDYE_YN == "Y", 1, 0),
    BAIT_CODE_VAL = first(BAIT_CODE_VAL),
    NUM_LITE_DEVICES = first(NUM_LITE_DEVICES),
    
    # catch
    NUM_CAUGHT = n(),
    KEPT = sum(KEPT),
    
    FISH = sum(TYPE == 'FISH'),
    SHARKS = sum(TYPE == 'SHARK'),
    ODONT = sum(TYPE == 'ODONT'),
    TURTLES = sum(TYPE == 'TURTLE'),
    SWO = sum(SWO),
    BET = sum(BET),
    TUNA = sum(TUNA),
    MAHI = sum(MAHI),
    
    CPUE = (FISH/NUM_HKS_SET)*1000,
    #DPUF = (MM_sum/(1+TARGET_sum))
    
    # mammals
    MM_YN = as.factor(ifelse('Marine mammal damage' %in% DAMAGE_CODE_VAL, 1, 0)),
    MM_sum = sum(MM),
    #MM_prop = (MM_sum/FISH),
    DPUE = (MM_sum/NUM_HKS_SET) *1000,
    DP = (MM_sum/KEPT),
    
    FKW = as.factor(ifelse('Whale, False Killer' %in% SPECIES_COMMON_NAME, 1, 0)),
    FKW_sum = sum(FKW_caught),
    SFPW = as.factor(ifelse('Whale, Short-finned Pilot' %in% SPECIES_COMMON_NAME, 1, 0)),
    SFPW_sum = sum(SFPW_caught),
    RISS = as.factor(ifelse('Dolphin, Risso\'s' %in% SPECIES_COMMON_NAME, 1, 0)),
    RISS_sum = sum(RISS_caught)
  ) %>%
  filter(!is.na(SET_BEGIN_DATETIME),!is.na(SET_END_DATETIME),!is.na(HAUL_BEGIN_DATETIME),!is.na(HAUL_END_DATETIME)) %>%
  filter(!is.na(HAUL_BEGIN_LAT), !is.na(HAUL_BEGIN_LON))

sapply(sets_deep_early, class)
summary(sets_deep_early)


####################################################################################
### add some time/date stuff
####################################################################################

sets_deep_early <- as.data.frame(sets_deep_early)

sets_deep_early$MONTH <- as.numeric(format(sets_deep_early$SET_BEGIN_DATE, "%m"))
sets_deep_early$YEAR <- as.numeric(format(sets_deep_early$SET_BEGIN_DATE, "%Y"))

## deep and shallow - make haul day in format for GIS
# to split must be in character
# then set to date before changing to desired format - reconvert to string?
sets_deep_early$HAUL_BEGIN_DATE <- as.character(sets_deep_early$HAUL_BEGIN_DATETIME, tz="Pacific/Honolulu")
sets_deep_early$HAUL_BEGIN_DATE <- sapply(strsplit(sets_deep_early$HAUL_BEGIN_DATE, " "), "[", 1) #, "%m/%d/%Y")
sets_deep_early$HAUL_BEGIN_DATE <- as.Date(sets_deep_early$HAUL_BEGIN_DATE)
sets_deep_early$HAUL_BEGIN_DATE <- format(sets_deep_early$HAUL_BEGIN_DATE, "%m/%d/%Y")
sets_deep_early$HAUL_BEGIN_DATE <- as.character(sets_deep_early$HAUL_BEGIN_DATE)

## reformat, when import datetimes as factor
sets_deep_early$SET_BEGIN_DATETIME <- as.character.Date(sets_deep_early$SET_BEGIN_DATETIME)
sets_deep_early$SET_END_DATETIME <- as.character.Date(sets_deep_early$SET_END_DATETIME)
sets_deep_early$HAUL_BEGIN_DATETIME <- as.character(sets_deep_early$HAUL_BEGIN_DATETIME)
sets_deep_early$HAUL_END_DATETIME <- as.character.Date(sets_deep_early$HAUL_END_DATETIME)

sets_deep_early$SET_BEGIN_DATETIME <- ymd_hms(sets_deep_early$SET_BEGIN_DATETIME, tz = "Pacific/Honolulu")
sets_deep_early$SET_END_DATETIME <- ymd_hms(sets_deep_early$SET_END_DATETIME, tz = "Pacific/Honolulu")
sets_deep_early$HAUL_BEGIN_DATETIME <- ymd_hms(sets_deep_early$HAUL_BEGIN_DATETIME, tz = "Pacific/Honolulu")
sets_deep_early$HAUL_END_DATETIME <- ymd_hms(sets_deep_early$HAUL_END_DATETIME, tz="Pacific/Honolulu")

## indicator variable for TRT implementation
sets_deep_early <- mutate(sets_deep_early, postTRT = as.factor(ifelse(SET_BEGIN_DATETIME > "2013-01-01 00:00:01 HST", 1, 0)))

## recalculate soak if failed to make it through the set aggregation step above (because dates as factors if import)
sets_deep_early$SOAK = as.numeric(sets_deep_early$HAUL_END_DATETIME - sets_deep_early$SET_END_DATETIME)


## find set lag
# might want to recode to do this better, loop based on ifelses? 
  # if a set is missing, will be wrong, also some double sets

# diff in rows, NAs for 1st sets and extreme values from missing set 1s
# note some sets with really huge lags that appear real
sets_deep_early$SET_LAG <- c(NA, diff(sets_deep_early$SET_BEGIN_DATETIME, lag=1, difference=1))
sets_deep_early[which(sets_deep_early$SET_NUM == 1), "SET_LAG"] <- NA
sets_deep_early[which(sets_deep_early$SET_LAG > 20160), "SET_LAG"] <- NA ## arbitrarily setting to 2 weeks
sets_deep_early[which(sets_deep_early$SET_LAG < 0), "SET_LAG"] <- NA
## days column
#all_sets.date$set.lag.days <- all_sets.date$set.lag / 60 / 24

####################################################################################
### prepare for GIS
####################################################################################
## give a row id
sets_deep_early <- tibble::rowid_to_column(sets_deep_early, "ID")

## write out all sets non-GIS ##
write.csv(sets_deep_early, "Data/sets_deep_early.csv")

write_feather(sets_deep_early, "Data/sets_deep_early.feather")
sets_deep_early <- read_feather("Data/sets_deep_early.feather")
####################################################################################
### back from GIS - import GIS files, creating df of all sets with GIS and saving hard copy
####################################################################################

## Can merge this with all non-GIS sets from above (sets_deep_early) in next section

# import shapefile attribute table - making one GIS data frame of bathy and ocean vars, can update/modify as needed
# then combine this with full combined df
# this is bathymetric GIS file, distances in planar
sets_deep_early_bathy <- as.data.frame(read.csv("Data/GIS_added_files/Vessel_bathy_planar.csv", header = TRUE))

# add FID column (in gdb in arc the FID starts at 0, adding 1 to match to other tables)
sets_deep_early_bathy$FID_one <- sets_deep_early_bathy$FID + 1

# seamounts dataset
seamounts <- as.data.frame(read.csv("Data/GIS_added_files/seamounts.csv", header = TRUE))
seamount_shapefile <- as.data.frame(read.csv("Data/GIS_added_files/seamount_shapefile.csv", header = TRUE))
seamounts <- left_join(seamounts, seamount_shapefile, by = c("ID" = "Name"))
# get column names, then select which ones to keep
cat(paste(shQuote(colnames(seamounts), type="cmd"), collapse=", "))
seamounts <- seamounts %>%
  select(c("Seamt_Height", "Seamt_Depth", "ID", "FID"))
sets_deep_early_bathy <- left_join(sets_deep_early_bathy, seamounts, by = c("Seamt_ID" = "FID"))

# pick columns to keep
sets_deep_early_bathy <- sets_deep_early_bathy %>%
  select(c(FID, FID_one, UID, TRIP_ID, VESSEL_ID, TRIP_TYPE, SET_NUM, DAY, MM_YN, 
           Slope, Depth, Cont_Dist, Seamt_ID, Seamt_Dist, Seamt_Height, Seamt_Depth))

# import oceanographic variables from csv
sets_deep_early_ocean <- as.data.frame(read.csv("Data/GIS_added_files/vessel_ocean.csv", header = TRUE))
sets_deep_early_SST2 <- as.data.frame(read.csv("Data/GIS_added_files/vessel_ocean_SST2.csv"))
sets_deep_early_ocean <- left_join(sets_deep_early_ocean, select(sets_deep_early_SST2, UID, SST_2), by = "UID")
sets_deep_early_moon <- as.data.frame(read.csv("Data/GIS_added_files/vessel_ocean_moon.csv"))
sets_deep_early_ocean <- left_join(sets_deep_early_ocean, select(sets_deep_early_moon, UID, Moon_Illum), by = "UID")

# import Eddy near tables
EddyLineDist_1 <- read.csv("Data//GIS_added_files/EddyLineDist_1.csv", header = TRUE)
EddyLineDist_2 <- read.csv("Data//GIS_added_files/EddyLineDist_2.csv", header = TRUE)

# need to rbind, make sure col names are the same first
cnames <- c(colnames(EddyLineDist_2))
colnames(EddyLineDist_1) <- cnames
EddyLineDistance <- rbind(EddyLineDist_1, EddyLineDist_2)

# rename eddy columns
EddyLineDistance$EDDY_FID <- EddyLineDistance$NEAR_FID
EddyLineDistance$EDDY_DIST <- EddyLineDistance$NEAR_DIST
# pick columns to keep
EddyLineDistance <- EddyLineDistance %>%
  select(c(IN_FID, EDDY_FID, EDDY_DIST, DAY, AMPLITUDE, CYCL_TYPE, SPEED, RADIUS))
# could do things by what dont want too: -c("OID":"OBJECTID", "OBSNUM", "TRACK", "DAY", ...))
EddyLineDistance$CYCL_TYPE <- as.factor(EddyLineDistance$CYCL_TYPE)

# merge using left join, keeps all x, specify which columns to join by
sets_deep_early_GIS <- left_join(sets_deep_early_bathy, EddyLineDistance, by = c("FID_one" = "IN_FID"))
sets_deep_early_GIS <- left_join(sets_deep_early_GIS, sets_deep_early_ocean, by = "UID")

# a little cleanup
sets_deep_early_GIS <- sets_deep_early_GIS %>% mutate(ChlA = ifelse(ChlA == -9999, NA, ChlA))
sets_deep_early_GIS <- sets_deep_early_GIS %>% mutate(SSH = ifelse(SSH == -9999, NA, SSH))

## write out all sets with GIS ##
write.csv(sets_deep_early_GIS, file = "Data/sets_deep_early_GIS.csv")
write_feather(sets_deep_early_GIS, "Data/sets_deep_early_GIS.feather")



#######################################################################################
## adding El Nino/La Nina indicator
####################################################################################

ElNino <- read_csv("Data/ElNino.csv")

cat(paste(sQuote(colnames(ElNino)), collapse=", "))
cat(paste(shQuote(colnames(ElNino), type="cmd"), collapse=", "))

# make tidy, set month names
ElNino <- ElNino %>%
  gather(DJF:NDJ, key = "MONTH_AVG", value = "ONI") %>%
  filter(Year != "Year") %>%
  mutate(MONTH = ifelse(MONTH_AVG == "DJF", 1,
                        ifelse(MONTH_AVG == "JFM", 2,
                               ifelse(MONTH_AVG == "FMA", 3,
                                      ifelse(MONTH_AVG == "MAM", 4,
                                             ifelse(MONTH_AVG == "AMJ", 5,
                                                    ifelse(MONTH_AVG == "MJJ", 6,
                                                           ifelse(MONTH_AVG == "JJA", 7,
                                                                  ifelse(MONTH_AVG == "JAS", 8,
                                                                         ifelse(MONTH_AVG == "ASO", 9,
                                                                                ifelse(MONTH_AVG == "SON", 10,
                                                                                       ifelse(MONTH_AVG == "OND", 11,
                                                                                              ifelse(MONTH_AVG == "NDJ", 12, "NA")))))))))))))
sapply(ElNino,class)

ElNino$ONI <- as.numeric(ElNino$ONI)
ElNino$MONTH <- as.numeric(ElNino$MONTH)
ElNino$YEAR <- as.numeric(ElNino$Year)

# make categorical levels
ElNino <- mutate(ElNino, EL_LA_NO = as.factor(ifelse(ONI > 0.5, "EL",
                                           ifelse(ONI < -0.5, "LA", "NO"))))

write_feather(ElNino, "Data/ElNino.feather")
ElNino <- read_feather("Data/ElNino.feather")


####################################################################################
### merging GIS dataframe and El Nino to all sets data
####################################################################################


sets_deep_early <- read_feather("Data/sets_deep_early.feather")
sets_deep_early_GIS <- read_feather("Data/sets_deep_early_GIS.feather")

sets_alldata <- left_join(sets_deep_early, sets_deep_early_GIS, by = c("ID" = "UID"))
sets_alldata <- left_join(sets_alldata, ElNino, by = c("YEAR", "MONTH"))

cat(paste(shQuote(colnames(sets_alldata), type="cmd"), collapse=", "))
cat(paste(sQuote(colnames(sets_alldata)), collapse=", "))

sets_alldata <- sets_alldata %>%
  select(c("ID", "TRIP_ID", "SET_NUM", "VESSEL_ID", "DECLARED_TRIP", "SET_BEGIN_DATETIME", "SET_BEGIN_TIME", "SET_END_DATETIME", 
           "SET_BEGIN_LAT", "SET_BEGIN_LON", "SET_END_LAT", "SET_END_LON", "HAUL_BEGIN_DATETIME", "HAUL_END_DATETIME", 
           "HAUL_BEGIN_DATE", "HAUL_BEGIN_TIME", "HAUL_BEGIN_LAT", "HAUL_BEGIN_LON", "HAUL_END_LAT", "HAUL_END_LON",
           "postTRT", "MONTH", "YEAR", "SOAK", "SET_LAG", "NUM_FLTS", "NUM_HKS_SET", "HKS_PER_FLT", "FLTLN_LEN", 
           "BRNCHLN_LEN", "LDR_LEN", "MIN_LEN", "BLUEDYE", "BAIT_CODE_VAL", "NUM_LITE_DEVICES", "NUM_CAUGHT", 
           "KEPT", "FISH", "SHARKS", "ODONT", "TURTLES", "SWO", "BET", "TUNA", "MAHI", "CPUE", "MM_YN", "MM_sum", 
           "DPUE", "DP", "FKW", "FKW_sum", "SFPW", "SFPW_sum", "RISS", "RISS_sum", "HAUL_BEGIN_DATE", 
           "Slope", "Depth", "Cont_Dist", "Seamt_ID", "Seamt_Dist", "Seamt_Height", "Seamt_Depth",
           "EDDY_FID", "EDDY_DIST", "AMPLITUDE", "CYCL_TYPE", "SPEED", "RADIUS", "SST_2", "ChlA", "SSH", "Moon_Illum", "ONI", "EL_LA_NO"))

summary(sets_alldata)
sapply(sets_alldata, class)

sets_deep_all <- sets_alldata %>%
  filter(DECLARED_TRIP == 'D')
sets_shallow_all <- sets_alldata %>%
  filter(DECLARED_TRIP != 'D')

summary(sets_deep_all)
summary(sets_shallow_all)

write_feather(sets_alldata, "Data/sets_alldata.feather")
write_feather(sets_deep_all, "Data/sets_deep_all.feather")
write_feather(sets_shallow_all, "Data/sets_shallow_all.feather")

sapply(sets_alldata,class)




#sets_alldata$BLUEDYE <- ifelse(sets_alldata$BLUEDYE=="Y",1,0)







