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
library(fields)


#############################################################################
### get data into R
#############################################################################
## import catch datasets into R objects - sourced from raw csv files in project data folder - original xlsx files backed up separately

LL2003 <- read.csv("Data/raw-csvs/LL2003.csv")
LL2004 <- read.csv("Data/raw-csvs/LL2004.csv")
LL2005 <- read.csv("Data/raw-csvs/LL2005.csv")
LL2006 <- read.csv("Data/raw-csvs/LL2006.csv")
LL2007 <- read.csv("Data/raw-csvs/LL2007.csv")
LL2008 <- read.csv("Data/raw-csvs/LL2008.csv")
LL2009 <- read.csv("Data/raw-csvs/LL2009.csv")
LL2010 <- read.csv("Data/raw-csvs/LL2010.csv")
LL2011 <- read.csv("Data/raw-csvs/LL2011.csv")
LL2012 <- read.csv("Data/raw-csvs/LL2012.csv")
LL2013 <- read.csv("Data/raw-csvs/LL2013.csv")
LL2014 <- read.csv("Data/raw-csvs/LL2014.csv")
LL2015 <- read.csv("Data/raw-csvs/LL2015.csv")
LL2016 <- read.csv("Data/raw-csvs/LL2016.csv")
LL2017 <- read.csv("Data/raw-csvs/LL2017.csv")
#LL2017 <- read.csv("/Users/josephfader/Google Drive/PhDuke/Projects/HPLL_TRT/Data/raw-csvs/LL2017.csv", header = TRUE)

## combine into single data frame - raw file so do not need to re-import indiv years
# DO NOT DELETE OR MODIFY 'all_years_catch_raw' ----- now 'raw_catch_allyears

# note warning messages for 'invalid factor levels'
catch_allyears_raw <- rbind(LL2003, LL2004, LL2005, LL2006, LL2007, LL2008, LL2009, LL2010, LL2011, LL2012, LL2013, LL2014, LL2015, LL2016, LL2017)
## remove single years from R environment - do after step below - making working copy
rm(LL2003, LL2004, LL2005, LL2006, LL2007, LL2008, LL2009, LL2010, LL2011, LL2012, LL2013, LL2014, LL2015, LL2016, LL2017)

rm(catch_allyears_raw)
write_feather(catch_allyears_raw, "Data/catch_allyears_raw.feather")

MMforms <- read.csv("Data/raw-csvs/MM-forms.csv")
#############################################################################
#################### TO START OVER - START HERE ##################
####################################################################################

## create working copy from the raw data frame
catch_allyearscsv <- read.csv("Data/catch_allyears_raw.csv")
catch_allyears <- read_feather("Data/catch_allyears_raw.feather")

####################################################################################
### check and set variable types
####################################################################################

sapply(catch_allyears, class)
summary(catch_allyears)

## convert certain factors to numeric (num hooks, light sticks) -- fixed so should be integer already
# catch_allyears$NUM_HKS_SET <- as.numeric(catch_allyears$NUM_HKS_SET)
# 
# catch_allyears$NUM_LITE_DEVICES <- as.numeric(catch_allyears$NUM_LITE_DEVICES)

## convert character to factor
catch_allyears$EXPR_TRIP_TYPE_CODE <- as.factor((catch_allyears$EXPR_TRIP_TYPE_CODE))

summary(catch_allyears$EXPR_TRIP_TYPE_CODE)

## here as.character.Date - alternative to lubridate/posix
catch_allyears$SET_BEGIN_DATETIME <- as.character.Date(catch_allyears$SET_BEGIN_DATETIME)
catch_allyears$SET_END_DATETIME <- as.character.Date(catch_allyears$SET_END_DATETIME)
catch_allyears$HAUL_BEGIN_DATETIME <- as.character.Date(catch_allyears$HAUL_BEGIN_DATETIME)
catch_allyears$HAUL_END_DATETIME <- as.character.Date(catch_allyears$HAUL_END_DATETIME)

## split date/time into new columns...only works if as.character.Date
catch_allyears$SET_BEGIN_TIME <- sapply(strsplit(catch_allyears$SET_BEGIN_DATETIME, " "), "[", 2)
catch_allyears$HAUL_BEGIN_DATE <- as.Date(sapply(strsplit(catch_allyears$HAUL_BEGIN_DATETIME, " "), "[", 1), "%m/%d/%Y")
catch_allyears$HAUL_BEGIN_TIME <- sapply(strsplit(catch_allyears$HAUL_BEGIN_DATETIME, " "), "[", 2)


## convert date time
# using lubridate which stores as POSIX
catch_allyears$SET_BEGIN_DATETIME <- mdy_hms(catch_allyears$SET_BEGIN_DATETIME, tz="")
catch_allyears$SET_END_DATETIME <- mdy_hms(catch_allyears$SET_END_DATETIME, tz="")
catch_allyears$HAUL_BEGIN_DATETIME <- mdy_hms(catch_allyears$HAUL_BEGIN_DATETIME, tz="")
catch_allyears$HAUL_END_DATETIME <- mdy_hms(catch_allyears$HAUL_END_DATETIME, tz="")
#catch_allyears$SET_BEGIN_TIME <- hms(catch_allyears$SET_BEGIN_DATETIME, tz="")
#catch_allyears$HAUL_BEGIN_DATE <- mdy_hms(catch_allyears$HAUL_BEGIN_DATE, tz="")
#catch_allyears$HAUL_BEGIN_TIME <- hms(catch_allyears$HAUL_BEGIN_TIME, tz="")

# add year for catch
catch_allyears$YEAR <- as.numeric(format(catch_allyears$SET_BEGIN_DATETIME, "%Y"))
####################################################################################
### catch variables
####################################################################################

## indicator for MM damage
catch_allyears <- mutate(catch_allyears, MM = ifelse(DAMAGE_CODE_VAL == 'Marine mammal damage', 1, 0))
## indicator for shark damage
catch_allyears <- mutate(catch_allyears, SP = ifelse(DAMAGE_CODE == 'SH' | DAMAGE_CODE == 'SB' | DAMAGE_CODE == 'ST', 1, 0))

## indicator for Kept
catch_allyears <- mutate(catch_allyears, KEPT = ifelse(KEPT_RETURN_CODE_VAL == 'Kept', 1, 0))
   
## code sps type - so can sum all fish and other catch categories
catch_allyears <- mutate(catch_allyears, TYPE = ifelse(grepl('Tuna', SPECIES_COMMON_NAME) | grepl('Fish', SPECIES_COMMON_NAME) |
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
species <- unique(catch_allyears$SPECIES_COMMON_NAME)
catch_allyears <- mutate(catch_allyears, FKW_caught = ifelse(SPECIES_COMMON_NAME == 'Whale, False Killer', 1, 0))
catch_allyears <- mutate(catch_allyears, SFPW_caught = ifelse(SPECIES_COMMON_NAME == 'Whale, Short-finned Pilot', 1, 0))
catch_allyears <- mutate(catch_allyears, RISS_caught = ifelse(SPECIES_COMMON_NAME == 'Dolphin, Risso\'s', 1, 0))

## target species
# note including all not just 'kept' fish

## SWO
catch_allyears <- mutate(catch_allyears, SWO = ifelse(SPECIES_COMMON_NAME == 'Swordfish', 1, 0))
## BET
catch_allyears <- mutate(catch_allyears, BET = ifelse(SPECIES_COMMON_NAME == 'Tuna, Bigeye', 1, 0))
## YFT
catch_allyears <- mutate(catch_allyears, YFT = ifelse(SPECIES_COMMON_NAME == 'Tuna, Yellowfin', 1, 0))
## all tunas
catch_allyears <- mutate(catch_allyears, TUNA = ifelse(grepl('Tuna', SPECIES_COMMON_NAME), 1, 0))
## mahi
catch_allyears <- mutate(catch_allyears, MAHI = ifelse(grepl('Dolphinfish', SPECIES_COMMON_NAME), 1, 0))
## wahoo
catch_allyears <- mutate(catch_allyears, WAHOO = ifelse(grepl('Wahoo', SPECIES_COMMON_NAME), 1, 0))
## billfish
catch_allyears <- mutate(catch_allyears, BILLFISH = ifelse(grepl('Marlin', SPECIES_COMMON_NAME) | grepl('Sailfish', SPECIES_COMMON_NAME) |
                                                          grepl('Billfish', SPECIES_COMMON_NAME) | grepl('Spearfish', SPECIES_COMMON_NAME), 1, 0))
## nontarget species
catch_allyears <- mutate(catch_allyears, NONTARGET = ifelse((grepl('Fish', SPECIES_COMMON_NAME) |
                                                            grepl('fish', SPECIES_COMMON_NAME) | grepl('Pomfret', SPECIES_COMMON_NAME) |
                                                            grepl('Mola', SPECIES_COMMON_NAME) |
                                                            grepl('Barracuda', SPECIES_COMMON_NAME) | grepl('Escolar', SPECIES_COMMON_NAME) |
                                                            grepl('Mackerel', SPECIES_COMMON_NAME) | grepl('Jack', SPECIES_COMMON_NAME) |
                                                            grepl('Puffer', SPECIES_COMMON_NAME) | SPECIES_COMMON_NAME == 'Opah' | 
                                                            SPECIES_COMMON_NAME == 'Louvar' | SPECIES_COMMON_NAME == 'Yellowtail' |
                                                            SPECIES_COMMON_NAME == 'Rainbow Runner' | grepl('jack', SPECIES_COMMON_NAME)) &
                                                            (!grepl('Dolphinfish', SPECIES_COMMON_NAME) & SPECIES_COMMON_NAME != 'Sailfish' &
                                                            !grepl('Billfish', SPECIES_COMMON_NAME) & SPECIES_COMMON_NAME != 'Spearfish, Shortbill' &
                                                            !grepl('Marlin', SPECIES_COMMON_NAME) & SPECIES_COMMON_NAME != 'Swordfish'), 1, 0))

write_feather(catch_allyears, "Data/catch_allyears.feather")

####################################################################################
### hooks
####################################################################################
summary(catch_allyears)
sapply(catch_allyears,class)
unique(catch_allyears$HK_TYPE_CODE_VAL_4)
catch_allyears$HK_TYPE_CODE_VAL_1 <- as.factor(catch_allyears$HK_TYPE_CODE_VAL_1)
catch_allyears$HK_TYPE_CODE_VAL_2 <- as.factor(catch_allyears$HK_TYPE_CODE_VAL_2)
catch_allyears$HK_TYPE_CODE_VAL_3 <- as.factor(catch_allyears$HK_TYPE_CODE_VAL_3)
catch_allyears$HK_TYPE_CODE_VAL_4 <- as.factor(catch_allyears$HK_TYPE_CODE_VAL_4)

# set blank hooks to NA
catch_allyears[which(catch_allyears$HK_TYPE_CODE_VAL_1 == ''), "HK_TYPE_CODE_VAL_1"] <- NA
catch_allyears[which(catch_allyears$HK_TYPE_CODE_VAL_2 == ''), "HK_TYPE_CODE_VAL_2"] <- NA
catch_allyears[which(catch_allyears$HK_TYPE_CODE_VAL_3 == ''), "HK_TYPE_CODE_VAL_3"] <- NA
catch_allyears[which(catch_allyears$HK_TYPE_CODE_VAL_4 == ''), "HK_TYPE_CODE_VAL_4"] <- NA

## sets with only circle hooks == CIRCLE, sets with only tuna or J == TUNA or J, everything else == OTHER
catch_allyears <- mutate(catch_allyears, HKS = ifelse((grepl('Circle', HK_TYPE_CODE_VAL_1) & !is.na(HK_TYPE_CODE_VAL_1)) &
                                                       (grepl('Circle', HK_TYPE_CODE_VAL_2) | is.na(HK_TYPE_CODE_VAL_2)) &
                                                       (grepl('Circle', HK_TYPE_CODE_VAL_3) | is.na(HK_TYPE_CODE_VAL_3)) &
                                                       (grepl('Circle', HK_TYPE_CODE_VAL_4) | is.na(HK_TYPE_CODE_VAL_4)),
                                                       'CIRCLE',
                                                      ifelse((grepl('Tuna', HK_TYPE_CODE_VAL_1) & !is.na(HK_TYPE_CODE_VAL_1)) &
                                                               (!grepl('Circle', HK_TYPE_CODE_VAL_2) | is.na(HK_TYPE_CODE_VAL_2)) &
                                                               (!grepl('Circle', HK_TYPE_CODE_VAL_3) | is.na(HK_TYPE_CODE_VAL_3)) &
                                                               (!grepl('Circle', HK_TYPE_CODE_VAL_4) | is.na(HK_TYPE_CODE_VAL_4)),
                                                               'TUNA',
                                                             ifelse(grepl('J', HK_TYPE_CODE_VAL_1) & !is.na(HK_TYPE_CODE_VAL_1),
                                                                'JHOOK','OTHER'))))
                                                             


View(catch_allyears[,grepl('HK',colnames(catch_allyears))])
View(catch_allyears %>%
  select(c(TRIP_ID, 25:109)))

# set NA leaders to 0
catch_allyears$LDR_LEN <- ifelse(is.na(catch_allyears$LDR_LEN), 0, catch_allyears$LDR_LEN)

# save out
write_feather(catch_allyears, "Data/catch_allyears.feather")

####################################################################################
### filter out undeclared sets, invalid trips, experimental trips
####################################################################################
catch_allyears <- read_feather("Data/catch_allyears.feather")

# 6-22-2004 was start of declaring deep vs shallow
# originally left out undeclared in 03-04 but now including, should all be deepset, need to set that way
# note including all except invalid trips, might not be appropriate to include unfinalized versions in analysis

unique(catch_allyears$TRIP_APPROVAL_STATUS)
View(catch_allyears %>%
  group_by(TRIP_ID) %>%
    summarise(
      approval = first(TRIP_APPROVAL_STATUS)
    ) %>%
  group_by(approval) %>%
  count())

# all sets pre 6/22/04 should be deep sets, keeping them in, but filtering out pre 8/03 when recording MM damage started
catch_allyears <- catch_allyears %>%
  filter(TRIP_APPROVAL_STATUS != 'INVALID') %>%
  filter(HAUL_BEGIN_DATE > "2003-08-20") #earliest found MM damagae is 2003-08-24 07:07:00 -  wont filter by date, leaving in for now

# setting undeclared to deepset
catch_allyears$DECLARED_TRIP_TYPE_CODE <- as.factor(catch_allyears$DECLARED_TRIP_TYPE_CODE)
catch_allyears$DECLARED_TRIP_TYPE_CODE <- as.factor(ifelse(catch_allyears$DECLARED_TRIP_TYPE_CODE != 'S' | is.na(catch_allyears$DECLARED_TRIP_TYPE_CODE), 'D', 'S'))

catch_allyears %>%
  group_by(DECLARED_TRIP_TYPE_CODE) %>%
  count()
## also must address experimental sets/vessels
# here removing all with any type of exp status
# tho note this is only for trips, Karin removed all
# subsequent trips for any vessel that participated

View(catch_allyears %>%
       group_by(TRIP_ID) %>%
       summarise(
         Exp = first(EXPR_TRIP_TYPE_CODE_VAL)
       ) %>%
       group_by(Exp) %>%
       count())
catch_allyears <- catch_allyears %>%
  filter(is.na(EXPR_TRIP_TYPE_CODE) | EXPR_TRIP_TYPE_CODE == "")

## write out prepped catch file - can use this if need to make changes
write.csv(catch_allyears, "Data/catch_allyears_prepd.csv")
## feather version conserves data types
write_feather(catch_allyears, "Data/catch_allyears_prepd.feather")

summary(catch_allyears)
rm(catch_allyears)


####################################################################################
############# START HERE for prepped catch data ########## now ALL available data included in prepped data
####################################################################################

catch_allyears <- read_feather("Data/catch_allyears_prepd.feather")
catch_allyears <- read.csv("Data/catch_allyears_prepd.csv", header = TRUE)
summary(catch_allyears)
catch_allyears <- as.data.frame(catch_allyears)

####################################################################################
### sets - aggregate catch data to set level
####################################################################################

sets_allyears <- group_by(catch_allyears, TRIP_ID, SET_NUM) %>%
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
    
    # hooks, all circle, all tunaj, or other
    HOOKS = first(as.factor(HKS)),
  
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
    YFT = sum(YFT),
    TUNA = sum(TUNA),
    MAHI = sum(MAHI),
    WAHOO = sum(WAHOO),
    BILLFISH = sum(BILLFISH),
    NONTARGET = sum(NONTARGET),
    
    CPUE = (FISH/NUM_HKS_SET)*1000,
    CPUE_FLT = (FISH/(NUM_FLTS)),
    ## depredation
    # mammals
    MM_YN = as.factor(ifelse('Marine mammal damage' %in% DAMAGE_CODE_VAL, 1, 0)),
    MM_sum = sum(MM),
    #MM_prop = (MM_sum/FISH),
    DPUE = (MM_sum/NUM_HKS_SET) *1000,
    DP = (MM_sum/KEPT),
    
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

sapply(sets_allyears, class)
summary(sets_allyears)

####################################################################################
### add some time/date stuff
####################################################################################



sets_allyears$MONTH <- as.numeric(format(sets_allyears$SET_BEGIN_DATETIME, "%m"))
sets_allyears$YEAR <- as.numeric(format(sets_allyears$SET_BEGIN_DATETIME, "%Y"))

## make haul day in format for GIS
# to split must be in character
# then set to date before changing to desired format - reconvert to string?
# careful of timezones, setting right away so should be ok, but if don't and do this step after have set tz below,
# will convert the date to local
sets_allyears <- as.data.frame(sets_allyears)
sets_allyears$HAUL_BEGIN_DATE <- as.character(sets_allyears$HAUL_BEGIN_DATETIME, tz="")
sets_allyears$HAUL_BEGIN_DATE <- sapply(strsplit(sets_allyears$HAUL_BEGIN_DATE, " "), "[", 1) #, "%m/%d/%Y")
sets_allyears$HAUL_BEGIN_DATE <- as.Date(sets_allyears$HAUL_BEGIN_DATE)
sets_allyears$HAUL_BEGIN_DATE <- format(sets_allyears$HAUL_BEGIN_DATE, "%m/%d/%Y")
sets_allyears$HAUL_BEGIN_DATE <- as.character.Date(sets_allyears$HAUL_BEGIN_DATE)

## reformat, when import datetimes as factor
# sets_allyears$SET_BEGIN_DATETIME <- as.character.Date(sets_allyears$SET_BEGIN_DATETIME)
# sets_allyears$SET_END_DATETIME <- as.character.Date(sets_allyears$SET_END_DATETIME)
# sets_allyears$HAUL_BEGIN_DATETIME <- as.character(sets_allyears$HAUL_BEGIN_DATETIME)
# sets_allyears$HAUL_END_DATETIME <- as.character.Date(sets_allyears$HAUL_END_DATETIME)

# removing tz for now, doesnt give warnings, before was using "Pacific/Honolulu"
sets_allyears$SET_BEGIN_DATETIME <- ymd_hms(sets_allyears$SET_BEGIN_DATETIME, tz = "")
sets_allyears$SET_END_DATETIME <- ymd_hms(sets_allyears$SET_END_DATETIME, tz = "")
sets_allyears$HAUL_BEGIN_DATETIME <- ymd_hms(sets_allyears$HAUL_BEGIN_DATETIME, tz = "")
sets_allyears$HAUL_END_DATETIME <- ymd_hms(sets_allyears$HAUL_END_DATETIME, tz="")

## indicator variable for TRT implementation
sets_allyears <- mutate(sets_allyears, postTRT = as.factor(ifelse(SET_BEGIN_DATETIME > "2013-01-01 00:00:01 HST", 1, 0)))

## recalculate soak if failed to make it through the set aggregation step above (because dates as factors if import)
sets_allyears$SOAK <- as.numeric(sets_allyears$HAUL_END_DATETIME - sets_allyears$SET_END_DATETIME)
sets_allyears$SOAK <- (sets_allyears$SOAK / 60)

## removing soaks over 2 days - remove low soak #s too?? --- not doing for now...
# sets_allyears <- sets_allyears %>%
#   filter(SOAK < 48)

## find set lag
# might want to recode to do this better, loop based on ifelses? 
  # if a set is missing, will be wrong, also some double sets

######### SEE BELOW - add set lag using 'lag' later

# diff in rows, NAs for 1st sets and extreme values from missing set 1s
# note some sets with really huge lags that appear real
# sets_allyears$SET_LAG <- c(NA, diff(sets_allyears$SET_BEGIN_DATETIME, lag=1, difference=1))
# sets_allyears[which(sets_allyears$SET_NUM == 1), "SET_LAG"] <- NA
# sets_allyears[which(sets_allyears$SET_LAG > 20160), "SET_LAG"] <- NA ## arbitrarily setting to 2 weeks
# sets_allyears[which(sets_allyears$SET_LAG < 0), "SET_LAG"] <- NA
## days column
#all_sets.date$set.lag.days <- all_sets.date$set.lag / 60 / 24

####################################################################################
### prepare for GIS
####################################################################################
## give a row id
sets_allyears <- tibble::rowid_to_column(sets_allyears, "ID")

## write out all sets non-GIS ##
write.csv(sets_allyears, "Data/sets_allyears.csv")
write_feather(sets_allyears, "Data/sets_allyears.feather")


####################################################################################
### back from GIS - import GIS files, creating df of all sets with GIS and saving hard copy
####################################################################################
## have two threads here, one for original and another for the early subset of data I added later
## could adapt if other additional pieces needed to add

## 12/15/18 - note did front and other additional oceanographic data just for deep sets,
## adding in at end after split of shallow/deep

## Ultimately can merge this with all non-GIS sets from above (sets_allyears) in next section

# import shapefile attribute table - making one GIS data frame of bathy and ocean vars, can update/modify as needed
# then combine this with full combined df
# this is bathymetric GIS file, distances in planar
sets_allyears_bathy <- as.data.frame(read.csv("Data/GIS_added_files/Vessel_bathy_planar.csv", header = TRUE))
sets_early_bathy <- as.data.frame(read.csv("Data/GIS_added_files/vessel_bathy_early.csv", header = TRUE))

# add FID column (in gdb in arc the FID starts at 0, adding 1 to match to other tables)
sets_allyears_bathy$FID_one <- sets_allyears_bathy$FID + 1
sets_early_bathy$FID_one <- sets_early_bathy$FID + 1

# seamounts dataset
seamounts <- as.data.frame(read.csv("Data/GIS_added_files/seamounts.csv", header = TRUE))
seamount_shapefile <- as.data.frame(read.csv("Data/GIS_added_files/seamount_shapefile.csv", header = TRUE))
seamounts <- left_join(seamounts, seamount_shapefile, by = c("ID" = "Name"))
# get column names, then select which ones to keep
cat(paste(shQuote(colnames(seamounts), type="cmd"), collapse=", "))
seamounts <- seamounts %>%
  select(c("Seamt_Height", "Seamt_Depth", "ID", "FID"))
sets_allyears_bathy <- left_join(sets_allyears_bathy, seamounts, by = c("Seamt_ID" = "FID"))
sets_early_bathy <- left_join(sets_early_bathy, seamounts, by = c("Seamt_ID" = "FID"))

# pick columns to keep
sets_allyears_bathy <- sets_allyears_bathy %>%
  select(c(FID, FID_one, UID, TRIP_ID, VESSEL_ID, TRIP_TYPE, SET_NUM, DAY, MM_YN, 
           Slope, Depth, Cont_Dist, Seamt_ID, Seamt_Dist, Seamt_Height, Seamt_Depth))
sets_early_bathy <- sets_early_bathy %>%
  select(c(FID, FID_one, UID, TRIP_ID, VESSEL_ID, TRIP_TYPE, SET_NUM, HaulDate, 
           Slope, Depth, Cont_Dist, Seamt_ID, Seamt_Dist, Seamt_Height, Seamt_Depth))

# import oceanographic variables from csv
sets_allyears_ocean <- as.data.frame(read.csv("Data/GIS_added_files/vessel_ocean.csv", header = TRUE))
sets_allyears_SST2 <- as.data.frame(read.csv("Data/GIS_added_files/vessel_ocean_SST2.csv"))
sets_allyears_ocean <- left_join(sets_allyears_ocean, select(sets_allyears_SST2, UID, SST_2), by = "UID")
sets_allyears_moon <- as.data.frame(read.csv("Data/GIS_added_files/vessel_ocean_moon.csv"))
sets_allyears_ocean <- left_join(sets_allyears_ocean, select(sets_allyears_moon, UID, Moon_Illum), by = "UID")

sets_early_ocean <- as.data.frame(read.csv("Data/GIS_added_files/vessel_ocean_early.csv", header = TRUE))

# import Eddy near tables
EddyLineDist_1 <- read.csv("Data//GIS_added_files/EddyLineDist_1.csv", header = TRUE)
EddyLineDist_2 <- read.csv("Data//GIS_added_files/EddyLineDist_2.csv", header = TRUE)

EddyLineDist_early <- read.csv("Data//GIS_added_files/EddyLineDist_early.csv", header = TRUE)

# need to rbind, make sure col names are the same first
cnames <- c(colnames(EddyLineDist_early))
colnames(EddyLineDist_1) <- cnames
EddyLineDistance <- rbind(EddyLineDist_1, EddyLineDist_2)

# pick columns, then rename
cat(paste(shQuote(colnames(EddyLineDist_early), type="cmd"), collapse=", "))
EddyLineDist_early <- EddyLineDist_early %>%
  select(c("eddy_linedist_early.IN_FID", "eddy_linedist_early.NEAR_FID", 
           "eddy_linedist_early.NEAR_DIST", "Eddy_buffer_early_line.DAY", "Eddy_buffer_early_line.AMPLITUDE", 
           "Eddy_buffer_early_line.CYCL_TYPE", "Eddy_buffer_early_line.SPEED", "Eddy_buffer_early_line.RADIUS"))
# use colnames to change names
colnames(EddyLineDist_early) <- c("IN_FID", "EDDY_FID", "EDDY_DIST", "DAY", "AMPLITUDE", "CYCL_TYPE", "SPEED", "RADIUS")
EddyLineDist_early$CYCL_TYPE <- as.factor(EddyLineDist_early$CYCL_TYPE)

# rename eddy columns
EddyLineDistance$EDDY_FID <- EddyLineDistance$NEAR_FID
EddyLineDistance$EDDY_DIST <- EddyLineDistance$NEAR_DIST
# pick columns to keep
EddyLineDistance <- EddyLineDistance %>%
  select(c(IN_FID, EDDY_FID, EDDY_DIST, DAY, AMPLITUDE, CYCL_TYPE, SPEED, RADIUS))
# could do things by what dont want too: -c("OID":"OBJECTID", "OBSNUM", "TRACK", "DAY", ...))
EddyLineDistance$CYCL_TYPE <- as.factor(EddyLineDistance$CYCL_TYPE)

# merge using left join, keeps all x, specify which columns to join by
sets_GIS_orig <- left_join(sets_allyears_bathy, EddyLineDistance, by = c("FID_one" = "IN_FID"))
sets_GIS_orig <- left_join(sets_GIS_orig, sets_allyears_ocean, by = "UID")


sets_GIS_early <- left_join(sets_early_bathy, EddyLineDist_early, by = c("FID_one" = "IN_FID"))
sets_GIS_early <- left_join(sets_GIS_early, sets_early_ocean, by = "UID")
sets_GIS_early <- sets_early_GIS

# a little cleanup
sets_GIS_orig <- sets_GIS_orig %>% mutate(ChlA = ifelse(ChlA == -9999, NA, ChlA))
sets_GIS_orig <- sets_GIS_orig %>% mutate(SSH = ifelse(SSH == -9999, NA, SSH))

sets_GIS_early <- sets_GIS_early %>% mutate(ChlA = ifelse(ChlA == -9999, NA, ChlA))
sets_GIS_early <- sets_GIS_early %>% mutate(SSH = ifelse(SSH == -9999, NA, SSH))


## combine any separate GIS files
# first make sure columns all same, then rbind and remove duplicates
cat(paste(shQuote(colnames(sets_GIS_orig), type="cmd"), collapse=", "))
sets_GIS_orig <- sets_GIS_orig %>%
  select(c("FID.x", "FID_one", "UID", "TRIP_ID.x", "VESSEL_ID.x", "TRIP_TYPE.x", "SET_NUM.x", "DAY.x",
           "Slope", "Depth", "Cont_Dist", "Seamt_ID", "Seamt_Dist", "Seamt_Height", "Seamt_Depth", 
           "EDDY_FID", "EDDY_DIST", "DAY.y", "AMPLITUDE", "CYCL_TYPE", "SPEED", "RADIUS", "ChlA", "SST_2", "SSH", "Moon_Illum"))

cat(paste(shQuote(colnames(sets_GIS_early), type="cmd"), collapse=", "))
sets_GIS_early <- sets_GIS_early %>%
  select(c("FID.x", "FID_one", "UID", "TRIP_ID.x", "VESSEL_ID.x", "TRIP_TYPE.x", "SET_NUM.x", "HaulDate.x",
           "Slope", "Depth", "Cont_Dist", "Seamt_ID", "Seamt_Dist", "Seamt_Height", "Seamt_Depth",
           "EDDY_FID", "EDDY_DIST", "DAY", "AMPLITUDE", "CYCL_TYPE", "SPEED", "RADIUS", "ChlA", "SST_2", "SSH", "Moon_Illum"))

cnames <- c(colnames(sets_GIS_orig))
colnames(sets_GIS_early) <- cnames

# rbind all GIS files
sets_GIS_final <- rbind(sets_GIS_orig, sets_GIS_early)
# !duplicated returns a single instance for combination of any specified columns
sets_GIS_final <- sets_GIS_final[!duplicated(sets_GIS_final["TRIP_ID.x", "VESSEL_ID.x", "SET_NUM.x", "DAY.y"]),]

summary(sets_GIS_final)

## write out all sets with GIS ##
# here's original, keeping just in case
# write.csv(sets_allyears_GIS, file = "Data/sets_allyears_GIS.csv")
# write_feather(sets_allyears_GIS, "Data/sets_allyears_GIS.feather")

# and the 'new' final gis file
write.csv(sets_GIS_final, file = "Data/sets_GIS_final.csv")
write_feather(sets_GIS_final, "Data/sets_GIS_final.feather")

#######################################################################################
## adding El Nino/La Nina indicator
####################################################################################

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

sets_allyears <- read_feather("Data/sets_allyears.feather")
sets_GIS_final <- read_feather("Data/sets_GIS_final.feather")

sets_alldata_test <- left_join(sets_allyears, ElNino, by = c("YEAR", "MONTH"))
sets_alldata_test <- left_join(sets_alldata_test, sets_GIS_final, by = c("TRIP_ID" = "TRIP_ID.x", "VESSEL_ID" = "VESSEL_ID.x", "SET_NUM" = "SET_NUM.x"))

sapply(sets_alldata_test,class)

cat(paste(shQuote(colnames(sets_alldata_test), type="cmd"), collapse=", "))

sets_alldata <- sets_alldata_test %>%
  select(c("ID", "TRIP_ID", "SET_NUM", "VESSEL_ID", "DECLARED_TRIP", "SET_BEGIN_DATETIME", "SET_BEGIN_TIME", "SET_END_DATETIME", 
           "SET_BEGIN_LAT", "SET_BEGIN_LON", "SET_END_LAT", "SET_END_LON", "HAUL_BEGIN_DATETIME", "HAUL_END_DATETIME", 
           "HAUL_BEGIN_DATE", "HAUL_BEGIN_TIME", "HAUL_BEGIN_LAT", "HAUL_BEGIN_LON", "HAUL_END_LAT", "HAUL_END_LON",
           "postTRT", "MONTH", "YEAR", "SOAK", "HOOKS", "NUM_FLTS", "NUM_HKS_SET", "HKS_PER_FLT", "FLTLN_LEN", 
           "BRNCHLN_LEN", "LDR_LEN", "MIN_LEN", "BLUEDYE_YN", "BLUEDYE", "BAIT_CODE_VAL", "NUM_LITE_DEVICES", "NUM_CAUGHT", 
           "KEPT", "FISH", "SHARKS", "ODONT", "TURTLES", "SWO", "BET", "YFT", "TUNA", "MAHI", "WAHOO", "BILLFISH", "NONTARGET",
           "CPUE", "CPUE_FLT","MM_YN", "MM_sum", "DPUE", "DP", "FKW", "FKW_sum", "SFPW", "SFPW_sum", "RISS", "RISS_sum", 
           "Slope", "Depth", "Cont_Dist", "Seamt_ID", "Seamt_Dist", "Seamt_Height", "Seamt_Depth",
           "EDDY_FID", "EDDY_DIST", "AMPLITUDE", "CYCL_TYPE", "SPEED", "RADIUS", "SST_2", "ChlA", "SSH", "Moon_Illum", "ONI", "EL_LA_NO"))

# sets_early_all <- sets_early_all %>%
#   select(c("ID", "TRIP_ID", "SET_NUM", "VESSEL_ID", "DECLARED_TRIP", "SET_BEGIN_DATETIME", "SET_BEGIN_TIME", "SET_END_DATETIME", 
#            "SET_BEGIN_LAT", "SET_BEGIN_LON", "SET_END_LAT", "SET_END_LON", "HAUL_BEGIN_DATETIME", "HAUL_END_DATETIME", 
#            "HAUL_BEGIN_DATE", "HAUL_BEGIN_TIME", "HAUL_BEGIN_LAT", "HAUL_BEGIN_LON", "HAUL_END_LAT", "HAUL_END_LON",
#            "postTRT", "MONTH", "YEAR", "SOAK", "SET_LAG", "NUM_FLTS", "NUM_HKS_SET", "HKS_PER_FLT", "FLTLN_LEN", 
#            "BRNCHLN_LEN", "LDR_LEN", "MIN_LEN", "BLUEDYE", "BAIT_CODE_VAL", "NUM_LITE_DEVICES", "NUM_CAUGHT", 
#            "KEPT", "FISH", "SHARKS", "ODONT", "TURTLES", "SWO", "BET", "TUNA", "MAHI", "CPUE", "MM_YN", "MM_sum", 
#            "DPUE", "DP", "FKW", "FKW_sum", "SFPW", "SFPW_sum", "RISS", "RISS_sum", "HAUL_BEGIN_DATE", 
#            "Slope", "Depth", "Cont_Dist", "Seamt_ID", "Seamt_Dist", "Seamt_Height", "Seamt_Depth",
#            "EDDY_FID", "EDDY_DIST", "AMPLITUDE", "CYCL_TYPE", "SPEED", "RADIUS", "SST_2", "ChlA", "SSH", "Moon_Illum", "ONI", "EL_LA_NO"))


# test if any duplicates
# !duplicated returns a single instance for combination of any specified columns
sets_alldata_test[duplicated(sets_alldata_test[2:4]),]

####################################################################################
### test, change variable types, or address additional concerns
####################################################################################

summary(sets_alldata)
sapply(sets_alldata, class)

sets_alldata$TRIP_ID <- as.factor(sets_alldata$TRIP_ID)
sets_alldata$VESSEL_ID <- as.factor(sets_alldata$VESSEL_ID)
sets_alldata$BLUEDYE <- as.factor(sets_alldata$BLUEDYE)
sets_alldata$EDDY_FID <- as.factor(sets_alldata$EDDY_FID)
sets_alldata$Seamt_ID <- as.factor(sets_alldata$Seamt_ID)

# filter sets with more fish caught than hooks set
# filter sets in eastern hemisphere (except the point right at the line), noticed in GIS one 
#     set end lon that probably just missed negative, way in west pacific so removed
# lots of short soaks but many hooks set, not sure what right cutoff should be..
# delete one set with zero floats
sets_alldata <- sets_alldata %>%
  filter(NUM_HKS_SET > NUM_CAUGHT) %>%
  filter(NUM_HKS_SET > NUM_FLTS) %>%
  filter(SET_END_LON > 175 | SET_END_LON < 0) %>%
  #filter(!(SOAK < 120 & NUM_HKS_SET > 1500)) %>%
  filter(NUM_FLTS > 0)

## sets with FKW OR depredation
sets_alldata <- sets_alldata %>%
  mutate(MM_any = ifelse(MM_sum >= 1 | FKW == 1, 1, 0))

## SST range
SST_range <- read_feather("Data/SST_range.feather")
sets_alldata <- left_join(sets_alldata, SST_range, by=c("TRIP_ID", "VESSEL_ID", "SET_NUM"))


####################################################################################
### lags, distances, levels of depredation
####################################################################################

#### SET LAG - for now set negatives to NA, should handle double sets better..
## end haul to begin set
sets_alldata <- sets_alldata %>% arrange(TRIP_ID, SET_NUM) %>%
  mutate(SET_LAG = SET_BEGIN_DATETIME - lag(HAUL_END_DATETIME))
sets_alldata[which(sets_alldata$SET_NUM == 1), "SET_LAG"] <- NA
sets_alldata <- sets_alldata %>% arrange(TRIP_ID, SET_NUM) %>%
  mutate(SET_LAG = ifelse(SET_NUM == (lag(SET_NUM) + 1), SET_LAG, NA)) %>%
  mutate(SET_LAG = ifelse(SET_LAG > 0, SET_LAG, NA)) %>% 
  mutate(SET_LAG = SET_LAG/60/60)

### SET LAG based on haul begins (to match move on rules from arc)
sets_alldata <- sets_alldata %>% arrange(TRIP_ID, SET_NUM) %>%
  mutate(SET_LAG_HAULS = HAUL_BEGIN_DATETIME - lag(HAUL_BEGIN_DATETIME))
sets_alldata[which(sets_alldata$SET_NUM == 1), "SET_LAG_HAULS"] <- NA
sets_alldata <- sets_alldata %>% arrange(TRIP_ID, SET_NUM) %>%
  mutate(SET_LAG_HAULS = ifelse(SET_NUM == (lag(SET_NUM) + 1), SET_LAG_HAULS, NA)) %>%
  mutate(SET_LAG_HAULS = ifelse(SET_LAG_HAULS > 0, SET_LAG_HAULS, NA)) %>% 
  mutate(SET_LAG_HAULS = SET_LAG_HAULS/60)

## set lag distance
# first column lon, second lat
#rdist.earth.vec(sets_endhaul, sets_beginset, miles = F, R = 6371)
sets_alldata <- sets_alldata %>%
  mutate(LAG_DIST = rdist.earth.vec(matrix(c(SET_BEGIN_LON, SET_BEGIN_LAT), ncol=2), matrix(c(lag(HAUL_END_LON), lag(HAUL_END_LAT)), ncol=2), miles = F, R = 6371))
sets_alldata[which(sets_alldata$SET_NUM == 1), "LAG_DIST"] <- NA
sets_alldata <- mutate(sets_alldata, LAG_DIST = ifelse(SET_NUM == (lag(SET_NUM) + 1), LAG_DIST, NA))
sets_alldata <- mutate(sets_alldata, SET_LAG = ifelse(SET_NUM == (lag(SET_NUM) + 1), SET_LAG, NA))

### LAG DIST based on haul begins (to match move on rules from arc)
sets_alldata <- sets_alldata %>%
  mutate(LAG_DIST_HAULS = rdist.earth.vec(matrix(c(HAUL_BEGIN_LON, HAUL_BEGIN_LAT), ncol=2), matrix(c(lag(HAUL_BEGIN_LON), lag(HAUL_BEGIN_LAT)), ncol=2), miles = F, R = 6371))
sets_alldata[which(sets_alldata$SET_NUM == 1), "LAG_DIST_HAULS"] <- NA
sets_alldata <- mutate(sets_alldata, LAG_DIST_HAULS = ifelse(SET_NUM == (lag(SET_NUM) + 1), LAG_DIST_HAULS, NA))

## previous set DP
sets_alldata <- sets_alldata %>%
	mutate(DP_LAG1 = ifelse(lag(MM_YN == 1), 1, 0)) %>%
	mutate(DP_LAG1 = ifelse(SET_NUM == (lag(SET_NUM) + 1), DP_LAG1, NA))
## previous set DP, MM > 1
sets_alldata <- sets_alldata %>%
  mutate(DP_LAG2 = ifelse(lag(MM_sum >= 2), 1, 0)) %>%
  mutate(DP_LAG2 = ifelse(SET_NUM == (lag(SET_NUM) + 1), DP_LAG2, NA))
## previous set DP, MM > 2
sets_alldata <- sets_alldata %>%
  mutate(DP_LAG3 = ifelse(lag(MM_sum >= 3), 1, 0)) %>%
  mutate(DP_LAG3 = ifelse(SET_NUM == (lag(SET_NUM) + 1), DP_LAG3, NA))
## previous set DP, MM > 3
sets_alldata <- sets_alldata %>%
  mutate(DP_LAG4 = ifelse(lag(MM_sum >= 4), 1, 0)) %>%
  mutate(DP_LAG4 = ifelse(SET_NUM == (lag(SET_NUM) + 1), DP_LAG4, NA))
## previous set DP exact number
sets_alldata <- sets_alldata %>%
  mutate(DP_LAG_NUM = lag(MM_sum)) %>%
  mutate(DP_LAG_NUM = ifelse(SET_NUM == (lag(SET_NUM) + 1), DP_LAG_NUM, NA))


## level of depredation
sets_deep_all <- sets_deep_all %>% 
  mutate(MMdep_level = ifelse(MM_sum == 0, "0",
                            ifelse(MM_sum > 0 & MM_sum <= 5, "1-5",
                                   ifelse(MM_sum > 5 & MM_sum <= 10, "6-10",
                                          ifelse(MM_sum > 10 & MM_sum <= 20, "1-5",
                                                 ifelse(MM_sum > 20, "20+", "NA"))))))

sets_deep_all <- sets_deep_all %>% 
  mutate(MMdep_level = as.factor(MMdep_level))

sets_deep_all$MM_cut <- cut(sets_deep_all$MM_sum, c(0, 1, 2, 4, 10, Inf), right = FALSE)
summary(sets_deep_all$MM_cut)

#sets_deep_all <- sets_deep_all %>% 
#  mutate(MMdep_rate = MM_sum / )
                              
        
####################################################################################
### split gear types, save csvs/feathers
####################################################################################

                      
## split to shallow/deep
# all sets pre 6/22/04 are classified as deep sets,
sets_deep_all <- sets_alldata %>%
  filter(DECLARED_TRIP != 'S')
sets_shallow_all <- sets_alldata %>%
  filter(DECLARED_TRIP == 'S')

summary(sets_deep_all)
summary(sets_shallow_all)

#sets_alldata <- read_feather("Data/sets_alldata.feather")

write_feather(sets_alldata, "Data/sets_alldata.feather")
write_feather(sets_deep_all, "Data/sets_deep_all.feather")
write_feather(sets_shallow_all, "Data/sets_shallow_all.feather")

sapply(sets_alldata,class)

# write csvs
write.csv(sets_alldata, "Data/sets_alldata.csv")
write.csv(sets_deep_all, "Data/sets_deep_all.csv")

sets_deep_all <- read_feather("Data/sets_deep_all.feather")



####################################################################################
### adding additional front/oceanographic data that I did just for deep sets (12/2018) - also chlorophyll
####################################################################################

## import table from GIS
sets_deep_fronts <- as.data.frame(read.csv("Data/GIS_added_files/sets_deep_fronts_2018-12-14.csv", header = TRUE))
cat(paste(shQuote(colnames(sets_deep_fronts), type="cmd"), collapse=", "))

sets_deep_fronts <- sets_deep_fronts %>%
  select(c("UID", "TRIP_ID", "VESSEL_ID", "SET_NUM",
           "nsst_mt9k", "nsst_8d9k", "nsst_ann9k", "front_dis", "tke", "adt", "eke", 
           "chla_1d_4k", "chla_1d_9k", "chla_8d_4k", "chla_8d_9k", "chla_mo_4k", "chla_mo_9k"))

sets_deep_all <- left_join(sets_deep_all, sets_deep_fronts, by = c("ID" = "UID", "TRIP_ID" = "TRIP_ID", "VESSEL_ID" = "VESSEL_ID", "SET_NUM" = "SET_NUM"))

sapply(sets_front_test,class)

cat(paste(shQuote(colnames(sets_alldata_test), type="cmd"), collapse=", "))

sets_deep_all <- sets_deep_all %>% mutate(front_dis = ifelse(front_dis == -9999, NA, front_dis))


## chlorophyll - original GIS file has 6 different resolutions, see how many NAs, monthly 9km is lowest
## located here after 'split' because this came from only deep sets

## setting -9999 to NA and counting NAs for different chlorophyl measurements
# (chla_1d_4k, chla_1d_9k, chla_8d_4k, chla_8d_9k, chla_mo_4k, chla_mo_9k)
sets_deep_all <- sets_deep_all %>% mutate(chla_1d_4k = ifelse(chla_1d_4k == -9999, NA, chla_1d_4k))
sets_deep_all <- sets_deep_all %>% mutate(chla_1d_9k = ifelse(chla_1d_9k == -9999, NA, chla_1d_9k))
sets_deep_all <- sets_deep_all %>% mutate(chla_8d_4k = ifelse(chla_8d_4k == -9999, NA, chla_8d_4k))
sets_deep_all <- sets_deep_all %>% mutate(chla_8d_9k = ifelse(chla_8d_9k == -9999, NA, chla_8d_9k))
sets_deep_all <- sets_deep_all %>% mutate(chla_mo_4k = ifelse(chla_mo_4k == -9999, NA, chla_mo_4k))
sets_deep_all <- sets_deep_all %>% mutate(chla_mo_9k = ifelse(chla_mo_9k == -9999, NA, chla_mo_9k))

# add up NAs for each type
c(sum(is.na(sets_deep_all$chla_1d_4k)), sum(is.na(sets_deep_all$chla_1d_9k)), 
  sum(is.na(sets_deep_all$chla_8d_4k)), sum(is.na(sets_deep_all$chla_8d_9k)),
  sum(is.na(sets_deep_all$chla_mo_4k)), sum(is.na(sets_deep_all$chla_mo_9k)),
  sum(is.na(sets_deep_all$ChlA)))

## remove chlorophylls not using (all but monthly 9km)
sets_deep_all <- sets_deep_all %>% select(-c(chla_1d_4k, chla_1d_9k, chla_8d_4k, chla_8d_9k, chla_mo_4k, ChlA))

## look at diff SST variables
View(sets_deep_all[,grepl('sst', colnames(sets_deep_all)) | grepl('SST', colnames(sets_deep_all))])
