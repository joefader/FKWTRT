#############################################################################
# Data import and cleaning of HI PLL observer collected data for FKWTRT analysis
# Joseph Fader
# January 4, 2018

###headers
##sub steps
#notes

library(ggplot2)
library(tidyverse)
library(dplyr)
library(lubridate)
library(chron)
library(summary_table)


#############################################################################
### get data into R

## import catch datasets into R objects - sourced from raw csv files in project data folder - original xlsx files backed up separately

LL2003 <- read.csv("/Users/josephfader/Google Drive/PhDuke/Projects/HPLL_TRT/Data/raw-csvs/LL2003.csv", header = TRUE)
LL2004 <- read.csv("/Users/josephfader/Google Drive/PhDuke/Projects/HPLL_TRT/Data/raw-csvs/LL2004.csv", header = TRUE)
LL2005 <- read.csv("/Users/josephfader/Google Drive/PhDuke/Projects/HPLL_TRT/Data/raw-csvs/LL2005.csv", header = TRUE)
LL2006 <- read.csv("/Users/josephfader/Google Drive/PhDuke/Projects/HPLL_TRT/Data/raw-csvs/LL2006.csv", header = TRUE)
LL2007 <- read.csv("/Users/josephfader/Google Drive/PhDuke/Projects/HPLL_TRT/Data/raw-csvs/LL2007.csv", header = TRUE)
LL2008 <- read.csv("/Users/josephfader/Google Drive/PhDuke/Projects/HPLL_TRT/Data/raw-csvs/LL2008.csv", header = TRUE)
LL2009 <- read.csv("/Users/josephfader/Google Drive/PhDuke/Projects/HPLL_TRT/Data/raw-csvs/LL2009.csv", header = TRUE)
LL2010 <- read.csv("/Users/josephfader/Google Drive/PhDuke/Projects/HPLL_TRT/Data/raw-csvs/LL2010.csv", header = TRUE)
LL2011 <- read.csv("/Users/josephfader/Google Drive/PhDuke/Projects/HPLL_TRT/Data/raw-csvs/LL2011.csv", header = TRUE)
LL2012 <- read.csv("/Users/josephfader/Google Drive/PhDuke/Projects/HPLL_TRT/Data/raw-csvs/LL2012.csv", header = TRUE)
LL2013 <- read.csv("/Users/josephfader/Google Drive/PhDuke/Projects/HPLL_TRT/Data/raw-csvs/LL2013.csv", header = TRUE)
LL2014 <- read.csv("/Users/josephfader/Google Drive/PhDuke/Projects/HPLL_TRT/Data/raw-csvs/LL2014.csv", header = TRUE)
LL2015 <- read.csv("/Users/josephfader/Google Drive/PhDuke/Projects/HPLL_TRT/Data/raw-csvs/LL2015.csv", header = TRUE)
LL2016 <- read.csv("/Users/josephfader/Google Drive/PhDuke/Projects/HPLL_TRT/Data/raw-csvs/LL2016.csv", header = TRUE)
LL2017 <- read.csv("/Users/josephfader/Google Drive/PhDuke/Projects/HPLL_TRT/Data/raw-csvs/LL2017.csv", header = TRUE)

## combine into single data frame - raw file so do not need to re-import indiv years
# DO NOT DELETE OR MODIFY 'all_years_catch_raw' ----- now 'raw_catch_allyears

# note warning messages for 'invalid factor levels'
all_years_catch_raw <- rbind(LL2004, LL2005, LL2006, LL2007, LL2008, LL2009, LL2010, LL2011, LL2012, LL2013, LL2014, LL2015, LL2016, LL2017)

## remove single years from R environment - do after step below - making working copy
rm(LL2003, LL2004, LL2005, LL2006, LL2007, LL2008, LL2009, LL2010, LL2011, LL2012, LL2013, LL2014, LL2015, LL2016, LL2017)






#############################################################################
#################### TO START OVER - START HERE ##################

## create working copy from the raw data frame
catch_allyears <- raw_catch_allyears

#############################################################################
### check and set variable types

sapply(catch_allyears, class)

## convert certain factors to numeric (num hooks, light sticks)
catch_allyears$NUM_HKS_SET <- as.numeric(catch_allyears$NUM_HKS_SET)
catch_allyears$NUM_LITE_DEVICES <- as.numeric(catch_allyears$NUM_LITE_DEVICES)

## convert character to factor
catch_allyears$EXPR_TRIP_TYPE_CODE <- as.factor((catch_allyears$EXPR_TRIP_TYPE_CODE))
summary(catch_allyears$EXPR_TRIP_TYPE_CODE)

## convert date time
# using lubridate which stores as POSIX
catch_allyears$SET_BEGIN_DATETIME <- mdy_hms(catch_allyears$SET_BEGIN_DATETIME, tz="")
catch_allyears$SET_END_DATETIME <- mdy_hms(catch_allyears$SET_END_DATETIME, tz="")
catch_allyears$HAUL_BEGIN_DATETIME <- mdy_hms(catch_allyears$HAUL_BEGIN_DATETIME, tz="")
catch_allyears$HAUL_END_DATETIME <- mdy_hms(catch_allyears$HAUL_END_DATETIME, tz="")

## here as.character.Date - alternative to lubridate/posix
# catch_allyears$SET_BEGIN_DATETIME <- as.character.Date(catch_allyears$SET_BEGIN_DATETIME)
# catch_allyears$SET_END_DATETIME <- as.character.Date(catch_allyears$SET_END_DATETIME)
# catch_allyears$HAUL_BEGIN_DATETIME <- as.character.Date(catch_allyears$HAUL_BEGIN_DATETIME)
# catch_allyears$HAUL_END_DATETIME <- as.character.Date(catch_allyears$HAUL_END_DATETIME)

## split date/time into new columns - if needed...only works if as.character.Date
catch_allyears$HAUL_BEGIN_DATE <- as.Date(sapply(strsplit(catch_allyears$HAUL_BEGIN_DATETIME, " "), "[", 1), "%m/%d/%Y")

#############################################################################
### new variables

## indicator for MM damage
catch_allyears <- mutate(catch_allyears, MM = ifelse(DAMAGE_CODE_VAL == 'Marine mammal damage', 1, 0))
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
unique(catch_allyears$SPECIES_COMMON_NAME)
catch_allyears <- mutate(catch_allyears, FKW_caught = ifelse(SPECIES_COMMON_NAME == 'Whale, False Killer', 1, 0))
catch_allyears <- mutate(catch_allyears, SFPW_caught = ifelse(SPECIES_COMMON_NAME == 'Whale, Short-finned Pilot', 1, 0))
catch_allyears <- mutate(catch_allyears, RISS_caught = ifelse(SPECIES_COMMON_NAME == 'Dolphin, Risso\'s', 1, 0))

## target species
# note including all not just 'kept' fish

## SWO
catch_allyears <- mutate(catch_allyears, SWO = ifelse(SPECIES_COMMON_NAME == 'Swordfish', 1, 0))
## BET
catch_allyears <- mutate(catch_allyears, BET = ifelse(SPECIES_COMMON_NAME == 'Tuna, Bigeye', 1, 0))
## all tunas
catch_allyears <- mutate(catch_allyears, TUNA = ifelse(grepl('Tuna', SPECIES_COMMON_NAME), 1, 0))
## mahi
catch_allyears <- mutate(catch_allyears, MAHI = ifelse(grepl('Dolphinfish', SPECIES_COMMON_NAME), 1, 0))


#############################################################################
### filter out undeclared sets, invalid trips, experimental trips

# only using 2004-2017, 6-22-2004 was start of declaring deep vs shallow
# not sure what to do with undeclared sets in 03-04, but for now filtering out any undeclared sets
# note including all except invalid trips, might not be appropriate to include unfinalized versions in analysis
catch_allyears <- catch_allyears %>%
  filter(DECLARED_TRIP_TYPE_CODE == 'S' | DECLARED_TRIP_TYPE_CODE == 'D') %>%
  filter(TRIP_APPROVAL_STATUS != 'INVALID')

## also must address experimental sets/vessels
# here removing all with any type of exp status
# tho note this is only for trips, Karin removed all
# subsequent trips for any vessel that participated
catch_allyears <- catch_allyears %>%
  filter(is.na(EXPR_TRIP_TYPE_CODE) | EXPR_TRIP_TYPE_CODE == "")

summary(catch_allyears)

#####################################################################
### sets - aggregate catch data to set level

sets_allyears <- group_by(catch_allyears, TRIP_ID, SET_NUM) %>%
  summarise(
    VESSEL_ID = first(VESSEL_ID),
    DECLARED_TRIP = first(DECLARED_TRIP_TYPE_CODE),
    #EXPR_TRIP_TYPE_CODE = first(EXPR_TRIP_TYPE_CODE),
    SET_BEGIN_DATETIME = first(SET_BEGIN_DATETIME),
    SET_END_DATETIME = first(SET_END_DATETIME),
    SET_BEGIN_LAT = first(SET_BEGIN_LAT),
    SET_BEGIN_LON = first(SET_BEGIN_LON),
    HAUL_BEGIN_DATETIME = first(HAUL_BEGIN_DATETIME),
    HAUL_END_DATETIME = first(HAUL_END_DATETIME),
    HAUL_BEGIN_LAT = first(HAUL_BEGIN_LAT),
    HAUL_BEGIN_LON = first(HAUL_BEGIN_LON),
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
    BLUEDYE = first(DS_BLUEDYED_YN),
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

sapply(sets_allyears.df, class)
summary(sets_allyears)


#####################################################################
### prepare for GIS

## deep and shallow - make haul day in format for GIS
# to split must be in character
# then set to date before changing to desired format - reconvert to string?
sets_allyears.df <- as.data.frame(sets_allyears)
sets_allyears.df$HAUL_BEGIN_DATE <- as.character(sets_allyears$HAUL_BEGIN_DATETIME, tz="")
sets_allyears.df$HAUL_BEGIN_DATE <- sapply(strsplit(sets_allyears.df$HAUL_BEGIN_DATE, " "), "[", 1) #, "%m/%d/%Y")
sets_allyears.df$HAUL_BEGIN_DATE <- as.Date(sets_allyears.df$HAUL_BEGIN_DATE)
sets_allyears.df$HAUL_BEGIN_DATE <- format(sets_allyears.df$HAUL_BEGIN_DATE, "%m/%d/%Y")

## give a row id
sets_allyears.df <- tibble::rowid_to_column(sets_allyears.df, "ID")

## write out for GIS ##
write.csv(sets_allyears.df, file = "/Users/josephfader/Google Drive/PhDuke/Projects/HPLL_TRT/Data/sets_allyears.csv", quote = FALSE, col.names = T)


#####################################################################
### back from GIS

# import shapefile attribute table - making one GIS data frame of bathy and ocean vars, can update/modify as needed
# then combine this with full combined df
# this is bathymetric GIS file, distances in planar
sets_allyears_bathy <- as.data.frame(read.csv("/Users/josephfader/Google Drive/PhDuke/Projects/HPLL_TRT/Data//GIS_added_files/Vessel_bathy_planar.csv", header = TRUE))

# add FID column (in gdb in arc the FID starts at 0, adding 1 to match to other tables)
sets_allyears_bathy$FID_one <- sets_allyears_bathy$FID + 1

# seamounts dataset
seamounts <- as.data.frame(read.csv("/Users/josephfader/Google Drive/PhDuke/Projects/HPLL_TRT/Data/GIS_added_files/seamounts.csv", header = TRUE))
seamount_shapefile <- as.data.frame(read.csv("/Users/josephfader/Google Drive/PhDuke/Projects/HPLL_TRT/Data/GIS_added_files/seamount_shapefile.csv", header = TRUE))
seamounts <- left_join(seamounts, seamount_shapefile, by = c("ID" = "Name"))
# get column names, then select which ones to keep
cat(paste(shQuote(colnames(seamounts), type="cmd"), collapse=", "))
seamounts <- seamounts %>%
  select(c("Seamt_Height", "Seamt_Depth", "ID", "FID"))
sets_allyears_bathy <- left_join(sets_allyears_bathy, seamounts, by = c("Seamt_ID" = "FID"))

# pick columns to keep
sets_allyears_bathy <- sets_allyears_bathy %>%
  select(c(FID, FID_one, UID, TRIP_ID, VESSEL_ID, TRIP_TYPE, SET_NUM, DAY, MM_YN, 
           Slope, Depth, Cont_Dist, Seamt_ID, Seamt_Dist, Seamt_Height, Seamt_Depth))

# import oceanographic variables from csv
sets_allyears_ocean <- as.data.frame(read.csv("/Users/josephfader/Google Drive/PhDuke/Projects/HPLL_TRT/Data/GIS_added_files/vessel_ocean.csv", header = TRUE))

# import Eddy near tables
EddyLineDist_1 <- read.csv("/Users/josephfader/Google Drive/PhDuke/Projects/HPLL_TRT/Data//GIS_added_files/EddyLineDist_1.csv", header = TRUE)
EddyLineDist_2 <- read.csv("/Users/josephfader/Google Drive/PhDuke/Projects/HPLL_TRT/Data//GIS_added_files/EddyLineDist_2.csv", header = TRUE)

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


# merge using left join, keeps all x, specify which columns to join by
sets_allyears_GIS <- left_join(sets_allyears_bathy, EddyLineDistance, by = c("FID_one" = "IN_FID"))
sets_allyears_GIS <- left_join(sets_allyears_GIS, sets_allyears_ocean, by = "UID")

# a little cleanup
sets_allyears_GIS <- sets_allyears_GIS %>% mutate(ChlA = ifelse(ChlA < -9998, NA, ChlA))
sets_allyears_GIS <- sets_allyears_GIS %>% mutate(SSH = ifelse(SSH == -9999, NA, SSH))

# merging GIS dataframe to all sets data
sets_allyears_alldata <- left_join(sets_allyears.df, sets_allyears_GIS, by = c("ID" = "UID"))
cat(paste(shQuote(colnames(sets_allyears_alldata), type="cmd"), collapse=", "))
cat(paste(sQuote(colnames(sets_allyears_alldata)), collapse=", "))

sets_allyears_alldata <- sets_allyears_alldata %>%
  select(c("ID", "TRIP_ID", "SET_NUM", "VESSEL_ID", "DECLARED_TRIP", "HAUL_BEGIN_DATETIME", "HAUL_END_DATETIME", 
           "HAUL_BEGIN_LAT", "HAUL_BEGIN_LON", "SOAK", "NUM_FLTS", "NUM_HKS_SET", "HKS_PER_FLT", "FLTLN_LEN", 
           "BRNCHLN_LEN", "LDR_LEN", "MIN_LEN", "BLUEDYE", "BAIT_CODE_VAL", "NUM_LITE_DEVICES", "NUM_CAUGHT", 
           "KEPT", "FISH", "SHARKS", "ODONT", "TURTLES", "SWO", "BET", "TUNA", "MAHI", "CPUE", "MM_YN", "MM_sum", 
           "DPUE", "DP", "FKW", "FKW_sum", "SFPW", "SFPW_sum", "RISS", "RISS_sum", "HAUL_BEGIN_DATE", 
           "Slope", "Depth", "Cont_Dist", "Seamt_ID", "Seamt_Dist", "Seamt_Height", "Seamt_Depth",
           "EDDY_FID", "EDDY_DIST", "AMPLITUDE", "CYCL_TYPE", "SPEED", "RADIUS", "SST", "ChlA", "SSH"))
summary(sets_allyears_alldata)

deepsets_all <- sets_allyears_alldata %>%
  filter(DECLARED_TRIP == 'D')
shallowsets_all <- sets_allyears_alldata %>%
  filter(DECLARED_TRIP != 'D')

summary(deepsets_all)
summary(shallowsets_all)
summary_table(deepsets_all)
