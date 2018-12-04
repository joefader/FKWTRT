#############################################################################
# Data summary and EDA
# Joseph Fader
# March, 2018

###headers
##sub steps
#notes

library(tidyverse)
library(qwraps2)
library(lubridate)
library(chron)
library(reshape2)
library(ecodist)

sets_alldata <- read_feather("Data/sets_alldata.feather")
sets_deep_all <- read_feather("Data/sets_deep_all.feather")
sets_shallow_all <- read_feather("Data/sets_shallow_all.feather")

sets_deep_dep <- sets_deep_all %>% filter(MM_YN == 1)

################################################################################
### look at correlations, modify vars as needed


# correlation among vars, have to remove categorical and nas
sets_deep_nona <- sets_deep_all %>% drop_na()
cortable <- cor(sets_deep_nona[sapply(sets_deep_nona, function(x) is.numeric(x))])


#ID, TRIP_ID, SET_NUM, "VESSEL_ID", "DECLARED_TRIP", "SET_BEGIN_DATETIME", "SET_END_DATETIME", "SET_BEGIN_LAT", "SET_BEGIN_LON", 
#"SET_END_LAT", "SET_END_LON", "HAUL_BEGIN_DATETIME", "HAUL_END_DATETIME", "HAUL_BEGIN_DATE", "HAUL_BEGIN_TIME", "HAUL_BEGIN_LAT",
#"HAUL_BEGIN_LON", "HAUL_END_LAT", "HAUL_END_LON", "SOAK", "SET_LAG", "NUM_FLTS", "NUM_HKS_SET", "HKS_PER_FLT", "FLTLN_LEN", 
#"BRNCHLN_LEN", "LDR_LEN", "MIN_LEN", "BLUEDYE", "BAIT_CODE_VAL", "NUM_LITE_DEVICES", "NUM_CAUGHT", "KEPT", "FISH", "SHARKS", "ODONT",
#"TURTLES", "SWO", "BET", "TUNA", "MAHI", "CPUE", "MM_YN", "MM_sum", "DPUE", "DP", "FKW", "FKW_sum", "SFPW", "SFPW_sum", "RISS", "RISS_sum",
#"Slope", "Depth", "Cont_Dist", "Seamt_ID", "Seamt_Dist", "Seamt_Height", "Seamt_Depth", "EDDY_FID", "EDDY_DIST", "AMPLITUDE", "CYCL_TYPE",
#"SPEED", "RADIUS", "SST", "ChlA", "SSH", "month", "year"

## correlate MM depredation with numeric preditors, cor2m keeps only those significant at .05
MM_YN_deep <- as.numeric(sets_deep_nona$MM_YN, na.rm=T)
sets_deep_num <-  sets_deep_nona[,sapply(sets_deep_nona, function(x) is.numeric(x))]
MM_YN_corr <- cor2m(as.matrix(MM_YN_deep), sets_deep_num[,-c(1:6,9:10)])
## creates table sorting from strongest corr to weakest, only significant
source("Scripts/Functions/spphab_cor.R")
MM_YN_habcor <- spphab.cor(MM_YN_deep,sets_deep_num[,-c(1:6,9:10)])

## ID correlated predictors, by default shows ones > 0.7, of possible concern:
# hooks set and num floats
# kept/caught and all variations
# speed and amplitude
source("Scripts/Functions/screen_cor.R")
screen.cor(sets_deep_num)
?cor2m

#############################################################################
### EDA


#############################################################################
### time and space

# scatter plot
sets_deep_dep %>%
  filter((DECLARED_TRIP == 'D')) %>%
  ggplot(aes(x = SET_LAG, y = MM_sum)) + 
  geom_point()


## histo of set lag
sets_deep_dep %>%
  filter(DECLARED_TRIP == 'D') %>%
  ggplot(aes(SET_LAG)) + geom_histogram(binwidth = 1000)
# boxplot of set lag for depred

## position
sets_deep_dep %>%
  ggplot(aes(x = HAUL_BEGIN_LAT, y = MM_sum)) + geom_point()


## pre/post TRT

summary(sets_deep_all$postTRT)
ggplot(sets_deep_dep, aes(x = postTRT, y = MM_sum)) +
  geom_count()
ggplot(sets_deep_dep, aes(x = postTRT, y = MM_sum)) +
  geom_boxplot()


## soak

# scatterplot
ggplot(sets_deep_dep, aes(x = SOAK, y = MM_sum)) +
  geom_point()

# just deep set
ggplot(data = sets_deep_all) +
  geom_histogram(mapping = aes(x = SOAK), binwidth = 100)
View(sets_deep_all %>%
       count(cut_width(SOAK, 100)))
quantile(sets_deep_all$SOAK, c(.5,.99))

#############################################################################
### gear stuff

## number of floats
ggplot(data = sets_deep_dep, mapping = aes(x = NUM_FLTS, y = MM_sum)) +
  geom_point( )



## number of hooks set - interesting bimodal pattern for shallow, why?
ggplot(data = sets_deep_dep, mapping = aes(x = NUM_HKS_SET, y = MM_sum)) +
  geom_point( )

## minimum length/depth

ggplot(data = sets_deep_dep, mapping = aes(x = MIN_LEN, y = MM_sum)) +
  geom_point( )


## blue dye - lower proportion of MM when dyed?? - do chi square
summary(sets_deep_dep$BLUEDYE)
ggplot(data = sets_deep_dep, aes(x = BLUEDYE, y = MM_sum)) +
  geom_count()

## bait code
# bait type
ggplot(data = sets_deep_dep) +
  geom_bar(mapping = aes(x = BAIT_CODE_VAL))
sets_deep_dep %>%
  count(BAIT_CODE_VAL)

ggplot(data = sets_deep_dep, mapping = aes(x = MM_sum, y = BAIT_CODE_VAL)) +
  geom_count()



#############################################################################
### catch stuff


## number caught
ggplot(data = sets_deep_dep, mapping = aes(x = NUM_CAUGHT, y = MM_sum)) +
  geom_count()

## number kept
ggplot(data = sets_deep_dep, mapping = aes(x = KEPT, y = MM_sum)) +
  geom_count()

## fish caught
ggplot(data = sets_deep_dep, mapping = aes(x = FISH, y = MM_sum)) +
  geom_count()

## sharks caught
ggplot(data = sets_deep_dep, mapping = aes(x = SHARKS, y = MM_sum)) +
  geom_count()


#############################################################################
### GIS stuff

## Slope
ggplot(data = sets_deep_dep, mapping = aes(x = Slope, y = MM_sum)) +
  geom_count()

## Depth
ggplot(data = sets_deep_dep, mapping = aes(x = Depth, y = MM_sum)) +
  geom_count()

## Contour Distance
ggplot(data = sets_deep_dep, mapping = aes(x = Cont_Dist, y = MM_sum)) +
  geom_point()

## Seamount distance
ggplot(data = sets_deep_dep, mapping = aes(x = Seamt_Dist, y = MM_sum)) +
  geom_point()

quantile(sets_deep_all$Seamt_Dist, c(.01,.99))


## eddy distance
ggplot(data = sets_deep_dep, mapping = aes(x = EDDY_DIST, y = MM_sum)) +
  geom_point()

## SST

## ChlA
ggplot(data = sets_deep_dep, mapping = aes(x = ChlA, y = MM_sum)) +
  geom_point()

## SSH


###########################################################################
## plotting by month
###########################################################################

## classify each observation by month

## this gives 1st day of year and thus can sum by month, but breaks down across years
# all_sets$month <- as.Date(cut(all_sets$SET_BEGIN_DATE, breaks = "month"))

## this gives actual year, but not in date format, allows to aggregate years though
sets_alldata$month <- as.numeric(format(sets_alldata$SET_BEGIN_DATE, "%m"))
sets_alldata$year <- as.numeric(format(sets_alldata$SET_BEGIN_DATE, "%Y"))

# grouping by month across all years, not using date functions
set_month <- group_by(sets_alldata, month)  %>%
  filter(DECLARED_TRIP == 'D') %>%
  summarise(
    MM = sum(as.numeric(MM_YN)),
    FKW = sum(as.integer(FKW)),
    TOT = n(),
    PROP = (MM/TOT),
    DPUE_occur = max(as.numeric(MM_YN)),
    DPUE_count = mean(DPUE)
    #CV = sd(MM_YN)
  )


# now grouping by month with averages by year
set_month_avg <- group_by(set_monthyear, month)  %>%
  summarise(
    MM = mean(MM, na.rm = TRUE),
    SET = mean(TOT, na.rm = TRUE),
    PROP = mean(PROP, na.rm = TRUE),
    MAX = max(PROP, na.rm = TRUE)
  )

# bar plot of month by MM metrics (count and proportion of sets)
set_month %>%
  ggplot(aes(month, DPUE)) + geom_bar(stat = "identity") + geom_errorbar()#aes(ymin=MM-CV, ymax=MM+CV))

set_month %>%
  ggplot(aes(month, PROP)) + geom_bar(stat = "identity") #+ geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd))

###########################################################################
## plotting by YEAR
###########################################################################

# grouping by year
set_year <- group_by(all_sets, year)  %>% 
  filter(DECLARED_TRIP == 'D') %>%
  summarise(
    MM = sum(MM_YN, na.rm = TRUE),
    TOT = sum(ones, na.rm = TRUE),
    DPUE = mean(DPUE),
    PROP = (MM/TOT)
  )

# bar plot of year by MM metrics (count and proportion of sets)
set_year %>%
  ggplot(aes(year, DPUE)) + geom_bar(stat = "identity") #+ geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd))

set_year %>%
  ggplot(aes(year, PROP)) + geom_bar(stat = "identity") #+ geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd))

