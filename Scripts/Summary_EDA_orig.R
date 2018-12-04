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

#############################################################################
### create summary tables

# define markup language
options(qwraps2_markup = "markdown")

# set variables to summarize
my_summary <- 
  with(sets_allyears_alldata, 
       list("SET_NUM" = tab_summary(SET_NUM),
            "SOAK" = tab_summary(SOAK),
            "NUM_FLTS" = tab_summary(NUM_FLTS),
            "NUM_HKS_SET" = tab_summary(NUM_HKS_SET),
            "HKS_PER_FLT" = tab_summary(HKS_PER_FLT),
            "FLTLN_LEN" = tab_summary(FLTLN_LEN),
            "BRNCHLN_LEN" = tab_summary(BRNCHLN_LEN),
            "LDR_LEN" = tab_summary(LDR_LEN),
            "MIN_LEN" = tab_summary(MIN_LEN),
            "BLUEDYE" = tab_summary(BLUEDYE),
            "BAIT_CODE_VAL" = tab_summary(BAIT_CODE_VAL),
            "NUM_LITE_DEVICES" = tab_summary(NUM_LITE_DEVICES),
            "KEPT" = tab_summary(KEPT),
            "FISH" = tab_summary(FISH),
            "SHARKS" = tab_summary(SHARKS),
            "ODONT" = tab_summary(ODONT),
            "TURTLES" = tab_summary(TURTLES),
            "SWO" = tab_summary(SWO),
            "BET" = tab_summary(BET),
            "TUNA" = tab_summary(TUNA),
            "MAHI" = tab_summary(MAHI),
            "CPUE" = tab_summary(CPUE),
            "MM_YN" = tab_summary(MM_YN),
            "MM_sum" = tab_summary(MM_sum),
            "DPUE" = tab_summary(DPUE),
            "DP" = tab_summary(DP),
            "FKW" = tab_summary(FKW),
            "FKW_sum" = tab_summary(FKW_sum),
            "SFPW" = tab_summary(SFPW),
            "SFPW_sum" = tab_summary(SFPW_sum),
            "RISS" = tab_summary(RISS),
            "RISS_sum" = tab_summary(RISS_sum),
            "Slope" = tab_summary(Slope),
            "Depth" = tab_summary(Depth),
            "Cont_Dist" = tab_summary(Cont_Dist),
            "Seamt_Dist" = tab_summary(Seamt_Dist),
            "Seamt_Height" = tab_summary(Seamt_Height),
            "Seamt_Depth" = tab_summary(Seamt_Depth),
            "EDDY_DIST" = tab_summary(EDDY_DIST),
            "AMPLITUDE" = tab_summary(AMPLITUDE),
            "CYCL_TYPE" = tab_summary(CYCL_TYPE),
            "SPEED" = tab_summary(SPEED),
            "RADIUS" = tab_summary(RADIUS),
            "SST" = tab_summary(SST),
            "ChlA" = tab_summary(ChlA),
            "SSH" = tab_summary(SSH)
       ))

args(tab_summary)
sum_tab <- summary_table(dplyr::group_by(sets_allyears_alldata, DECLARED_TRIP), my_summary)
qable(sum_tab)

#############################################################################
### EDA

### trip level
trip_all <- group_by(sets_allyears_alldata, TRIP_ID) %>%
  summarise(
    VESSEL_ID = first(VESSEL_ID),
    SET_NUM = max(SET_NUM),
    DECLARED_TRIP = first(DECLARED_TRIP)
  )

## unique trips = 3,579
dstrip <- group_by(deepsets_all, TRIP_ID) %>%
  summarise(
    VESSEL_ID = first(VESSEL_ID),
    SET_NUM = max(SET_NUM)
    )

## number of sets 

#mean (sd) = 13.41 (3.71)
mean_sd(dstrip$SET_NUM)
# sets/trip plot
ggplot(data = dstrip) +
  geom_bar(mapping = aes(x = SET_NUM))
# sets/trip, both types
ggplot(data = trip_all, mapping = aes(x = SET_NUM, colour = DECLARED_TRIP)) +
  geom_freqpoly(binwidth = 1 )

## number of unique vessels (DS = 175)
length(unique(dstrip$VESSEL_ID))
# and counts by vessel
deepsets_all %>%
  count(VESSEL_ID)
  
## soak

# plotting both deep and shallow with lines
ggplot(data = sets_allyears_alldata, mapping = aes(x = SOAK, colour = DECLARED_TRIP)) +
  geom_freqpoly(binwidth = 1 )
# just deep set
ggplot(data = deepsets_all) +
  geom_histogram(mapping = aes(x = SOAK), binwidth = 1)
deepsets_all %>%
  count(cut_width(SOAK, 1))
# depredation occurrence
ggplot(data = deepsets_all, mapping = aes(x = SOAK, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 0.5 )
ggplot(data = deepsets_all, mapping = aes(x = MM_YN, y = SOAK)) +
  geom_boxplot()

## number of floats
ggplot(data = sets_allyears_alldata, mapping = aes(x = NUM_FLTS, colour = DECLARED_TRIP)) +
  geom_freqpoly(binwidth = 5 )


## number of hooks set
ggplot(data = sets_allyears_alldata, mapping = aes(x = NUM_HKS_SET, colour = DECLARED_TRIP)) +
  geom_freqpoly(binwidth = 100 )
ggplot(data = deepsets_all, mapping = aes(x = NUM_HKS_SET)) +
  geom_histogram(binwidth = 200 )
ggplot(data = deepsets_all, mapping = aes(x = NUM_HKS_SET, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 100 )
ggplot(data = deepsets_all, mapping = aes(x = MM_YN, y = NUM_HKS_SET)) +
  geom_boxplot()

## minimum length/depth
ggplot(data = sets_allyears_alldata, mapping = aes(x = MIN_LEN, colour = DECLARED_TRIP)) +
  geom_freqpoly(binwidth = 1 )
ggplot(data = deepsets_all, mapping = aes(x = MIN_LEN, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 1 )
ggplot(data = deepsets_all, mapping = aes(x = MM_YN, y = MIN_LEN)) +
  geom_boxplot()

## blue dye
summary(deepsets_all$BLUEDYE)

## bait code
deepsets_all %>%
  filter(MM_YN == 1) %>%
  ggplot() +
  geom_bar(mapping = aes(x = BAIT_CODE_VAL))

ggplot(data = deepsets_all, mapping = aes(x = MM_YN, y = BAIT_CODE_VAL)) +
  geom_count()

deepsets_all %>%
  count(MM_YN, BAIT_CODE_VAL) %>%
  ggplot(mapping = aes(x = MM_YN, y = BAIT_CODE_VAL)) + 
    geom_tile(mapping = aes(fill = n))


## number light devices
ggplot(data = deepsets_all, mapping = aes(x = NUM_LITE_DEVICES, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(stat = )

## number caught
ggplot(data = deepsets_all, mapping = aes(x = NUM_CAUGHT, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 10 )

## number kept
ggplot(data = deepsets_all, mapping = aes(x = KEPT, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 10 )
ggplot(data = deepsets_all, mapping = aes(x = MM_YN, y = KEPT)) +
  geom_boxplot()

## fish caught
ggplot(data = deepsets_all, mapping = aes(x = FISH, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 10 )

## sharks caught
ggplot(data = deepsets_all, mapping = aes(x = SHARKS, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 1 )
ggplot(data = deepsets_all, mapping = aes(x = MM_YN, y = SHARKS)) +
  geom_boxplot()

## Slope
ggplot(data = deepsets_all, mapping = aes(x = Slope, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 0.5 )

## Depth
ggplot(data = deepsets_all, mapping = aes(x = Depth, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 100 )

## Contour Distance
ggplot(data = deepsets_all, mapping = aes(x = Cont_Dist/1000, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 10 )

## Seamount distance
ggplot(data = sets_allyears_alldata, mapping = aes(x = Seamt_Dist, colour = DECLARED_TRIP)) +
  geom_freqpoly(binwidth = 1000 )
ggplot(data = deepsets_all, mapping = aes(x = Seamt_Dist/1000, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 10 )
ggplot(data = deepsets_all, mapping = aes(x = MM_YN, y = Seamt_Dist/1000)) +
  geom_boxplot()

## eddy distance
ggplot(data = sets_allyears_alldata, mapping = aes(x = EDDY_DIST, colour = DECLARED_TRIP)) +
  geom_freqpoly(binwidth = 1000 )
ggplot(data = deepsets_all, mapping = aes(x = EDDY_DIST/1000, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 10 )
ggplot(data = deepsets_all, mapping = aes(x = MM_YN, y = EDDY_DIST/1000)) +
  geom_boxplot()

## SST

## ChlA
ggplot(data = deepsets_all, mapping = aes(x = ChlA, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 0.1 )

## SSH

cont_vars <- deepsets_all %>%
  select(-c(ID, TRIP_ID,SET_NUM,VESSEL_ID, DECLARED_TRIP, HAUL_BEGIN_DATETIME, HAUL_END_DATETIME, BLUEDYE, BAIT_CODE_VAL, 
            HAUL_BEGIN_DATE, MM_YN, DP, FKW, FKW_sum, SFPW_sum, SFPW, RISS_sum, RISS, Seamt_ID, EDDY_FID, CYCL_TYPE))
cat(paste(shQuote(colnames(deepsets_all), type="cmd"), collapse=", "))

for (v in cont_var_names){
  ggplot(data = deepsets_all) +
      geom_histogram(mapping = aes(x = cont_var_names[v]), binwidth = 1 )
}


ggplot(data = melt(cont_var_names), mapping = aes(x = value)) + 
  geom_histogram(bins = 10) + facet_wrap(~variable, scales = 'free_x')



# bait type
ggplot(data = deepsets_all) +
  geom_bar(mapping = aes(x = BAIT_CODE_VAL))
deepsets_all %>%
  count(BAIT_CODE_VAL)



