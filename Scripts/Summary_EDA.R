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
library(feather)
library(grid )

sets_alldata <- read_feather("Data/sets_alldata.feather")
sets_deep_all <- read_feather("Data/sets_deep_all.feather")
sets_shallow_all <- read_feather("Data/sets_shallow_all.feather")


#############################################################################
### create summary tables
#############################################################################
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



################################################################################
### look at correlations, modify vars as needed
#############################################################################

pairs(sets_alldata)
?pairs
# correlation among vars, have to remove categorical and nas
# if remove all nas, only 20 observations left
#sets_deep_nona <- sets_deep_all %>% drop_na()
sets_deep_num <-  sets_deep_all[,sapply(sets_deep_all, function(x) is.numeric(x))]
sets_deep_num <-  data.frame(sapply(sets_deep_num, function(x) as.numeric(x)))

# all variables correlated w each other
#cortable <- as.data.frame(cor(sets_deep_all[sapply(sets_deep_all, function(x) is.numeric(x))],use = "complete.obs"))

# above version doesnt work, spurious very high correlations, need to select variables directly
cat(paste(shQuote(colnames(sets_deep_all), type="cmd"), collapse=", "))
cortable <- as.data.frame(cor(subset(sets_deep_all, select = c("HAUL_BEGIN_LAT", "HAUL_BEGIN_LON", "MONTH", "YEAR", "SOAK", 
                                                      "SET_LAG", "NUM_FLTS", "NUM_HKS_SET", "HKS_PER_FLT", "FLTLN_LEN", 
                                                      "BRNCHLN_LEN", "LDR_LEN", "MIN_LEN", "NUM_CAUGHT", "KEPT", "FISH", "SHARKS",
                                                      "ODONT", "TURTLES", "SWO", "BET", "YFT", "TUNA", "MAHI", "WAHOO", "BILLFISH",
                                                      "NONTARGET", "CPUE", "CPUE_FLT", "MM_sum", "DPUE", "FKW_sum", 
                                                      "SFPW_sum", "RISS_sum", "Slope", "Depth", "Cont_Dist", 
                                                      "Seamt_Dist", "Seamt_Height", "Seamt_Depth", "EDDY_DIST", "AMPLITUDE", "SPEED",
                                                      "RADIUS", "SST_2", "ChlA", "SSH", "Moon_Illum", "ONI")), use = "complete.obs"))
write.csv(cortable,"Docs/cortable.csv")
prin
sapply(sets_deep_all,class)
## correlate MM depredation with numeric preditors, cor2m keeps only those significant at .05
# make sure factor considered as 1,0 - had to convert to chr first
MM_YN_deep <- as.data.frame(as.numeric(as.character((sets_deep_all$MM_YN))))
MM_YN_corr <- cor2m(as.matrix(MM_YN_deep), sets_deep_num[,-c(1:6,9:10)])
write.csv(MM_YN_corr, "Docs/MM_YN_corr.csv")
head(sets_deep_num)

## creates table sorting from strongest corr to weakest, only significant
source("Scripts/Functions/spphab_cor.R")
MM_YN_habcor <- spphab.cor(MM_YN_deep, sets_deep_num[,-c(1:6,9:10)])
.
## ID correlated predictors, by default shows ones > 0.7, of possible concern:
# hooks set and num floats
# kept/caught and all variations
# speed and amplitude

par("mar") 
par(mar=c(5.1,4.1,4.1,2.1))
par(mar=c(1,1,1,1))
pairs(sets_deep_num, lower.panel = NULL)
?pairs
source("Scripts/Functions/screen_cor.R")
sets_deep_habcorr <- sets_deep_num %>% drop_na() %>%
  screen.cor()
  write.csv("Docs/sets_deep_habcorr.csv")

sets_deep_all %>%
  filter(MONTH == 6) %>%
  ggplot(aes(x=YEAR, y=ONI)) + geom_jitter()
ggplot(sets_deep_all,aes(x=SST_2, y=HAUL_BEGIN_LAT, colour=MM_YN)) + geom_jitter()

ggplot(sets_deep_all,aes(x=YEAR, y=MIN_LEN)) +
  geom_smooth()
#############################################################################
### EDA - summary tables
#############################################################################

View(sets_alldata %>%
  group_by(DECLARED_TRIP) %>%
  summarise(
    totalsets = n(),
    totalMM = sum(as.numeric(as.character(MM_YN))),
    nonMM = totalsets - totalMM,
    rate = totalMM/totalsets,
    sumFKW = sum(FKW_sum)
  ))

sets_alldata %>%
  group_by(DECLARED_TRIP, MM_YN) %>%
  filter(FKW == 1) %>%
  summarise(
    totalFKW= n(),
    sumFKW = sum(FKW_sum)
  )

#############################################################################
### trip level
#############################################################################

sets_alldata %>%
  filter(DECLARED_TRIP =='S') %>%
  filter(YEAR == 2015) %>%
  summarise(
    n_distinct(VESSEL_ID)
  )
  


trip_all <- group_by(sets_alldata, TRIP_ID) %>%
  summarise(
    VESSEL_ID = first(VESSEL_ID),
    SET_NUM = max(SET_NUM),
    DECLARED_TRIP = first(DECLARED_TRIP)
  )


## num. vessel and trip total or by trt
sets_alldata %>%
  filter(DECLARED_TRIP == 'D') %>%
  #group_by(postTRT) %>%
  summarise(
    n_distinct(TRIP_ID),
    n_distinct(VESSEL_ID),
    n_distinct(SET_NUM)
  )

## unique trips = 3,856
dstrip <- group_by(sets_deep_all, TRIP_ID) %>%
  summarise(
    VESSEL_ID = first(VESSEL_ID),
    SET_NUM = max(SET_NUM)
    )

## number of sets 
sets_alldata %>%
  filter(DECLARED_TRIP == 'D') %>% 
  group_by(postTRT) %>%
  summarise(
    numsets = n()
  ) %>%
  summarise(
    sum(numsets)
  )

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
sets_deep_all %>%
  count(VESSEL_ID)
dstrip %>%
  count(VESSEL_ID)
  

#############################################################################
### time and space
#############################################################################

## set lag - note also code for polished figures in 'depredation_lags' file
# binary plot like log reg
sets_alldata %>%
  filter((DECLARED_TRIP == 'D')) %>%
  ggplot(aes(x = SET_LAG, y = MM_YN)) + 
    geom_point()
# freq plot
sets_alldata %>%
  filter(DECLARED_TRIP == 'D', DP_LAG1 == 1) %>%
  ggplot(mapping = aes(x = SET_LAG, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 10 )
# histo of set lag
sets_alldata %>%
  filter(DECLARED_TRIP == 'D') %>%
  ggplot(aes(SET_LAG)) + geom_histogram(binwidth = 5)
# boxplot of set lag for depred
ggplot(data = sets_deep_all, mapping = aes(x = MM_YN, y = SET_LAG)) +
  geom_boxplot()

## set lag distance
sets_alldata %>%
  filter(DECLARED_TRIP == 'D', DP_LAG1 == 1) %>%
  ggplot(aes(x = LAG_DIST, y = MM_YN)) + 
  geom_point()
# freq plot
sets_alldata %>%
  filter(DECLARED_TRIP == 'D', DP_LAG1 == 1) %>%
  ggplot(aes(x = LAG_DIST, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 75 )
sets_alldata %>%
  filter((DECLARED_TRIP == 'D')) %>%
  ggplot(aes(x = LAG_DIST, y = MM_sum)) + 
  geom_point()
  
# dist vs time
sets_alldata %>%
  filter(DECLARED_TRIP == 'D', DP_LAG1 == 1) %>%
  ggplot(aes(x = LAG_DIST, y = SET_LAG/60/60, colour = MM_YN)) + 
  geom_point(aes(colour= MM_YN, group = rev(MM_YN))) +  scale_colour_manual(values=c("1" = "red", "0" = "blue"))
sets_alldata %>%
  filter(DECLARED_TRIP == 'D', DP_LAG1 == 1) %>%
  ggplot(aes(x = LAG_DIST, y = SET_LAG/60/60, colour = MM_sum)) + 
  geom_point(aes(colour= MM_sum)) +  scale_colour_gradient(low = "blue", high = "red")
sets_alldata %>%
  filter(DECLARED_TRIP == 'D', DP_LAG1 == 1) %>%
  ggplot(aes(x = LAG_DIST, y = SET_LAG/60/60)) + 
  geom_point() + facet_grid(MM_YN~.)


## position
sets_alldata %>%
  filter((DECLARED_TRIP == 'D')) %>%
  ggplot(aes(x = HAUL_BEGIN_LAT, y = MM_YN)) + geom_point()
ggplot(data = sets_deep_all, mapping = aes(x = HAUL_BEGIN_LAT, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 5)

# latitude with ssh
sets_deep_all %>% 
  ggplot(aes(x = HAUL_BEGIN_LAT, y = adt, colour = MM_YN)) + geom_jitter()

sets_deep_all %>% 
  # ggplot(aes(x = HAUL_BEGIN_LON, y = HAUL_BEGIN_LAT, colour = adt)) + geom_jitter() +
  # ggplot(aes(x = HAUL_BEGIN_LON, y = HAUL_BEGIN_LAT, colour = tke)) + geom_jitter() +
  ggplot(aes(x = HAUL_BEGIN_LON, y = HAUL_BEGIN_LAT, colour = chla_mo_9k)) + geom_jitter() +
  facet_wrap(~ MONTH)

ggplot(sets_deep_all, aes(x = HAUL_BEGIN_LAT, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 5 )


## pre/post TRT
summary(sets_deep_all$postTRT)
ggplot(data = sets_deep_all, aes(x = MM_YN, y = postTRT)) +
  geom_count()

sets_deep_all %>%
  group_by(postTRT, HOOKS) %>%
  count()

sets_deep_all %>%
  group_by(postTRT) %>%
  summarise(
    n_distinct(VESSEL_ID)
  )

hook_tbl <- matrix(c(14307,18757,17478,47), nrow=2, ncol=2, byrow=T)
dimnames(hook_tbl) = list(TRT=c('0', '1'), Hook=c('C', 'T'))

chi2 = chisq.test(hook_tbl, correct=F)
c(chi2$statistic, chi2$p.value)
sqrt(chi2$statistic / sum(hook_tbl))

## soak
max(sets_deep_all$SOAK)
min(sets_deep_all$SOAK)

# plotting both deep and shallow with lines
ggplot(data = sets_alldata, mapping = aes(x = SOAK, y = ..density.., colour = DECLARED_TRIP)) +
  geom_freqpoly(binwidth = 1 )
ggplot(data = sets_alldata, mapping = aes(x = DECLARED_TRIP, y = SOAK)) +
  geom_boxplot()

# just deep set
ggplot(data = sets_deep_all) +
  geom_histogram(mapping = aes(x = SOAK), binwidth = 100)
View(sets_deep_all %>%
  count(cut_width(SOAK, 100)))
quantile(sets_deep_all$SOAK, c(.5,.9999))

# depredation occurrence
sets_deep_all$MM_YN <- as.factor(sets_deep_all$MM_YN)
ggplot(data = sets_deep_all, mapping = aes(x = SOAK, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 20 )
ggplot(data = sets_deep_all, mapping = aes(x = MM_YN, y = SOAK)) +
  geom_boxplot()
ggplot(sets_deep_all, aes(x=SOAK)) +
  geom_histogram(binwidth = 20)


sets_deep_all <- sets_deep_all %>% 
  mutate(Q = ifelse(MONTH == 1 | MONTH == 2 | MONTH == 3, 1,
                           ifelse(MONTH == 4 | MONTH == 5 | MONTH == 6, 2,
                                  ifelse(MONTH == 7 | MONTH == 8 | MONTH == 9, 3,
                                         ifelse(MONTH == 10 | MONTH == 11 | MONTH == 12, 4, 0
                                  )))))

sets_deep_all %>% 
  group_by(Q) %>% 
  summarize(
    tots = n(),
    mm = sum(as.numeric(as.character(MM_any))),
    mm/tots
  )

#############################################################################
### gear stuff
#############################################################################
## what to do about short soaks? some have 1000s of hooks and no fish, others with fish


## number of floats
ggplot(data = sets_alldata, mapping = aes(x = NUM_FLTS, colour = DECLARED_TRIP)) +
  geom_freqpoly(binwidth = 5 )
ggplot(data = sets_alldata, aes(x = NUM_FLTS, y = ..density.., colour = DECLARED_TRIP)) +
  geom_freqpoly(binwidth = 5 )

par(mfrow=c(2,2))
## look at rel'p between effort indicators, hooks set is very unreliable, float seems better
ggplot(sets_deep_all, aes(x= NUM_FLTS, y = NUM_CAUGHT)) + geom_point() + ylab("Total Caught") + xlab("Number of Floats")
ggplot(sets_deep_all, aes(x= NUM_HKS_SET, y = NUM_CAUGHT)) + geom_point()+ ylab("Total Caught") + xlab("Number of Hooks")
ggplot(sets_deep_all, aes(x= NUM_FLTS, y = SOAK)) + geom_point()+ ylab("Soak Time") + xlab("Number of Floats")
ggplot(sets_deep_all, aes(x= NUM_HKS_SET, y = SOAK)) + geom_point()+ ylab("Soak Time") + xlab("Number of Hooks")
ggplot(sets_deep_all, aes(x= NUM_HKS_SET, y = NUM_FLTS)) + geom_point()+ ylab("Number of Floats") + xlab("Number of Hooks")

ggplot(sets_deep_all, aes(x= CPUE_FLT, y = NUM_CAUGHT)) + geom_point()
sets_deep_all %>%
  #filter(postTRT == 1) %>%
  #ggplot(aes(x= NUM_HKS_SET, y = NUM_CAUGHT)) + geom_point() + ylab("Total Caught") + xlab("Number of Hooks") +
  ggplot(aes(x= NUM_FLTS, y = NUM_CAUGHT)) + geom_point() + ylab("Total Caught") + xlab("Number of Floats") +
  #ggplot(aes(x= NUM_HKS_SET, y = NUM_FLTS)) + geom_point()+ ylab("Number of Floats") + xlab("Number of Hooks") +
  facet_grid(postTRT~.)

  ggplot(aes(x= NUM_HKS_SET, y = NUM_FLTS)) + geom_point()+ ylab("Number of Floats") + xlab("Number of Hooks")



## number of hooks set - bimodal pattern for shallow, why?
ggplot(data = sets_alldata, aes(x = NUM_HKS_SET, y = ..density.., colour = DECLARED_TRIP)) +
  geom_freqpoly(binwidth = 100 )
ggplot(data = sets_deep_all, mapping = aes(x = NUM_HKS_SET)) +
  geom_bar(binwidth = 200 )
ggplot(sets_deep_all, aes(x = NUM_HKS_SET, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 100 )
ggplot(data = sets_deep_all, mapping = aes(x = MM_YN, y = NUM_HKS_SET)) +
  geom_boxplot()



## minimum length/depth - shallow at about 25m, deep at 35m
ggplot(data = sets_alldata, mapping = aes(x = MIN_LEN, y=..density..,colour = DECLARED_TRIP)) +
  geom_freqpoly(binwidth = 1 )
ggplot(data = sets_deep_all, mapping = aes(x = MIN_LEN, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 1 )
ggplot(data = sets_deep_all, aes(x = MM_YN, y = MIN_LEN)) +
  geom_boxplot()

## blue dye - lower proportion of MM when dyed?? - NOOOO tightly associated with latitude
summary(sets_deep_all$BLUEDYE)
ggplot(data = sets_deep_all, aes(x = MM_YN, y = BLUEDYE)) +
  geom_count()

ggplot(sets_deep_all, aes(x = HAUL_BEGIN_LAT, y = ..density.., colour = BLUEDYE)) +
  geom_freqpoly(binwidth = 10 )
ggplot(sets_deep_all, aes(x = HAUL_BEGIN_LON, y = ..density.., colour = BLUEDYE)) +
  geom_freqpoly(binwidth = 10 )

## bait code
# bait type
ggplot(data = sets_deep_all) +
  geom_bar(mapping = aes(x = BAIT_CODE_VAL))
sets_deep_all %>%
  count(BAIT_CODE_VAL)

sets_deep_all %>%
  filter(MM_YN == 1) %>%
  ggplot() +
  geom_bar(mapping = aes(x = BAIT_CODE_VAL))

ggplot(data = sets_deep_all, mapping = aes(x = MM_YN, y = BAIT_CODE_VAL)) +
  geom_count()

sets_deep_all %>%
  count(MM_YN, BAIT_CODE_VAL) %>%
  ggplot(mapping = aes(x = MM_YN, y = BAIT_CODE_VAL)) + 
    geom_tile(mapping = aes(fill = n))



## number light devices -- note no use of lightsticks in deep set (one set > 1)
ggplot(data = sets_deep_all, mapping = aes(x = NUM_LITE_DEVICES, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(bins = 1)



## hooks

catch_allyears %>%
  group_by(YEAR) %>%
  summarise(
    circle = sum(HKS == 'CIRCLE'),
    tunaj = sum(HKS == 'TUNA'),
    other = sum(HKS == 'OTHER')
  )
sets_alldata %>%
  group_by(YEAR) %>%
  summarise(
    circle = sum(HOOKS == 'CIRCLE'),
    tunaj = sum(HOOKS == 'TUNAJ'),
    other = sum(HOOKS == 'OTHER')
  )

View(catch_allyears %>%
  group_by(YEAR,HK_TYPE_CODE_VAL_1) %>%
  filter(DECLARED_TRIP_TYPE_CODE == 'D') %>%
  count())
View(catch_allyears %>%
       group_by(YEAR,HK_TYPE_CODE_VAL_2) %>%
       filter(DECLARED_TRIP_TYPE_CODE == 'D') %>%
       count())
View(catch_allyears %>%
       group_by(YEAR,HK_TYPE_CODE_VAL_3) %>%
       filter(DECLARED_TRIP_TYPE_CODE == 'D') %>%
       count())
View(catch_allyears %>%
       group_by(YEAR,HK_TYPE_CODE_VAL_4) %>%
       filter(DECLARED_TRIP_TYPE_CODE == 'D') %>%
       count())
#############################################################################
### catch stuff
#############################################################################

## number caught
ggplot(data = sets_deep_all, mapping = aes(x = NUM_CAUGHT, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 10 )

## number kept
ggplot(data = sets_deep_all, mapping = aes(x = KEPT, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 5 )
ggplot(data = sets_deep_all, mapping = aes(x = (KEPT/NUM_HKS_SET)*1000, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 2 )

## kept density against dep
sets_deep_all %>%
  ggplot(aes(x= KEPT/NUM_HKS_SET*1000, y = ..density.., colour = MM_YN)) + geom_freqpoly(binwidth=3) +
  theme_classic() + ylab("Density") + xlab("Kept fish / 1000 hooks") +
  scale_x_continuous(breaks=seq(0,60,10),limits=c(0,60)) + labs(color="Depredation") +
  scale_color_manual(labels = c("No", "Yes"), values = c("#56B4E9", "#E69F00")) +
  theme(text = element_text(size=25), 
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
        legend.position = c(0.8, 0.8))


## kept fish density against categorical level/intensity of dep

ggplot(data = sets_deep_all, mapping = aes(x = (KEPT/NUM_HKS_SET)*1000, y = ..density.., 
              colour = MM_cut)) + geom_freqpoly(binwidth = 3 ) +
  theme_classic() + ylab("Density") + xlab("Marketable fish / 1000 hooks") +
  scale_x_continuous(breaks=seq(0,50,10),limits=c(0,50)) + labs(color="Depredation") +
  #scale_color_manual(labels = c("No", "Yes"), values = c("#56B4E9", "#E69F00")) +
  theme(text = element_text(size=25), 
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
        legend.position = c(0.8, 0.8))
## boxplot for same
ggplot(data = sets_deep_all, mapping = aes(x = MM_cut, y = (KEPT/NUM_HKS_SET)*1000)) + geom_boxplot() +
  theme_classic() + ylab("Marketable fish / 1000 hooks") + xlab("MM depredation") +
  #scale_color_manual(labels = c("No", "Yes"), values = c("#56B4E9", "#E69F00")) +
  theme(text = element_text(size=25), 
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
        legend.position = c(0.8, 0.8))

## scatter plots of MMdep vs kept or kpue
sets_deep_all %>%
  filter(MM_YN == 1) %>%
  ggplot(aes(x= MM_sum/FISH, y = KEPT/NUM_HKS_SET*1000)) + geom_jitter()
sets_deep_all %>%
  filter(MM_YN == 1) %>%
  ggplot(aes(x= MM_sum, y = KEPT)) + geom_jitter()
sets_deep_all %>%
  filter(MM_YN == 1) %>%
  ggplot(aes(x= MM_sum/NUM_HKS_SET*1000, y = KEPT/NUM_HKS_SET*1000)) + geom_jitter()
  theme_classic() + ylab("Density") + xlab("Marketable fish / 1000 hooks") +
  scale_x_continuous(breaks=seq(0,100,20),limits=c(0,100)) + labs(color="Depredation") +
  scale_color_manual(labels = c("No", "Yes"), values = c("#56B4E9", "#E69F00")) +
  theme(text = element_text(size=25), 
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
        legend.position = c(0.8, 0.8))

  
# ## kept/cpue with distance/depredation since depredation
sets_deep_all %>%
  filter(DP_LAG1 == 1) %>%
  ggplot(aes(x= LAG_DIST, y = KEPT/NUM_HKS_SET*1000, colour = MM_YN)) + geom_smooth() +
  theme_classic() + ylab("Kept fish / 1000 hooks") + xlab("Distance") +
  scale_x_continuous(breaks=seq(0,100,20),limits=c(0,100)) + labs(color="Depredation")

## numbers
sets_deep_all %>%
  group_by(MM_any) %>%
  summarise(
    totalsets = n(),
    totalhooks = sum(NUM_HKS_SET),
    CPUE_all = sum(FISH) / totalhooks * 1000, # note does not include any sharks
    CPUE_K = sum(KEPT) / totalhooks * 1000 # should be comparable to LPUE below?
  )
    
ggplot(data = sets_deep_all, mapping = aes(x = MM_YN, y = KEPT)) +
  geom_boxplot()

## tuna, sqrt transform makes more normal
ggplot(data = sets_deep_all, mapping = aes(x = TUNA^0.5, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 1 )
ggplot(data = sets_deep_all, mapping = aes(x = MM_YN, y = KEPT)) +
  geom_boxplot()

## fish caught
ggplot(data = sets_deep_all, mapping = aes(x = FISH, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 10 )

## sharks caught
ggplot(data = sets_deep_all, mapping = aes(x = SHARKS, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 1 )
ggplot(data = sets_deep_all, mapping = aes(x = MM_YN, y = SHARKS)) +
  geom_boxplot()


## effect of surrounding vessel cpue on depredation
ggplot(data = sets_deep_all, mapping = aes(x = cpue_avg, y = ..density.., 
                                           colour = MM_YN)) + geom_freqpoly(binwidth = 2) +
  theme_classic() + ylab("Density") + xlab("Marketable fish / 1000 hooks") +
  scale_x_continuous(breaks=seq(0,50,10),limits=c(0,50)) + labs(color="Depredation") +
  #scale_color_manual(labels = c("No", "Yes"), values = c("#56B4E9", "#E69F00")) +
  theme(text = element_text(size=25), 
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
        legend.position = c(0.8, 0.8)) + 
ggplot(data = sets_deep_all, mapping = aes(x = MM_YN, y = cpue_avg)) +
  geom_boxplot()


ggplot(data = sets_deep_all, mapping = aes(x = cpue_avg, y = ..density.., 
                                             colour = MM_cut)) + geom_freqpoly(binwidth = 3)
ggplot(data = sets_deep_all, mapping = aes(x = MM_cut, y = cpue_n)) + geom_boxplot() +
  theme_classic()
#############################################################################
### GIS stuff
#############################################################################

## Slope
ggplot(data = sets_deep_all, mapping = aes(x = Slope, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 0.5 )

## Depth
ggplot(data = sets_deep_all, mapping = aes(x = Depth, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 100 )
ggplot(data = sets_deep_all, mapping = aes(x = MM_YN, y = Depth)) + geom_boxplot() +
  theme_classic()

## Contour Distance
ggplot(data = sets_deep_all, mapping = aes(x = Cont_Dist/1000, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 100 )

## Seamount distance
ggplot(data = sets_alldata, mapping = aes(x = Seamt_Dist/1000, y = ..density.., colour = DECLARED_TRIP)) +
  geom_freqpoly(binwidth = 10 )
ggplot(data = sets_deep_all, mapping = aes(x = Seamt_Dist/1000, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 10 )
ggplot(data = sets_deep_all, mapping = aes(x = MM_YN, y = Seamt_Dist/1000)) +
  geom_boxplot()

quantile(sets_deep_all$Seamt_Dist, c(.01,.99))
# transformed
ggplot(data = sets_deep_all, mapping = aes(x = Seamt_Dist^0.5, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 20 )

## eddy distance
ggplot(data = sets_alldata, mapping = aes(x = EDDY_DIST/1000, colour = DECLARED_TRIP)) +
  geom_freqpoly(binwidth = 10 )
ggplot(data = sets_alldata, mapping = aes(x = EDDY_DIST/1000, y =..density.., colour = DECLARED_TRIP)) +
  geom_freqpoly(binwidth = 10 )
ggplot(data = sets_deep_all, mapping = aes(x = EDDY_DIST/1000, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 10 )
ggplot(data = sets_deep_all, mapping = aes(x = MM_YN, y = EDDY_DIST/1000)) +
  geom_boxplot()

# transformed
ggplot(data = sets_deep_all, mapping = aes(x = EDDY_DIST^0.5, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 20 )

## Eddy chars with depredation
ggplot(data = sets_deep_all, mapping = aes(x = SPEED, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 10)
ggplot(data = sets_deep_all, mapping = aes(x = RADIUS, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 10 )
ggplot(data = sets_deep_all, mapping = aes(x = AMPLITUDE, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 10 )

# explore eddy features
sets_deep_all %>%
  ggplot(aes(x = SPEED, y = ..density.., colour = CYCL_TYPE)) + geom_freqpoly(binwidth = 10 )
  # ggplot(aes(x = RADIUS, y = ..density.., colour = CYCL_TYPE)) + geom_freqpoly(binwidth = 20 )
  # ggplot(aes(x = AMPLITUDE, y = ..density.., colour = CYCL_TYPE)) + geom_freqpoly(binwidth = 2)
sets_deep_all %>% 
  ggplot(aes(x = RADIUS, y = AMPLITUDE, colour = CYCL_TYPE)) + geom_jitter()
  # ggplot(aes(x = RADIUS, y = SPEED, colour = CYCL_TYPE)) + geom_jitter()
  # ggplot(aes(x = AMPLITUDE, y = SPEED, colour = CYCL_TYPE)) + geom_jitter()
sets_deep_all %>% 
  ggplot(aes(x = tke, y = eke, colour = CYCL_TYPE)) + geom_jitter()
  # ggplot(aes(x = tke, y = RADIUS, colour = CYCL_TYPE)) + geom_jitter()
  # ggplot(aes(x = tke, y = AMPLITUDE, colour = CYCL_TYPE)) + geom_jitter()
  # ggplot(aes(x = tke, y = SPEED, colour = CYCL_TYPE)) + geom_jitter()


summary(as.factor(sets_deep_all$CYCL_TYPE))
ggplot(data = sets_deep_all, aes(x = MM_YN, y = CYCL_TYPE)) +
  geom_count()


## SST - bump at low SST for no depred.
ggplot(data = sets_deep_all, mapping = aes(x = SST_2, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = .5 )
ggplot(data = sets_deep_all, mapping = aes(x = SST_2)) +
  geom_histogram(binwidth = 1 )

ggplot(sets_deep_all, aes(x= SST_2,y=HAUL_BEGIN_LAT)) + geom_count()

## SST range - no difference
ggplot(data = sets_deep_all, mapping = aes(x = SST_RANGE, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 0.1 )
ggplot(data = sets_deep_all, mapping = aes(x = SST_RANGE)) +
  geom_histogram(binwidth = 0.2 )

## ChlA - monthly at 9 km
sets_deep_all %>%
  filter(EL_LA_NO == 'EL') %>%
  ggplot(mapping = aes(x = chla_mo_9k, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 0.01 )


sets_deep_all %>%
  group_by(EL_LA_NO, MM_YN) %>%
  summarise(
    ChlA = mean(ChlA, na.rm=T)
  )


## fronts and other oceanographics
sets_deep_all %>%
  # ggplot(aes(x = front_dis/1000, y = ..density.., colour = MM_YN)) + geom_freqpoly(bins = 10)
  # ggplot(aes(x = tke, y = ..density.., colour = MM_YN)) + geom_freqpoly()
  # ggplot(aes(x = adt, y = ..density.., colour = MM_YN)) + geom_freqpoly()
  # ggplot(aes(x = eke, y = ..density.., colour = MM_YN)) + geom_freqpoly()
  # ggplot(aes(x = chla_mo_9k, y = ..density.., colour = MM_YN)) + geom_freqpoly()
  ggplot(aes(x = SSH, y = ..density.., colour = MM_YN)) + geom_freqpoly()
  


#############################################################################
### depredation
#############################################################################

## MM damage for all depredated sets
sets_deep_all %>%
  filter(MM_YN == 1) %>%
  ggplot(aes(x= MM_sum)) + geom_bar() + theme_classic() + theme(text = element_text(size=18)) +
  ylab("Number of Sets") + xlab("MM damaged fish") +
  scale_x_continuous(breaks=seq(0,60,10),limits=c(0,60)) +
  scale_y_continuous(breaks=seq(0,1300,300),limits=c(0,1300))

## MM damage for FKW hooked
sets_deep_all %>%
  filter(FKW == 1) %>%
  ggplot(aes(x= MM_sum)) + geom_bar() + ylab("Count") + theme_classic() +
  theme(text = element_text(size=18)) +
  ylab("Number of Sets") + xlab("MM damaged fish") +
  scale_x_continuous(breaks=seq(0,60,5),limits=c(0,40)) +
  scale_y_continuous(breaks=seq(0,20,5),limits=c(0,20))

## density plots of MMsum and FKW
sets_deep_all %>%
  filter(MM_YN == 1) %>%
  ggplot(aes(x= MM_sum, y = ..density.., colour = FKW)) + geom_freqpoly(bins=15) +
  theme_classic() + ylab("Density") + xlab("MM damaged fish") + theme(text = element_text(size=18)) + 
  scale_x_continuous(breaks=seq(0,60,15),limits=c(-10,60)) + labs(color="Caught FKW") +
  scale_color_manual(labels = c("No", "Yes"), values = c("#56B4E9", "#E69F00")) +
  theme(text = element_text(size=25), 
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
        legend.position = c(0.8, 0.8))

## CPUE - all
sets_deep_all %>%
  ggplot(aes(x= CPUE, y = ..density.., colour = MM_YN)) + geom_freqpoly(bins=15) +
  theme_classic() + ylab("Density") + xlab("CPUE (Fish/1000 hooks)") +
  scale_x_continuous(breaks=seq(0,120,20),limits=c(0,120)) + labs(color="Depredation") +
  scale_color_manual(labels = c("No", "Yes"), values = c("#56B4E9", "#E69F00")) +
  theme(text = element_text(size=25), 
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
        legend.position = c(0.8, 0.8))
#scale_colour_discrete(name = "Caught FKW", breaks=c("Yes","No"))
#theme(legend.title = element_text(colour="blue", size=16, face="bold"))

## CPUE - FKW
sets_deep_all %>%
  filter(MM_YN == 1) %>%
  ggplot(aes(x= CPUE, y = ..density.., colour = FKW)) + geom_freqpoly(bins=12) +
  theme_classic() + ylab("Density") + xlab("CPUE (Fish/1000 hooks)") +
  scale_x_continuous(breaks=seq(0,120,20),limits=c(0,120)) + labs(color="Caught FKW") +
  scale_color_manual(labels = c("No", "Yes"), values = c("#56B4E9", "#E69F00")) +
  theme(text = element_text(size=25), 
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
        legend.position = c(0.8, 0.8))
  #scale_colour_discrete(name = "Caught FKW", breaks=c("Yes","No"))
  #theme(legend.title = element_text(colour="blue", size=16, face="bold"))
  

sets_deep_all %>%
  filter(FKW == 1 & MM_YN == 1) %>%
  ggplot(aes(x= CPUE_FLT)) + geom_histogram() + ylab("Count") + theme_classic() +xlab("MM damaged fish")


## cpue of boats nearby is significant in GAM, but isn't evident with overall density of cpue 
## and depred y vs n. Number of boats doesnt seem to matter in either. Same with FKW
sets_deep_all %>%
  #filter(MM_any == 1) %>% 
  #ggplot(aes(x= cpue_avg_3d_100k, y = ..density.., colour = MM_YN)) + geom_freqpoly(bins=10) +
  ggplot(aes(x= num_vessels_3d_100k, y = ..density.., colour = MM_YN)) + geom_freqpoly(bins=10) +
  theme_classic() + ylab("Density") + xlab(" x - value ") +
  scale_x_continuous(breaks=seq(0,120,20),limits=c(0,120)) + labs(color="Y/N") +
  scale_color_manual(labels = c("No", "Yes"), values = c("#56B4E9", "#E69F00"))




#### types of fish

View(catch_allyears %>%
  #filter(MM == 1) %>% 
  group_by(SPECIES_COMMON_NAME) %>%
  count())
  
View(catch_allyears %>%
       filter(MM == 1) %>% 
       filter(DECLARED_TRIP_TYPE_CODE == 'D') %>%
       group_by(SPECIES_COMMON_NAME) %>%
       count(sort = 'T'))

View(catch_allyears %>%
  filter(MM == 1) %>%
    summarise(
      
    ),
       count())




###########################################################################
## plotting by month
###########################################################################

# look at all columns to see what would be interesting to track monthly
cat(paste(shQuote(colnames(sets_alldata), type="cmd"), collapse=", "))

# grouping by month across all years, not using date functions
set_month <- group_by(sets_alldata, MONTH)  %>%
  filter(DECLARED_TRIP == 'D') %>%
  summarise(
    MED_LAT = median(HAUL_BEGIN_LAT),
    TOTAL_SETS = n(),
    HKS = sum(NUM_HKS_SET),
    HKS_PER_SET = (HKS/TOTAL_SETS),
    SOAK = median(SOAK),
    BLUEDYE = sum(BLUEDYE)/TOTAL_SETS,
    CPUE_MONTH = mean(sum(FISH)/HKS)*1000,
    CPUE_SETAVG = mean(CPUE),
    MM = sum(as.numeric(as.character(MM_YN))),
    MM_SUM = sum(MM_sum),
    FKW = sum(as.integer(as.character(FKW))),
    SET_PROP = (MM/TOTAL_SETS),
    DPUE_YN = (MM/HKS)*100000,
    DPUE_SUM = (MM_SUM/HKS)*100000
    #CV = sd(MM_YN)
  )
sum(set_month$TOT) # confirm adding up each set

# now grouping by month with averages by year
set_month_avg <- group_by(set_monthyear, month)  %>%
  summarise(
    MM = mean(MM, na.rm = TRUE),
    SET = mean(TOT, na.rm = TRUE),
    PROP = mean(PROP, na.rm = TRUE),
    MAX = max(PROP, na.rm = TRUE)
  )


ggplot(data = set_month) +
  geom_line(aes(x = MONTH, y = DPUE_SUM), color = "blue") +
  geom_line(aes(x = MONTH, y = MED_LAT), color = "red")

# plotting two plots with different scales
# https://gist.github.com/tomhopper/faa24797bb44addeba79
plot_DPUE_SUM <- set_month %>%
  select(MONTH, DPUE_SUM) %>%
  ggplot() +
  geom_point(aes(x = MONTH, y = DPUE_SUM), size = 2.0, alpha = 1.0, color="red") +
  ylab("DPUE by counts") +
  theme_minimal() +
  theme(axis.title.x = element_blank())

plot_DPUE_YN <- set_month %>%
  select(MONTH, DPUE_YN) %>%
  ggplot() +
  geom_point(aes(x = MONTH, y = DPUE_YN), size = 2.0, alpha = 1.0, color="red") +
  ylab("DPUE occurrence") +
  theme_minimal() +
  theme(axis.title.x = element_blank())

plot_BLUEDYE <- set_month %>%
  select(MONTH, BLUEDYE) %>%
  ggplot() +
  geom_point(aes(x = MONTH, y = BLUEDYE), size = 2.0, alpha = 1.0, color="blue") +
  ylab("Blue DYE") +
  theme_minimal() +
  theme(axis.title.x = element_blank())

plot_LAT <- set_month %>%
  select(MONTH, MED_LAT) %>%
  ggplot() +
  geom_point(aes(x = MONTH, y = MED_LAT), size = 2.0, alpha = 1.0, color="green") +
  ylab("Latitude") +
  theme_minimal() +
  theme(axis.title.x = element_blank())

grid.newpage()
grid.draw(rbind(ggplotGrob(plot1), ggplotGrob(plot2),ggplotGrob(plot3), size = "last"))
grid.draw(rbind(ggplotGrob(plot_DPUE_SUM), ggplotGrob(plot_DPUE_YN),ggplotGrob(plot_LAT), size = "last"))


# bar plot of month by MM metrics (count and proportion of sets)
set_month %>%
  ggplot(aes(MONTH, DPUE_SUM)) + geom_bar(stat = "identity") #+ geom_errorbar()#aes(ymin=MM-CV, ymax=MM+CV))

set_month %>%
  ggplot(aes(month, PROP)) + geom_bar(stat = "identity") #+ geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd))

###########################################################################
## plotting by YEAR
###########################################################################
sapply(sets_alldata,class)

sets_alldata %>%
  as.numeric(as.character(MM_YN))

?dplyr

set_year <- group_by(sets_alldata, YEAR)  %>%
  filter(DECLARED_TRIP == 'D') %>%
  summarise(
    MED_LAT = median(HAUL_BEGIN_LAT),
    MEAN_LAT = mean(HAUL_BEGIN_LAT),
    TOTAL_SETS = n(),
    HKS = sum(NUM_HKS_SET),
    HKS_PER_SET = (HKS/TOTAL_SETS),
    SOAK = median(SOAK),
    BLUEDYE = sum(as.numeric(BLUEDYE))/TOTAL_SETS,
    CPUE_MONTH = mean(sum(FISH)/HKS)*1000,
    CPUE_SETAVG = mean(CPUE),
    MM = sum(as.numeric(as.character(MM_YN))),
    MM_SUM = sum(MM_sum),
    FKW = sum(as.integer(as.character(FKW))),
    SET_PROP = (MM/TOTAL_SETS),
    DPUE_YN = (MM_SUM/HKS)*10000,
    DPUE_sd = sd(as.numeric(as.character(MM_YN))),
    DPUE_SUM = (MM_SUM/HKS)*10000
    #CV = sd(MM_YN)
  )
catch_allyears %>%
  group_by(YEAR) %>%
  summarise(
    mean(HKS_PERCENTAGE_1, na.rm=T),
    n_distinct(HKS_PERCENTAGE_1)
  )
  

# bar plot of year by MM metrics (count and proportion of sets)
set_year %>%
  ggplot(aes(YEAR, SET_PROP)) + geom_bar(stat = "identity") #+ geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd))
set_year %>%
  ggplot(aes(YEAR, DPUE_YN)) + geom_bar(stat = "identity") #+ geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd))
set_year %>%
  ggplot(aes(YEAR, DPUE_SUM)) + geom_bar(stat = "identity") #+ geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd))
set_year %>%
  ggplot(aes(YEAR, DPUE_YN)) + geom_line() #+ geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd))
set_year %>%
  ggplot(aes(YEAR, DPUE_SUM)) + geom_line() #+ geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd))



## grid plots
plot_DPUE_SUM_yr <- set_year %>%
  select(YEAR, DPUE_SUM) %>%
  ggplot() +
  geom_point(aes(x = YEAR, y = DPUE_SUM), size = 2.0, alpha = 1.0, color="red") +
  ylab("DPUE by counts") +
  theme_minimal() +
  theme(axis.title.x = element_blank())

plot_DPUE_YN_yr <- set_year %>%
  select(YEAR, DPUE_YN) %>%
  ggplot() +
  geom_point(aes(x = YEAR, y = DPUE_YN), size = 2.0, alpha = 1.0, color="red") +
  ylab("DPUE occurrence") +
  theme_minimal() +
  theme(axis.title.x = element_blank())

plot_LAT_yr <- set_year %>%
  select(YEAR, MED_LAT) %>%
  ggplot() +
  geom_point(aes(x = YEAR, y = MED_LAT), size = 2.0, alpha = 1.0, color="green") +
  ylab("Latitude") +
  theme_minimal() +
  theme(axis.title.x = element_blank())
grid.newpage()
grid.draw(rbind(ggplotGrob(plot_DPUE_SUM_yr), ggplotGrob(plot_DPUE_YN_yr),ggplotGrob(plot_LAT_yr), size = "last"))




#############################################################################
### El Nino
#############################################################################

ElNino %>% group_by(YEAR) %>%
  filter(YEAR>=2003) %>%
  summarise(
    ElNinoYr = mean(ONI)
  ) %>%
  ggplot(aes(x=YEAR, y=ElNinoYr)) + geom_line()

## plot El Nino across all years
ElNino %>% group_by(YEAR) %>%
  ggplot(aes(x=MONTH, y=ONI)) + geom_area() + facet_grid(.~YEAR)

## chlorophyll in el nino
ggplot(data = sets_deep_all, mapping = aes(x = ONI, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 0.2)


## histo of chlorophyll relationship in el nino years, with MMYN
sets_deep_all %>%
  ggplot(aes(x=chla_mo_9k, y=..density.., colour=MM_YN)) + geom_freqpoly(binwidth=0.01) +
  facet_grid(EL_LA_NO~.) + theme_classic() +scale_x_continuous(limits=c(0,0.2))

# location in elnino
sets_deep_all %>%
  ggplot(aes(x=HAUL_BEGIN_LAT, colour=EL_LA_NO)) + geom_freqpoly(binwidth=1)+ ylab("Count") + 
  xlab("Haul Begin Latitude") + theme_classic()

# chla by enso and month
sets_deep_all %>% 
  ggplot(aes(x = HAUL_BEGIN_LON, y = HAUL_BEGIN_LAT, colour = chla_mo_9k)) + geom_jitter() +
  facet_wrap(EL_LA_NO ~ MONTH)

## need to look at rate of depredation, over months in different el ninos
# this on just number of sets with MM, by el nino - so basically just fishing more in winter and/or
# less in summer during el/la compared to non enso years
sets_deep_all %>%
  # filter(MM_YN == 1) %>%
  ggplot(aes(x=MONTH, colour=EL_LA_NO, y =..density..)) + geom_freqpoly(binwidth=1)+
  theme_classic() + ylab("Relative Depredation") + scale_x_discrete(name ="Month", limits=c(1:12))
  

## table of el nino by month
ElNino %>%
  group_by(MONTH) %>%
  summarise(
    total = n(),
    EL = sum(EL_LA_NO == 'EL'),
    LA = sum(EL_LA_NO == 'LA'),
    NO = sum(EL_LA_NO == 'NO')
  )

ElNino %>%
  group_by(MONTH, EL_LA_NO) %>%
  summarise(
    total = n(),
  ) %>%
  ggplot(aes(x=MONTH, y=total, color = EL_LA_NO))+ geom_line() +
  theme_classic() + ylab("Number of sets") + scale_x_discrete(name ="Month", limits=c(1:12))

# effort, depredation and catch per effort, by month and el nino
sets_deep_all %>%
  group_by(MONTH, EL_LA_NO) %>%
  summarise(
    MM = sum(MM_YN == 1),
    SETS = n(),
    FLTS = sum(NUM_FLTS),
    NUMFISH = sum(FISH),
    CPUE = NUMFISH/FLTS,
    DPUE = MM/FLTS,
    FKW = sum(FKW_sum)
  ) %>%
  # ggplot(aes(x=MONTH, y=SETS, color = EL_LA_NO))+ geom_line() +
  # theme_classic() + ylab("Number of sets") + scale_x_discrete(name ="Month", limits=c(1:12))
  # ggplot(aes(x=MONTH, y=CPUE, color = EL_LA_NO))+ geom_line() +
  # theme_classic() + ylab("CPUE (floats)") + scale_x_discrete(name ="Month", limits=c(1:12))
  # ggplot(aes(x=MONTH, y=DPUE, color = EL_LA_NO))+ geom_line() +
  # theme_classic() + ylab("Depredation per effort (floats)") + scale_x_discrete(name ="Month", limits=c(1:12))
  ggplot(aes(x=MONTH, y=FKW, color = EL_LA_NO))+ geom_point() +
  theme_classic() + ylab("Number of FKW") + scale_x_discrete(name ="Month", limits=c(1:12))

sets_deep_all %>%
  group_by(MONTH) %>%
  summarise(
    sum(MM_YN == 1)
  )
  ggplot(aes(x=MONTH, colour=EL_LA_NO, y =..density..)) + geom_freqpoly(binwidth=1)

  
  #facet_grid(EL_LA_NO~.) + 
  #ylab("Month") + xlab("MM") + theme_classic()



## boxplots of month by el nino
sets_deep_all %>%
  ggplot(aes(x =MONTH, y = HAUL_BEGIN_LAT, group=MONTH)) + 
  geom_boxplot() + facet_grid(EL_LA_NO~.) +
  theme_classic() +
  #theme(axis.line = element_line(colour = "darkblue", size = 1, linetype = "solid")) + 
  ylab("Haul Being Latitude") + scale_x_discrete(name ="Month", limits=c(1:12))
  
  
## boxplot - higher in certain PDO?
sets_deep_all %>%
  group_by(EL_LA_NO) %>%
  summarise(
    MMYN = sum(as.numeric(as.character(MM_YN))),
    Prop = MMYN/n(),
    meanDP = mean(DPUE)
  ) %>%
  ggplot(aes(x =EL_LA_NO, y = meanDP)) +
  geom_boxplot()

ggplot(sets_deep_all, aes(x =EL_LA_NO, y = as.numeric(MM_YN))) +
  geom_boxplot()

sets_deep_all %>%
  group_by(EL_LA_NO, MONTH) %>%
  ggplot(aes(x=chla_mo_9k, y=..density.., colour=MM_YN)) + geom_freqpoly(binwidth=0.1) + 
  facet_grid(EL_LA_NO~MONTH)

  summarise(
    MMYN = sum(as.numeric(as.character(MM_YN))),
    Prop = MMYN/n(),
    meanDP = mean(DPUE)
  ) %>%

###########################################################################
## filter dp sets to look at gear chars of caught animals w/in dp sets
###########################################################################






###########################################################################
## caught FKWs
###########################################################################

FKW_sets <- sets_alldata %>%
  filter(FKW == 1)
summary(FKW_sets)




###########################################################################
## getting trip numbers and locations for trips with caught FKWs
###########################################################################

## vector of TRIP_IDs with FKWs, rename column to match sets dataset
FKWtrips <- sets_deep_all %>% filter(FKW ==1) %>% distinct(TRIP_ID) %>% pull()
FKWtrips <- as.data.frame(FKWtrips)
colnames(FKWtrips) <- "TRIP_ID"

# semijoin to extract entire trips in which a FKW was hooked
FKWtripsets <- semi_join(sets_deep_all, FKWtrips)
## write out
write.csv(FKWtripsets, "Data/FKWtrips.csv")

