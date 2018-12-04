#############################################################################
## forreals
#############################################################################
#############################################################################
# Joseph Fader
# March, 2018

library(tidyverse)
library(mgcv)
library(feather)
library(grid)

## read in data, make sure var classes as needed
setwd("~/Google Drive/Projects/FKWTRT")
sets_alldata <- read_feather("Data/sets_alldata.feather")
sets_deep_all <- read_feather("Data/sets_deep_all.feather")
sets_shallow_all <- read_feather("Data/sets_shallow_all.feather")
summary(sets_deep_all)
summary(sets_shallow_all)
sapply(sets_alldata,class)
sapply(sets_deep_all,class)

## make subsample to test models on
sets_deep_sample <- sets_deep_all[sample(1:nrow(sets_deep_all), 20000,
                                         replace=FALSE),]

# code to separate in training and test chunks
n <-  nrow(sets_deep_sample)
trainIndex <- sample(1:n, size = round(0.5*n), replace=FALSE)
sets_deep_train <- sets_deep_sample[trainIndex ,]
sets_deep_test <- sets_deep_sample[-trainIndex ,]

summary(sets_deep_train)

cat(paste(shQuote(colnames(sets_deep_sample), type="cmd"), collapse=", "))



sets_deep_depr <- sets_deep_all %>%
  #filter(HOOKS != 'OTHER') %>%
  #filter(HOOKS != 'OTHER' & HOOKS != 'JHOOK') %>%
  #mutate(HOOKS = ifelse((HOOKS == 'TUNA' | HOOKS == 'JHOOK'), 'TUNAJ', 'CIRCLE')) %>%
  select(TRIP_ID, VESSEL_ID, SET_NUM, HAUL_BEGIN_LAT, HAUL_BEGIN_LON, MONTH, YEAR, postTRT,
         SOAK, HOOKS, NUM_FLTS, NUM_HKS_SET, HKS_PER_FLT, MIN_LEN, BLUEDYE, BET, YFT, TUNA, 
         MAHI, BILLFISH, SWO, CPUE, CPUE_FLT, MM_YN, MM_sum, FKW, FKW_sum, Slope, Depth, Cont_Dist,
         Seamt_Dist, EDDY_DIST, CYCL_TYPE, SST_2, SST_RANGE, ChlA, SSH, EL_LA_NO, Moon_Illum) %>%
  drop_na()



#############################################################################
## depredation, dropping sets w/ prior depredation
#############################################################################

## drop all sets with depredation on previous set
sets_deep_noprior <- sets_deep_all %>%
  filter(!(lag(MM_YN) == 1 & SET_NUM == (lag(SET_NUM) + 1))) %>%
  select(TRIP_ID, VESSEL_ID, SET_NUM, HAUL_BEGIN_LAT, HAUL_BEGIN_LON, MONTH, YEAR, postTRT,
       SOAK, HOOKS, NUM_FLTS, NUM_HKS_SET, HKS_PER_FLT, MIN_LEN, BLUEDYE, BET, YFT, TUNA, 
       MAHI, BILLFISH, SWO, CPUE_FLT, MM_YN, MM_sum, FKW, FKW_sum, Slope, Depth, Cont_Dist,
       Seamt_Dist, EDDY_DIST, CYCL_TYPE, SST_2, SST_RANGE, ChlA, SSH, EL_LA_NO, Moon_Illum) %>%
  drop_na()
summary(sets_deep_noprior)

depred_noprior1 <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                          s(ChlA, by=EL_LA_NO, bs='ts', k = 10) + s(SSH, by=EL_LA_NO, bs='ts', k = 10) +
                          s(SST_2, by=EL_LA_NO, bs='ts', k = 10) +
                          s(YEAR, bs='ts') + s(MONTH, bs='cc') + 
                          s(NUM_FLTS, bs='ts') + s(SOAK, bs='ts') + 
                          s(HKS_PER_FLT, bs='ts') + s(MIN_LEN, bs='ts') +
                          s(BET, bs='ts') + s(YFT, bs='ts') + s(MAHI, bs='ts') + 
                          s(BILLFISH, bs='ts') + s(CPUE_FLT, bs='ts') +
                          s(Slope, bs='ts') + s(Depth, bs='ts') + 
                          s(Cont_Dist, bs='ts') + s(Seamt_Dist, bs='ts') + s(SST_RANGE, bs='ts') +
                          s(Moon_Illum, bs='ts') + s(VESSEL_ID, bs='re') + postTRT + EL_LA_NO,
                        data=sets_deep_noprior, family=binomial, gamma=1.4)

summary(depred_noprior1)

# rm range, moon, trt
depred_noprior2 <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                               s(ChlA, by=EL_LA_NO, bs='ts', k = 10) + s(SSH, by=EL_LA_NO, bs='ts', k = 10) +
                               s(SST_2, by=EL_LA_NO, bs='ts', k = 10) +
                               s(YEAR, bs='ts') + s(MONTH, bs='cc') + 
                               s(NUM_FLTS, bs='ts') + s(SOAK, bs='ts') + 
                               s(HKS_PER_FLT, bs='ts') + s(MIN_LEN, bs='ts') +
                               s(BET, bs='ts') + s(YFT, bs='ts') + s(MAHI, bs='ts') + 
                               s(BILLFISH, bs='ts') + s(CPUE_FLT, bs='ts') +
                               s(Slope, bs='ts') + s(Depth, bs='ts') + 
                               s(Cont_Dist, bs='ts') + s(Seamt_Dist, bs='ts') +
                               s(VESSEL_ID, bs='re') + EL_LA_NO,
                             data=sets_deep_noprior, family=binomial, gamma=1.4)

summary(depred_noprior2)
AIC(depred_noprior1,depred_noprior2)

# rm billfish, slope, depth
depred_noprior3 <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                               s(ChlA, by=EL_LA_NO, bs='ts', k = 10) + s(SSH, by=EL_LA_NO, bs='ts', k = 10) +
                               s(SST_2, by=EL_LA_NO, bs='ts', k = 10) +
                               s(YEAR, bs='ts') + s(MONTH, bs='cc') + 
                               s(NUM_FLTS, bs='ts') + s(SOAK, bs='ts') + 
                               s(HKS_PER_FLT, bs='ts') + s(MIN_LEN, bs='ts') +
                               s(BET, bs='ts') + s(YFT, bs='ts') + s(MAHI, bs='ts') + 
                               s(CPUE_FLT, bs='ts') +
                               s(Cont_Dist, bs='ts') + s(Seamt_Dist, bs='ts') +
                               s(VESSEL_ID, bs='re') + EL_LA_NO,
                             data=sets_deep_noprior, family=binomial, gamma=1.4)

summary(depred_noprior3)
AIC(depred_noprior1,depred_noprior2,depred_noprior3)

# rm min len
depred_noprior4 <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                               s(ChlA, by=EL_LA_NO, bs='ts', k = 10) + s(SSH, by=EL_LA_NO, bs='ts', k = 10) +
                               s(SST_2, by=EL_LA_NO, bs='ts', k = 10) +
                               s(YEAR, bs='ts') + s(MONTH, bs='cc') + 
                               s(NUM_FLTS, bs='ts') + s(SOAK, bs='ts') + 
                               s(HKS_PER_FLT, bs='ts') +
                               s(BET, bs='ts') + s(YFT, bs='ts') + s(MAHI, bs='ts') + 
                               s(CPUE_FLT, bs='ts') +
                               s(Cont_Dist, bs='ts') + s(Seamt_Dist, bs='ts') +
                               s(VESSEL_ID, bs='re') + EL_LA_NO,
                             data=sets_deep_noprior, family=binomial, gamma=1.4)

summary(depred_noprior4)
AIC(depred_noprior1,depred_noprior2,depred_noprior3,depred_noprior4)

#############################################################################
## month by EL Nino
#############################################################################
depred_elmo1 <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                          s(ChlA, by=EL_LA_NO, bs='ts', k = 10) + s(SSH, by=EL_LA_NO, bs='ts', k = 10) +
                          s(SST_2, by=EL_LA_NO, bs='ts', k = 10) +
                          s(YEAR, bs='ts') +
                          s(MONTH, bs='cc', by=EL_LA_NO, k = 10) + 
                          s(NUM_FLTS, bs='ts') + s(SOAK, bs='ts') + 
                          s(HKS_PER_FLT, bs='ts') + s(MIN_LEN, bs='ts') +
                          s(BET, bs='ts') + s(YFT, bs='ts') + s(MAHI, bs='ts') + 
                          s(BILLFISH, bs='ts') + s(CPUE_FLT, bs='ts') +
                          s(Slope, bs='ts') + s(Depth, bs='ts') + 
                          s(Cont_Dist, bs='ts') + s(Seamt_Dist, bs='ts') + s(SST_RANGE, bs='ts') +
                          s(Moon_Illum, bs='ts') + s(VESSEL_ID, bs='re') + postTRT + EL_LA_NO,
                        data=sets_deep_depr, family=binomial, gamma=1.4)

summary(depred_elmo1)
plot(depred_elmo1, se=T, scale=0,
     jit=F, all.terms=T, shade=FALSE, ask=F)
#############################################################################
## throwing in dplag
#############################################################################
sets_deep_depr <- sets_deep_all %>%
  select(TRIP_ID, VESSEL_ID, SET_NUM, HAUL_BEGIN_LAT, HAUL_BEGIN_LON, MONTH, YEAR, postTRT,
         SOAK, HOOKS, NUM_FLTS, NUM_HKS_SET, HKS_PER_FLT, MIN_LEN, BLUEDYE, BET, YFT, TUNA, 
         MAHI, BILLFISH, SWO, CPUE, CPUE_FLT, MM_YN, MM_sum, FKW, FKW_sum, Slope, Depth, Cont_Dist,
         Seamt_Dist, EDDY_DIST, CYCL_TYPE, SST_2, SST_RANGE, ChlA, SSH, EL_LA_NO, Moon_Illum, DP_LAG1) %>%
  drop_na()

# ts ---month as cc (cyclic)
depred_lag1 <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                           s(ChlA, by=EL_LA_NO, bs='ts', k = 10) + s(SSH, by=EL_LA_NO, bs='ts', k = 10) +
                           s(SST_2, by=EL_LA_NO, bs='ts', k = 10) +
                           s(YEAR, bs='ts') + s(MONTH, bs='cc') + 
                           s(NUM_HKS_SET, bs='ts') + s(SOAK, bs='ts') + 
                           s(HKS_PER_FLT, bs='ts') + s(MIN_LEN, bs='ts') +
                           s(BET, bs='ts') + s(YFT, bs='ts') + s(MAHI, bs='ts') + 
                           s(BILLFISH, bs='ts') + s(CPUE, bs='ts') +
                           s(Slope, bs='ts') + s(Depth, bs='ts') + 
                           s(Cont_Dist, bs='ts') + s(Seamt_Dist, bs='ts') + s(SST_RANGE, bs='ts') +
                           s(Moon_Illum, bs='ts') + s(VESSEL_ID, bs='re') + postTRT + EL_LA_NO + DP_LAG1,
                         data=sets_deep_depr, family=binomial, gamma=1.4)

summary(depred_lag1)
# drop minlen, sst range, billfish, moon illum
depred_lag2 <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                           s(ChlA, by=EL_LA_NO, bs='ts', k = 10) + s(SSH, by=EL_LA_NO, bs='ts', k = 10) +
                           s(SST_2, by=EL_LA_NO, bs='ts', k = 10) +
                           s(YEAR, bs='ts') + s(MONTH, bs='cc') + 
                           s(NUM_HKS_SET, bs='ts') + s(SOAK, bs='ts') + 
                           s(HKS_PER_FLT, bs='ts') +
                           s(BET, bs='ts') + s(YFT, bs='ts') + s(MAHI, bs='ts') + 
                           s(CPUE, bs='ts') +
                           s(Slope, bs='ts') + s(Depth, bs='ts') + 
                           s(Cont_Dist, bs='ts') + s(Seamt_Dist, bs='ts') +
                           s(VESSEL_ID, bs='re') + postTRT + EL_LA_NO + DP_LAG1,
                         data=sets_deep_depr, family=binomial, gamma=1.4)

summary(depred_lag2)
AIC(depred_lag1,depred_lag2)

# drop depth, slope, seamt, sst
depred_lag3 <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                           s(ChlA, by=EL_LA_NO, bs='ts', k = 10) + s(SSH, by=EL_LA_NO, bs='ts', k = 10) +
                           s(YEAR, bs='ts') + s(MONTH, bs='cc') + 
                           s(NUM_HKS_SET, bs='ts') + s(SOAK, bs='ts') + 
                           s(HKS_PER_FLT, bs='ts') +
                           s(BET, bs='ts') + s(YFT, bs='ts') + s(MAHI, bs='ts') + 
                           s(CPUE, bs='ts') +
                           s(Cont_Dist, bs='ts') +
                           s(VESSEL_ID, bs='re') + postTRT + EL_LA_NO + DP_LAG1,
                         data=sets_deep_depr, family=binomial, gamma=1.4)

summary(depred_lag3)
AIC(depred_lag1,depred_lag2,depred_lag3)

#### USING THIS
# drop trt -- using this one
depred_lag4 <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                           s(ChlA, by=EL_LA_NO, bs='ts', k = 10) + s(SSH, by=EL_LA_NO, bs='ts', k = 10) +
                           s(SST_2, by=EL_LA_NO, bs='ts', k = 10) +
                           s(YEAR, bs='ts') + s(MONTH, bs='cc') + 
                           s(NUM_HKS_SET, bs='ts') + s(SOAK, bs='ts') + 
                           s(HKS_PER_FLT, bs='ts') +
                           s(BET, bs='ts') + s(YFT, bs='ts') + s(MAHI, bs='ts') + 
                           s(CPUE, bs='ts') +
                           s(Cont_Dist, bs='ts') +
                           s(VESSEL_ID, bs='re') + EL_LA_NO,
                         data=sets_deep_depr, family=binomial, gamma=1.4)

summary(depred_hks4)
AIC(depred_hks1,depred_hks2,depred_hks3,depred_hks4)

#############################################################################
## PRESENTATION -- same as below but w hooks and cpue instead of floats
#############################################################################
# ts ---month as cc (cyclic)
depred_hks1 <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                          s(ChlA, by=EL_LA_NO, bs='ts', k = 10) + s(SSH, by=EL_LA_NO, bs='ts', k = 10) +
                          s(SST_2, by=EL_LA_NO, bs='ts', k = 10) +
                          s(YEAR, bs='ts') + s(MONTH, bs='cc') + 
                          s(NUM_HKS_SET, bs='ts') + s(SOAK, bs='ts') + 
                          s(HKS_PER_FLT, bs='ts') + s(MIN_LEN, bs='ts') +
                          s(BET, bs='ts') + s(YFT, bs='ts') + s(MAHI, bs='ts') + 
                          s(BILLFISH, bs='ts') + s(CPUE, bs='ts') +
                          s(Slope, bs='ts') + s(Depth, bs='ts') + 
                          s(Cont_Dist, bs='ts') + s(Seamt_Dist, bs='ts') + s(SST_RANGE, bs='ts') +
                          s(Moon_Illum, bs='ts') + s(VESSEL_ID, bs='re') + postTRT + EL_LA_NO,
                        data=sets_deep_depr, family=binomial, gamma=1.4)

summary(depred_hks1)

# drop minlen, sst range, billfish, moon illum
depred_hks2 <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                          s(ChlA, by=EL_LA_NO, bs='ts', k = 10) + s(SSH, by=EL_LA_NO, bs='ts', k = 10) +
                          s(SST_2, by=EL_LA_NO, bs='ts', k = 10) +
                          s(YEAR, bs='ts') + s(MONTH, bs='cc') + 
                          s(NUM_HKS_SET, bs='ts') + s(SOAK, bs='ts') + 
                          s(HKS_PER_FLT, bs='ts') +
                          s(BET, bs='ts') + s(YFT, bs='ts') + s(MAHI, bs='ts') + 
                          s(CPUE, bs='ts') +
                          s(Slope, bs='ts') + s(Depth, bs='ts') + 
                          s(Cont_Dist, bs='ts') + s(Seamt_Dist, bs='ts') +
                          s(VESSEL_ID, bs='re') + postTRT + EL_LA_NO,
                        data=sets_deep_depr, family=binomial, gamma=1.4)

summary(depred_hks2)
AIC(depred_hks1,depred_hks2)

# drop depth, slope, seamt
depred_hks3 <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                           s(ChlA, by=EL_LA_NO, bs='ts', k = 10) + s(SSH, by=EL_LA_NO, bs='ts', k = 10) +
                           s(SST_2, by=EL_LA_NO, bs='ts', k = 10) +
                           s(YEAR, bs='ts') + s(MONTH, bs='cc') + 
                           s(NUM_HKS_SET, bs='ts') + s(SOAK, bs='ts') + 
                           s(HKS_PER_FLT, bs='ts') +
                           s(BET, bs='ts') + s(YFT, bs='ts') + s(MAHI, bs='ts') + 
                           s(CPUE, bs='ts') +
                           s(Cont_Dist, bs='ts') +
                           s(VESSEL_ID, bs='re') + postTRT + EL_LA_NO,
                         data=sets_deep_depr, family=binomial, gamma=1.4)

summary(depred_hks3)
AIC(depred_hks1,depred_hks2,depred_hks3)

#### USING THIS
# drop trt -- using this one
depred_hks4 <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                           s(ChlA, by=EL_LA_NO, bs='ts', k = 10) + s(SSH, by=EL_LA_NO, bs='ts', k = 10) +
                           s(SST_2, by=EL_LA_NO, bs='ts', k = 10) +
                           s(YEAR, bs='ts') + s(MONTH, bs='cc') + 
                           s(NUM_HKS_SET, bs='ts') + s(SOAK, bs='ts') + 
                           s(HKS_PER_FLT, bs='ts') +
                           s(BET, bs='ts') + s(YFT, bs='ts') + s(MAHI, bs='ts') + 
                           s(CPUE, bs='ts') +
                           s(Cont_Dist, bs='ts') +
                           s(VESSEL_ID, bs='re') + EL_LA_NO,
                         data=sets_deep_depr, family=binomial, gamma=1.4)

summary(depred_hks4)
AIC(depred_hks1,depred_hks2,depred_hks3,depred_hks4)

# drop cont
depred_hks5 <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                           s(ChlA, by=EL_LA_NO, bs='ts', k = 10) + s(SSH, by=EL_LA_NO, bs='ts', k = 10) +
                           s(SST_2, by=EL_LA_NO, bs='ts', k = 10) +
                           s(YEAR, bs='ts') + s(MONTH, bs='cc') + 
                           s(NUM_HKS_SET, bs='ts') + s(SOAK, bs='ts') + 
                           s(HKS_PER_FLT, bs='ts') +
                           s(BET, bs='ts') + s(YFT, bs='ts') + s(MAHI, bs='ts') + 
                           s(CPUE, bs='ts') +
                           s(VESSEL_ID, bs='re') + EL_LA_NO,
                         data=sets_deep_depr, family=binomial, gamma=1.4)
summary(depred_hks5)
AIC(depred_hks1,depred_hks2,depred_hks3,depred_hks4,depred_hks5)

# drop ocean
depred_hks6 <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                           s(YEAR, bs='ts') + s(MONTH, bs='cc') + 
                           s(NUM_HKS_SET, bs='ts') + s(SOAK, bs='ts') + 
                           s(HKS_PER_FLT, bs='ts') +
                           s(BET, bs='ts') + s(YFT, bs='ts') + s(MAHI, bs='ts') + 
                           s(CPUE, bs='ts') +
                           s(VESSEL_ID, bs='re'),
                         data=sets_deep_depr, family=binomial, gamma=1.4)
summary(depred_hks6)
AIC(depred_hks1,depred_hks2,depred_hks3,depred_hks4,depred_hks5,depred_hks6)

plot(depred_hks1, se=T, select=NULL, scale=0,
     jit=F, xlim=NULL, all.terms=T, shade=FALSE, ask=F)

par(mfrow=c(2,2))
plot(depred_ts4, se=T, select=16, scale=0,
     jit=F, xlim=c(0,100), all.terms=T, shade=FALSE, ask=F)
plot(depred_ts4, se=T, select=17, scale=0,
     jit=F, xlim=c(0,100), all.terms=T, shade=FALSE, ask=F)
plot(depred_ts4, se=T, select=18, scale=0,
     jit=F, xlim=c(0,100), all.terms=T, shade=FALSE, ask=F)
plot(depred_ts4, se=T, select=19, scale=0,
     jit=F, xlim=c(0,2), all.terms=T, shade=FALSE, ask=F)
#############################################################################
## start fresh for depredation - ts
#############################################################################
# ts ---month as cc (cyclic)
depred_ts1 <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                       s(ChlA, by=EL_LA_NO, bs='ts', k = 10) + s(SSH, by=EL_LA_NO, bs='ts', k = 10) +
                       s(SST_2, by=EL_LA_NO, bs='ts', k = 10) +
                       s(YEAR, bs='ts') + s(MONTH, bs='cc') + 
                       s(NUM_FLTS, bs='ts') + s(SOAK, bs='ts') + 
                       s(HKS_PER_FLT, bs='ts') + s(MIN_LEN, bs='ts') +
                       s(BET, bs='ts') + s(YFT, bs='ts') + s(MAHI, bs='ts') + 
                       s(BILLFISH, bs='ts') + s(CPUE_FLT, bs='ts') +
                       s(Slope, bs='ts') + s(Depth, bs='ts') + 
                       s(Cont_Dist, bs='ts') + s(Seamt_Dist, bs='ts') + s(SST_RANGE, bs='ts') +
                       s(Moon_Illum, bs='ts') + s(VESSEL_ID, bs='re') + postTRT + EL_LA_NO,
                     data=sets_deep_depr, family=binomial, gamma=1.4)

summary(depred_ts1)

# drop minlen, sst range, billfish, moon illum
depred_ts2 <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                          s(ChlA, by=EL_LA_NO, bs='ts', k = 10) + s(SSH, by=EL_LA_NO, bs='ts', k = 10) +
                          s(SST_2, by=EL_LA_NO, bs='ts', k = 10) +
                          s(YEAR, bs='ts') + s(MONTH, bs='cc') + 
                          s(NUM_FLTS, bs='ts') + s(SOAK, bs='ts') + 
                          s(HKS_PER_FLT, bs='ts') +
                          s(BET, bs='ts') + s(YFT, bs='ts') + s(MAHI, bs='ts') + 
                          s(CPUE_FLT, bs='ts') +
                          s(Slope, bs='ts') + s(Depth, bs='ts') + 
                          s(Cont_Dist, bs='ts') + s(Seamt_Dist, bs='ts') +
                          s(VESSEL_ID, bs='re') + postTRT + EL_LA_NO,
                        data=sets_deep_depr, family=binomial, gamma=1.4)

summary(depred_ts2)
AIC(depred_ts1,depred_ts2)

# drop depth, slope
depred_ts3 <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                          s(ChlA, by=EL_LA_NO, bs='ts', k = 10) + s(SSH, by=EL_LA_NO, bs='ts', k = 10) +
                          s(SST_2, by=EL_LA_NO, bs='ts', k = 10) +
                          s(YEAR, bs='ts') + s(MONTH, bs='cc') + 
                          s(NUM_FLTS, bs='ts') + s(SOAK, bs='ts') + 
                          s(HKS_PER_FLT, bs='ts') +
                          s(BET, bs='ts') + s(YFT, bs='ts') + s(MAHI, bs='ts') + 
                          s(CPUE_FLT, bs='ts') +
                          s(Cont_Dist, bs='ts') + s(Seamt_Dist, bs='ts') +
                          s(VESSEL_ID, bs='re') + postTRT + EL_LA_NO,
                        data=sets_deep_depr, family=binomial, gamma=1.4)

summary(depred_ts3)
AIC(depred_ts1,depred_ts2,depred_ts3)

# drop trt -- everything now <= 0.05 pvalue
depred_ts4 <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                          s(ChlA, by=EL_LA_NO, bs='ts', k = 10) + s(SSH, by=EL_LA_NO, bs='ts', k = 10) +
                          s(SST_2, by=EL_LA_NO, bs='ts', k = 10) +
                          s(YEAR, bs='ts') + s(MONTH, bs='cc') + 
                          s(NUM_FLTS, bs='ts') + s(SOAK, bs='ts') + 
                          s(HKS_PER_FLT, bs='ts') +
                          s(BET, bs='ts') + s(YFT, bs='ts') + s(MAHI, bs='ts') + 
                          s(CPUE_FLT, bs='ts') +
                          s(Cont_Dist, bs='ts') + s(Seamt_Dist, bs='ts') +
                          s(VESSEL_ID, bs='re') + EL_LA_NO,
                        data=sets_deep_depr, family=binomial, gamma=1.4)

summary(depred_ts4)
AIC(depred_ts1,depred_ts2,depred_ts3,depred_ts4)

## note continuing by dropping things below 0.01
# drop hks per flt - AIC jumps, try seamount instead?
depred_ts5 <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                          s(ChlA, by=EL_LA_NO, bs='ts', k = 10) + s(SSH, by=EL_LA_NO, bs='ts', k = 10) +
                          s(SST_2, by=EL_LA_NO, bs='ts', k = 10) +
                          s(YEAR, bs='ts') + s(MONTH, bs='cc') + 
                          s(NUM_FLTS, bs='ts') + s(SOAK, bs='ts') + s(HKS_PER_FLT, bs='ts') +
                          s(BET, bs='ts') + s(YFT, bs='ts') + s(MAHI, bs='ts') + 
                          s(CPUE_FLT, bs='ts') +
                          s(Cont_Dist, bs='ts') +
                          s(VESSEL_ID, bs='re') + EL_LA_NO,
                        data=sets_deep_depr, family=binomial, gamma=1.4)

summary(depred_ts5)
AIC(depred_ts1,depred_ts2,depred_ts3,depred_ts4,depred_ts5)

par(mfrow=c(2,2))
plot(depred_ts4, se=T, select=16, scale=0,
     jit=F, xlim=c(0,100), all.terms=T, shade=FALSE, ask=F)
plot(depred_ts4, se=T, select=17, scale=0,
     jit=F, xlim=c(0,100), all.terms=T, shade=FALSE, ask=F)
plot(depred_ts4, se=T, select=18, scale=0,
     jit=F, xlim=c(0,100), all.terms=T, shade=FALSE, ask=F)
plot(depred_ts4, se=T, select=19, scale=0,
     jit=F, xlim=c(0,2), all.terms=T, shade=FALSE, ask=F)
#############################################################################
## cs - all
#############################################################################

depred_cs1 <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                          s(ChlA, by=EL_LA_NO, bs='cs', k = 10) + s(SSH, by=EL_LA_NO, bs='cs', k = 10) +
                          s(SST_2, by=EL_LA_NO, bs='cs', k = 10) +
                          s(YEAR, bs='cs') + s(MONTH, bs='cc') + 
                          s(NUM_FLTS, bs='cs') + s(SOAK, bs='cs') + 
                          s(HKS_PER_FLT, bs='cs') + s(MIN_LEN, bs='cs') +
                          s(BET, bs='cs') + s(YFT, bs='cs') + s(MAHI, bs='cs') + 
                          s(BILLFISH, bs='cs') + s(CPUE_FLT, bs='cs') + 
                          s(Slope, bs='cs') + s(Depth, bs='cs') + 
                          s(Cont_Dist, bs='cs') + s(Seamt_Dist, bs='cs') + s(SST_RANGE, bs='cs') +
                          s(VESSEL_ID, bs='re') + postTRT + EL_LA_NO,
                        data=sets_deep_depr, family=binomial, gamma=1.4)

summary(depred_cs1)

# removing below 1 and not sign - run next
# drop minlen, billfish, slope, depth, sstrange
depred_cs2 <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                          s(ChlA, by=EL_LA_NO, bs='cs', k = 10) + s(SSH, by=EL_LA_NO, bs='cs', k = 10) +
                          s(SST_2, by=EL_LA_NO, bs='cs', k = 10) +
                          s(YEAR, bs='cs') + s(MONTH, bs='cc') + 
                          s(NUM_FLTS, bs='cs') + s(SOAK, bs='cs') + 
                          s(HKS_PER_FLT, bs='cs') + 
                          s(BET, bs='cs') + s(YFT, bs='cs') + s(MAHI, bs='cs') + 
                          s(CPUE_FLT, bs='cs') + 
                          s(Cont_Dist, bs='cs') + s(Seamt_Dist, bs='cs')+
                          s(VESSEL_ID, bs='re') + postTRT + EL_LA_NO,
                        data=sets_deep_depr, family=binomial, gamma=1.4)

summary(depred_cs2)
#############################################################################
## reml whole dataset
#############################################################################

gam_ts_reml1 <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k=20) + 
                              s(ChlA, by=EL_LA_NO, bs='ts', k =10) + s(SSH, by=EL_LA_NO, bs='ts', k =10) +
                              s(SST_2, by=EL_LA_NO, bs='ts', k =10) +
                              s(YEAR, bs='ts') + s(MONTH, bs='cs') + 
                              s(NUM_HKS_SET, bs='ts') + s(NUM_FLTS, bs='ts') + s(SOAK, bs='ts') + 
                              s(BET, bs='ts') + s(CPUE, bs='ts') + 
                              s(Slope, bs='ts') + s(Depth, bs='ts') + 
                              s(Cont_Dist, bs='ts') + s(Seamt_Dist, bs='ts') + s(SST_RANGE, bs='ts') +
                              s(EDDY_DIST, by = CYCL_TYPE, bs='ts', k=10) +
                              s(VESSEL_ID, bs='re') + postTRT + CYCL_TYPE + EL_LA_NO,
                              data=sets_deep_all, family=binomial, method="REML")

# drop num hks, sst range, eddy distance
gam_ts_reml2 <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k=20) + 
                            s(ChlA, by=EL_LA_NO, bs='ts', k =10) + s(SSH, by=EL_LA_NO, bs='ts', k =10) +
                            s(SST_2, by=EL_LA_NO, bs='ts', k =10) +
                            s(YEAR, bs='ts') + s(MONTH, bs='cs') + 
                            s(NUM_FLTS, bs='ts') + s(SOAK, bs='ts') + 
                            s(BET, bs='ts') + s(CPUE, bs='ts') + 
                            s(Slope, bs='ts') + s(Depth, bs='ts') + 
                            s(Cont_Dist, bs='ts') + s(Seamt_Dist, bs='ts') +
                            s(VESSEL_ID, bs='re') + postTRT + CYCL_TYPE + EL_LA_NO,
                          data=sets_deep_all, family=binomial, method="REML")
#

#############################################################################
## ts, estimate lambda - maybe soak as linear next..subset
#############################################################################

# ts penalizes coefficients to zero if unimportant
# month as cs (cyclic), sp estimated (-1)
gam_ts_sp1 <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                            s(ChlA, by=EL_LA_NO, bs='ts', k = 10) + s(SSH, by=EL_LA_NO, bs='ts', k = 10) +
                            s(SST_2, by=EL_LA_NO, bs='ts', k = 10) +
                            s(YEAR, bs='ts', sp=-1) + s(MONTH, bs='cs', sp=-1) + 
                            s(NUM_HKS_SET, bs='ts', sp=-1) + s(NUM_FLTS, bs='ts', sp=-1) + s(SOAK, bs='ts', sp=-1) + 
                            s(BET, bs='ts', sp=-1) + s(CPUE, bs='ts', sp=-1) + 
                            s(Slope, bs='ts', sp=-1) + s(Depth, bs='ts', sp=-1) + 
                            s(Cont_Dist, bs='ts', sp=-1) + s(Seamt_Dist, bs='ts', sp=-1) + s(SST_RANGE, bs='ts', sp=-1) +
                            s(EDDY_DIST, by = CYCL_TYPE, bs='ts', k=10) +
                            s(VESSEL_ID, bs='re') + postTRT + CYCL_TYPE + EL_LA_NO,
                          data=sets_deep_train, family=binomial) #, gamma=1.4)
# drop sst range, depth, seamount - according to Zuur, can drop all with EDF of 0 at same time
gam_ts_sp2 <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                          s(ChlA, by=EL_LA_NO, bs='ts', k = 10) + s(SSH, by=EL_LA_NO, bs='ts', k = 10) +
                          s(SST_2, by=EL_LA_NO, bs='ts', k = 10) +
                          s(YEAR, bs='ts', sp=-1) + s(MONTH, bs='cs', sp=-1) + 
                          s(NUM_HKS_SET, bs='ts', sp=-1) + s(NUM_FLTS, bs='ts', sp=-1) + s(SOAK, bs='ts', sp=-1) + 
                          s(BET, bs='ts', sp=-1) + s(CPUE, bs='ts', sp=-1) + 
                          s(Slope, bs='ts', sp=-1) +
                          s(Cont_Dist, bs='ts', sp=-1) +
                          s(EDDY_DIST, by = CYCL_TYPE, bs='ts', k=10) +
                          s(VESSEL_ID, bs='re') + postTRT + CYCL_TYPE + EL_LA_NO,
                        data=sets_deep_train, family=binomial) #, gamma=1.4)

# drop non-significant terms, one at a time

# drop slope
gam_ts_sp3 <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                          s(ChlA, by=EL_LA_NO, bs='ts', k = 10) + s(SSH, by=EL_LA_NO, bs='ts', k = 10) +
                          s(SST_2, by=EL_LA_NO, bs='ts', k = 10) +
                          s(YEAR, bs='ts', sp=-1) + s(MONTH, bs='cs', sp=-1) + 
                          s(NUM_HKS_SET, bs='ts', sp=-1) + s(NUM_FLTS, bs='ts', sp=-1) + s(SOAK, bs='ts', sp=-1) + 
                          s(BET, bs='ts', sp=-1) + s(CPUE, bs='ts', sp=-1) + 
                          s(Cont_Dist, bs='ts', sp=-1) +
                          s(EDDY_DIST, by = CYCL_TYPE, bs='ts', k=10) +
                          s(VESSEL_ID, bs='re') + postTRT + CYCL_TYPE + EL_LA_NO,
                        data=sets_deep_train, family=binomial) #, gamma=1.4)

# drop trt
gam_ts_sp4 <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                          s(ChlA, by=EL_LA_NO, bs='ts', k = 10) + s(SSH, by=EL_LA_NO, bs='ts', k = 10) +
                          s(SST_2, by=EL_LA_NO, bs='ts', k = 10) +
                          s(YEAR, bs='ts', sp=-1) + s(MONTH, bs='cs', sp=-1) + 
                          s(NUM_HKS_SET, bs='ts', sp=-1) + s(NUM_FLTS, bs='ts', sp=-1) + s(SOAK, bs='ts', sp=-1) + 
                          s(BET, bs='ts', sp=-1) + s(CPUE, bs='ts', sp=-1) + 
                          s(Cont_Dist, bs='ts', sp=-1) +
                          s(VESSEL_ID, bs='re') + EL_LA_NO,
                        data=sets_deep_train, family=binomial) #, gamma=1.4)

# drop year
gam_ts_sp5 <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                          s(ChlA, by=EL_LA_NO, bs='ts', k = 10) + s(SSH, by=EL_LA_NO, bs='ts', k = 10) +
                          s(SST_2, by=EL_LA_NO, bs='ts', k = 10) +
                          s(MONTH, bs='cs', sp=-1) + 
                          s(NUM_HKS_SET, bs='ts', sp=-1) + s(NUM_FLTS, bs='ts', sp=-1) + s(SOAK, bs='ts', sp=-1) + 
                          s(BET, bs='ts', sp=-1) + s(CPUE, bs='ts', sp=-1) + 
                          s(Cont_Dist, bs='ts', sp=-1) +
                          s(VESSEL_ID, bs='re') + EL_LA_NO,
                        data=sets_deep_train, family=binomial) #, gamma=1.4)
# drop hooks
gam_ts_sp6 <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                          s(ChlA, by=EL_LA_NO, bs='ts', k = 10) + s(SSH, by=EL_LA_NO, bs='ts', k = 10) +
                          s(SST_2, by=EL_LA_NO, bs='ts', k = 10) +
                          s(MONTH, bs='cs', sp=-1) + 
                          s(NUM_FLTS, bs='ts', sp=-1) + s(SOAK, bs='ts', sp=-1) + 
                          s(BET, bs='ts', sp=-1) + s(CPUE, bs='ts', sp=-1) + 
                          s(Cont_Dist, bs='ts', sp=-1) +
                          s(VESSEL_ID, bs='re') + EL_LA_NO,
                        data=sets_deep_train, family=binomial) #, gamma=1.4)
# drop chlA
gam_ts_sp7 <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                          s(SSH, by=EL_LA_NO, bs='ts', k = 10) +
                          s(SST_2, by=EL_LA_NO, bs='ts', k = 10) +
                          s(MONTH, bs='cs', sp=-1) + 
                          s(NUM_FLTS, bs='ts', sp=-1) + s(SOAK, bs='ts', sp=-1) + 
                          s(BET, bs='ts', sp=-1) + s(CPUE, bs='ts', sp=-1) + 
                          s(Cont_Dist, bs='ts', sp=-1) +
                          s(VESSEL_ID, bs='re') + EL_LA_NO,
                        data=sets_deep_train, family=binomial) #, gamma=1.4)

#############################################################################
## ts, estimate lambda, set gamma - subset data
#############################################################################
dt = sets_deep_train
# ts penalizes coefficients to zero if unimportant
# month as cs (cyclic), sp estimated (-1)
gam_ts_gam1 <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                          s(ChlA, by=EL_LA_NO, bs='ts', k = 10) + s(SSH, by=EL_LA_NO, bs='ts', k = 10) +
                          s(SST_2, by=EL_LA_NO, bs='ts', k = 10) +
                          s(YEAR, bs='ts', sp=-1) + s(MONTH, bs='cs', sp=-1) + 
                          s(NUM_HKS_SET, bs='ts', sp=-1) + s(NUM_FLTS, bs='ts', sp=-1) + s(SOAK, bs='ts', sp=-1) + 
                          s(BET, bs='ts', sp=-1) + s(CPUE, bs='ts', sp=-1) + 
                          s(Slope, bs='ts', sp=-1) + s(Depth, bs='ts', sp=-1) + 
                          s(Cont_Dist, bs='ts', sp=-1) + s(Seamt_Dist, bs='ts', sp=-1) + s(SST_RANGE, bs='ts', sp=-1) +
                          s(EDDY_DIST, by = CYCL_TYPE, bs='ts', k=10) +
                          s(VESSEL_ID, bs='re') + postTRT + CYCL_TYPE + EL_LA_NO,
                        data=dt, family=binomial, gamma=1.4)

# drop sst range, depth, seamount - according to Zuur, can drop all with EDF of 0 at same time
gam_ts_gam2 <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                          s(ChlA, by=EL_LA_NO, bs='ts', k = 10) + s(SSH, by=EL_LA_NO, bs='ts', k = 10) +
                          s(SST_2, by=EL_LA_NO, bs='ts', k = 10) +
                          s(YEAR, bs='ts', sp=-1) + s(MONTH, bs='cs', sp=-1) + 
                          s(NUM_HKS_SET, bs='ts', sp=-1) + s(NUM_FLTS, bs='ts', sp=-1) + s(SOAK, bs='ts', sp=-1) + 
                          s(BET, bs='ts', sp=-1) + s(CPUE, bs='ts', sp=-1) + 
                          s(Slope, bs='ts', sp=-1) +
                          s(Cont_Dist, bs='ts', sp=-1) +
                          s(EDDY_DIST, by = CYCL_TYPE, bs='ts', k=10) +
                          s(VESSEL_ID, bs='re') + postTRT + CYCL_TYPE + EL_LA_NO,
                        data=dt, family=binomial, gamma=1.4)

# drop non-significant terms, one at a time

# drop slope
gam_ts_gam3 <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                          s(ChlA, by=EL_LA_NO, bs='ts', k = 10) + s(SSH, by=EL_LA_NO, bs='ts', k = 10) +
                          s(SST_2, by=EL_LA_NO, bs='ts', k = 10) +
                          s(YEAR, bs='ts', sp=-1) + s(MONTH, bs='cs', sp=-1) + 
                          s(NUM_HKS_SET, bs='ts', sp=-1) + s(NUM_FLTS, bs='ts', sp=-1) + s(SOAK, bs='ts', sp=-1) + 
                          s(BET, bs='ts', sp=-1) + s(CPUE, bs='ts', sp=-1) + 
                          s(Cont_Dist, bs='ts', sp=-1) +
                          s(EDDY_DIST, by = CYCL_TYPE, bs='ts', k=10) +
                          s(VESSEL_ID, bs='re') + postTRT + CYCL_TYPE + EL_LA_NO,
                        data=dt, family=binomial, gamma=1.4)

# drop trt
gam_ts_gam4 <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                          s(ChlA, by=EL_LA_NO, bs='ts', k = 10) + s(SSH, by=EL_LA_NO, bs='ts', k = 10) +
                          s(SST_2, by=EL_LA_NO, bs='ts', k = 10) +
                          s(YEAR, bs='ts', sp=-1) + s(MONTH, bs='cs', sp=-1) + 
                          s(NUM_HKS_SET, bs='ts', sp=-1) + s(NUM_FLTS, bs='ts', sp=-1) + s(SOAK, bs='ts', sp=-1) + 
                          s(BET, bs='ts', sp=-1) + s(CPUE, bs='ts', sp=-1) + 
                          s(Cont_Dist, bs='ts', sp=-1) +
                          s(VESSEL_ID, bs='re') + EL_LA_NO,
                        data=dt, family=binomial, gamma=1.4)

# drop year
gam_ts_gam5 <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                          s(ChlA, by=EL_LA_NO, bs='ts', k = 10) + s(SSH, by=EL_LA_NO, bs='ts', k = 10) +
                          s(SST_2, by=EL_LA_NO, bs='ts', k = 10) +
                          s(MONTH, bs='cs', sp=-1) + 
                          s(NUM_HKS_SET, bs='ts', sp=-1) + s(NUM_FLTS, bs='ts', sp=-1) + s(SOAK, bs='ts', sp=-1) + 
                          s(BET, bs='ts', sp=-1) + s(CPUE, bs='ts', sp=-1) + 
                          s(Cont_Dist, bs='ts', sp=-1) +
                          s(VESSEL_ID, bs='re') + EL_LA_NO,
                        data=dt, family=binomial, gamma=1.4)
# drop chla
gam_ts_gam6 <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                          s(SSH, by=EL_LA_NO, bs='ts', k = 10) +
                          s(SST_2, by=EL_LA_NO, bs='ts', k = 10) +
                          s(MONTH, bs='cs', sp=-1) + 
                          s(NUM_HKS_SET, bs='ts', sp=-1) + s(NUM_FLTS, bs='ts', sp=-1) + s(SOAK, bs='ts', sp=-1) + 
                          s(BET, bs='ts', sp=-1) + s(CPUE, bs='ts', sp=-1) + 
                          s(Cont_Dist, bs='ts', sp=-1) +
                          s(VESSEL_ID, bs='re') + EL_LA_NO,
                        data=dt, family=binomial, gamma=1.4)
# drop hooks
gam_ts_gam7 <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                           s(SSH, by=EL_LA_NO, bs='ts', k = 10) +
                           s(SST_2, by=EL_LA_NO, bs='ts', k = 10) +
                           s(MONTH, bs='cs', sp=-1) + 
                           s(NUM_FLTS, bs='ts', sp=-1) + s(SOAK, bs='ts', sp=-1) + 
                           s(BET, bs='ts', sp=-1) + s(CPUE, bs='ts', sp=-1) + 
                           s(Cont_Dist, bs='ts', sp=-1) +
                           s(VESSEL_ID, bs='re') + EL_LA_NO,
                         data=dt, family=binomial, gamma=1.4)




#############################################################################
## ts, set lambda - subset
#############################################################################
# ts penalizes coefficients to zero if unimportant - month as cs (cyclic)
gam_ts_rand1 <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k=30) + 
                            s(ChlA, by=EL_LA_NO, bs='ts', k =20) + s(SSH, by=EL_LA_NO, bs='ts', k =20) +
                            s(SST_2, by=EL_LA_NO, bs='ts', k =20) +
                            s(YEAR, bs='ts', sp=0.6) + s(MONTH, bs='cs', sp=0.6) + 
                            s(NUM_HKS_SET, bs='ts', sp=0.6) + s(NUM_FLTS, bs='ts', sp=0.6) + s(SOAK, bs='ts', sp=0.6) + 
                            s(BET, bs='ts', sp=0.6) + s(CPUE, bs='ts', sp=0.6) + 
                            s(Slope, bs='ts', sp=0.6) + s(Depth, bs='ts', sp=0.6) + 
                            s(Cont_Dist, bs='ts', sp=0.6) + s(Seamt_Dist, bs='ts', sp=0.6) + s(SST_RANGE, bs='ts') +
                            s(EDDY_DIST, by = CYCL_TYPE, bs='ts', k=10) +
                            s(VESSEL_ID, bs='re') + postTRT + CYCL_TYPE + EL_LA_NO,
                          data=sets_deep_train, family=binomial) #, gamma=1.4)
# removing eddy distance, sst range
gam_ts_rand2 <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k=30) + 
                            s(ChlA, by=EL_LA_NO, bs='ts', k =20) + s(SSH, by=EL_LA_NO, bs='ts', k =20) +
                            s(SST_2, by=EL_LA_NO, bs='ts', k =20) +
                            s(YEAR, bs='ts', sp=0.6) + s(MONTH, bs='cs', sp=0.6) + 
                            s(NUM_HKS_SET, bs='ts', sp=0.6) + s(NUM_FLTS, bs='ts', sp=0.6) + s(SOAK, bs='ts', sp=0.6) + 
                            s(BET, bs='ts', sp=0.6) + s(CPUE, bs='ts', sp=0.6) + 
                            s(Slope, bs='ts', sp=0.6) + s(Depth, bs='ts', sp=0.6) + 
                            s(Cont_Dist, bs='ts', sp=0.6) + s(Seamt_Dist, bs='ts', sp=0.6) +
                            s(VESSEL_ID, bs='re') + postTRT + CYCL_TYPE + EL_LA_NO,
                          data=sets_deep_train, family=binomial) #, gamma=1.4)
# removing trt, cycle type, changed BET to linear
gam_ts_rand3 <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k=30) + 
                            s(ChlA, by=EL_LA_NO, bs='ts', k =20) + s(SSH, by=EL_LA_NO, bs='ts', k =20) +
                            s(SST_2, by=EL_LA_NO, bs='ts', k =20) +
                            s(YEAR, bs='ts', sp=0.6) + s(MONTH, bs='cs', sp=0.6) + 
                            s(NUM_HKS_SET, bs='ts', sp=0.6) + s(NUM_FLTS, bs='ts', sp=0.6) + s(SOAK, bs='ts', sp=0.6) + 
                            BET + s(CPUE, bs='ts', sp=0.6) + 
                            s(Slope, bs='ts', sp=0.6) + s(Depth, bs='ts', sp=0.6) + 
                            s(Cont_Dist, bs='ts', sp=0.6) + s(Seamt_Dist, bs='ts', sp=0.6) +
                            s(VESSEL_ID, bs='re') + EL_LA_NO,
                          data=sets_deep_train, family=binomial) #, gamma=1.4)
