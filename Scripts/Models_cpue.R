
library(tidyverse)
library(mgcv)
library(feather)
library(grid)

## filter out nas for needed vars
sets_deep_depr <- sets_deep_all %>%
  #filter(HOOKS != 'OTHER') %>%
  #filter(HOOKS != 'OTHER' & HOOKS != 'JHOOK') %>%
  #mutate(HOOKS = ifelse((HOOKS == 'TUNA' | HOOKS == 'JHOOK'), 'TUNAJ', 'CIRCLE')) %>%
  select(TRIP_ID, VESSEL_ID, SET_NUM, HAUL_BEGIN_LAT, HAUL_BEGIN_LON, MONTH, YEAR, postTRT,
         SOAK, HOOKS, NUM_FLTS, NUM_HKS_SET, HKS_PER_FLT, MIN_LEN, BLUEDYE, BET, YFT, TUNA, 
         MAHI, BILLFISH, SWO, CPUE, CPUE_FLT, MM_YN, MM_sum, FKW, FKW_sum, Slope, Depth, Cont_Dist,
         Seamt_Dist, EDDY_DIST, CYCL_TYPE, SST_2, SST_RANGE, ChlA, SSH, EL_LA_NO, Moon_Illum) %>%
  drop_na()

## make subsample to test models on
sets_deep_sample <- sets_deep_depr[sample(1:nrow(sets_deep_depr), 20000,
                                         replace=FALSE),]
# code to separate in training and test chunks
n <-  nrow(sets_deep_sample)
trainIndex <- sample(1:n, size = round(0.5*n), replace=FALSE)
sets_deep_train <- sets_deep_sample[trainIndex ,]
sets_deep_test <- sets_deep_sample[-trainIndex ,]


# ts ---month as cc (cyclic)
cpue_1 <- mgcv::gam(CPUE ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                           s(ChlA, by=EL_LA_NO, bs='ts', k = 10) + s(SSH, by=EL_LA_NO, bs='ts', k = 10) +
                           s(SST_2, by=EL_LA_NO, bs='ts', k = 10) +
                           s(YEAR, bs='ts') + s(MONTH, bs='cc') + 
                           s(NUM_HKS_SET, bs='ts') + s(SOAK, bs='ts') + 
                           s(HKS_PER_FLT, bs='ts') + s(MIN_LEN, bs='ts') + s(MM_sum, bs='ts') + 
                           s(BET, bs='ts') + s(YFT, bs='ts') + s(MAHI, bs='ts') + 
                           s(BILLFISH, bs='ts') + s(CPUE, bs='ts') +
                           s(Slope, bs='ts') + s(Depth, bs='ts') + 
                           s(Cont_Dist, bs='ts') + s(Seamt_Dist, bs='ts') + s(SST_RANGE, bs='ts') +
                           s(Moon_Illum, bs='ts') + s(VESSEL_ID, bs='re') + 
                           postTRT + EL_LA_NO,
                         data=sets_deep_train, family=poisson(link="log"))






