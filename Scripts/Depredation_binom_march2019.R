#############################################################################
## Depred models - March 2019
#############################################################################
#############################################################################
# Joseph Fader
# March, 2019

library(tidyverse)
library(mgcv)
library(feather)
library(grid)


sets_deep_all <- read_feather("Data/sets_deep_all.feather")
summary(sets_deep_all)
sapply(sets_deep_all, class)


# code to separate in training and test chunks
n <-  nrow(sets_deep_all)
trainIndex <- sample(1:n, size = round(0.75*n), replace=FALSE)
sets_deep_train <- sets_deep_all[trainIndex ,]
sets_deep_test <- sets_deep_all[-trainIndex ,]

summary(sets_deep_train)
cat(paste(shQuote(colnames(sets_deep_all), type="cmd"), collapse=", "))

# sets_deep_train <- sets_deep_train %>%
sets_deep_test <- sets_deep_test %>%
  select(TRIP_ID, VESSEL_ID, SET_NUM, HAUL_BEGIN_LAT, HAUL_BEGIN_LON, MONTH, YEAR, postTRT,
         SOAK, HOOKS, NUM_FLTS, NUM_HKS_SET, HKS_PER_FLT, MIN_LEN, BLUEDYE, 
         KEPT, BET, YFT, TUNA, MAHI, BILLFISH, SWO, SHARKS, CPUE, cpue_avg_3d_100k, num_vessels_3d_100k,
         MM_YN, MM_sum, MM_any, FKW, FKW_sum, 
         Slope, Depth, Cont_Dist, Seamt_Dist, 
         EDDY_DIST, AMPLITUDE, CYCL_TYPE, SPEED, RADIUS,
         SST_2, front_dis, tke, adt, eke, chla_mo_9k, EL_LA_NO, Moon_Illum) %>%
  drop_na()

# min length has 445 NAs
# hks/flt 19 NAs
# eddy stuff 2342 NAs
# front dist 31 NAs
# chla_mo_9k has 757 NAs

#############################################################################
## with el nino interactions
#############################################################################
# ts ---month as cc (cyclic)
depred_all1 <- mgcv::gam(MM_any ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                           s(chla_mo_9k, by=EL_LA_NO, bs='ts', k = 10) + 
                           s(adt, by=EL_LA_NO, bs='ts', k = 10) +
                           s(tke, by=EL_LA_NO, bs='ts', k = 10) +
                           s(EDDY_DIST, by=EL_LA_NO, bs='ts', k = 10) +
                           s(AMPLITUDE, by=EL_LA_NO, bs='ts', k = 10) +
                           s(front_dis, by=EL_LA_NO, bs='ts', k = 10) +
                           s(SST_2, by=EL_LA_NO, bs='ts', k = 10) +
                           s(YEAR, bs='ts') + s(MONTH, bs='cc') + 
                           s(NUM_HKS_SET, bs='ts') + s(SOAK, bs='ts') + 
                           s(HKS_PER_FLT, bs='ts') + s(MIN_LEN, bs='ts') +
                           s(BET, bs='ts') + s(YFT, bs='ts') + s(MAHI, bs='ts') + 
                           s(BILLFISH, bs='ts') + s(SWO, bs='ts') + s(SHARKS, bs='ts') + 
                           s(cpue_avg_3d_100k, bs='ts') + s(num_vessels_3d_100k, bs='ts') +
                           s(Slope, bs='ts') + s(Depth, bs='ts') + 
                           s(Cont_Dist, bs='ts') + s(Seamt_Dist, bs='ts') +
                           s(Moon_Illum, bs='ts') + s(VESSEL_ID, bs='re') + postTRT + EL_LA_NO,
                         data=sets_deep_train, family=binomial, gamma=1.4)

summary(depred_all1)

# drop tke, eddy dist, amp, front dist, sst, hooks/flt, minlen,
#      swo, sharks, num vessels, cont, trt, moon illum
depred_all2 <- mgcv::gam(MM_any ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                           s(chla_mo_9k, by=EL_LA_NO, bs='ts', k = 10) + 
                           s(adt, by=EL_LA_NO, bs='ts', k = 10) +
                           s(YEAR, bs='ts') + s(MONTH, bs='cc') + 
                           s(NUM_HKS_SET, bs='ts') + s(SOAK, bs='ts') + 
                           s(BET, bs='ts') + s(YFT, bs='ts') + s(MAHI, bs='ts') + 
                           s(BILLFISH, bs='ts') +
                           s(cpue_avg_3d_100k, bs='ts') +
                           s(Slope, bs='ts') + s(Depth, bs='ts') + 
                           s(Seamt_Dist, bs='ts') +
                           s(VESSEL_ID, bs='re') + EL_LA_NO,
                         data=sets_deep_train, family=binomial, gamma=1.4)

summary(depred_all2)
AIC(depred_all1,depred_all2)

# drop seamt
depred_all3 <- mgcv::gam(MM_any ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                           s(chla_mo_9k, by=EL_LA_NO, bs='ts', k = 10) + 
                           s(adt, by=EL_LA_NO, bs='ts', k = 10) +
                           s(YEAR, bs='ts') + s(MONTH, bs='cc') + 
                           s(NUM_HKS_SET, bs='ts') + s(SOAK, bs='ts') + 
                           s(BET, bs='ts') + s(YFT, bs='ts') + s(MAHI, bs='ts') + 
                           s(BILLFISH, bs='ts') +
                           s(cpue_avg_3d_100k, bs='ts') +
                           s(Slope, bs='ts') + s(Depth, bs='ts') + 
                           s(VESSEL_ID, bs='re') + EL_LA_NO,
                         data=sets_deep_train, family=binomial, gamma=1.4)

summary(depred_all3)
AIC(depred_all1,depred_all2,depred_all3) ## UBRE 2nd lowest for 3rd model
gam.check(depred_all3) ## provides more diagnostics

# plots
plot(depred_all3, se=T, select=NULL, scale=0,
     jit=F, xlim=NULL, all.terms=T, shade=FALSE, ask=F)



### testing prediction

test_gam <- predict(depred_all3, sets_deep_test, type = "response")
?table
dim(test_gam)
## see how test did
gam_pred = rep(0, length(sets_deep_test))
gam_pred[test_gam >.2] = 1
max(test_gam)
cont_table <- table(gam_pred, sets_deep_test$MM_any) # confusion matrix to show classification errors
(cont_table[1,1] + cont_table[2,2]) / sum(cont_table) # training error rate (1 minus this number)
mean(gam_pred == sets_deep_test$MM_any, na.rm=T) # same, fraction predicted correctly
mean(glm.pred != sets_deep_test$MM_any, na.rm = T)  ## test error rate




