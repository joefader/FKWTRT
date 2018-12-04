#############################################################################
# Models
# Joseph Fader
# March, 2018

library(tidyverse)
library(mgcv)
library(feather)
library(grid )

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

#############################################################################
### select variables
#############################################################################

gam_mgcv_int_ts_rand_nohk <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k=30) + 
                                    s(ChlA, by=EL_LA_NO, bs='ts', k =20) + s(SSH, by=EL_LA_NO, bs='ts', k =20) +
                                    s(SST_2, by=EL_LA_NO, bs='ts', k =20) +
                                    s(YEAR, bs='ts', sp=0.6) + s(MONTH, bs='ts', sp=0.6) + 
                                    s(SOAK, bs='ts', sp=0.6) + 
                                    s(TUNA, bs='ts', sp=0.6) + s(CPUE, bs='ts', sp=0.6) + 
                                    s(Slope, bs='ts', sp=0.6) + s(Depth, bs='ts', sp=0.6) + 
                                    s(Cont_Dist, bs='ts', sp=0.6) + s(Seamt_Dist, bs='ts', sp=0.6) + 
                                    s(EDDY_DIST, by = CYCL_TYPE, bs='ts', k=10) +
                                    s(VESSEL_ID, bs='re') + postTRT + CYCL_TYPE + EL_LA_NO,
                                  data=sets_deep_train, family=binomial, gamma=1.4)

gam_mgcv_int_ts_rand_ <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k=30) + 
                                         s(ChlA, by=EL_LA_NO, bs='ts', k =20) + s(SSH, by=EL_LA_NO, bs='ts', k =20) +
                                         s(SST_2, by=EL_LA_NO, bs='ts', k =20) +
                                         s(YEAR, bs='ts', sp=0.6) + s(MONTH, bs='ts', sp=0.6) + 
                                         s(SOAK, bs='ts', sp=0.6) + 
                                         s(TUNA, bs='ts', sp=0.6) + s(CPUE, bs='ts', sp=0.6) + 
                                         s(Cont_Dist, bs='ts', sp=0.6) + s(Seamt_Dist, bs='ts', sp=0.6) + 
                                         s(EDDY_DIST, by = CYCL_TYPE, bs='ts', k=10) +
                                         s(VESSEL_ID, bs='re') + postTRT + CYCL_TYPE + EL_LA_NO,
                                       data=sets_deep_train, family=binomial, gamma=1.4)
#############################################################################
### vessel as random
#############################################################################

# ps spline with vessel random
gam_mgcv_int_rand <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, k=30) + 
                                s(ChlA, by=EL_LA_NO, k =20) + s(SSH, by=EL_LA_NO, k =20) + s(SST_2, by=EL_LA_NO, k =20) +
                                s(YEAR, bs='ps', sp=0.6) + s(MONTH, bs='ps', sp=0.6) + 
                                s(NUM_FLT_SET, bs='ps', sp=0.6) + s(SOAK, bs='ps', sp=0.6) + 
                                s(TUNA, bs='ps', sp=0.6) + s(CPUE, bs='ps', sp=0.6) + 
                                s(Slope, bs='ps', sp=0.6) + s(Depth, bs='ps', sp=0.6) + 
                                s(Cont_Dist, bs='ps', sp=0.6) + s(Seamt_Dist, bs='ps', sp=0.6) + 
                                s(EDDY_DIST, by = CYCL_TYPE, k=10) +
                                s(VESSEL_ID, bs='re') + postTRT + CYCL_TYPE + EL_LA_NO,
                              data=sets_deep_train, family=binomial, gamma=1.4)

# ts penalizes coefficients to zero if unimportant - month as cs (cyclic)
gam_mgcv_int_ts_rand <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k=30) + 
                               s(ChlA, by=EL_LA_NO, bs='ts', k =20) + s(SSH, by=EL_LA_NO, bs='ts', k =20) +
                               s(SST_2, by=EL_LA_NO, bs='ts', k =20) +
                               s(YEAR, bs='ts', sp=0.6) + s(MONTH, bs='cs', sp=0.6) + 
                               s(NUM_HKS_SET, bs='ts', sp=0.6) + s(SOAK, bs='ts', sp=0.6) + 
                               s(TUNA, bs='ts', sp=0.6) + s(CPUE, bs='ts', sp=0.6) + 
                               s(Slope, bs='ts', sp=0.6) + s(Depth, bs='ts', sp=0.6) + 
                               s(Cont_Dist, bs='ts', sp=0.6) + s(Seamt_Dist, bs='ts', sp=0.6) + 
                               s(EDDY_DIST, by = CYCL_TYPE, bs='ts', k=10) +
                               s(VESSEL_ID, bs='re') + postTRT + CYCL_TYPE + EL_LA_NO,
                             data=sets_deep_train, family=binomial, gamma=1.4)

#############################################################################
### ps
#############################################################################

# levels for oni and cycl type, combined smooth for lat/lon
gam_mgcv_int_bin <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, k=30) + 
                                s(ChlA, by=EL_LA_NO, k =20) + s(SSH, by=EL_LA_NO, k =20) + s(SST_2, by=EL_LA_NO, k =20) +
                                s(YEAR, bs='ps', sp=0.6) + s(MONTH, bs='ps', sp=0.6) + 
                                s(NUM_HKS_SET, bs='ps', sp=0.6) + s(SOAK, bs='ps', sp=0.6) + 
                                s(TUNA, bs='ps', sp=0.6) + s(CPUE, bs='ps', sp=0.6) + 
                                s(Slope, bs='ps', sp=0.6) + s(Depth, bs='ps', sp=0.6) + 
                                s(Cont_Dist, bs='ps', sp=0.6) + s(Seamt_Dist, bs='ps', sp=0.6) + 
                                s(EDDY_DIST, by = CYCL_TYPE, k=10) +
                                VESSEL_ID + postTRT + CYCL_TYPE + EL_LA_NO,
                              data=sets_deep_train, family=binomial, gamma=1.4)
summary(gam_mgcv_int_bin)

# gamma 1.4 potentially reduces overfitting
gam_mgcv_int_qubin <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, k=30) + 
                                s(ChlA, by=EL_LA_NO, k =20) + s(SSH, by=EL_LA_NO, k =20) + s(SST_2, by=EL_LA_NO, k =20) +
                                s(YEAR, bs='ps', sp=0.6) + s(MONTH, bs='ps', sp=0.6) + 
                                s(NUM_HKS_SET, bs='ps', sp=0.6) + s(SOAK, bs='ps', sp=0.6) + 
                                s(TUNA, bs='ps', sp=0.6) + s(CPUE, bs='ps', sp=0.6) + 
                                s(Slope, bs='ps', sp=0.6) + s(Depth, bs='ps', sp=0.6) + 
                                s(Cont_Dist, bs='ps', sp=0.6) + s(Seamt_Dist, bs='ps', sp=0.6) + 
                                s(EDDY_DIST, by = CYCL_TYPE, k=10) +
                                VESSEL_ID + postTRT + CYCL_TYPE + EL_LA_NO,
                              data=sets_deep_train, family=quasibinomial, gamma=1.4)

summary(gam_mgcv_int_qubin)



#############################################################################
### GAMs in mgcv - using ts and set lambda
#############################################################################

# ts penalizes coefficients to zero if unimportant
gam_mgcv_int_ts <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k=30) + 
                                      s(ChlA, by=EL_LA_NO, bs='ts', k =20) + s(SSH, by=EL_LA_NO, bs='ts', k =20) +
                                      s(SST_2, by=EL_LA_NO, bs='ts', k =20) +
                                      s(YEAR, bs='ts', sp=0.6) + s(MONTH, bs='ts', sp=0.6) + 
                                      s(NUM_HKS_SET, bs='ts', sp=0.6) + s(SOAK, bs='ts', sp=0.6) + 
                                      s(TUNA, bs='ts', sp=0.6) + s(CPUE, bs='ts', sp=0.6) + 
                                      s(Slope, bs='ts', sp=0.6) + s(Depth, bs='ts', sp=0.6) + 
                                      s(Cont_Dist, bs='ts', sp=0.6) + s(Seamt_Dist, bs='ts', sp=0.6) + 
                                      s(EDDY_DIST, by = CYCL_TYPE, bs='ts', k=10) +
                                      VESSEL_ID + postTRT + CYCL_TYPE + EL_LA_NO,
                                    data=sets_deep_train, family=binomial, gamma=1.4)

# remove vessel to work with test
gam_mgcv_int_ts_novess <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k=50) + 
                               s(ChlA, by=EL_LA_NO, bs='ts', k =20) + s(SSH, by=EL_LA_NO, bs='ts', k =20) +
                               s(SST_2, by=EL_LA_NO, bs='ts', k =20) +
                               s(YEAR, bs='ts', sp=0.6) + s(MONTH, bs='ts', sp=0.6) + 
                               s(NUM_HKS_SET, bs='ts', sp=0.6) + s(SOAK, bs='ts', sp=0.6) + 
                               s(TUNA, bs='ts', sp=0.6) + s(CPUE, bs='ts', sp=0.6) + 
                               s(Slope, bs='ts', sp=0.6) + s(Depth, bs='ts', sp=0.6) + 
                               s(Cont_Dist, bs='ts', sp=0.6) + s(Seamt_Dist, bs='ts', sp=0.6) + 
                               s(EDDY_DIST, by = CYCL_TYPE, bs='ts', k=10) +
                               postTRT + CYCL_TYPE + EL_LA_NO,
                             data=sets_deep_train, family=binomial, gamma=1.4)

# transforms - cant get to work for now
gam_mgcv_int_tssqrt <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k=50) + 
                               s(ChlA, by=EL_LA_NO, bs='ts', k =20) + s(SSH, by=EL_LA_NO, bs='ts', k =20) +
                               s(SST_2, by=EL_LA_NO, bs='ts', k =20) +
                               s(YEAR, bs='ts', sp=0.6) + s(MONTH, bs='ts', sp=0.6) + 
                               s(NUM_HKS_SET, bs='ts', sp=0.6) + s(SOAK, bs='ts', sp=0.6) + 
                               s(TUNA, bs='ts', sp=0.6) + s(CPUE, bs='ts', sp=0.6) + 
                               s(Slope, bs='ts', sp=0.6) + s(Depth, bs='ts', sp=0.6) + 
                               s(Cont_Dist, bs='ts', sp=0.6) + s(Seamt_Dist, bs='ts', sp=0.6) + 
                               s(EDDY_DIST, by = CYCL_TYPE, bs='ts', k=10) +
                               VESSEL_ID + postTRT + CYCL_TYPE + EL_LA_NO,
                             data=sets_deep_train, family=binomial, gamma=1.4)


summary(gam_mgcv_int_ts)

## plots
par(mfrow=c(2,2))
plot(gam_mgcv_int_ts, residuals = T, col= "black")
anova(gam_mgcv_int_bin_gamma)
vis.gam(gam_mgcv_int_bin_gamma)

concurvity(gam_mgcv_int_ts)


## check residuals
gam.check(gam_mgcv_int_bin_gamma)
rsd <- residuals(gam_mgcv_int_bin_gamma) # resids for every observation
qq.gam(gam_mgcv_int_bin_gamma,rep=100); plot(fitted(gam_mgcv_int_bin_gamma),rsd)
plot(sets_deep_sample$HAUL_BEGIN_LAT,rsd); plot(sets_deep_sample$HAUL_BEGIN_LON,rsd)
#############################################################################
### GAMs in mgcv - using cc and set lambda
#############################################################################

# - 530-~6 - slightly lower deviance than ps
gam_mgcv_cc <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, k=50) + 
                            s(ChlA, by=EL_LA_NO, k =20) + s(SSH, by=EL_LA_NO, k =20) + s(SST_2, by=EL_LA_NO, k =20) +
                            s(YEAR, bs='cc', sp=0.6) + s(MONTH, bs='cc', sp=0.6) + 
                            s(NUM_HKS_SET, bs='cc', sp=0.6) + s(SOAK, bs='cc', sp=0.6) + 
                            s(TUNA, bs='cc', sp=0.6) + s(CPUE, bs='cc', sp=0.6) + 
                            s(Slope, bs='cc', sp=0.6) + s(Depth, bs='cc', sp=0.6) + 
                            s(Cont_Dist, bs='cc', sp=0.6) + s(Seamt_Dist, bs='cc', sp=0.6) + 
                            s(EDDY_DIST, by = CYCL_TYPE, k=10) +
                            VESSEL_ID + postTRT + CYCL_TYPE + EL_LA_NO,
                          data=sets_deep_sample, family=binomial)

summary(gam_mgcv_cc)

#############################################################################
### GAMs in mgcv - set lambda
#############################################################################

# with interactions 
# - no info about some basis coefficients when elnino combined with all
# - does fit when elnino used as by term with each oceanographic variable
# - does fit when elnino and cycl type as interactions but not separate factors - big jump in deviance (24.8%)
# - start 227, finish before 256, same as above but separate factors too (24.9%)
#       -also reduced ks (very little effect down to 30/20, try 20/10 next)
# - last run trying above for quasi, start ~3 ..took two hours - failed when k=40, although dev>30%
gam_mgcv_int_qu <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, k=50) + 
                            s(ChlA, by=EL_LA_NO, k =30) + s(SSH, by=EL_LA_NO, k =30) + s(SST_2, by=EL_LA_NO, k =30) +
                            s(YEAR, bs='ps', sp=0.6) + s(MONTH, bs='ps', sp=0.6) + 
                            s(NUM_HKS_SET, bs='ps', sp=0.6) + s(SOAK, bs='ps', sp=0.6) + 
                            s(TUNA, bs='ps', sp=0.6) + s(CPUE, bs='ps', sp=0.6) + 
                            s(Slope, bs='ps', sp=0.6) + s(Depth, bs='ps', sp=0.6) + 
                            s(Cont_Dist, bs='ps', sp=0.6) + s(Seamt_Dist, bs='ps', sp=0.6) + 
                            s(EDDY_DIST, by = CYCL_TYPE, k=20) +
                            VESSEL_ID + postTRT + CYCL_TYPE + EL_LA_NO,
                          data=sets_deep_sample, family=quasibinomial)
summary(gam_mgcv_int_qu)
summary(gam_mgcv_int_bin)



# fitted probs of 0 or 1 warning, otherwise fits
gam_mgcv_lam <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, bs='ps', sp=0.6) + s(HAUL_BEGIN_LON, bs='ps', sp=0.6) + 
                            s(YEAR, bs='ps', sp=0.6) + s(MONTH, bs='ps', sp=0.6) + 
                            s(NUM_HKS_SET, bs='ps', sp=0.6) + s(SOAK, bs='ps', sp=0.6) + 
                            s(TUNA, bs='ps', sp=0.6) + s(CPUE, bs='ps', sp=0.6) +  s(Slope, bs='ps', sp=0.6) +
                            s(Depth, bs='ps', sp=0.6) + s(Cont_Dist, bs='ps', sp=0.6) + s(Seamt_Dist, bs='ps', sp=0.6) +
                            s(Seamt_Height, bs='ps', sp=0.6) + s(ChlA, bs='ps', sp=0.6) + s(SSH, bs='ps', sp=0.6) + s(SST_2, bs='ps', sp=0.6) +
                            s(EDDY_DIST, bs='ps', sp=0.6) + s(ONI, bs='ps', sp=0.6) +
                            VESSEL_ID + postTRT + CYCL_TYPE,
                          data=sets_deep_sample, family=binomial)


# fit fine, pretty high deviance - better with quasi - k=100 seems reasonable, 500 takes a very long time
gam_mgcv_lam <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, k=50) + 
                            s(YEAR, bs='ps', sp=0.6) + s(MONTH, bs='ps', sp=0.6) + 
                            s(NUM_HKS_SET, bs='ps', sp=0.6) + s(SOAK, bs='ps', sp=0.6) + 
                            s(TUNA, bs='ps', sp=0.6) + s(CPUE, bs='ps', sp=0.6) +  s(Slope, bs='ps', sp=0.6) +
                            s(Depth, bs='ps', sp=0.6) + s(Cont_Dist, bs='ps', sp=0.6) + s(Seamt_Dist, bs='ps', sp=0.6) +
                            s(Seamt_Height, bs='ps', sp=0.6) + s(ChlA, bs='ps', sp=0.6) + s(SSH, bs='ps', sp=0.6) +
                            s(SST_2, bs='ps', sp=0.6) + s(EDDY_DIST, bs='ps', sp=0.6) + s(ONI, bs='ps', sp=0.6) +
                            VESSEL_ID + postTRT + CYCL_TYPE,
                          data=sets_deep_sample, family=quasibinomial)

# fit with cr
gam_mgcv_lam <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, k=50) + 
                            s(YEAR, bs='ps', sp=0.6) + s(MONTH, bs='ps', sp=0.6) + 
                            s(NUM_HKS_SET, bs='ps', sp=0.6) + s(SOAK, bs='ps', sp=0.6) + 
                            s(TUNA, bs='ps', sp=0.6) + s(CPUE, bs='ps', sp=0.6) +  s(Slope, bs='ps', sp=0.6) +
                            s(Depth, bs='ps', sp=0.6) + s(Cont_Dist, bs='ps', sp=0.6) + s(Seamt_Dist, bs='ps', sp=0.6) +
                            s(Seamt_Height, bs='ps', sp=0.6) + s(ChlA, bs='ps', sp=0.6) + s(SSH, bs='ps', sp=0.6) +
                            s(SST_2, bs='ps', sp=0.6) + s(EDDY_DIST, bs='ps', sp=0.6) + s(ONI, bs='ps', sp=0.6) +
                            VESSEL_ID + postTRT + CYCL_TYPE,
                          data=sets_deep_sample, family=quasibinomial)
summary(gam_mgcv_lam)


#############################################################################
### GAMs in mgcv - REML - select smoothing parameters and variables - step failures
#############################################################################

# fitting terminated step failure
gam_mgcv_select <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT) + (HAUL_BEGIN_LON) + 
                               s(ChlA, by=EL_LA_NO, k =30) + s(SSH, by=EL_LA_NO, k =30) + s(SST_2, by=EL_LA_NO, k =30) +
                               s(YEAR) + s(MONTH) + 
                               s(NUM_HKS_SET) + s(SOAK) + 
                               s(TUNA) + s(CPUE) + 
                               s(Slope) + s(Depth) + 
                               s(Cont_Dist) + s(Seamt_Dist) + 
                               s(EDDY_DIST, by = CYCL_TYPE, k=20) +
                               VESSEL_ID + postTRT + CYCL_TYPE + EL_LA_NO,
                             data=sets_deep_sample, family=binomial, method="REML", select=TRUE)

# start around 11:50 - terminated step failure 1200
# tried again with updated interaction terms and ks, still fails
gam_mgcv_select <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, k=50) + 
                               s(ChlA, by=EL_LA_NO, k =30) + s(SSH, by=EL_LA_NO, k =30) + s(SST_2, by=EL_LA_NO, k =30) +
                               s(YEAR) + s(MONTH) + 
                               s(NUM_HKS_SET) + s(SOAK) + 
                               s(TUNA) + s(CPUE) + 
                               s(Slope) + s(Depth) + 
                               s(Cont_Dist) + s(Seamt_Dist) + 
                               s(EDDY_DIST, by = CYCL_TYPE, k=20) +
                               VESSEL_ID + postTRT + CYCL_TYPE + EL_LA_NO,
                             data=sets_deep_sample, family=binomial, method="REML", select=TRUE)

summary(gam_mgcv_select)

#############################################################################
### GAMs in mgcv - REML tp splines - all terminate
#############################################################################

# no xy
# fitting terminated with step failure
gam_mgcv_REML_tp <- mgcv::gam(MM_YN ~
                                s(YEAR, bs='tp') + s(MONTH, bs='tp') + 
                                s(NUM_HKS_SET, bs='tp') + s(SOAK, bs='tp') + 
                                s(TUNA, bs='tp') + s(CPUE, bs='tp') +  s(Slope, bs='tp') +
                                s(Depth, bs='tp') + s(Cont_Dist, bs='tp') + s(Seamt_Dist, bs='tp') +
                                s(Seamt_Height, bs='tp') + s(ChlA, bs='tp') + s(SSH, bs='tp') + s(SST_2, bs='tp') +
                                s(EDDY_DIST, bs='tp') + s(ONI, bs='tp') +
                                VESSEL_ID + postTRT + CYCL_TYPE,
                              data=sets_deep_sample, family=binomial, method = "REML")
# x y separate
# fitting terminated with step failure
gam_mgcv_REML_tp <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, bs='tp') + s(HAUL_BEGIN_LON, bs='tp') + 
                                s(YEAR, bs='tp') + s(MONTH, bs='tp') + 
                                s(NUM_HKS_SET, bs='tp') + s(SOAK, bs='tp') + 
                                s(TUNA, bs='tp') + s(CPUE, bs='tp') +  s(Slope, bs='tp') +
                                s(Depth, bs='tp') + s(Cont_Dist, bs='tp') + s(Seamt_Dist, bs='tp') +
                                s(Seamt_Height, bs='tp') + s(ChlA, bs='tp') + s(SSH, bs='tp') + s(SST_2, bs='tp') +
                                s(EDDY_DIST, bs='tp') + s(ONI, bs='tp') +
                                VESSEL_ID + postTRT + CYCL_TYPE,
                              data=sets_deep_sample, family=binomial, method = "REML")

summary(gam_mgcv_REML_tp)

# xy combined
# step failure with 50 and 100
# both optimizers - perf just resets to outer anyway and then terminates
gam_mgcv_REML_tp <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, k=100) + 
                                s(YEAR, bs='tp') + s(MONTH, bs='tp') + 
                                s(NUM_HKS_SET, bs='tp') + s(SOAK, bs='tp') + 
                                s(TUNA, bs='tp') + s(CPUE, bs='tp') +  s(Slope, bs='tp') +
                                s(Depth, bs='tp') + s(Cont_Dist, bs='tp') + s(Seamt_Dist, bs='tp') +
                                s(Seamt_Height, bs='tp') + s(ChlA, bs='tp') + s(SSH, bs='tp') + s(SST_2, bs='tp') +
                                s(EDDY_DIST, bs='tp') + s(ONI, bs='tp') +
                                VESSEL_ID + postTRT + CYCL_TYPE,
                              data=sets_deep_sample, family=binomial, method = "REML", optimizer="perf")

summary(gam_mgcv_REML_tp)


#############################################################################
### GAMs in mgcv - REML and p splines - all terminate with step failure - regardless of xy, both families
#############################################################################
gam_mgcv_REML <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, bs='ps') + s(HAUL_BEGIN_LON, bs='ps') + 
                             s(YEAR, bs='ps') + s(MONTH, bs='ps') + 
                             s(NUM_HKS_SET, bs='ps') + s(SOAK, bs='ps') + 
                             s(TUNA, bs='ps') + s(CPUE, bs='ps') +  s(Slope, bs='ps') +
                             s(Depth, bs='ps') + s(Cont_Dist, bs='ps') + s(Seamt_Dist, bs='ps') +
                             s(Seamt_Height, bs='ps') + s(ChlA, bs='ps') + s(SSH, bs='ps') + s(SST_2, bs='ps') +
                             s(EDDY_DIST, bs='ps') + s(ONI, bs='ps') +
                             VESSEL_ID + postTRT + CYCL_TYPE,
                           data=sets_deep_sample, family=binomial, method = "REML")

# xy interact
gam_mgcv_REML_xy <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, k=50) + 
                                s(YEAR, bs='ps') + s(MONTH, bs='ps') + 
                                s(NUM_HKS_SET, bs='ps') + s(SOAK, bs='ps') + 
                                s(TUNA, bs='ps') + s(CPUE, bs='ps') +  s(Slope, bs='ps') +
                                s(Depth, bs='ps') + s(Cont_Dist, bs='ps') + s(Seamt_Dist, bs='ps') +
                                s(Seamt_Height, bs='ps') + s(ChlA, bs='ps') + s(SSH, bs='ps') + s(SST_2, bs='ps') +
                                s(EDDY_DIST, bs='ps') + s(ONI, bs='ps') +
                                VESSEL_ID + postTRT + CYCL_TYPE,
                              data=sets_deep_sample, family=binomial, method = "REML")


# no xy
gam_mgcv_REML_noxy <- mgcv::gam(MM_YN ~
                                  s(YEAR, bs='ps') + s(MONTH, bs='ps') + 
                                  s(NUM_HKS_SET, bs='ps') + s(SOAK, bs='ps') + 
                                  s(TUNA, bs='ps') + s(CPUE, bs='ps') +  s(Slope, bs='ps') +
                                  s(Depth, bs='ps') + s(Cont_Dist, bs='ps') + s(Seamt_Dist, bs='ps') +
                                  s(Seamt_Height, bs='ps') + s(ChlA, bs='ps') + s(SSH, bs='ps') + s(SST_2, bs='ps') +
                                  s(EDDY_DIST, bs='ps') + s(ONI, bs='ps') +
                                  VESSEL_ID + postTRT + CYCL_TYPE,
                                data=sets_deep_sample, family=binomial, method = "REML")

#############################################################################
### GAMs in mgcv - REML with cr - all terminate
#############################################################################
gam_mgcv_REML_cr <- mgcv::gam(MM_YN ~
                                s(YEAR, bs='cr') + s(MONTH, bs='cr') + 
                                s(NUM_HKS_SET, bs='cr') + s(SOAK, bs='cr') + 
                                s(TUNA, bs='cr') + s(CPUE, bs='cr') +  s(Slope, bs='cr') +
                                s(Depth, bs='cr') + s(Cont_Dist, bs='cr') + s(Seamt_Dist, bs='cr') +
                                s(Seamt_Height, bs='cr') + s(ChlA, bs='cr') + s(SSH, bs='cr') + s(SST_2, bs='cr') +
                                s(EDDY_DIST, bs='cr') + s(ONI, bs='cr') +
                                VESSEL_ID + postTRT + CYCL_TYPE,
                              data=sets_deep_sample, family=binomial, method = "REML")




#############################################################################
### mess around
#############################################################################


## interaction bt min length and hks per float - no evidence that this better
gam_mgcv_minlenhks <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, k=30) + s(MIN_LEN, HKS_PER_FLT, k=30) +
                                s(ChlA, by=EL_LA_NO, k =20) + s(SSH, by=EL_LA_NO, k =20) + s(SST_2, by=EL_LA_NO, k =20) +
                                s(YEAR, bs='ps', sp=0.6) + s(MONTH, bs='ps', sp=0.6) + 
                                s(NUM_HKS_SET, bs='ps', sp=0.6) + s(SOAK, bs='ps', sp=0.6) + 
                                s(TUNA, bs='ps', sp=0.6) + s(CPUE, bs='ps', sp=0.6) + 
                                s(Slope, bs='ps', sp=0.6) + s(Depth, bs='ps', sp=0.6) + 
                                s(Cont_Dist, bs='ps', sp=0.6) + s(Seamt_Dist, bs='ps', sp=0.6) + 
                                s(EDDY_DIST, by = CYCL_TYPE, k=10) +
                                VESSEL_ID + postTRT + CYCL_TYPE + EL_LA_NO,
                              data=sets_deep_train, family=binomial, gamma=1.4)


