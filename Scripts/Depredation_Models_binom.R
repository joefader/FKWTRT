#############################################################################
# Models
# Joseph Fader
# March, 2018

library(tidyverse)
library(mgcv)


## read in data, make sure var classes as needed

sets_alldata <- read_feather("Data/sets_alldata.feather")
sets_deep_all <- read_feather("Data/sets_deep_all.feather")
sets_shallow_all <- read_feather("Data/sets_shallow_all.feather")
summary(sets_deep_all)
summary(sets_shallow_all)
sapply(sets_alldata,class)
sapply(sets_deep_all,class)

#############################################################################
###  practice run with logistic regression
#############################################################################

fit=glm(MM_YN ~ HAUL_BEGIN_LAT + HAUL_BEGIN_LON + year + month + VESSEL_ID + NUM_HKS_SET + NUM_FLTS + MIN_LEN +
          SOAK + SET_LAG + BAIT_CODE_VAL + BLUEDYE + NUM_CAUGHT + TUNA + CPUE +
          Slope + Depth + Cont_Dist + Seamt_Dist + Seamt_Height + ChlA + SSH + EDDY_DIST, data=sets_deep_all, family=binomial)
summary(fit)


#fit_lag = glm(MM_YN ~ set.lag, data = D_sets.dplag, family = binomial)
#summary(fit_lag)

## predictions
glm.probs = predict(fit, type = "response")
glm.probs[1:10]
contrasts(MM_YN)

glm.pred =rep(0, 44980)
glm.pred[glm.probs >.1] = 1 # changes to Up any of vector that are >.5

table(glm.pred, MM_YN ) # confusion matrix to show classification errors
(40096+145) / 44980 # training error rate (1 minus this number)
mean(glm.pred==MM_YN) # same, fraction predicted correctly

## use subset of data to train, and remaining to test

D_sets.date$year <- format(D_sets.date$SET_BEGIN_DATE, "%Y")
train <- (year < 2011) #creates boolean vector in which observations before 2011 are TRUE
D_sets.2011 <- D_sets.date[!train,] #subsets data to those in which train is FALSE, ie in 2010
dim(D_sets.2011)
MM_YN.2011 = MM_YN [!train]

## now do log regression on training data (pre 2011), followed by test on 2011-2016 data
fit = glm(MM_YN ~ SET_BEGIN_LAT + SET_BEGIN_LON + NUM_HKS_SET + TARGET_sum + set.lag,
          data=D_sets.date, family=binomial, subset = train)

glm.probs <- predict(fit, D_sets.2011, type = "response")

## see how test did
glm.pred = rep(0, 22376)
glm.pred[glm.probs >.1] = 1
table(glm.pred, MM_YN.2011)
mean(glm.pred == MM_YN.2011)
mean(glm.pred != MM_YN.2011)  ## test error rate

###only got to here, not able to get mean/ error rates for subset data returns NA

# ## try removing least important variables, none were significant, but using 2 highest here
# glm.fit <- glm(Direction ~ Lag1 + Lag2,
#                data=Smarket, family = binomial, subset = train)
# glm.probs <- predict(glm.fit, Smarket.2005, type = "response")
# 
# ## this improves things slightly, if model predicts decline, right 50% of time,
# ## if it predicts an increase, right 56% of time
# glm.pred =rep("Down", 252)
# glm.pred[glm.probs >.5] = "Up"
# table(glm.pred, Direction.2005)
# mean(glm.pred==Direction.2005)
# mean(glm.pred!=Direction.2005)
# 
# ## what does model predict based on specific values?
# predict(glm.fit, newdata=data.frame(Lag1=c(1.2, 1.5),
#                                     Lag2 =c(1.1, -0.8)), type ="response")
# 


#############################################################################
### GAMs in gam package - restart R to switch between gam and mgcv
#############################################################################

gam_gam_cubspline = gam (MM_YN ~ s(HAUL_BEGIN_LAT,3) + s(HAUL_BEGIN_LON,3) + s(year,3) + s(month,3) + 
                    VESSEL_ID + s(NUM_HKS_SET,3) + s(NUM_FLTS,3) + s(MIN_LEN,3) + s(SOAK,3) + 
                    s(SET_LAG,3) + BAIT_CODE_VAL + BLUEDYE + s(NUM_CAUGHT,3) + s(TUNA,3) +
                    s(CPUE,3) + s(Slope,3) + s(Depth,3) + s(Cont_Dist,3) + s(Seamt_Dist,3) + 
                    s(Seamt_Height,3) + s(ChlA,3) + s(SSH,3) + s(EDDY_DIST,3),
                  data=sets_deep_all, family=binomial, subset = (SOAK < 1279))
summary(gam_gam_cubspline)

par(mfrow=c(2,2))
plot(gam_fit_dp, se = T, col= "green")


#############################################################################
### GAMs in mgcv - p spline
#############################################################################

# P-spline smoothers (with lambda=0.6) used for x's

# quasibinom- 7.4% dev, R2 - 0.03
# binom - 7.39%

gam_mgcv_pspine <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, k=100) + 
                               s(YEAR, bs='ps', sp=0.6) + s(MONTH, bs='ps', sp=0.6) + 
                               s(NUM_HKS_SET, bs='ps', sp=0.6) + s(SOAK, bs='ps', sp=0.6) + 
                               s(SET_LAG, bs='ps', sp=0.6) + s(NUM_CAUGHT, bs='ps', sp=0.6) + 
                               s(TUNA, bs='ps', sp=0.6) + s(CPUE, bs='ps', sp=0.6) +  s(Slope, bs='ps', sp=0.6) +
                               s(Depth, bs='ps', sp=0.6) + s(Cont_Dist, bs='ps', sp=0.6) + s(Seamt_Dist, bs='ps', sp=0.6) +
                               s(Seamt_Height, bs='ps', sp=0.6) + s(ChlA, bs='ps', sp=0.6) + s(SSH, bs='ps', sp=0.6) +
                               s(SST_2, bs='ps', sp=0.6) + s(Moon_Illum, bs='ps', sp=0.6) +
                               s(EDDY_DIST, bs='ps', sp=0.6) + s(SPEED, bs='ps', sp=0.6) + s(AMPLITUDE, bs='ps', sp=0.6) +
                               VESSEL_ID + BLUEDYE + postTRT + CYCL_TYPE,
                             data=sets_deep_all, family=quasibinomial)
                           
summary(gam_mgcv_pspine)
plot(gam_mgcv_pspine)
concurvity(gam_mgcv_pspine)

## check residuals

gam.check(gam_mgcv_dp)
rsd <- residuals(gam_mgcv_dp) # resids for every observation
qq.gam(gam_mgcv_dp,rep=100); plot(fitted(gam_mgcv_dp),rsd)
plot(dat$HAUL_BEGIN_LAT,rsd); plot(dat$HAUL_BEGIN_LON,rsd)


# plot the smooth predictor function for x1 with ggplot to get a nicer looking graph
p <- predict(gam_mgcv_dp, type="lpmatrix")
beta <- coef(m)[grepl("x1", names(coef(gam_mgcv_dp)))]
s <- p[,grepl("HAUL_BEGIN_LAT", colnames(p))] %*% beta
ggplot(data=cbind.data.frame(s, dat$x1), aes(x=dat$x1, y=s)) + geom_line()

#############################################################################
### GAMs in mgcv - p spline - leaving out lat/lon - quasibin

# P-spline smoothers (with lambda=0.6) used for x's

# no xy large drop in explained deviance, most of same predictors sign

gam_mgcv_pspine_noxy <- mgcv::gam(MM_YN ~
                               s(YEAR, bs='ps', sp=0.6) + s(MONTH, bs='ps', sp=0.6) + 
                               s(NUM_HKS_SET, bs='ps', sp=0.6) + s(SOAK, bs='ps', sp=0.6) + 
                               s(SET_LAG, bs='ps', sp=0.6) + s(NUM_CAUGHT, bs='ps', sp=0.6) + 
                               s(TUNA, bs='ps', sp=0.6) + s(CPUE, bs='ps', sp=0.6) +  s(Slope, bs='ps', sp=0.6) +
                               s(Depth, bs='ps', sp=0.6) + s(Cont_Dist, bs='ps', sp=0.6) + s(Seamt_Dist, bs='ps', sp=0.6) +
                               s(Seamt_Height, bs='ps', sp=0.6) + s(ChlA, bs='ps', sp=0.6) + s(SSH, bs='ps', sp=0.6) +
                               s(SST_2, bs='ps', sp=0.6) + s(Moon_Illum, bs='ps', sp=0.6) +
                               s(EDDY_DIST, bs='ps', sp=0.6) + s(SPEED, bs='ps', sp=0.6) + s(AMPLITUDE, bs='ps', sp=0.6) +
                               VESSEL_ID + BLUEDYE + postTRT + CYCL_TYPE,
                             data=sets_deep_all, family=quasibinomial)

summary(gam_mgcv_pspine_noxy)
plot(gam_mgcv_pspine_noxy)
concurvity(gam_mgcv_pspine)


#############################################################################
### GAMs in mgcv - p spline with REML and setting lambda - doesnt make sense, reml is for optimizing lambda
#############################################################################

# P-spline smoothers (with lambda=0.6) used for x's

gam_mgcv_pspine_reml <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, k=100) + 
                               s(YEAR, bs='ps', sp=0.6) + s(MONTH, bs='ps', sp=0.6) + 
                               s(NUM_HKS_SET, bs='ps', sp=0.6) + s(SOAK, bs='ps', sp=0.6) + 
                               s(SET_LAG, bs='ps', sp=0.6) + s(NUM_CAUGHT, bs='ps', sp=0.6) + 
                               s(TUNA, bs='ps', sp=0.6) + s(CPUE, bs='ps', sp=0.6) +  s(Slope, bs='ps', sp=0.6) +
                               s(Depth, bs='ps', sp=0.6) + s(Cont_Dist, bs='ps', sp=0.6) + s(Seamt_Dist, bs='ps', sp=0.6) +
                               s(Seamt_Height, bs='ps', sp=0.6) + s(ChlA, bs='ps', sp=0.6) + s(SSH, bs='ps', sp=0.6) +
                               s(EDDY_DIST, bs='ps', sp=0.6) + s(SPEED, bs='ps', sp=0.6) + s(AMPLITUDE, bs='ps', sp=0.6) +
                               VESSEL_ID + BLUEDYE + postTRT + CYCL_TYPE,
                             data=sets_deep_all, family=binomial, optimizer="perf", method="REML")

summary(gam_mgcv_pspine)
plot(gam_mgcv_pspine)
concurvity(gam_mgcv_pspine)


#############################################################################
### GAMs in mgcv - REML and p splines
#############################################################################

# select smoothing parameters with REML, using P-splines

# binomial - so far only REML to converge (outer only, not perf)
gam_mgcv_REML <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, bs='ps') + s(HAUL_BEGIN_LON, bs='ps') + 
                           s(YEAR, bs='ps') + s(MONTH, bs='ps') + 
                           s(NUM_HKS_SET, bs='ps') + s(SOAK, bs='ps') + 
                           s(SET_LAG, bs='ps') + s(NUM_CAUGHT, bs='ps') + 
                           s(TUNA, bs='ps') + s(CPUE, bs='ps') +  s(Slope, bs='ps') +
                           s(Depth, bs='ps') + s(Cont_Dist, bs='ps') + s(Seamt_Dist, bs='ps') +
                           s(Seamt_Height, bs='ps') + s(ChlA, bs='ps') + s(SSH, bs='ps') +
                           s(EDDY_DIST, bs='ps') + s(SPEED, bs='ps') + s(AMPLITUDE, bs='ps') +
                           VESSEL_ID + BLUEDYE + postTRT + CYCL_TYPE,
                         data=sets_deep_all, family=binomial, method = "REML", optimizer = "perf")

# quasibinomial
gam_mgcv_REML_smt <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, bs='ps') + s(HAUL_BEGIN_LON, bs='ps') + 
                             s(YEAR, bs='ps') + s(MONTH, bs='ps') + 
                             s(NUM_HKS_SET, bs='ps') + s(SOAK, bs='ps') + 
                             #s(SET_LAG, bs='ps') + 
                             s(NUM_CAUGHT, bs='ps') + 
                             s(TUNA, bs='ps') + s(CPUE, bs='ps') +  s(Slope, bs='ps') +
                             s(Depth, bs='ps') + s(Cont_Dist, bs='ps') + 
                             s(Seamt_Dist, Seamt_Height, k=50) + 
                             s(ChlA, bs='ps') + s(SSH, bs='ps') + 
                             s(SST_2, bs='ps') + s(Moon_Illum, bs='ps') +
                             s(EDDY_DIST, bs='ps') + s(SPEED, bs='ps') + s(AMPLITUDE, bs='ps') +
                             VESSEL_ID + BLUEDYE + postTRT + CYCL_TYPE,
                           data=sets_deep_all, family=quasibinomial, method = "REML")

summary(gam_mgcv_REML)

bam_mgcv_REML <- mgcv::bam(MM_YN ~ s(HAUL_BEGIN_LAT, bs='ps') + s(HAUL_BEGIN_LON, bs='ps') + 
                             s(YEAR, bs='ps') + s(MONTH, bs='ps') + 
                             s(NUM_HKS_SET, bs='ps') + s(SOAK, bs='ps') + 
                             s(SET_LAG, bs='ps') + s(NUM_CAUGHT, bs='ps') + 
                             s(TUNA, bs='ps') + s(CPUE, bs='ps') +  s(Slope, bs='ps') +
                             s(Depth, bs='ps') + s(Cont_Dist, bs='ps') + s(Seamt_Dist, bs='ps') +
                             s(Seamt_Height, bs='ps') + s(ChlA, bs='ps') + s(SSH, bs='ps') +
                             s(EDDY_DIST, bs='ps') + s(SPEED, bs='ps') + s(AMPLITUDE, bs='ps') +
                             VESSEL_ID + BLUEDYE + postTRT + CYCL_TYPE,
                           data=sets_deep_all, family=binomial, method = "REML")

summary(bam_mgcv_REML)
plot(gam_mgcv_REML)

concurvity(gam_mgcv_REML)

## check residuals

gam.check(gam_mgcv_dp)
rsd <- residuals(gam_mgcv_dp) # resids for every observation
qq.gam(gam_mgcv_dp,rep=100); plot(fitted(gam_mgcv_dp),rsd)
plot(dat$HAUL_BEGIN_LAT,rsd); plot(dat$HAUL_BEGIN_LON,rsd)


# plot the smooth predictor function for x1 with ggplot to get a nicer looking graph
p <- predict(gam_mgcv_dp, type="lpmatrix")
beta <- coef(m)[grepl("x1", names(coef(gam_mgcv_dp)))]
s <- p[,grepl("HAUL_BEGIN_LAT", colnames(p))] %*% beta
ggplot(data=cbind.data.frame(s, dat$x1), aes(x=dat$x1, y=s)) + geom_line()

#############################################################################
### GAMs in mgcv - REML and p splines with single lat/lon variable
#############################################################################
# select smoothing parameters with REML, using P-splines, seems to fail, better dev explained of the two REMLs
cat(paste(shQuote(colnames(sets_deep_all), type="cmd"), collapse=", "))

gam_mgcv_REML_xy <- mgcv::gam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, k = 50) + 
                                s(YEAR, bs='ps') + s(MONTH, bs='ps') + 
                                s(NUM_HKS_SET, bs='ps') + s(SOAK, bs='ps') + 
                                s(SET_LAG, bs='ps') + s(NUM_CAUGHT, bs='ps') + 
                                s(TUNA, bs='ps') + s(CPUE, bs='ps') +  s(Slope, bs='ps') +
                                s(Depth, bs='ps') + s(Cont_Dist, bs='ps') + s(Seamt_Dist, bs='ps') +
                                s(Seamt_Height, bs='ps') + s(ChlA, bs='ps') + s(SSH, bs='ps') +
                                s(EDDY_DIST, bs='ps') + s(SPEED, bs='ps') + s(AMPLITUDE, bs='ps') +
                                VESSEL_ID + BLUEDYE + postTRT + CYCL_TYPE,
                              data=sets_deep_all, family=binomial, method = "REML")

#, optimizer = "perf"
# some diagnostics
summary(gam_mgcv_REML_xy)
summary(gam_mgcv_REML_xy)$sp.criterion
summary(gam_mgcv_REML_xy)$s.table
AIC(gam_mgcv_REML_xy) # can look at multiple models at once
gam.check(gam_mgcv_REML_xy) # gives some diagnostics,eg rank, check of basis dimensions, residual plots

layout(matrix(1:2, nrow = 1))
plot(gam_mgcv_REML_xy, shade = TRUE)

# plot the smooth predictor function for x1 with ggplot to get a nicer looking graph
p <- predict(gam_mgcv_REML_xy, type="lpmatrix")
beta <- coef(m)[grepl("NUM_HKS_SET", names(coef(NUM_HKS_SET)))]
s <- p[,grepl("x1", colnames(p))] %*% beta
ggplot(data=cbind.data.frame(s, dat$x1), aes(x=dat$x1, y=s)) + geom_line()
concurvity(gam_mgcv_REML)

#############################################################################
### GAMs in mgcv - REML and p splines with single lat/lon variable
## quasi binomial - good for under, over, or unknown dispersion
#############################################################################
gam_mgcv_REML_xy <- mgcv::bam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, k = 100) + 
                                s(YEAR, bs='ps') + s(MONTH, bs='ps') + 
                                s(NUM_HKS_SET, bs='ps') + s(SOAK, bs='ps') + 
                                s(SET_LAG, bs='ps') + s(NUM_CAUGHT, bs='ps') + 
                                s(TUNA, bs='ps') + s(CPUE, bs='ps') +  s(Slope, bs='ps') +
                                s(Depth, bs='ps') + s(Cont_Dist, bs='ps') + s(Seamt_Dist, bs='ps') +
                                s(Seamt_Height, bs='ps') + s(ChlA, bs='ps') + s(SSH, bs='ps') +
                                s(EDDY_DIST, bs='ps') + s(SPEED, bs='ps') + s(AMPLITUDE, bs='ps') +
                                VESSEL_ID + BLUEDYE + postTRT + CYCL_TYPE,
                              data=sets_deep_all, family=quasibinomial(link = "logit"), method = "REML")

#, optimizer = "perf"
# some diagnostics
summary(gam_mgcv_REML_xy)
summary(gam_mgcv_REML_xy)$sp.criterion
summary(gam_mgcv_REML_xy)$s.table
AIC(gam_mgcv_REML_xy) # can look at multiple models at once
gam.check(gam_mgcv_REML_xy) # gives some diagnostics,eg rank, check of basis dimensions, residual plots

layout(matrix(1:2, nrow = 1))
plot(gam_mgcv_REML_xy, shade = TRUE)

# plot the smooth predictor function for x1 with ggplot to get a nicer looking graph
p <- predict(gam_mgcv_REML_xy, type="lpmatrix")
beta <- coef(m)[grepl("NUM_HKS_SET", names(coef(NUM_HKS_SET)))]
s <- p[,grepl("x1", colnames(p))] %*% beta
ggplot(data=cbind.data.frame(s, dat$x1), aes(x=dat$x1, y=s)) + geom_line()
concurvity(gam_mgcv_REML)

#############################################################################
### GAMs in mgcv - REML and cr splines with single lat/lon variable
## bam better for big data sets, 10s of thousands
#############################################################################
bam_mgcv_REML_xy <- mgcv::bam(MM_YN ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, k = 50) + 
                                s(YEAR, bs='ps') + s(MONTH, bs='ps') + 
                                s(NUM_HKS_SET, bs='ps') + s(SOAK, bs='ps') + 
                                s(SET_LAG, bs='ps') + s(NUM_CAUGHT, bs='ps') + 
                                s(TUNA, bs='ps') + s(CPUE, bs='ps') +  s(Slope, bs='ps') +
                                s(Depth, bs='ps') + s(Cont_Dist, bs='ps') + s(Seamt_Dist, bs='ps') +
                                s(Seamt_Height, bs='ps') + s(ChlA, bs='ps') + s(SSH, bs='ps') +
                                s(EDDY_DIST, bs='ps') + s(SPEED, bs='ps') + s(AMPLITUDE, bs='ps') +
                                VESSEL_ID + BLUEDYE + postTRT + CYCL_TYPE,
                              data=sets_deep_all, family=binomial, method = "REML")

#############################################################################
### GAMs in mgcv
## cr doesnt seem to work well

gam_mgcv_REML_xycr <- mgcv::gam(MM_YN ~ 
                                s(YEAR, bs='cr') + s(MONTH, bs='cr') + 
                                s(NUM_HKS_SET, bs='cr') + s(SOAK, bs='cr') + 
                                s(SET_LAG, bs='cr') + s(NUM_CAUGHT, bs='cr') + 
                                s(TUNA, bs='cr') + s(CPUE, bs='cr') +  s(Slope, bs='cr') +
                                s(Depth, bs='cr') + s(Cont_Dist, bs='cr') + s(Seamt_Dist, bs='cr') +
                                s(Seamt_Height, bs='cr') + s(ChlA, bs='cr') + s(SSH, bs='cr') +
                                s(EDDY_DIST, bs='cr') + s(SPEED, bs='cr') + s(AMPLITUDE, bs='cr') +
                                VESSEL_ID + BLUEDYE + postTRT + CYCL_TYPE,
                              data=sets_deep_all, family=binomial, method = "REML")
summary(gam_mgcv_REML_xycr)


#############################################################################
### GAMs in mgcv - REML and ps splines with no xy
bam_mgcv_REML_noxy <- mgcv::bam(MM_YN ~ 
                                s(YEAR, bs='ps') + s(MONTH, bs='ps') + 
                                s(NUM_HKS_SET, bs='ps') + s(SOAK, bs='ps') + 
                                s(SET_LAG, bs='ps') + s(NUM_CAUGHT, bs='ps') + 
                                s(TUNA, bs='ps') + s(CPUE, bs='ps') +  s(Slope, bs='ps') +
                                s(Depth, bs='ps') + s(Cont_Dist, bs='ps') + s(Seamt_Dist, bs='ps') +
                                s(Seamt_Height, bs='ps') + s(ChlA, bs='ps') + s(SSH, bs='ps') +
                                s(EDDY_DIST, bs='ps') + s(SPEED, bs='ps') + s(AMPLITUDE, bs='ps') +
                                VESSEL_ID + BLUEDYE + postTRT + CYCL_TYPE,
                              data=sets_deep_all, family=binomial, method = "REML")

summary(bam_mgcv_REML_noxy)

#############################################################################
### GAMs in mgcv - REML and ps splines with no xy
gam_mgcv_REML_eddyint <- mgcv::gam(MM_YN ~ 
                                  s(YEAR, bs='ps') + s(MONTH, bs='ps') + 
                                  s(NUM_HKS_SET, bs='ps') + s(SOAK, bs='ps') + 
                                  s(SET_LAG, bs='ps') + s(NUM_CAUGHT, bs='ps') + 
                                  s(TUNA, bs='ps') + s(CPUE, bs='ps') +  s(Slope, bs='ps') +
                                  s(Depth, bs='ps') + s(Cont_Dist, bs='ps') + s(Seamt_Dist, bs='ps') +
                                  s(Seamt_Height, bs='ps') + s(ChlA, bs='ps') + s(SSH, bs='ps') +
                                  s(EDDY_DIST, bs='ps') + s(SPEED, bs='ps') + s(AMPLITUDE, bs='ps') +
                                  VESSEL_ID + BLUEDYE + postTRT + CYCL_TYPE,
                                data=sets_deep_all, family=binomial, method = "REML")

summary(bam_mgcv_REML_noxy)