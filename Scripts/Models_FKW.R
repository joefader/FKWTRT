
#############################################################################
# models for FKW bycatch
# Joseph Fader
# March, 2018

library(tidyverse)
library(feather)
library(grid )

sets_deep_FKW <- sets_deep_all %>%
  filter(MM_YN ==  1 | FKW == 1) %>%
  filter(HOOKS != 'OTHER') %>%
  select(TRIP_ID, VESSEL_ID, SET_NUM, HAUL_BEGIN_LAT, HAUL_BEGIN_LON, MONTH, YEAR, postTRT,
         SOAK, HOOKS, NUM_FLTS, NUM_HKS_SET, HKS_PER_FLT, MIN_LEN, BLUEDYE, BET, CPUE,
         MM_YN, MM_sum, FKW, FKW_sum, Slope, Depth, Cont_Dist, Seamt_Dist, EDDY_DIST,
         CYCL_TYPE, SST_2, SST_RANGE, ChlA, SSH, EL_LA_NO) #%>%
 drop_na()

sets_shallow_FKW <- sets_shallow_all %>%
  filter(MM_YN == 1 | FKW == 1)

cat(paste(shQuote(colnames(sets_deep_FKW), type="cmd"), collapse=", "))
#############################################################################
## gear/catch only - tuna/J combined -- hooks and cpue
#############################################################################

sets_deep_FKW <- sets_deep_all %>%
  filter(MM_YN ==  1 | FKW == 1) %>%
  filter(HOOKS != 'OTHER') %>% # & HOOKS != 'JHOOK') %>%
  mutate(HOOKS = ifelse((HOOKS == 'TUNA' | HOOKS == 'JHOOK'), 'TUNAJ', 'CIRCLE')) %>%
  select(TRIP_ID, VESSEL_ID, SET_NUM, HAUL_BEGIN_LAT, HAUL_BEGIN_LON, MONTH, YEAR, postTRT,
         SOAK, HOOKS,NUM_HKS_SET, NUM_FLTS, HKS_PER_FLT, MIN_LEN, BLUEDYE, BET, YFT, TUNA, MAHI, BILLFISH, SWO,
         CPUE, CPUE_FLT, MM_YN, MM_sum, FKW, FKW_sum) %>%
  drop_na()


fkw_hks1 <- mgcv::gam(FKW ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                      s(YEAR, bs='cs') + s(MONTH, bs='cc') + 
                      s(NUM_HKS_SET, bs='cs') + s(SOAK, bs='cs') + 
                      s(HKS_PER_FLT, bs='cs') + s(MIN_LEN, bs='cs') +
                      s(BET, bs='cs') + s(YFT, bs='cs') + s(MAHI, bs='cs') + s(BILLFISH, bs='cs') +
                      s(CPUE, bs='cs') + s(MM_sum, bs='cs') + 
                      HOOKS,
                    data=sets_deep_FKW, family=binomial, gamma=1.4)

summary(fkw_hks1)

## drop year, month, num hks, soak, min len, yft, mahi
fkw_hks2 <- mgcv::gam(FKW ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                      s(HKS_PER_FLT, bs='cs') +
                      s(BET, bs='cs') + s(BILLFISH, bs='cs') + 
                      s(CPUE, bs='cs') +
                      s(MM_sum, bs='cs') + 
                      HOOKS,
                    data=sets_deep_FKW, family=binomial, gamma=1.4)

summary(fkw_hks2)
AIC(fkw_hks1, fkw_hks2)

## drop hooks
## stop here, dropping further (bet, bills), just raises aic
fkw_hks3 <- mgcv::gam(FKW ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                        s(HKS_PER_FLT, bs='cs') +
                        s(BET, bs='cs') + s(BILLFISH, bs='cs') + 
                        s(CPUE, bs='cs') +
                        s(MM_sum, bs='cs'),
                      data=sets_deep_FKW, family=binomial, gamma=1.4)

summary(fkw_hks3)
AIC(fkw_hks1, fkw_hks2,fkw_hks3)


par(mfrow=c(2,2))
plot(fkw_hks3, se=T, select=NULL, scale=0,
     jit=F, all.terms=T, shade=FALSE, ask=F, rug=F )

## term plots
termplot(fkw_tuna2, se=T, ask=F, ylim='free', rug=F)
#termplot(fkw_tuna2, se=T, ask=F, ylim='free', rug=F, xlabs=c("CPUE (catch per float)"))

#############################################################################
## all variables - tunaJ -- hooks and cpue -- using this in presentation, same story as gear only
#############################################################################
sets_deep_FKW <- sets_deep_all %>%
  filter(MM_YN ==  1 | FKW == 1) %>%
  filter(HOOKS != 'OTHER') %>%
  #filter(HOOKS != 'OTHER' & HOOKS != 'JHOOK') %>%
  mutate(HOOKS = ifelse((HOOKS == 'TUNA' | HOOKS == 'JHOOK'), 'TUNAJ', 'CIRCLE')) %>%
  select(TRIP_ID, VESSEL_ID, SET_NUM, HAUL_BEGIN_LAT, HAUL_BEGIN_LON, MONTH, YEAR, postTRT,
         SOAK, HOOKS, NUM_FLTS, NUM_HKS_SET, HKS_PER_FLT, MIN_LEN, BLUEDYE, BET, YFT, TUNA, 
         MAHI, BILLFISH, SWO, CPUE, CPUE_FLT, MM_YN, MM_sum, FKW, FKW_sum, Slope, Depth, Cont_Dist,
         Seamt_Dist, EDDY_DIST, CYCL_TYPE, SST_2, SST_RANGE, ChlA, SSH, EL_LA_NO) %>%
  drop_na()

# month as cc (cyclic), 
fkw_hksall1 <- mgcv::gam(FKW ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                       s(ChlA, by=EL_LA_NO, bs='cs', k = 10) + s(SSH, by=EL_LA_NO, bs='cs', k = 10) +
                       s(SST_2, by=EL_LA_NO, bs='cs', k = 10) +
                       s(YEAR, bs='cs') + s(MONTH, bs='cc') + 
                       s(NUM_HKS_SET, bs='cs') + s(SOAK, bs='cs') + 
                       s(HKS_PER_FLT, bs='cs') + s(MIN_LEN, bs='cs') +
                       s(BET, bs='cs') + s(YFT, bs='cs') + s(MAHI, bs='cs') + 
                       s(BILLFISH, bs='cs') + s(CPUE, bs='cs') + s(MM_sum, bs='cs') + 
                       s(Slope, bs='cs') + s(Depth, bs='cs') + 
                       s(Cont_Dist, bs='cs') + s(Seamt_Dist, bs='cs') + s(SST_RANGE, bs='cs') +
                       EL_LA_NO + HOOKS,
                     data=sets_deep_FKW, family=binomial, gamma=1.4)

summary(fkw_hksall1)

# drop chla, ssh, sst, month, year, num hks, soak, minlen, yft, billfish, depth, seamt, sstrange, cont
fkw_hksall2 <- mgcv::gam(FKW ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                       s(HKS_PER_FLT, bs='cs') +
                       s(BET, bs='cs') + s(MAHI, bs='cs') + 
                       s(CPUE, bs='cs') + s(MM_sum, bs='cs') +
                       s(Slope, bs='cs') +
                       EL_LA_NO + HOOKS,
                     data=sets_deep_FKW, family=binomial, gamma=1.4)
summary(fkw_hksall2)
AIC(fkw_hksall1,fkw_hksall2)

## use this - cpue NOT linear
# drop elnino, hooks -- lowest aic other than original model
fkw_hksall3 <- mgcv::gam(FKW ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                           s(HKS_PER_FLT, bs='cs') +
                           s(BET, bs='cs') + s(MAHI, bs='cs') + 
                           s(CPUE, bs='cs') + 
                           s(MM_sum, bs='cs') ,
                         data=sets_deep_FKW, family=binomial, gamma=1.4)
summary(fkw_hksall3)
AIC(fkw_hksall1,fkw_hksall2,fkw_hksall3)


plot(fkw_hksall3, se=T, select=NULL, scale=0,
     jit=F, xlim=NULL, all.terms=T, shade=FALSE, ask=F)
plot.gam(model_name, page=5)
plot(fkw_ts_reml9,shade=F,seWithMean=TRUE,scale=0) # better for linear?

## term plots
termplot(fkw_cs4, se=T, ask=F, ylim='free', rug=T)
#############################################################################
## gear/catch -- eliminating fkws with no catch damage
## only cpue flt is significant (linear)
#############################################################################

sets_deep_FKW_MM <- sets_deep_all %>%
  filter(MM_YN ==  1) %>%
  filter(HOOKS != 'OTHER') %>% # & HOOKS != 'JHOOK') %>%
  mutate(HOOKS = ifelse((HOOKS == 'TUNA' | HOOKS == 'JHOOK'), 'TUNAJ', 'CIRCLE')) %>%
  select(TRIP_ID, VESSEL_ID, SET_NUM, HAUL_BEGIN_LAT, HAUL_BEGIN_LON, MONTH, YEAR, postTRT,
         SOAK, HOOKS, NUM_FLTS, HKS_PER_FLT, MIN_LEN, BLUEDYE, BET, YFT, TUNA, MAHI, BILLFISH, SWO,
         CPUE_FLT, MM_YN, MM_sum, FKW, FKW_sum) %>%
  drop_na()


fkw_MM1 <- mgcv::gam(FKW ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                         s(YEAR, bs='cs') + s(MONTH, bs='cc') + 
                         s(NUM_FLTS, bs='cs') + s(SOAK, bs='cs') + 
                         s(HKS_PER_FLT, bs='cs') + s(MIN_LEN, bs='cs') +
                         s(BET, bs='cs') + s(YFT, bs='cs') + s(MAHI, bs='cs') + s(BILLFISH, bs='cs') +
                         s(CPUE_FLT, bs='cs') + s(MM_sum, bs='cs') + 
                         HOOKS,
                       data=sets_deep_FKW_MM, family=binomial, gamma=1.4)

summary(fkw_MM1)

## drop year, month, num floats, soak, min len, bet, yft, mahi, billfish, mmsum

fkw_MM2 <- mgcv::gam(FKW ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                       s(HKS_PER_FLT, bs='cs') + #s(CPUE_FLT, bs='cs')
                       CPUE_FLT,
                     data=sets_deep_FKW_MM, family=binomial, gamma=1.4)

summary(fkw_MM2)
AIC(fkw_MM1,fkw_MM2)

plot(fkw_MM2, se=T, select=7, scale=0,
     jit=F, all.terms=T, shade=FALSE, ask=F, rug=F )
## term plots
termplot(fkw_MM2, se=T, ask=F, ylim='free', rug=F)

#############################################################################
## gear/catch only -- ps/cs -- tuna hooks only -- bet and yft, no nontarget or swo
# model 4 best - cpue linear, 19.7% -- cpue linear, mmsum
#############################################################################

sets_deep_FKW <- sets_deep_all %>%
  filter(MM_YN ==  1 | FKW == 1) %>%
  filter(HOOKS != 'OTHER' & HOOKS != 'JHOOK') %>%
  #mutate(HOOKS = ifelse((HOOKS == 'TUNA' | HOOKS == 'JHOOK'), 'TUNAJ', 'CIRCLE')) %>%
  select(TRIP_ID, VESSEL_ID, SET_NUM, HAUL_BEGIN_LAT, HAUL_BEGIN_LON, MONTH, YEAR, postTRT,
         SOAK, HOOKS, NUM_FLTS, HKS_PER_FLT, MIN_LEN, BLUEDYE, BET, YFT, TUNA, MAHI, BILLFISH, SWO,
         CPUE_FLT, MM_YN, MM_sum, FKW, FKW_sum) %>%
  drop_na()


fkw_tuna1 <- mgcv::gam(FKW ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                         s(YEAR, bs='cs') + s(MONTH, bs='cc') + 
                         s(NUM_FLTS, bs='cs') + s(SOAK, bs='cs') + 
                         s(HKS_PER_FLT, bs='cs') + s(MIN_LEN, bs='cs') +
                         s(BET, bs='cs') + s(YFT, bs='cs') + s(MAHI, bs='cs') + s(BILLFISH, bs='cs') +
                         s(CPUE_FLT, bs='cs') + s(MM_sum, bs='cs') + 
                         HOOKS,
                       data=sets_deep_FKW, family=binomial, gamma=1.4)

summary(fkw_tuna1)

## drop year, month, num floats, soak, min len, yft, mahi,
fkw_tuna2 <- mgcv::gam(FKW ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                         s(HKS_PER_FLT, bs='cs') +
                         s(BET, bs='cs') + s(BILLFISH, bs='cs') + 
                         s(CPUE_FLT, bs='cs') +
                         s(MM_sum, bs='cs') + 
                         HOOKS,
                       data=sets_deep_FKW, family=binomial, gamma=1.4)

summary(fkw_tuna2)
AIC(fkw_tuna1,fkw_tuna2)

## drop hooks
fkw_tuna3 <- mgcv::gam(FKW ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                         s(HKS_PER_FLT, bs='cs') +
                         s(BET, bs='cs') + s(BILLFISH, bs='cs') + 
                         s(CPUE_FLT, bs='cs') +
                         s(MM_sum, bs='cs'),
                       data=sets_deep_FKW, family=binomial, gamma=1.4)

summary(fkw_tuna3)
AIC(fkw_tuna1,fkw_tuna2,fkw_tuna3)

## drop hooks
fkw_tuna4 <- mgcv::gam(FKW ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                         s(HKS_PER_FLT, bs='cs') +
                         s(BET, bs='cs') +
                         CPUE_FLT + #s(CPUE_FLT, bs='cs') +
                         s(MM_sum, bs='cs'),
                       data=sets_deep_FKW, family=binomial, gamma=1.4)

summary(fkw_tuna4)
AIC(fkw_tuna1,fkw_tuna2,fkw_tuna3,fkw_tuna4)

plot(fkw_tuna2, se=T, select=7, scale=0,
     jit=F, all.terms=T, shade=FALSE, ask=F, rug=F )
## term plots
termplot(fkw_tuna2, se=T, ask=F, ylim='free', rug=F)
#termplot(fkw_tuna2, se=T, ask=F, ylim='free', rug=F, xlabs=c("CPUE (catch per float)"))

#############################################################################
## gear/catch only - tuna/J combined -- exact same answer as above, cpue linear
## 20.2%
#############################################################################

sets_deep_FKW <- sets_deep_all %>%
  filter(MM_YN ==  1 | FKW == 1) %>%
  filter(HOOKS != 'OTHER') %>% # & HOOKS != 'JHOOK') %>%
  mutate(HOOKS = ifelse((HOOKS == 'TUNA' | HOOKS == 'JHOOK'), 'TUNAJ', 'CIRCLE')) %>%
  select(TRIP_ID, VESSEL_ID, SET_NUM, HAUL_BEGIN_LAT, HAUL_BEGIN_LON, MONTH, YEAR, postTRT,
         SOAK, HOOKS, NUM_FLTS, HKS_PER_FLT, MIN_LEN, BLUEDYE, BET, YFT, TUNA, MAHI, BILLFISH, SWO,
         CPUE_FLT, MM_YN, MM_sum, FKW, FKW_sum) %>%
  drop_na()


fkw_J1 <- mgcv::gam(FKW ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                         s(YEAR, bs='cs') + s(MONTH, bs='cc') + 
                         s(NUM_FLTS, bs='cs') + s(SOAK, bs='cs') + 
                         s(HKS_PER_FLT, bs='cs') + s(MIN_LEN, bs='cs') +
                         s(BET, bs='cs') + s(YFT, bs='cs') + s(MAHI, bs='cs') + s(BILLFISH, bs='cs') +
                         s(CPUE_FLT, bs='cs') + s(MM_sum, bs='cs') + 
                         HOOKS,
                       data=sets_deep_FKW, family=binomial, gamma=1.4)

summary(fkw_J1)

## drop year, month, num floats, soak, min len, yft, mahi,
fkw_J2 <- mgcv::gam(FKW ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                         s(HKS_PER_FLT, bs='cs') +
                         s(BET, bs='cs') + s(BILLFISH, bs='cs') + 
                         s(CPUE_FLT, bs='cs') +
                         s(MM_sum, bs='cs') + 
                         HOOKS,
                       data=sets_deep_FKW, family=binomial, gamma=1.4)

summary(fkw_J2)
AIC(fkw_J1, fkw_J2)

## drop billfish
fkw_J3 <- mgcv::gam(FKW ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                         s(HKS_PER_FLT, bs='cs') +
                         s(BET, bs='cs') +
                         s(CPUE_FLT, bs='cs') +
                         s(MM_sum, bs='cs') + HOOKS,
                       data=sets_deep_FKW, family=binomial, gamma=1.4)

summary(fkw_J3)
AIC(fkw_J1,fkw_J2,fkw_J3)

## drop hooks
fkw_J4 <- mgcv::gam(FKW ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                         s(HKS_PER_FLT, bs='cs') +
                         s(BET, bs='cs') +
                         CPUE_FLT +#s(CPUE_FLT, bs='cs') +
                         s(MM_sum, bs='cs'),
                       data=sets_deep_FKW, family=binomial, gamma=1.4)

summary(fkw_J4)
AIC(fkw_J1,fkw_J2,fkw_J3,fkw_J4)

par(mfrow=c(2,2))
plot(fkw_J4, se=T, select=5, scale=0,
     jit=F, all.terms=T, shade=FALSE, ask=F, rug=F )

## term plots
termplot(fkw_tuna2, se=T, ask=F, ylim='free', rug=F)
#termplot(fkw_tuna2, se=T, ask=F, ylim='free', rug=F, xlabs=c("CPUE (catch per float)"))

#############################################################################
## gear/catch only  - ts - go with cs for the gear only version (above) ##
# same outcome as with cs, bet slightly less sign, but patterns all the same
# also tried combined tuna and j, story also the same, stick with tuna v circle cs
############################################################################
## trying ts
fkw_tunats1 <- mgcv::gam(FKW ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                         s(YEAR, bs='ts') + s(MONTH, bs='cc') + 
                         s(NUM_FLTS, bs='ts') + s(SOAK, bs='ts') + 
                         s(HKS_PER_FLT, bs='ts') + s(MIN_LEN, bs='ts') +
                         s(BET, bs='ts') + s(YFT, bs='ts') + s(MAHI, bs='ts') + s(BILLFISH, bs='ts') +
                         s(CPUE_FLT, bs='ts') + s(MM_sum, bs='ts') + 
                         HOOKS,
                       data=sets_deep_FKW, family=binomial, gamma=1.4)
summary(fkw_tunats1)

## drop year, month, numflts, soak, minlen, yft, mahi, billfish
fkw_tunats2 <- mgcv::gam(FKW ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                           s(HKS_PER_FLT, bs='ts') + 
                           s(BET, bs='ts') +
                           CPUE_FLT +#s(CPUE_FLT, bs='ts') + 
                           s(MM_sum, bs='ts'),
                         data=sets_deep_FKW, family=binomial, gamma=1.4)
summary(fkw_tunats2)
AIC(fkw_tunats1,fkw_tunats2)
plot(fkw_tunats2, se=T, select=NULL, scale=0,
     jit=F, xlim=NULL, all.terms=T, shade=FALSE, ask=F)


#############################################################################
## all variables - ts - no bueno, step failures for both hook comparisons
#############################################################################
sets_deep_FKW <- sets_deep_all %>%
  filter(MM_YN ==  1 | FKW == 1) %>%
  filter(HOOKS != 'OTHER') %>%
  #filter(HOOKS != 'OTHER' & HOOKS != 'JHOOK') %>%
  mutate(HOOKS = ifelse((HOOKS == 'TUNA' | HOOKS == 'JHOOK'), 'TUNAJ', 'CIRCLE')) %>%
  select(TRIP_ID, VESSEL_ID, SET_NUM, HAUL_BEGIN_LAT, HAUL_BEGIN_LON, MONTH, YEAR, postTRT,
         SOAK, HOOKS, NUM_FLTS, NUM_HKS_SET, HKS_PER_FLT, MIN_LEN, BLUEDYE, BET, YFT, TUNA, 
         MAHI, BILLFISH, SWO, CPUE_FLT, MM_YN, MM_sum, FKW, FKW_sum, Slope, Depth, Cont_Dist,
         Seamt_Dist, EDDY_DIST, CYCL_TYPE, SST_2, SST_RANGE, ChlA, SSH, EL_LA_NO) %>%
  drop_na()

# ts ---month as cc (cyclic)
fkw_ts1 <- mgcv::gam(FKW ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                       s(ChlA, by=EL_LA_NO, bs='ts', k = 10) + s(SSH, by=EL_LA_NO, bs='ts', k = 10) +
                       s(SST_2, by=EL_LA_NO, bs='ts', k = 10) +
                       s(YEAR, bs='ts') + s(MONTH, bs='cc') + 
                       s(NUM_FLTS, bs='ts') + s(SOAK, bs='ts') + 
                       s(HKS_PER_FLT, bs='ts') + s(MIN_LEN, bs='ts') +
                       s(BET, bs='ts') + s(YFT, bs='ts') + s(MAHI, bs='ts') + 
                       s(BILLFISH, bs='ts') + s(CPUE_FLT, bs='ts') + s(MM_sum, bs='ts') + 
                       s(Slope, bs='ts') + s(Depth, bs='ts') + 
                       s(Cont_Dist, bs='ts') + s(Seamt_Dist, bs='ts') + s(SST_RANGE, bs='ts') +
                       EL_LA_NO + HOOKS,
                     data=sets_deep_FKW, family=binomial, gamma=1.4)

summary(fkw_ts1)

# drop chla, ssh, year, month, num flts, soak, minlen, yft, mahi, bill, slope, depth, cont, seamt
fkw_ts2 <- mgcv::gam(FKW ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                       s(SST_2, by=EL_LA_NO, bs='ts', k = 10) +
                       s(HKS_PER_FLT, bs='ts') +
                       s(BET, bs='ts') +
                       s(CPUE_FLT, bs='ts') + s(MM_sum, bs='ts') + 
                       postTRT + EL_LA_NO + HOOKS,
                     data=sets_deep_FKW, family=binomial, gamma=1.4)

summary(fkw_ts2)
AIC(fkw_ts1,fkw_ts2)
#############################################################################
## all variables - ps/cs - tuna v circle
# stop at model 4- cpue linear, mm sum
#############################################################################
sets_deep_FKW <- sets_deep_all %>%
filter(MM_YN ==  1 | FKW == 1) %>%
  filter(HOOKS != 'OTHER') %>%
  #filter(HOOKS != 'OTHER' & HOOKS != 'JHOOK') %>%
  mutate(HOOKS = ifelse((HOOKS == 'TUNA' | HOOKS == 'JHOOK'), 'TUNAJ', 'CIRCLE')) %>%
  select(TRIP_ID, VESSEL_ID, SET_NUM, HAUL_BEGIN_LAT, HAUL_BEGIN_LON, MONTH, YEAR, postTRT,
         SOAK, HOOKS, NUM_FLTS, NUM_HKS_SET, HKS_PER_FLT, MIN_LEN, BLUEDYE, BET, YFT, TUNA, 
         MAHI, BILLFISH, SWO, CPUE_FLT, MM_YN, MM_sum, FKW, FKW_sum, Slope, Depth, Cont_Dist,
         Seamt_Dist, EDDY_DIST, CYCL_TYPE, SST_2, SST_RANGE, ChlA, SSH, EL_LA_NO) %>%
  drop_na()

# month as cc (cyclic), 
fkw_cs1 <- mgcv::gam(FKW ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                       s(ChlA, by=EL_LA_NO, bs='cs', k = 10) + s(SSH, by=EL_LA_NO, bs='cs', k = 10) +
                       s(SST_2, by=EL_LA_NO, bs='cs', k = 10) +
                       s(YEAR, bs='cs') + s(MONTH, bs='cc') + 
                       s(NUM_FLTS, bs='cs') + s(SOAK, bs='cs') + 
                       s(HKS_PER_FLT, bs='cs') + s(MIN_LEN, bs='cs') +
                       s(BET, bs='cs') + s(YFT, bs='cs') + s(MAHI, bs='cs') + 
                       s(BILLFISH, bs='cs') + s(CPUE_FLT, bs='cs') + s(MM_sum, bs='cs') + 
                       s(Slope, bs='cs') + s(Depth, bs='cs') + 
                       s(Cont_Dist, bs='cs') + s(Seamt_Dist, bs='cs') + s(SST_RANGE, bs='cs') +
                       EL_LA_NO + HOOKS,
                     data=sets_deep_FKW, family=binomial, gamma=1.4)

summary(fkw_cs1)

# drop chla, ssh, month, year, num flts, soak, minlen, yft, billfish, slope, depth, seamt, sstrange, cont
fkw_cs2 <- mgcv::gam(FKW ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                       s(SST_2, by=EL_LA_NO, bs='cs', k = 10) +
                       s(HKS_PER_FLT, bs='cs') +
                       s(BET, bs='cs') + s(MAHI, bs='cs') + 
                       s(CPUE_FLT, bs='cs') + s(MM_sum, bs='cs') + 
                       EL_LA_NO + HOOKS,
                     data=sets_deep_FKW, family=binomial, gamma=1.4)
summary(fkw_cs2)
AIC(fkw_cs1,fkw_cs2)
# drop mahi
fkw_cs3 <- mgcv::gam(FKW ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                       s(SST_2, by=EL_LA_NO, bs='cs', k = 10) +
                       s(HKS_PER_FLT, bs='cs') +
                       s(BET, bs='cs') +
                       CPUE_FLT +#s(CPUE_FLT, bs='cs') +
                       s(MM_sum, bs='cs') + 
                       EL_LA_NO,
                     data=sets_deep_FKW, family=binomial, gamma=1.4)
summary(fkw_cs3)
AIC(fkw_cs1,fkw_cs2,fkw_cs3)

# several non-sign terms but AIC jumps if take anything else out
fkw_cs4 <- mgcv::gam(FKW ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                       s(SST_2, by=EL_LA_NO, bs='cs', k = 10) +
                       s(HKS_PER_FLT, bs='cs') +
                       s(BET, bs='cs') + s(MAHI, bs='cs') + 
                       CPUE_FLT +#s(CPUE_FLT, bs='cs') + 
                       s(MM_sum, bs='cs') + 
                       postTRT + EL_LA_NO + HOOKS,
                     data=sets_deep_FKW, family=binomial, gamma=1.4)

summary(fkw_cs4)
AIC(fkw_cs1,fkw_cs2,fkw_cs3,fkw_cs4)

plot(fkw_cs4, se=T, select=NULL, scale=0,
     jit=F, xlim=NULL, all.terms=T, shade=FALSE, ask=F)
plot.gam(model_name, page=5)
plot(fkw_ts_reml9,shade=F,seWithMean=TRUE,scale=0) # better for linear?

## term plots
termplot(fkw_cs4, se=T, ask=F, ylim='free', rug=T)
#############################################################################
## reml
#############################################################################

# basically tells same story as ubre - tuna and mmsum most important, cpue linear but not highly sign
fkw_ts_reml1 <- mgcv::gam(FKW ~ s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k= 20) + 
                           s(ChlA, by=EL_LA_NO, bs='cs', k = 10) + 
                           s(SST_2, by=EL_LA_NO, bs='cs', k = 10) +
                           s(HKS_PER_FLT, bs='cs') + s(MIN_LEN, bs='cs') +
                           s(TUNA, bs='cs') + s(MAHI, bs='cs') +
                           s(CPUE_FLT, bs='cs') + 
                           s(MM_sum, bs='cs') + 
                           postTRT + EL_LA_NO,
                         data=sets_deep_FKW, family=binomial, method='REML')
  
  
summary(fkw_ts_reml1)
  

plot(fkw_ts_reml9, se=T, select=NULL, scale=0,
     jit=F, xlim=NULL, all.terms=FALSE, shade=FALSE, ask=F)
plot.gam(model_name, page=5)
plot(fkw_ts_reml9,shade=F,seWithMean=TRUE,scale=0) # better for linear?

## term plots
termplot(fkw_ts_reml8, se=T, ask=F, ylim='free', rug=T)
