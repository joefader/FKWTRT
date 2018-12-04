
library(tidyverse)
library(gjam)

browseVignettes("gjam")

##vignette example
f <- gjamSimData(n = 500, S = 11, Q = 4, typeNames = 'CA')
summary(f)
f$formula
typeof(f$typeNames)
f$ydata
f$xdata
f$trueValues

par(bty = 'n', mfrow = c(1,2), family='')
h <- hist(c(-1,f$y),nclass = 50,plot = F)
plot(h$counts,h$mids,type = 's')
plot(f$w,f$y,cex = .2)

# need model, y data, x data, and a modelList specifying number of 
# Gibbs steps ng, the burnin, and typeNames.

ml  <- list(ng = 1000, burnin = 100, typeNames = f$typeNames)
out <- gjam(~x2 + x3 + x4, f$xdata, f$ydata, modelList = ml)
summary(out)
print(out)
summary(out$inputs$x)
summary(out$inputs$y)
summary(out$chains)
summary(out$inputs)


### set up my data

# set data with counts for species and species with depredation
sets_gjam_all <- group_by(catch_allyears, TRIP_ID, SET_NUM) %>%
  summarise(
    VESSEL_ID = first(VESSEL_ID),
    DECLARED_TRIP = first(DECLARED_TRIP_TYPE_CODE),
    SET_END_DATETIME = first(SET_END_DATETIME),
    HAUL_BEGIN_DATETIME = first(HAUL_BEGIN_DATETIME),
    HAUL_END_DATETIME = first(HAUL_END_DATETIME),
    HAUL_BEGIN_DATE = first(HAUL_BEGIN_DATE),
    HAUL_BEGIN_TIME = first(HAUL_BEGIN_TIME),
    HAUL_BEGIN_LAT = first(HAUL_BEGIN_LAT),
    HAUL_BEGIN_LON = first(HAUL_BEGIN_LON),
    SOAK = as.numeric(HAUL_END_DATETIME - SET_END_DATETIME),
    YEAR = first(YEAR),
    # other gear chars
    NUM_FLTS = first(NUM_FLTS),
    NUM_HKS_SET = first(NUM_HKS_SET),
    HKS_PER_FLT = first(HKS_PER_FLT),
    
    # length/depth
    # MIN_LEN = sum(first(FLTLN_LEN) + first(BRNCHLN_LEN) + first(LDR_LEN)),
    
    # bait etc
    # BLUEDYE_YN = first(DS_BLUEDYED_YN),
    # BLUEDYE = ifelse(BLUEDYE_YN == "Y", 1, 0),
    # BAIT_CODE_VAL = first(BAIT_CODE_VAL),
    # NUM_LITE_DEVICES = first(NUM_LITE_DEVICES),
    
    # catch
    SWO = sum(SPECIES_COMMON_NAME == 'Swordfish'),
    SWO_dp = sum(SPECIES_COMMON_NAME == 'Swordfish' & DAMAGE_CODE_VAL == 'Marine mammal damage'),
    BET = sum(SPECIES_COMMON_NAME == 'Tuna, Bigeye'),
    BET_dp = sum(SPECIES_COMMON_NAME == 'Tuna, Bigeye' & DAMAGE_CODE_VAL == 'Marine mammal damage'),
    YFT = sum(SPECIES_COMMON_NAME == 'Tuna, Yellowfin'),
    YFT_dp = sum(SPECIES_COMMON_NAME == 'Tuna, Yellowfin' & DAMAGE_CODE_VAL == 'Marine mammal damage'),
    UNID_TUNA = sum(SPECIES_COMMON_NAME == 'Tuna, Unidentified' & DAMAGE_CODE_VAL == 'Marine mammal damage'),
    UNID_TUNA_dp = sum(SPECIES_COMMON_NAME == 'Tuna, Unidentified' & DAMAGE_CODE_VAL == 'Marine mammal damage'),
    MAHI =  sum(SPECIES_COMMON_NAME == 'Dolphinfish'),
    MAHI_dp =  sum(SPECIES_COMMON_NAME == 'Dolphinfish' & DAMAGE_CODE_VAL == 'Marine mammal damage'),
    BLUESHARK = sum(SPECIES_COMMON_NAME == 'Shark, Blue'),
  
    # aggregated depredation
    MM_YN = as.factor(ifelse('Marine mammal damage' %in% DAMAGE_CODE_VAL, 1, 0)),
    MM_sum = sum(MM)
  ) %>% drop_na() %>%
  filter(YEAR > 2012 & YEAR < 2017)

## split to x
sets_gjam_x <- sets_gjam_all %>%  ungroup() %>%
  filter(DECLARED_TRIP == 'D') %>%
  select(YEAR, SOAK, NUM_HKS_SET, HKS_PER_FLT)
# add intercept
sets_gjam_x <- cbind(rep(1,dim(sets_gjam_x)[1]), sets_gjam_x)

sets_gjam_y <- sets_gjam_all %>% ungroup() %>%
  filter(DECLARED_TRIP == 'D') %>%
  select(BET, BET_dp, YFT, YFT_dp, UNID_TUNA, UNID_TUNA_dp, MAHI, MAHI_dp)
sets_gjam_yMM <- sets_gjam_all %>% ungroup() %>%
  filter(DECLARED_TRIP == 'D') %>%
  select(BET, YFT, UNID_TUNA, MAHI, MM_sum)
#SWO, SWO_dp, BLUESHARK

# need model, y data, x data, and a modelList specifying number of 
# Gibbs steps ng, the burnin, and typeNames.
datatypes = c(rep("DA",5))
ml  <- list(ng = 1000, burnin = 100, typeNames = datatypes)
gjam_formula <- "~ YEAR + SOAK + NUM_HKS_SET"
gjam_depsps <- gjam(~ YEAR + SOAK + NUM_HKS_SET, sets_gjam_x, sets_gjam_y, modelList = ml)
gjam_MM <- gjam(~ YEAR + SOAK + NUM_HKS_SET, sets_gjam_x, sets_gjam_yMM, modelList = ml)

summary(gjam_MM)
print(gjam_MM)
summary(gjam_MM$inputs$x)
summary(gjam_MM$inputs$y)
summary(gjam_MM$chains)
summary(gjam_MM$inputs)


qr(sets_gjam_x)$rank
