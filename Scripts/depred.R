library(lubridate)
library(tidyverse)
library(data.table)
library(dplyr)
library(mgcv)
?dplyr
###########################################################################
## time between sets...see date/time for more details
###########################################################################

# identify variable type
sapply (all_sets.date, class) 
sapply (all_sets, class) 

## date time
# all_sets.date <- all_sets
# all_sets.date$SET_BEGIN_DATETIME<- mdy_hms(all_sets.date$SET_BEGIN_DATETIME, tz = "")
# all_sets.date$HAUL_BEGIN_DATETIME<- mdy_hms(all_sets.date$HAUL_BEGIN_DATETIME, tz = "")


## remove nas?? careful..if do diff after this then any after na will be inflated
# sum(is.na(all_sets.date$SET_BEGIN_DATETIME))
# which(is.na(all_sets.date$SET_BEGIN_DATETIME))
# all_sets.date <- all_sets.date[-which(is.na(all_sets.date$SET_BEGIN_DATETIME)),]

# ## diff in rows, NAs for 1st sets and extreme values from missing set 1s
# ## might not be the best way, some sets with really huge lags that appear real
# ## also very small lags, seem to set right after 1st
# all_sets.date$set.lag <- c(NA, diff(all_sets.date$SET_BEGIN_DATETIME, lag=1, difference=1))
# all_sets.date[which(all_sets.date$SET_NUM == 1), "set.lag"] <- NA
# all_sets.date[which(all_sets.date$set.lag > 10000), "set.lag"] <- NA
# all_sets.date[which(all_sets.date$set.lag < 200), "set.lag"] <- NA
# all_sets.date$set.lag.days <- all_sets.date$set.lag / 24 / 60
# 
# ## check out min and max easily
# View(arrange(all_sets.date, set.lag))
# 
# ## creating column indicating whether previous set had DP (DP_LAG1), use shift function in package data.table
# all_sets.date$DP_LAG1 <- shift(all_sets.date$MM_YN, n = 1, type = "lag")
# ## need to control for first sets though
# all_sets.date[which(all_sets.date$SET_NUM == 1), "DP_LAG1"] <- NA
# 
# # sum(is.na(all_sets.date$MM_YN)) #

# histo of set lag
sets_alldata %>%
  filter(DECLARED_TRIP == 'D') %>%
  ggplot(aes(SET_LAG)) + geom_histogram(binwidth = 500)

## dp occurrence - 6% in DS
sets_alldata %>%
  group_by(DECLARED_TRIP, MM_YN) %>%
  filter(DP_LAG1 == 1) %>%
  filter(MM_sum != 1) %>%
  summarise(
    sumMM = sum(as.numeric(as.character(MM_YN))),
    length = n(),
    prop = sumMM / n()
  )
sets_alldata %>%
  filter(DECLARED_TRIP == 'D') %>%
  #group_by(FKW) %>%
  summarise(
    n(),
    n_distinct(TRIP_ID)
  )
###########################################################################
## repeat depredation - trip level
###########################################################################

## depredation at trip level
dp_trip <- group_by(sets_deep_all, TRIP_ID) %>%
  summarise(
    VESSEL_ID = first(VESSEL_ID),
    MM_sets = sum(as.numeric(as.character(MM_YN))),
    MM_YN = ifelse(MM_sets >= 1, 1, 0),
    MM_rpt = ifelse(MM_sets >= 2, 1, 0)
  )

## proportion of repeat DP on DS trips with DP
## 44.8% of deep set depredation repeated within a trip
dp_trip %>%
  #filter(DECLARED_TRIP == 'D') %>%
  summarise(
    sum(MM_rpt)/sum(MM_YN),
    sum(MM_rpt),
    sum(MM_YN),
    n()
  )

###########################################################################
## repeat DP by set
###########################################################################

## add up instances of DP in which DP also occurred on previous set...
sets_deep_all %>%
  group_by(MM_YN) %>%
  #filter(DECLARED_TRIP == 'D') %>%
  #filter(MM_YN == 1) %>%
  summarise(
    sum(DP_LAG1, na.rm = TRUE),
    sum(as.numeric(as.character((MM_YN))))
  )

## make dataset of deep sets with depredation
deep_DP <- sets_alldata %>%
  filter(DECLARED_TRIP == 'D') %>%
  filter(MM_YN == 1 | DP_LAG2 == 1)
deep_DP$MM_YN <- as.numeric(deep_DP$MM_YN)
###########################################################################
## repeat depredation visuals
###########################################################################

## boxplot
#deep_DP %>%
#  ggplot(aes(x = MM_damage, y = FISH)) + geom_boxplot()

## histo of lag all dp sets, days
sets_deep_all %>%
  filter(MM_YN == 1) %>%
  ggplot(aes(SET_LAG)) + geom_histogram(binwidth = 1)

## histo of lag, rpt dp only, days
sets_deep_all %>%
  filter(MM_YN == 1) %>%
  filter(DP_LAG1 == 1) %>%
  ggplot(aes(SET_LAG)) + geom_histogram(binwidth = 1)

## histo of lag, not rpt dp, days
sets_deep_all %>%
  filter(DP_LAG1 != 1) %>%
  ggplot(aes(SET_LAG)) + geom_histogram(binwidth = 1)


## look at set lag based on whether had dp prev or not
sets_deep_all %>%
  filter(SET_LAG < 5000) %>%
  ggplot(aes(x=SET_LAG/60, y=..density.., colour = as.factor(DP_LAG1))) + geom_freqpoly()
sets_deep_all %>%
  filter(LAG_DIST < 1000) %>%
  ggplot(aes(x=LAG_DIST, y=..density.., colour = as.factor(DP_LAG1))) + geom_freqpoly()

## diff in depredation probs by set lag
sets_deep_all %>%
  filter(SET_LAG > 1000 & SET_LAG < 6000) %>%
  filter(DP_LAG1 == 1) %>%
  ggplot(aes(x=SET_LAG/60, y=..density.., colour = as.factor(MM_YN))) + geom_freqpoly(bins=15) +
  theme_classic() + ylab("Density") + xlab("Hours since last haul") + theme(text = element_text(size=15)) + 
  scale_x_continuous(breaks=seq(0,100,24),limits=c(0,100)) + labs(color="Repeat Depredation") +
  scale_color_manual(labels = c("No", "Yes"), values = c("#56B4E9", "#E69F00")) +
  theme(text = element_text(size=25), 
        plot.margin = margin(25, 0, 1, 0),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 10)),
        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 10, l = 0)),
        legend.position = c(0.7, 0.8))

sets_deep_all %>%
  filter(LAG_DIST < 1000) %>%
  filter(DP_LAG1 == 1) %>%
  ggplot(aes(x=LAG_DIST, y=..density.., colour = as.factor(MM_YN))) + geom_freqpoly(bins=15) +
  theme_classic() + ylab("Density") + xlab("Distance since last haul (km)") + theme(text = element_text(size=15)) + 
  scale_x_continuous(breaks=seq(0,500,100),limits=c(0,500)) + labs(color="Repeat Depredation") +
  scale_color_manual(labels = c("No", "Yes"), values = c("#56B4E9", "#E69F00")) +
  theme(text = element_text(size=25), 
        plot.margin = margin(25, 25, 0, 0),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 10)),
        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 10, l = 0)),
        legend.position = 'none')

###########################################################################
## proportions and averages
###########################################################################

## proportion of 1 day vs >= 1 day lags 
## add columns to deepDP indicating lag bins
deep_DP$Lag1 <- ifelse(deep_DP$SET_LAG < 2160, 1, 0)
deep_DP$Lag2 <- ifelse(deep_DP$SET_LAG >= 2160, 1, 0)

## overall proportion of lag classes for all DP sets
deep_DP %>%
  group_by(DECLARED_TRIP) %>%
  summarize(
    sum(Lag1, na.rm = T) / sum(MM_YN),
    sum(Lag2, na.rm = T) / sum(MM_YN)
  )

######################################################
## proportion of lags on dp vs no dp on prev set...if look at over all sets, whether or not
## current set has dp, then highly significant that fm wait longer after experiencing dp
######################################################
## 1st add lag category to all_sets
all_sets.date$Lag1 <- ifelse(all_sets.date$set.lag.days < 1.5, 1, 0)
all_sets.date$Lag2 <- ifelse(all_sets.date$set.lag.days >= 1.5, 1, 0)

lagtbl <- as.matrix(
  sets_deep_all %>%
  group_by(DP_LAG1) %>%
  summarise(
    sum(Lag1, na.rm = T),
    sum(Lag2, na.rm = T)
  ))

lagtbl <- lagtbl[-3,-1]
chisq.test(lagtbl)


###########################################################################
## prop of lags for y or no dp, for all sets with dp previously...diff than above
## and makes more sense..i think...
###########################################################################

lagtime_tbl <- as.matrix(
  deep_DP %>%
    filter(DECLARED_TRIP == 'D', DP_LAG2 == 1) %>%
    group_by(MM_YN) %>%
    summarise(
      sum(Lag1, na.rm = T),
      sum(Lag2, na.rm = T)
    ))

lagtbl <- lagtbl[-3,-1]
chisq.test(lagtbl) ## 19 vs 14% if wait >1 day, decrease of 26%

lagdist_tbl <- as.matrix(
  deep_DP %>%
    mutate(Lag = ifelse(LAG_DIST < 60 & SET_LAG <= 2160, 1,0)) %>%
    filter(MM_sum != 1) %>%
    filter(DECLARED_TRIP == 'D' & DP_LAG2 == 1) %>%
    group_by(MM_YN) %>%
    summarise(
      Lag1 = sum(Lag, na.rm = T),
      Lag2 =(n() - Lag1)
    ))
lagdist_tbl <- lagdist_tbl[-3,-1]
chisq.test(lagdist_tbl)
lagdist_tbl[2,1]/lagdist_tbl[1,1]
lagdist_tbl[2,2]/lagdist_tbl[1,2]

## distance bins
sets_alldata %>%
  filter(DP_LAG4 == 1) %>%
  filter(DECLARED_TRIP == 'D') %>%
  #filter(MM_sum != 1) %>%
  # filter(LAG_DIST < 50) %>%
  # filter(LAG_DIST > 50 & LAG_DIST <= 75) %>%
  # filter(LAG_DIST > 75 & LAG_DIST <= 100) %>%
  # filter(LAG_DIST > 100 & LAG_DIST <= 125) %>%
  filter(LAG_DIST >= 100) %>%
  group_by(MM_YN) %>%
  summarise(
    n()
  )

#View(sets_deep_all %>%
      # count(cut_width(MM_YN, 50)))

## time bins
sets_alldata %>%
  filter(DP_LAG4 == 1) %>%
  filter(DECLARED_TRIP == 'D') %>%
  # filter(MM_sum != 1) %>%
  # filter(SET_LAG < 2160) %>%
  # filter(SET_LAG >= 2160 & SET_LAG < 3600) %>%
  filter(SET_LAG >= 3600) %>%
  group_by(MM_YN) %>%
  summarise(
    n()
  )

## now check out avg lag for mm_yn following dp_lag == 1
## slight difference, but how to compare?? def not normally dist so t-test not
## appropriate, transform?
sets_alldata %>%
  filter(DECLARED_TRIP == 'D', DP_LAG1 == 1) %>%
  group_by(MM_YN) %>%
  summarise(
    mean(SET_LAG, na.rm = T),
    sd(SET_LAG),
    n()
  )

all_sets.date %>%
  filter(DECLARED_TRIP == 'D', DP_LAG1 == 1, MM_YN == 1) %>%
  ggplot(aes(set.lag.days)) + geom_histogram(binwidth = 1)
all_sets.date %>%
  filter(DECLARED_TRIP == 'D', DP_LAG1 == 1, MM_YN == 0) %>%
  ggplot(aes(set.lag.days)) + geom_histogram(binwidth = 1)

all_sets.date %>%
  filter(DECLARED_TRIP == 'D', DP_LAG1 == 1) %>%
  ggplot(aes(x = set.lag.days, y = MM_YN)) + geom_point()

###########################################################################
## binomial glm on prob of rpt dp according to set lag - code also in GLM_GAM script
## YES there is negative and significant exponent of lag time on prob of rpt dp
###########################################################################
sets_deep_MMlag = sets_alldata %>%
  filter(DECLARED_TRIP == 'D', DP_LAG1 == 1)

fit_lag = glm(MM_YN ~ SET_LAG, data = sets_deep_MMlag, family = binomial)
fit_lag = glm(MM_YN ~ LAG_DIST, data = sets_deep_MMlag, family = binomial)
fit_lag = gam(MM_YN ~ s(LAG_DIST, bs='ts'), method='REML', data = sets_deep_MMlag, family = binomial)

par(mfrow=c(1,1), mar=c(0,0,0,0), oma=c(0,0,0,0))
summary(fit_lag)
plot(fit_lag)


###########################################################################
## break down as in Forney etal...ie rate of dp on sets preceded by dp vs those not 
###########################################################################

sets_alldata %>%
  group_by(MM_YN) %>%
  filter(DECLARED_TRIP == 'D') %>%
  filter(DP_LAG1 ==1) %>%
  filter(MM_sum != 1) %>%
  summarize(
    n()
  )

prior_dp <- matrix(c(3082,259,48073,2376), ncol=2, byrow = T)
chisq.test(prior_dp) ##
  


  
## mean lag for DP_LAG 1 vs 0
deep_DP %>%
  group_by() %>%
  filter(DP_LAG1 == 1) %>%
  summarize(
    mean(set.lag.days),
    sum(MM_YN)
  )

deep_DP %>%
  group_by() %>%
  filter(DP_LAG1 != 1) %>%
  summarize(
    mean(set.lag.days),
    sum(MM_YN)
  )


## histo of lag, hours
deep_DP %>%
  ggplot(aes(set.lag)) + geom_histogram(binwidth = 100)

# depredation after fkw
View(sets_deep_all %>%
  filter(lag(FKW == 1)) %>%
  # filter(DP_LAG1 == 1) %>%
  #group_by(MM_YN) %>%
  summarise(
    mean(LAG_DIST, na.rm=T),
    sd(LAG_DIST, na.rm=T),
    mean(SET_LAG/60/24, na.rm=T),
    sd(SET_LAG/60/24, na.rm=T)
  ))# %>%

sets_deep_all %>%
  #filter(lag(FKW == 1)) %>%
  # filter(DP_LAG1 == 1) %>%
  #group_by(MM_YN) %>%
  ggplot(aes(x = MM_YN, y = LAG_DIST)) + geom_boxplot()
    
    
    