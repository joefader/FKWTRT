#############################################################################
# Data prep and EDA for FKW bycatch
# Joseph Fader
# March, 2018

library(tidyverse)
library(ecodist)
library(feather)
library(grid )

sets_deep_FKW <- sets_deep_all %>%
  filter(MM_YN == 1 | FKW == 1)
sets_shallow_FKW <- sets_shallow_all %>%
  filter(MM_YN == 1 | FKW == 1)


#############################################################################
### data import/prep- add some gear chars then join with larger dataset
#############################################################################
catch_allyears <- read_feather("Data/catch_allyears_prepd.feather")

### sets - aggregate catch data to set level- only variables to add on to larger dataset
sets_FKW <- group_by(catch_allyears, TRIP_ID, SET_NUM) %>%
  summarise(
    VESSEL_ID = first(VESSEL_ID),
    
    # hooks, leave out for now
    #HOOK_TYPE = first(HK_TYPE_CODE_VAL_1),
    #HOOK_SZ = first(SZ_HKS_1),
    
    # other gear chars

    LEADER = first(LDR_MAT_CODE_VAL),
    DROP_WT_SZ = first(DROP_WT_SZ),
    BRANCHLN_DIAM = first(BRNCHLN_DIAM)
  )


## breaking down sets with different branchline diameters
View(catch_allyears %>%
  filter(DECLARED_TRIP_TYPE_CODE == 'D') %>%
  group_by(TRIP_ID,SET_NUM) %>%
  summarise(
    brnchln = first(BRNCHLN_DIAM),
    FKW = max(FKW_caught)
  ) %>%
  group_by(brnchln, FKW) %>%
  summarise(
    n()
  ))

unique(catch_allyears$BRNCHLN_DIAM)

catch_allyears %>%
  filter(DECLARED_TRIP_TYPE_CODE == 'D') %>%
  group_by(TRIP_ID, SET_NUM) %>%
  filter(FKW_caught == 1) %>%
  ggplot(aes(x = BRNCHLN_DIAM, y = ..density..)) + geom_freqpoly(binwidth = 0.5)

## look at histogram by fkw yes or no
catch_allyears %>%
  filter(DECLARED_TRIP_TYPE_CODE == 'D') %>%
  group_by(TRIP_ID, SET_NUM) %>%
  filter(FKW_caught == 0) %>%
  ggplot(aes(x = BRNCHLN_DIAM)) + geom_histogram()

sets_alldata_FKW <- left_join(sets_alldata, sets_FKW, by = c("TRIP_ID", "VESSEL_ID", "SET_NUM"))
sapply(sets_alldata_FKW, class)
summary(sets_alldata_FKW)

FKWall <- left_join(sets_alldata, MMforms, by = c("TRIP_ID", "VESSEL_ID", c("SET_NUM"="S_SET_NUM")))
rm(FKWall)
# fkwteste <- sets_alldata
#   mutate(FKWall = ifelse(sets_alldata$TRIP_ID == MMforms$TRIP_ID & sets_alldata$VESSEL_ID == MMforms$VESSEL_ID 
#                     & sets_alldata$SET_NUM == MMforms$S_SET_NUM, 1, 0))


sets_deep_FKW <- sets_alldata_FKW %>%
  filter(DECLARED_TRIP != 'S') %>%
  filter(MM_YN == 1 | FKW == 1)
sets_shallow_FKW <- sets_alldata_FKW %>%
  filter(MM_YN == 1 | FKW == 1) %>%
  filter(DECLARED_TRIP == 'S')

write_feather(sets_deep_FKW, "Data/sets_deep_FKW.feather")
write_feather(sets_shallow_FKW, "Data/sets_shallow_FKW.feather")


#############################################################################
### gear stuff
#############################################################################
## what to do about short soaks? some have 1000s of hooks and no fish, others with fish

## sets
sets_alldata %>%
  filter(DECLARED_TRIP == 'D') %>%
  filter(MM_YN == 1 | FKW == 1) %>%
  summarise(
    n(),
    sum(as.numeric(as.character(FKW)))
  )

## vessels - few with 2 but none more than that
View(sets_deep_FKW %>%
  group_by(VESSEL_ID) %>%
  filter(FKW == 1) %>%
  summarise(
    tots = n()
  ))

## number of floats
ggplot(data = sets_deep_FKW, mapping = aes(x = NUM_FLTS, y = ..density.., colour = FKW)) +
  geom_freqpoly(binwidth = 10)

## number of hooks set - maybe bump at upper end
ggplot(data = sets_deep_FKW, aes(x = NUM_HKS_SET, y = ..density.., colour = FKW)) +
  geom_freqpoly(binwidth = 200)

ggplot(data = sets_deep_FKW, mapping = aes(x = NUM_HKS_SET)) +
  geom_histogram( )

## minimum length/depth - very slightly shifted right for fkw but hardly strong and depends on bin
ggplot(data = sets_deep_FKW, mapping = aes(x = MIN_LEN, y=..density..,colour = FKW)) +
  geom_freqpoly(binwidth = 3)
ggplot(data = sets_deep_FKW, aes(x = FKW, y = MIN_LEN)) +
  geom_boxplot()

## hooks per float - slight shift to right for no whales, probably just distribution wider
ggplot(data = sets_deep_FKW, mapping = aes(x = HKS_PER_FLT, y=..density..,colour = FKW)) +
  geom_freqpoly(binwidth = 4)
ggplot(data = sets_deep_FKW, aes(x = FKW, y = HKS_PER_FLT)) +
  geom_boxplot()

## blue dye - 29/69 caught had blue dye compared to 783/2246 overall (42 to 26%)
summary(sets_deep_FKW$BLUEDYE)
sets_deep_FKW %>%
  group_by(FKW) %>%
  summarise(
    n = n(),
    Y = sum(as.numeric(as.character(BLUEDYE))),
    N = n - Y,
    proportion=Y/n()
  )
ggplot(data = sets_deep_FKW, aes(x = FKW, y = BLUEDYE)) +
  geom_count()

# ggplot(sets_deep_all, aes(x = HAUL_BEGIN_LAT, y = ..density.., colour = BLUEDYE)) +
#   geom_freqpoly(binwidth = 10 )
# ggplot(sets_deep_all, aes(x = HAUL_BEGIN_LON, y = ..density.., colour = BLUEDYE)) +
#   geom_freqpoly(binwidth = 10 )


## bait code - almost all saury
# bait type
ggplot(data = sets_deep_FKW) +
  geom_bar(mapping = aes(x = BAIT_CODE_VAL))
sets_deep_FKW %>%
  count(BAIT_CODE_VAL)

## LEADER
summary(sets_deep_FKW$LEADER)
sets_deep_FKW %>%
  group_by(FKW, LEADER) %>%
  summarise(
    n = n()
  )

## DROP WT
summary(sets_deep_FKW$DROP_WT_SZ)
ggplot(data = sets_deep_FKW, mapping = aes(x = DROP_WT_SZ, y=..density..,colour = FKW)) +
  geom_freqpoly(binwidth = 3)

## number light devices -- note no use of lightsticks in deep set (one set > 1)
ggplot(data = sets_deep_all, mapping = aes(x = NUM_LITE_DEVICES, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(bins = 1)


#############################################################################
### hooks
#############################################################################

sets_deep_FKW %>%
  group_by(HOOKS) %>%
  summarise(
    total = n(),
    FKW = sum(as.numeric(as.character(FKW))),
    FKW_tot = sum(FKW_sum)
  )

sum(as.numeric(as.character(sets_deep_all$FKW)))
sum(as.numeric(as.character(sets_deep_FKW$FKW_sum)))

## not lumping hooks, total sets table
#hooks_all <- 
  sets_deep_all %>%
  #filter(YEAR>2003) %>%
  group_by(YEAR) %>%
  summarise(
    circle = sum(HOOKS == 'CIRCLE'),
    tuna = sum(HOOKS == 'TUNA'),
    J = sum(HOOKS == 'JHOOK'),
    tunaj = sum(tuna + J),
    other = sum(HOOKS == 'OTHER'),
    tots = sum(circle+tuna+J+other),
    FKW = sum(FKW_sum)*1000
  ) %>%
  ggplot() + geom_line(aes(x=YEAR, y=circle, color='CIRCLE')) + 
  geom_line(aes(x=YEAR, y=tuna, color='TUNAJ')) + 
  geom_line(aes(x=YEAR, y=FKW, color='FKW')) + ylab('Number')

## table for floats by hooks
sets_deep_all %>%
  filter(YEAR>2003) %>%
    group_by(YEAR,HOOKS) %>%
    summarise(
      flts = sum(NUM_FLTS),
      FKW = sum(FKW_sum)
    ) %>%
  ggplot() + geom_line(aes(x=YEAR, y=flts, colour = HOOKS, group = HOOKS)) + theme_classic()


## difference between hooks?
sets_deep_FKW %>%
  mutate(HOOKS = ifelse((HOOKS == 'TUNA' | HOOKS == 'JHOOK'), 'TUNAJ', 'CIRCLE')) %>%
  filter(HOOKS == 'TUNAJ' | HOOKS == 'CIRCLE') %>%
  group_by(HOOKS) %>%
  ggplot(aes(x = HOOKS, y = FKW)) + geom_count()


par(mfrow=c(1,1), mar=c(0,0,0,0), oma=c(0,0,0,0))

## summarise and plot hooks/fkw by year - lumping tunaj
sets_deep_all %>%
  filter(YEAR>2003) %>%
  mutate(HOOKS = ifelse((HOOKS == 'TUNA' | HOOKS == 'JHOOK'), 'TUNAJ', 'CIRCLE')) %>%
  group_by(YEAR) %>%
  summarise(
    circle = sum(HOOKS == 'CIRCLE'),
    tunaj = sum(HOOKS == 'TUNAJ'),
    other = sum(HOOKS == 'OTHER'),
    cum = sum(circle+tunaj),
    FKW = sum(FKW_sum)
  ) %>%
  ggplot() + 
  geom_line(aes(x=YEAR, y=circle, color='Circle')) + theme_classic() +
  geom_line(aes(x=YEAR, y=tunaj, color='Tuna/J')) + ylab('Number of sets') +
  geom_line(aes(x=YEAR, y=cum, color='Total')) + scale_fill_discrete(name="Hook Type") +
  scale_x_continuous(breaks=seq(2004,2017,1),limits=c(2003,2018)) + xlab("") +
  scale_color_discrete(name = "Hook Type", breaks=c("Circle","Tuna/J","Total")) +
  # geom_bar(stat="identity", aes(x=YEAR, y=FKW)) + theme_classic() + ylab("Num. of FKW") +
  # scale_x_continuous(breaks=seq(2004,2017,1),limits=c(2003,2018)) +
  # scale_y_continuous(breaks=seq(0,12,2),limits=c(0,12)) +
  theme(text = element_text(size=15))
  
## hooks by year for set, hooks, floats
sets_deep_all %>%
  mutate(HOOKS = ifelse((HOOKS == 'TUNA' | HOOKS == 'JHOOK'), 'TUNAJ', 'CIRCLE')) %>%
  filter(HOOKS == 'TUNAJ' | HOOKS == 'CIRCLE') %>%
  group_by(YEAR, HOOKS) %>%
  summarise(
    Sets = n(),
    Hooks = sum(NUM_HKS_SET),
    Floats = sum(NUM_FLTS),
    FKW = sum(as.numeric(as.character(FKW))),
    FKWsum = sum(FKW_sum)
  ) %>%
  #filter(HOOKS == 'CIRCLE') %>%
  filter(YEAR >2003) %>%
  ggplot() + geom_line(aes(x=YEAR, y=Hooks, color=HOOKS))



##Chi square to test difference between hooks
fkw_hooks <- sets_deep_all %>%
  mutate(HOOKS = ifelse((HOOKS == 'TUNA' | HOOKS == 'JHOOK'), 'TUNAJ', 'CIRCLE')) %>%
  filter(HOOKS == 'TUNAJ' | HOOKS == 'CIRCLE') %>%
  group_by(HOOKS) %>%
  summarise(
    Sets = n(),
    Hooks = sum(NUM_HKS_SET),
    Floats = sum(NUM_FLTS),
    FKW = sum(as.numeric(as.character(FKW))),
    FKWsum = sum(FKW_sum)
  )

# by set
hook_chi <- chisq.test(fkw_hooks[,c("Sets","FKWsum")], correct=F)
# by hooks
hook_chi <- chisq.test(fkw_hooks[,c("Hooks","FKWsum")], correct=F)
# by floats
hook_chi <- chisq.test(fkw_hooks[,c("Floats","FKWsum")], correct=F)

c(hook_chi$statistic, hook_chi$p.value)
sqrt(hook_chi$statistic / sum(fkw_hooks[,c("Floats","FKWsum")]))

