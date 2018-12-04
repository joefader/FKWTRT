
###########################################################################
## hooks per float
###########################################################################

###########################################################################
## some prep
###########################################################################

cat(paste(shQuote(colnames(catch_allyears), type="cmd"), collapse=", "))

# filtering to dataset with DP or FKW and hook location information
catch_deep_all <- group_by(catch_allyears, TRIP_ID, SET_NUM, FLT_NUM) %>%
  filter(DECLARED_TRIP_TYPE_CODE == 'D') %>%
  #filter(MM == 1 | SPECIES_COMMON_NAME == 'Whale, False Killer') %>%
  select("TRIP_ID", "VESSEL_ID", "SET_NUM", "HAUL_BEGIN_DATETIME", "YEAR", "HAUL_BEGIN_LAT", "HAUL_BEGIN_LON", 
         "NUM_FLTS", "HKS_PER_FLT", "NUM_HKS_SET", "SPECIES_COMMON_NAME",
         "KEPT", "TYPE", "SWO", "BET", "YFT", "TUNA", "MAHI", "WAHOO", "BILLFISH",
         "MM", "FKW_caught", "FLT_NUM", "HK_NUM")

## summarising to find max hook per float
# some hooks greater than declared hook number, so finding max in data which will use as max for position
floats_hkmax <-  group_by(catch_allyears, TRIP_ID, SET_NUM, FLT_NUM) %>%
  filter(DECLARED_TRIP_TYPE_CODE == 'D') %>%
  summarise(
    HK_DECLARED = first(HKS_PER_FLT),
    HK_MIN = min(HK_NUM),
    HK_MAX = max(HK_NUM)
    )

## join max hook variable to all catch dataset sorted by floats
# add column of hook position, based on declared floats unless 
# there was a hook number reported higher for that set/float
catch_deep_all <- left_join(catch_deep_all, floats_hkmax, by = c("TRIP_ID", "SET_NUM", "FLT_NUM")) %>%
  mutate(HK_MAX = ifelse(HK_MAX > HKS_PER_FLT, HK_MAX, HKS_PER_FLT)) 

catch_deep_all <- catch_deep_all %>%
  mutate(HK_DIST = ifelse(HK_NUM > (HK_MAX/2), ((HK_MAX + 1) - HK_NUM), HK_NUM)) %>%
  mutate(HK_DIST_STD = HK_DIST/(HK_MAX+1))
       
cat(paste(shQuote(colnames(catch_deep_all), type="cmd"), collapse=", "))

# save out
write_feather(catch_deep_all, "Data/catch_deep_all.feather")


###########################################################################
## read in
###########################################################################

#floats_deep_dp <- read_feather("Data/floats_deep_dp.feather")


###########################################################################
## visuals
###########################################################################

catch_depred <- catch_deep_all %>%
  filter(MM == 1 | FKW_caught ==1)

# FKWs by hook position
catch_depred %>%
  filter(FKW_caught == 1) %>%
  ggplot( mapping = aes(x = HK_DIST_STD)) +
  geom_histogram(binwidth = 0.2) + theme_classic()

## dotplot of counts
catch_depred %>%
  filter(FKW_caught == 1) %>%
  ggplot( mapping = aes(x = HK_DIST_STD)) +
  geom_dotplot(bins=1) + theme_classic()


## distribution of hook positions - depredated sets vs caught FKWs
catch_depred %>%
  ggplot(aes(x = HK_DIST_STD, y=..density.., color = as.factor(FKW_caught))) + 
  geom_freqpoly(bins=12) +
  theme_classic() + ylab("Density") + xlab("Standardized distance from float") + theme(text = element_text(size=15)) + 
  scale_x_continuous(breaks=seq(0,0.5,0.1), limits=c(-0.1,0.6)) + labs(color="Caught FKW") +
  scale_y_continuous(limits=c(0,3.5)) +
  scale_color_manual(labels = c("No", "Yes"), values = c("#56B4E9", "#E69F00")) +
  theme(text = element_text(size=20), 
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
        legend.position = c(0.85, 0.85))

## distribution of hook positions - depredated sets vs all sets
catch_deep_all %>%
  mutate(mammal = ifelse((MM == 1 | FKW_caught == 1),1,0)) %>%
  filter(TYPE == 'FISH' | MM == 1 | FKW_caught == 1) %>%
  ggplot(aes(x = HK_DIST_STD, y=..density.., color = as.factor(mammal))) + 
  geom_freqpoly(bins=12) +
  theme_classic() + ylab("Density") + xlab("Standardized distance from float") + theme(text = element_text(size=15)) + 
  scale_x_continuous(breaks=seq(0,0.5,0.1), limits=c(0,0.6)) + labs(color="Depredated hook") +
  scale_y_continuous(limits=c(0,3.5)) +
  scale_color_manual(labels = c("No", "Yes"), values = c("#56B4E9", "#E69F00")) +
  theme(text = element_text(size=20), 
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
        legend.position = c(0.75, 0.85))

# just depredation
floats_deep_dp %>%
  filter(MM == 1) %>%
  ggplot( mapping = aes(x = HK_POSITION)) +
  geom_histogram(binwidth = .1) + theme_classic()




