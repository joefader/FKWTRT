#############################################################################
# Import and visualization of move on rules for PLL observer data
# Joseph Fader
# August 2018


## load packages needed
library(tidyverse)
library(feather) # easy and useful for saving and loading dataframes in R

## import data
moveon_tbl <- read.csv("Data/MoveOnRules/MoveOnRuleTable.csv")
moveon_tbl_hrs <- read.csv("Data/MoveOnRules/MoveOnHours.csv")

write_feather(moveon_tbl, "Data/MoveOnRules/MoveOnRuleTable.feather")
write_feather(moveon_tbl_hrs, "Data/MoveOnRules/MoveOnHours.feather")

moveon_tbl <- read_feather("Data/MoveOnRules/MoveOnRuleTable.feather")
moveon_tbl_hrs <- read_feather("Data/MoveOnRules/MoveOnHours.feather")

## tile plots

moveon_tbl %>%
  ggplot(mapping = aes(x = distance, y = days)) + theme_classic() +
  geom_tile(mapping = aes(fill = prob_marked_in)) +
  theme(text = element_text(size=18)) +
  ylab("Days since previous set") + xlab("Distance since previous set (km)") +
  scale_x_continuous(breaks=seq(0,250,50),limits=c(0,300)) +
  scale_y_continuous(breaks=seq(0,5,1),limits=c(0,6)) +
  theme(legend.title = element_text(colour="black", size=16, face="bold"))

moveon_tbl_hrs %>%
  ggplot(mapping = aes(x = distance, y = days)) + theme_classic() +
  geom_tile(mapping = aes(fill = prob_marked_in)) +
  theme(text = element_text(size=18)) +
  ylab("Hours since previous set") + xlab("Distance since previous set (km)") +
  scale_x_continuous(breaks=seq(0,250,50),limits=c(0,300)) +
  scale_y_continuous(breaks=seq(0,120,12),limits=c(0,120)) +
  theme(legend.title = element_text(colour="black", size=16, face="bold"))


moveon_tbl %>%
  ggplot(mapping = aes(x = distance, y = days)) + 
  geom_tile(mapping = aes(fill = prob_marked_out))


## contour of marked probabilities 
moveon_tbl %>%
  ggplot(mapping = aes(x = distance, y = days)) + theme_bw() +
  geom_contour(aes(z = prob_marked_in), binwidth= 0.05) +
  theme(text = element_text(size=18)) +
  ylab("Days since previous set") + xlab("Distance since previous set (km)") +
  scale_x_continuous(breaks=seq(25,250,25),limits=c(25,250)) +
  scale_y_continuous(breaks=seq(1,5,1),limits=c(1,5)) 

moveon_tbl_hrs %>%
  ggplot(mapping = aes(x = distance, y = days)) + theme_bw() +
  geom_contour(aes(z = prob_marked_in), binwidth= 0.05) +
  theme(text = element_text(size=18)) +
  ylab("Hours since previous set") + xlab("Distance since previous set (km)") +
  scale_x_continuous(breaks=seq(25,250,25),limits=c(25,250)) +
  scale_y_continuous(breaks=seq(0,120,12),limits=c(12,120)) 

moveon_tbl %>%
  ggplot(mapping = aes(x = distance, y = days)) + 
  geom_contour(mapping = aes(z = prob_marked_out))

moveon_tbl %>% 
  summarize(
    
  )
  

## look at distributions of time and distance lags in general

# histogram - all lag
ggplot(data = sets_deep_all) +
  geom_histogram(mapping = aes(x = LAG_DIST), binwidth = 20) + xlab("Histo all hooks")


# lag distance
sets_deep_all %>%
  ggplot() + geom_histogram(aes(x = LAG_DIST_HAULS), binwidth = 10)
sets_deep_all %>% filter(DP_LAG1 == 1) %>% 
  ggplot() + geom_histogram(aes(x = LAG_DIST_HAULS), binwidth = 10)
sets_deep_all %>% filter(FKW == 1) %>% 
  ggplot() + geom_dotplot(aes(x = LAG_DIST), binwidth = 25)
  
# time vs distance

sets_deep_all %>%
  #filter(MM_YN == 1) %>%
  ggplot(aes(x = LAG_DIST, y = SET_LAG_HAULS)) + 
  #ggplot(aes(x = LAG_DIST_HAULS, y = SET_LAG_HAULS)) + 
  geom_jitter()

sets_deep_all %>%
  #filter(MM_YN == 1) %>%
  ggplot(aes(x = LAG_DIST, y = SET_LAG)) + 
  #ggplot(aes(x = LAG_DIST_HAULS, y = SET_LAG_HAULS)) + 
  geom_bin2d() +
  scale_x_continuous(breaks=seq(25,500,25),limits=c(25,500)) +
  scale_y_continuous(breaks=seq(0,120,12),limits=c(12,120)) 
  
sets_deep_all %>%
  #filter(MM_YN == 1) %>%
  ggplot(aes(x = LAG_DIST, y = SET_LAG)) + 
  geom_density_2d() +
  scale_x_continuous(breaks=seq(25,500,25),limits=c(25,500)) +
  scale_y_continuous(breaks=seq(0,120,12),limits=c(12,120)) 


sets_deep_all %>%
  #filter(MM_YN == 1) %>%
  ggplot(aes(x = LAG_DIST_HAULS, y = SET_LAG_HAULS)) + 
  geom_density_2d() +
  scale_x_continuous(breaks=seq(25,100,25),limits=c(25,100)) +
  scale_y_continuous(breaks=seq(0,36,12),limits=c(12,36)) 
