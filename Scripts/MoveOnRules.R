#############################################################################
# Import and visualization of move on rules for PLL observer data
# Joseph Fader
# August 2018


## load packages needed
library(tidyverse)
library(feather) # easy and useful for saving and loading dataframes in R
library(ggplot2)
library(metR)  # some meteorological package, allows adding contour labels to ggplot
library(directlabels) # other option for getting labels on contours
library(RColorBrewer)
library(reshape)
## import data
moveon_tbl <- read.csv("Data/MoveOnRules/SQ4_compiled.csv")
moveon_tbl_hrs <- read.csv("Data/MoveOnRules/MoveOnHours.csv")
moveon_tbl <- read.csv("Data/MoveOnRules/MoveOnCompiled/NQ1_days10_km500_compiled.csv")

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
  scale_x_continuous(breaks=seq(0,500,50),limits=c(0,500)) +
  scale_y_continuous(breaks=seq(0,10,1),limits=c(0,11)) +
  theme(legend.title = element_text(colour="black", size=16, face="bold"))

moveon_tbl %>%
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

## put file name here

#moveon_tbl <- read.csv("Data/MoveOnRules/MoveOnCompiled/hours240_25_500km/NQ4_hours240_25_500km_compiled.csv")
#moveon_tbl <- read.csv("Data/MoveOnRules/MoveOnCompiled/hours240_25_500km/NQ2_hours240_25_500km_compiled.csv")
#moveon_tbl <- read.csv("Data/MoveOnRules/SEFSC_moveon_500k_10d_compiled.csv")
## contour of marked probabilities - set for days and km

## file names
moveon_tbl <- read.csv("Data/MoveOnRules/MoveOnCompiled/Centroid_95perchull/NQ4_12-240h_25-500k.csv")


## if need to downsample...
NQ4 <- moveon_tbl %>% 
  filter(days %in% seq(24, 240, 24)) %>%
  filter(distance %in% seq(50, 500, 50))
    #days == 24 | days == 48 | days == 72 | days == 96 | days == 120 | days == 24 | 144)

NQ4 %>%
  #filter(days <= 120 & distance <= 250) %>% 
  ggplot(mapping = aes(x = distance, y = days/24)) + theme_classic() +
  geom_contour(aes(z = prob_marked_in*100, colour = ..level..), binwidth= 1) +
  geom_contour(aes(z = prob_marked_in*100), binwidth= 5, color = "red") +
  geom_text_contour(aes(z = prob_marked_in*100), stroke = 0.3, 
                    binwidth= 5, color = "red", rotate = T, size = 6) +
  theme(text = element_text(size=18)) +
  #ggtitle("North, Quarter 1 - 12 h x 50 km") +
  ylab("\nDays since previous set") + xlab("\nDistance since previous set (km)") +
  scale_x_continuous(breaks=seq(0,500,50),limits=c(50,500)) +
  scale_y_continuous(breaks=seq(0,10,1),limits=c(1,10)) +
  # scale_y_continuous(breaks=seq(0,10,1),limits=c(0,10)) +
  # scale_x_continuous(breaks=seq(0,250,50),limits=c(25,250)) +
  # scale_y_continuous(breaks=seq(0,120,24),limits=c(0,120)) +
  scale_color_continuous(name = "Prob. rpt. depr.") +
  theme(legend.justification=c(1, 0), legend.position=c(1, .7))
  #geom_text_contour(aes(z = prob_marked_in), stroke = 0.2, check_overlap=T, rotate = T)
  #geom_label_contour(aes(z = prob_marked_in))
direct.label(g, method="bottom.pieces")


## playing around with different metrics to visualize
# ratio of marked:umarked, percent reduction, etc
moveon_tbl <- read.csv("Data/MoveOnRules/MoveOnCompiled/hours240_25_500km/SQ1_hours240_25_500km_compiled.csv")
moveon_tbl %>%
  #filter(days <= 120 & distance <= 250) %>% 
  ggplot(aes(x = distance, y = days, 
             fill =  100*(1 - (prob_marked_in/max(moveon_tbl$prob_marked_in))))) + 
  theme_classic() + 
  geom_tile() +
  # scale_fill_gradient(low = "red", high = "blue", limits = c(0, 99)) +
  scale_fill_gradientn(colours = c("red", "blue"),
                       guide = "colourbar",
                       limits = c(0, 99),
                       breaks = c(0, 50, 99),
                       labels = c("0", "50", "100")) +
  # geom_tile(mapping = aes(fill = 
  #         100*(1 - (prob_marked_in/max(moveon_tbl$prob_marked_in))))) +  ### percent reduction from max rate
  geom_text(aes(label = 
          round(100*(1 - (prob_marked_in/max(moveon_tbl$prob_marked_in))), digits=0), 
          fontface = "bold"), size = 10, show.legend = F) +  ## add value to tiles
  theme(text = element_text(size=18)) +
  #ggtitle("North, Quarter 3 - 12 h x 50 km") +
  ylab("Days since previous set") + xlab("Distance since previous set (km)") +
  scale_x_continuous(breaks=seq(0,250,50),limits=c(12,265))+ #,expand = c(0,0)) +
  scale_y_continuous(breaks=seq(0,120,24),limits=c(6,126), 
                     labels=c("0", "1", "2", "3", "4", "5"))+ #, expand = c(0,0)) +
  #scale_color_continuous(name = "Prob. rpt. depr.") +
  theme(legend.justification=c(1, 0), legend.position=c(1, .7)) +
  labs(fill='percent 
 reduction') 



moveon_tbl %>%
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
