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