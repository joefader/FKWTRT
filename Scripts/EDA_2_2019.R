#############################################################################
# EDA
# Joseph Fader
# February, 2019

library(tidyverse)


sets_alldata <- read_feather("Data/sets_alldata.feather")
sets_deep_all <- read_feather("Data/sets_deep_all.feather")
sets_shallow_all <- read_feather("Data/sets_shallow_all.feather")


#############################################################################
### correlations
#############################################################################

sapply(sets_deep_all, class)

### testing diff corr plots

## base r..
pairs(Filter(is.numeric, sets_deep_all))

## ggplot2 - apparently not in dplyr anymore
plotmatrix(Filter(is.numeric, sets_deep_all))

## ggcorplot function
source("Scripts/Functions/ggcorplot.R")
ggcorplot(
  data = Filter(is.numeric, sets_deep_all),
  var_text_size = 5,
  cor_text_limits = c(5,10))






