#############################################################################
# EDA
# Joseph Fader
# February, 2019

library(tidyverse)
library(feather)
library(GGally)

sets_alldata <- read_feather("Data/sets_alldata.feather")
sets_deep_all <- read_feather("Data/sets_deep_all.feather")
sets_shallow_all <- read_feather("Data/sets_shallow_all.feather")


#############################################################################
### correlations
#############################################################################

sapply(sets_deep_all, class)
cat(paste(shQuote(colnames(sets_deep_all), type="cmd"), collapse=", "))

# make data frame of only variables to inspect
sets_deep_forcorr <- subset(sets_deep_all, select = c(
    #"SET_NUM", "HAUL_BEGIN_DATE", "HAUL_BEGIN_TIME", 
  "HAUL_BEGIN_LAT", "HAUL_BEGIN_LON", "postTRT", "MONTH", "YEAR", "SOAK", "HOOKS",
  "NUM_FLTS", "NUM_HKS_SET", "HKS_PER_FLT", "MIN_LEN", "BLUEDYE_YN", 
  "NUM_CAUGHT", "KEPT", "FISH", "SHARKS", "ODONT", "TURTLES", "SWO", "BET", "YFT", "TUNA", "MAHI", 
  "WAHOO", "BILLFISH", "CPUE", "MM_YN", "MM_sum", "MM_any", "Slope", "Depth", "Cont_Dist", 
  "Seamt_Dist", "EDDY_DIST", "AMPLITUDE", "CYCL_TYPE", "SPEED", "RADIUS", "SST_2", "Moon_Illum",
  "ONI", "EL_LA_NO", "SST_RANGE", "SET_LAG", "SET_LAG_HAULS", "LAG_DIST", 
  "LAG_DIST_HAULS", "front_dis", "tke", "adt", "eke", "chla_mo_9k", "cpue_avg_3d_100k", 
  "num_vessels_3d_100k", "cpue_avg_1d_100k", "num_vessels_1d_100k"))


### testing diff corr plots

## base r..
pairs(Filter(is.numeric, sets_deep_all))
sets_deep_numeric <- Filter(is.numeric, sets_deep_all)


## write to a pdf to supposedly get around limited window margins, crashes adobe tho
pdf(file = "Docs/pairplots.pdf", height=15, width=15) 
pairs(Filter(is.numeric, sets_deep_all))
dev.off() 


## ggpairs in GGally (extension to ggplot2)
?ggpairs
pdf(file = "Docs/EDA/Ggally-plots.pdf", height=15, width=15)
ggpairs(sets_deep_forcorr, gap=0)
dev.off() 


pairsplot <- ggpairs(sets_deep_forcorr, gap=0)
quartz(width=100, height=100)   #make a big graphics window 



## ggplot2 - apparently not in dplyr anymore
plotmatrix(Filter(is.numeric, sets_deep_all))

## ggcorplot function
source("Scripts/Functions/ggcorplot.R")
ggcorplot(
  data = Filter(is.numeric, sets_deep_all),
  var_text_size = 5,
  cor_text_limits = c(5,10))

## psych package
library(psych)
quartz(width=30, height=30)   #make a big graphics window 
pdf(file = "Docs/EDA/psych-plots.pdf", height=15, width=15)
pairs.panels(sets_deep_forcorr)
dev.off



