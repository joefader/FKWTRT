#############################################################################
# EDA on FKW
# Joseph Fader
# MArch, 2019

library(tidyverse)
library(feather)



# depredation on previous sets when FKW caught

sets_deep_all %>% 
  filter(FKW == 1) %>% 
  summarize(
    n(),
    sum(DP_LAG1, na.rm = T),
    sum(DP_LAG2, na.rm = T),
    sum(DP_LAG3, na.rm= T),
    sum(DP_LAG4, na.rm = T),
    mean(DP_LAG_NUM, na.rm = T)
  )
