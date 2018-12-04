#############################################################################
### prepare SST range across set
#############################################################################

SST_setbegin <- as.data.frame(rbind(read.csv("Data/GIS_added_files/SSTs/SST_setbeg1.csv"), read.csv("Data/GIS_added_files/SSTs/SST_setbeg2.csv")))
SST_setend <- as.data.frame(rbind(read.csv("Data/GIS_added_files/SSTs/SST_setend1.csv"), read.csv("Data/GIS_added_files/SSTs/SST_setend2.csv")))
SST_haulend <- as.data.frame(rbind(read.csv("Data/GIS_added_files/SSTs/SST_haulend1.csv"), read.csv("Data/GIS_added_files/SSTs/SST_haulend2.csv")))

SST_range <- left_join(sets_alldata, SST_setbegin, by=c("TRIP_ID", "VESSEL_ID", "SET_NUM"))
SST_range <- left_join(SST_range, SST_setend, by=c("TRIP_ID", "VESSEL_ID", "SET_NUM"))
SST_range <- left_join(SST_range, SST_haulend, by=c("TRIP_ID", "VESSEL_ID", "SET_NUM"))
SST_range <- SST_range %>%
  select(TRIP_ID, VESSEL_ID, SET_NUM, SST_SETBEG, SST_SETEND, SST_2, SST_ENDHL)


SST_range$SST_RANGE = apply(SST_range[,4:7], 1,max) - apply(SST_range[,4:7], 1,min)
SST_range <- SST_range %>%
  select(TRIP_ID,VESSEL_ID,SET_NUM,SST_RANGE)

write_feather(SST_range, "Data/SST_range.feather")
