#############################################################################
# Data import and manipulation of netcdf eddy data from aviso
# Joseph Fader
# Feb 4, 2018


# load packages
library(ncdf4)
library(chron)
library(RColorBrewer)
library(tidyverse)
library(lubridate)
library(feather)

# set path and filename
ncpath <- "Data/"
ncname <- "eddy_trajectory_19930101_20170106"
ncfname <- paste(ncpath, ncname, ".nc", sep="")

# open a netCDF file
ncin <- nc_open(ncfname)
print(ncin)

## get each variable separate, indexed by observation number
## ncvar_get returns array format, convert to numeric vector

# get longitude and latitude
lon <- as.character(ncvar_get(ncin,"longitude"))
lon <- as.numeric(lon)
nlon  <- dim(lon)
head(lon)

lat <- as.character(ncvar_get(ncin,"latitude"))
lat <- as.numeric(lat)
nlat <- dim(lat)
head(lat)

print(c(nlon,nlat))

# get time - as.numeric is important for chron, will treat as julian days since origin
time <- as.numeric(ncvar_get(ncin,"time"))
tunits <- ncatt_get(ncin,"time","units")

# convert time -- split the time units string into fields
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])
eddy_date <- as.character(chron(time,origin=c(tmonth, tday, tyear), out.format = "m/d/y"))

# get amplitude
amplitude <- as.character(ncvar_get(ncin,"amplitude"))
amplitude <- as.numeric(amplitude)
#dlname <- ncatt_get(ncin,dname,"long_name")
#dunits <- ncatt_get(ncin,dname,"units")

# get cyclonic_type
cycl_type <- as.character(ncvar_get(ncin,"cyclonic_type"))
#dlname <- ncatt_get(ncin,dname,"long_name")
#dunits <- ncatt_get(ncin,dname,"units")

# get observation_number
obsnum <- as.vector(ncvar_get(ncin,"observation_number"))

# get speed_average
speed_avg<- as.character(ncvar_get(ncin,"speed_average"))
speed_avg <- as.numeric(speed_avg)

# get speed_radius
speed_rad <- as.character(ncvar_get(ncin,"speed_radius"))
speed_rad <- as.numeric(speed_rad)

# get track
track <- as.character(ncvar_get(ncin,"track"))
track <- as.numeric(track)

# get global attributes
title <- ncatt_get(ncin,0,"title")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
#references <- ncatt_get(ncin,0,"references")
#history <- ncatt_get(ncin,0,"history")
#Conventions <- ncatt_get(ncin,0,"Conventions")

nc_close(ncin)
ls()

### cbind all vectors into data frame
## default is to convert all strings as factors, must define as false to be able to make numeric later

eddy_full <- as.data.frame(cbind(obsnum, track, eddy_date, lat, lon, amplitude, cycl_type, speed_avg, speed_rad),stringsAsFactors=FALSE)
View(eddy_full[1:1000,])


# convert as needed
sapply(eddy_full, class)

eddy_full$DAY <- eddy_full$eddy_date
eddy_early$DAY <- mdy(eddy_early$DAY)
eddy_early$DAY <- as.Date(eddy_early$DAY)
eddy_early$DAY <- as.character.Date(eddy_early$DAY)
eddy_early$DAY <- format(eddy_early$DAY, "%m/%d/%Y")
eddy_full$eddy_date <- mdy(eddy_full$eddy_date)
eddy_full$lat <- as.numeric(eddy_full$lat)
eddy_full$lon <- as.numeric(eddy_full$lon)
eddy_full$amplitude <- as.numeric(eddy_full$amplitude)
eddy_full$cycl_type <- as.factor(eddy_full$cylc_type)
eddy_full$speed_avg <- as.numeric(eddy_full$speed_avg)
eddy_full$speed_rad <- as.numeric(eddy_full$speed_rad)

# filter out whatcha don need
eddy_Pacific <- eddy_full %>%
  filter(lat < 47 & lat > -15) %>%
  filter(lon >= 178 & lon <= 240) %>%
  filter(eddy_date > "2003-08-01")

eddy_2015 <- eddy_Pacific %>%
  filter(date >= "2015-01-01" & date < "2016-01-01")

#make subset for finishing up early years
eddy_early <- eddy_Pacific %>%
  filter(eddy_date < "2004-12-31")




## write out for GIS ##
write.csv(eddy_Pacific, file = "Data/eddy_Pacific.csv", quote = FALSE)
write.csv(eddy_2015, file = "Data/eddy_2015.csv", quote = FALSE, col.names = T)
write.csv(eddy_early, file = "Data/eddy_early.csv", quote = FALSE)

sapply(eddy_Pacific, class)

eddyPac <- read.csv("Data/eddy_Pacific.csv")
