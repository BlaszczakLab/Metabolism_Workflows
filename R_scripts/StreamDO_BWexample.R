# Example for Blackwood stream DO

#=========== Preliminaries
#rm(list=ls())
# load packages
library(tidyverse)
library(lubridate)
library(plotly)
library(devtools)
library(nrlmetab)
library(zoo)
library(suncalc)
library(readxl)
library(patchwork)
library(gridExtra)
library(dplyr)
library(reshape)
library(scales)

#===========================================
# Get and process high frequency sensor data
#===========================================
getwd()

rawdat = list.files(paste("./data_raw/StreamEx_BW/DO_StreamDat/",sep=""), full.names = T) %>%
  lapply(read_csv) %>%
  bind_rows()

### do timeseries object to clean do observations
do.ts <- rawdat %>%
  dplyr::rename(datetime = "Pacific.Standard.Time", do.obs = "Dissolved.Oxygen") %>%
  mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%d %H:%M:%S"))%>%
  select("datetime", "do.obs", "Battery", "Q")


do.ts <- subset(do.ts,
                datetime > "2021-04-30 00:00:00", # date time of firtst day of obs
                Q>0.7) # sensor quality minimum threshold

## option to shift time window:
# do.ts$datetime <- do.ts$datetime - hours(4)

### Water temptimeseries object to clean do observations
wtr.ts <- rawdat %>% select("Pacific.Standard.Time", "Temperature", "Battery", "Q") %>%
  dplyr::rename(datetime = "Pacific.Standard.Time", wtr = "Temperature") %>%
  mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%d %H:%M:%S"))

wtr.ts <- subset(wtr.ts,
                 datetime > "2021-04-30 00:00:00",
                 Q>0.7) # sensor quality minimum threshold

### Check the diurnal patterns:
# pick random 24 hour windows
tempcheck <- subset(wtr.ts,
                    datetime > '2021-06-16 00:00:00' & datetime < '2021-06-17 :00:00') # sensor quality minimum threshold

qplot(datetime, wtr, data = tempcheck, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("4 hours"))
## DO data might be 7 hours off so need a UTC correction...
# tempcheck$datetime <- tempcheck$datetime - hours(4)
# wtr.ts$datetime <- wtr.ts$datetime - hours(4)


### clean DO data
ggplot(data = do.ts,aes(x=datetime,y=do.obs)) + geom_line()
describe.ts(do.ts)
do.ts.clean <- trim.outliers(do.ts,width = 5,sd.dev = 3) # usually use 3, but the spikes might be poor.
ggplot(data = do.ts,aes(x=datetime,y=do.obs)) + geom_line() +
  geom_line(data=do.ts.clean,aes(x=datetime,y=do.obs),col="red")
do.ts.avg <- aggregate.data(data = do.ts.clean,time.step = 60)

### if data needs to be drift corrected:
# do.ts.drift <- drift.correction(dat = do.ts.avg,dat_lter = profile[,c(1:2)],var_dat = "do.obs",var_lter = "doobs_3.0")
# ggplot(data = do.ts.avg,aes(x=datetime,y=do.obs)) + geom_line() +
#   geom_line(data=do.ts.drift,aes(x=datetime,y=do.obs),col="blue") +
#   geom_point(data=profile,aes(x=datetime,y=doobs_3.0),color="red")

## A way of merging data in case the data is split in 2 in order to run different
## drifftcorrections.

# do.ts.drift <- rbind(do.ts.drift_series1,do.ts.drift_series2)

### clean wtr data
ggplot(data = ,aes(x=datetime,y=wtr)) + geom_line()
describe.ts(wtr.ts)
wtr.ts.clean <- trim.outliers(wtr.ts,width = 5,sd.dev = 3)
ggplot(data = wtr.ts,aes(x=datetime,y=wtr)) + geom_line() +
  geom_line(data=wtr.ts.clean,aes(x=datetime,y=wtr),col="red")
wtr.ts.avg <- aggregate.data(data = wtr.ts.clean,time.step = 60)

### add in climate:
## get data from https://synopticdata.com/
climate <- read_csv("./data_raw/StreamEx_BW/D9413.2022-04-01.csv") %>%
  dplyr::rename(datetime='Date_Time',
                Atemp='air_temp_set_1',
                wspeed='wind_speed_set_1',
                baro='pressure_set_1d')%>%
  mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%d %H:%M:%S")) %>%
  mutate(year=year(datetime),
         yday=yday(datetime),
         hour=hour(datetime)) %>%
  select(datetime,
         Atemp,
         baro,
         wspeed,
         hour,
         yday) # datetime in UTC

climate$datetime <-  (as.POSIXct(round_date(
  as.POSIXct(climate$datetime, format="%Y-%m-%d %H:%M:%S"),
  hour, unit="5 minutes")))


## Check the dates:
tempcheck <- subset(climate,
                    datetime > '2021-08-10 00:00:00' & datetime < '2021-08-11 :00:00') # sensor quality minimum threshold

qplot(datetime, Atemp, data = tempcheck, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("4 hours"))
# DO data might be 7 hours off so need a UTC correction...
tempcheck$datetime <- tempcheck$datetime - hours(7)
# climate$datetime <- climate$datetime - hours(7)

climate <- climate %>%
  select(datetime,
         baro) #

ggplot(data = ,aes(x=datetime,y=climate)) + geom_line()
describe.ts(climate)
baro.ts.clean <- trim.outliers(climate,width = 5,sd.dev = 3)
ggplot(data = climate,aes(x=datetime,y=baro)) + geom_line() +
  geom_line(data=baro.ts.clean,aes(x=datetime,y=baro),col="red")
baro.ts.avg <- aggregate.data(data = baro.ts.clean,time.step = 60)

### For this site need a second dataset for Solar radiation
clim2 <- read_csv("./data_raw/StreamEx_BW/TADC1.2022-04-01.csv") %>%
  dplyr::rename(datetime='Date_Time',
                Atemp='air_temp_set_1',
                par='solar_radiation_set_1',
                wspeed='wind_speed_set_1')%>%
  mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%d %H:%M:%S")) %>%
  mutate(year=year(datetime),
         yday=yday(datetime),
         hour=hour(datetime)) %>%
  select(datetime,
         Atemp,
         par,
         wspeed,
         hour,
         yday) # datetime in UTC


## Check the dates:
tempcheck <- subset(clim2,
                    datetime > '2021-08-10 00:00:00' & datetime < '2021-08-11 :00:00') # sensor quality minimum threshold

qplot(datetime, par, data = tempcheck, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("4 hours"))
# DO data might be 7 hours off so need a UTC correction...
tempcheck$datetime <- tempcheck$datetime - hours(8)
# clim2$datetime <- clim2$datetime - hours(8)

clim2 <- clim2 %>%
  select(datetime,
         par) #

ggplot(data = ,aes(x=datetime,y=par)) + geom_line()
describe.ts(clim2)
par.ts.clean <- trim.outliers(clim2,width = 5,sd.dev = 3)
ggplot(data = clim2,aes(x=datetime,y=par)) + geom_line() +
  geom_line(data=par.ts.clean,aes(x=datetime,y=par),col="red")
par.ts.avg <- aggregate.data(data = par.ts.clean,time.step = 60)

## Aggregate all data
dat <- do.ts.avg %>% #note in this case I am not using the drift correction -KL
  full_join(wtr.ts.avg) %>%
  mutate(year=year(datetime),
         yday=yday(datetime),
         hour=hour(datetime)+1) %>%
  full_join(par.ts.avg) %>%
  full_join(baro.ts.avg)

dat.na <- na.omit(dat)

# write.csv(x = dat.na, file = "./data_raw/StreamEx_BW/FinalInput/BWLInputs.csv", row.names = TRUE)
