library(dplyr)
library(readr)
library(tibble)
library(lubridate)
library(dygraphs)
library(tidyr)
library(tidyselect)
library(zoo)
library(xts)
library(pracma)
library(reshape2)
library(Hmisc)
library(ggplot2)
library(grid)
library(gridExtra)
library(wesanderson)
library(data.table)
library(RColorBrewer)
library(plyr)
library(stringr)
library(chron)
library(streamMetabolizer)

######################################################
## Splitting and interpolating barometer time series
########################################################

## Read in data
setwd("C:/Users/lkatona.BLASZCZAKLB4-PC/Dropbox/Truckee River Metabolism/GLDAS Pressure Data")

read_csv_filename <- function(filename){
  ret <- read.csv(filename)
  ret$Source <- filename #EDIT
  ret$Site <- sub(pattern="*_GLDAS_Pressure.csv", "", filename)
  ret
}

GLDS.data <- ldply(list.files(pattern = "csv"), function(filename) {
  dum = read_csv_filename(filename)
  return(dum)
})

GLDS.data_split <- split(GLDS.data, GLDS.data$Source)

## Convert DateTime
Create_DT <- function(x){
  x_1980 <- x[which(x$Year == "1980"),]
  x_1980$Date <- as.Date(x_1980$DOY, origin="1979-12-31")
  x_1981 <- x[which(x$Year == "1981"),]
  x_1981$Date <- as.Date(x_1981$DOY, origin="1980-12-31")
  x_1982 <- x[which(x$Year == "1982"),]
  x_1982$Date <- as.Date(x_1982$DOY, origin="1981-12-31")
  x_1983 <- x[which(x$Year == "1983"),]
  x_1983$Date <- as.Date(x_1983$DOY, origin="1982-12-31")
  x_1984 <- x[which(x$Year == "1984"),]
  x_1984$Date <- as.Date(x_1984$DOY, origin="1983-12-31")
  x_1985 <- x[which(x$Year == "1985"),]
  x_1985$Date <- as.Date(x_1985$DOY, origin="1984-12-31")
  x_1986 <- x[which(x$Year == "1986"),]
  x_1986$Date <- as.Date(x_1986$DOY, origin="1985-12-31")
  x_1987 <- x[which(x$Year == "1987"),]
  x_1987$Date <- as.Date(x_1987$DOY, origin="1986-12-31")
  x_1988 <- x[which(x$Year == "1988"),]
  x_1988$Date <- as.Date(x_1988$DOY, origin="1987-12-31")
  x_1989 <- x[which(x$Year == "1989"),]
  x_1989$Date <- as.Date(x_1989$DOY, origin="1988-12-31")
  x_1990 <- x[which(x$Year == "1990"),]
  x_1990$Date <- as.Date(x_1990$DOY, origin="1989-12-31")
  x_1991 <- x[which(x$Year == "1991"),]
  x_1991$Date <- as.Date(x_1991$DOY, origin="1990-12-31")
  x_1992 <- x[which(x$Year == "1992"),]
  x_1992$Date <- as.Date(x_1992$DOY, origin="1991-12-31")
  x_1993 <- x[which(x$Year == "1993"),]
  x_1993$Date <- as.Date(x_1993$DOY, origin="1992-12-31")
  x_1994 <- x[which(x$Year == "1994"),]
  x_1994$Date <- as.Date(x_1994$DOY, origin="1993-12-31")
  x_1995 <- x[which(x$Year == "1995"),]
  x_1995$Date <- as.Date(x_1995$DOY, origin="1994-12-31")
  x_1996 <- x[which(x$Year == "1996"),]
  x_1996$Date <- as.Date(x_1996$DOY, origin="1995-12-31")
  x_1997 <- x[which(x$Year == "1997"),]
  x_1997$Date <- as.Date(x_1997$DOY, origin="1996-12-31")
  x_1998 <- x[which(x$Year == "1998"),]
  x_1998$Date <- as.Date(x_1998$DOY, origin="1997-12-31")
  x_1999 <- x[which(x$Year == "1999"),]
  x_1999$Date <- as.Date(x_1999$DOY, origin="1998-12-31")
  x_2000 <- x[which(x$Year == "2000"),]
  x_2000$Date <- as.Date(x_2000$DOY, origin="1999-12-31")
  x_2001 <- x[which(x$Year == "2001"),]
  x_2001$Date <- as.Date(x_2001$DOY, origin="2000-12-31")
  x_2002 <- x[which(x$Year == "2002"),]
  x_2002$Date <- as.Date(x_2002$DOY, origin="2001-12-31")
  x_2003 <- x[which(x$Year == "2003"),]
  x_2003$Date <- as.Date(x_2003$DOY, origin="2002-12-31")
  x_2004 <- x[which(x$Year == "2004"),]
  x_2004$Date <- as.Date(x_2004$DOY, origin="2003-12-31")
  x_2005 <- x[which(x$Year == "2005"),]
  x_2005$Date <- as.Date(x_2005$DOY, origin="2004-12-31")
  x_2006 <- x[which(x$Year == "2006"),]
  x_2006$Date <- as.Date(x_2006$DOY, origin="2005-12-31")
  x_2007 <- x[which(x$Year == "2007"),]
  x_2007$Date <- as.Date(x_2007$DOY, origin="2006-12-31")
  x_2008 <- x[which(x$Year == "2008"),]
  x_2008$Date <- as.Date(x_2008$DOY, origin="2007-12-31")
  x_2009 <- x[which(x$Year == "2009"),]
  x_2009$Date <- as.Date(x_2009$DOY, origin="2008-12-31")
  x_2010 <- x[which(x$Year == "2010"),]
  x_2010$Date <- as.Date(x_2010$DOY, origin="2009-12-31")
  x_2011 <- x[which(x$Year == "2011"),]
  x_2011$Date <- as.Date(x_2011$DOY, origin="2010-12-31")
  x_2012 <- x[which(x$Year == "2012"),]
  x_2012$Date <- as.Date(x_2012$DOY, origin="2011-12-31")
  x_2013 <- x[which(x$Year == "2013"),]
  x_2013$Date <- as.Date(x_2013$DOY, origin="2012-12-31")
  x_2014 <- x[which(x$Year == "2014"),]
  x_2014$Date <- as.Date(x_2014$DOY, origin="2013-12-31")
  x_2015 <- x[which(x$Year == "2015"),]
  x_2015$Date <- as.Date(x_2015$DOY, origin="2014-12-31")
  x_2016 <- x[which(x$Year == "2016"),]
  x_2016$Date <- as.Date(x_2016$DOY, origin="2015-12-31")
  x_2017 <- x[which(x$Year == "2017"),]
  x_2017$Date <- as.Date(x_2017$DOY, origin="2016-12-31")
  x_2018 <- x[which(x$Year == "2018"),]
  x_2018$Date <- as.Date(x_2018$DOY, origin="2017-12-31")
  x_2019 <- x[which(x$Year == "2019"),]
  x_2019$Date <- as.Date(x_2019$DOY, origin="2018-12-31")
  x_2020 <- x[which(x$Year == "2020"),]
  x_2020$Date <- as.Date(x_2020$DOY, origin="2019-12-31")
  x_2021 <- x[which(x$Year == "2021"),]
  x_2021$Date <- as.Date(x_2021$DOY, origin="2020-12-31")
  x_2022 <- x[which(x$Year == "2022"),]
  x_2022$Date <- as.Date(x_2022$DOY, origin="2021-12-31")
  
  
  Recom <- rbind(x_1980, x_1981, x_1982, x_1983, x_1984, x_1985, x_1986,
                 x_1987, x_1988, x_1989, x_1990, x_1991, x_1992, x_1993, x_1994,
                 x_1995, x_1996, x_1997, x_1998, x_1999, 
                 x_2000, x_2001, x_2002, x_2003, x_2004, x_2005, x_2006,
                 x_2007, x_2008, x_2009, x_2010, x_2011, x_2012, x_2013, x_2014,
                 x_2015, x_2016, x_2017, x_2018, x_2019, x_2020, x_2021, x_2022)
  Recom$DateTime <- as.POSIXct(paste(Recom$Date, Recom$Hour), format="%Y-%m-%d %H")
  
  R <- Recom[,c("DateTime","Pressure")]
  R$DateTime <- force_tz(R$DateTime, 'Etc/GMT-7')
  
  ## Create a dataset with the appropriate time values that you want
  spread_ts = function(x, samp_freq){
    re = regexec("([0-9]+)([A-Z])",samp_freq)[[1]]
    if(-1%in%re){
      stop("Please enter a correct string")
    }else{
      ml = attr(re,"match.length")
      nn = as.numeric(substr(samp_freq, re[2], ml[2]))
      uu = substr(samp_freq, re[3], ml[1])
      if(uu=="D"){td = 24*60*60*nn
      }else if(uu=="H"){td = 60*60*nn
      }else if(uu=="M"){td = 60*nn
      }else if(uu=="S"){td = nn
      }else{stop("Please enter a correct string")}
    }
    seq(x[1], x[length(x)], by=td)
  }
  
  ## Want 15 minute data
  #sampfreq = "15M"
  
  ## Want 1 HR data
  sampfreq = "1H"
  # Generate a full timeseries at a given interval
  DateTimeNew <- spread_ts(R$DateTime, sampfreq)
  xspread <- tibble(DateTime=DateTimeNew)
  # Create a new table with NAs for missing time steps
  Rnew <- merge(R,xspread,by="DateTime", all.y=TRUE)
  
  ## Use Approx
  Rnew$Pressure_filled <- na.approx(Rnew$Pressure)
  return(Rnew)
}

## Apply to all
GLDS.data_spread <- lapply(GLDS.data_split, function(x) Create_DT(x))

## Recombine
GLDS_All <- ldply(GLDS.data_spread, data.frame)

## add in site name
GLDS_All$Site <- sub(pattern="*_GLDAS_Pressure.csv", "", GLDS_All$.id)

## Visualize
vis_spread <- function(x){
  Rnew_xts <- as.xts(x$Pressure_filled, order.by = x$DateTime)
  dygraph(Rnew_xts)%>% dyAxis("y", valueRange= c(85000, 88000))  
}

vis_spread(GLDS.data_spread)

## Export
#names(GLDS.data_spread)
#GLDS.data_spread <- GLDS.data_spread[,c("DateTime","Pressure_filled")]

write.csv(GLDS_All, "GLDAS_interp_1HR.csv")




