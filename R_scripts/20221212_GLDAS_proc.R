#' Processes downloaded NLDAS light data
#' @description This function processes downloaded NLDAS incoming shortwave radiation
#' data (w m-2) for a given Latitude and Longitude. Processing steps include:
#'
#' \itemize{
#'   \item Converting UTC time to local time
#'   \item Converting incoming shotwave radiation (w m-2) to PPFD (umol m-2 s-1)
#' }
#'
#' @param read_dir The read directory for downloaded files. For example, "C:/myfolder
#' @param save_dir The save directory for files to be placed in. For example, "C:/myfolder
#' @param Site The site name, for example "FL_ICHE2700"
#' @param Lat The site Latitude
#' @param Lon The site Longitude
#'
#' @return Returns a time series of incoming light data
#' @export

#===============================================================================
#Function for processing the downloaded data
#Created 11/27/2017
#===============================================================================
  GLDAS_proc <- function(read_dir, save_dir, Site, Lat, Lon){
    #Reading in the table, skipping the first 13 lines of header information
      setwd(read_dir)
      file_name <- paste(Site, "_GLDAS.asc", sep = "")
      gldas <- read.table(file_name, skip = 13, nrows = length(readLines(file_name))- 14)
          #colnames(gldas) <- c("Date", "hour_raw", "pressure")

          #file_name <- read.table(file.choose(),skip = 13)
          #gldas <- file_name
          
          colnames(gldas) <- c("DateTime", "Pressure")
          
          library(anytime)
          gldas$date2<-strptime(gldas$DateTime, format = '%Y-%m-%dT%H:%M:%S')
          
          #Adding in Year, DOY, and hour information
          gldas[, "Year"] <- as.numeric(format(gldas[, "date2"], format = "%Y", tz = "UTC"))
          gldas[, "DOY"] <- as.numeric(format(gldas[, "date2"], format = "%j", tz = "UTC"))
          gldas[, "Hour"] <- as.numeric(format(gldas[, "date2"], format = "%H", tz = "UTC"))
          
          #Selecting the final column
          final <- gldas[, c("DateTime", "Year", "DOY", "Hour", "Pressure")]
          
          #Writing the final output
          setwd(save_dir)
          write.csv(final, paste(Site, "_GLDAS_Pressure.csv", sep = ""), quote = FALSE, row.names = FALSE)
          
  } #End GLDAS_proc
  
### general Reno  
 GLDAS_proc("C:/Users/lkatona.BLASZCZAKLB4-PC/Desktop/GLDAS test",
            "C:/Users/lkatona.BLASZCZAKLB4-PC/Desktop/GLDAS test",
            "Reno",
            39.52963, -119.8138)

### Desktop/Dropbox
  ## Reno test
 GLDAS_proc("C:/Users/lkatona.BLASZCZAKLB4-PC/Dropbox/Truckee River Metabolism/GLDAS Pressure Data",
            "C:/Users/lkatona.BLASZCZAKLB4-PC/Dropbox/Truckee River Metabolism/GLDAS Pressure Data",
            "Reno",
            39.52963, -119.8138)






### Site-specific pressure data

GLDAS_proc("C:/Users/lkatona.BLASZCZAKLB4-PC/Dropbox/Truckee River Metabolism/GLDAS Pressure Data",
           "C:/Users/lkatona.BLASZCZAKLB4-PC/Dropbox/Truckee River Metabolism/GLDAS Pressure Data",
         "Marble Bluff",
         39.7773722, 
         -119.3375222)

GLDAS_proc("C:/Users/lkatona.BLASZCZAKLB4-PC/Dropbox/Truckee River Metabolism/GLDAS Pressure Data",
           "C:/Users/lkatona.BLASZCZAKLB4-PC/Dropbox/Truckee River Metabolism/GLDAS Pressure Data",
         "McCarran",
         39.51722, 
         -119.7397)

GLDAS_proc("C:/Users/lkatona.BLASZCZAKLB4-PC/Dropbox/Truckee River Metabolism/GLDAS Pressure Data",
           "C:/Users/lkatona.BLASZCZAKLB4-PC/Dropbox/Truckee River Metabolism/GLDAS Pressure Data",
         "Mogul",
         39.50713218, 
         -119.931864)

GLDAS_proc("C:/Users/lkatona.BLASZCZAKLB4-PC/Dropbox/Truckee River Metabolism/GLDAS Pressure Data",
           "C:/Users/lkatona.BLASZCZAKLB4-PC/Dropbox/Truckee River Metabolism/GLDAS Pressure Data",
         "North Truckee Drain",
         39.5204672, 
         -119.7010198)

GLDAS_proc("C:/Users/lkatona.BLASZCZAKLB4-PC/Dropbox/Truckee River Metabolism/GLDAS Pressure Data",
           "C:/Users/lkatona.BLASZCZAKLB4-PC/Dropbox/Truckee River Metabolism/GLDAS Pressure Data",
         "Painted Rock",
         39.58463448, 
         -119.4412872)

GLDAS_proc("C:/Users/lkatona.BLASZCZAKLB4-PC/Dropbox/Truckee River Metabolism/GLDAS Pressure Data",
           "C:/Users/lkatona.BLASZCZAKLB4-PC/Dropbox/Truckee River Metabolism/GLDAS Pressure Data",
         "Steamboat Creek",
         39.5129672, 
         -119.712409)

GLDAS_proc("C:/Users/lkatona.BLASZCZAKLB4-PC/Dropbox/Truckee River Metabolism/GLDAS Pressure Data",
           "C:/Users/lkatona.BLASZCZAKLB4-PC/Dropbox/Truckee River Metabolism/GLDAS Pressure Data",
         "Tracy",
         39.57222, 
         -119.4914)

GLDAS_proc("C:/Users/lkatona.BLASZCZAKLB4-PC/Dropbox/Truckee River Metabolism/GLDAS Pressure Data",
           "C:/Users/lkatona.BLASZCZAKLB4-PC/Dropbox/Truckee River Metabolism/GLDAS Pressure Data",
         "Wadsworth",
         39.74556, 
         -119.3222)

GLDAS_proc("C:/Users/lkatona.BLASZCZAKLB4-PC/Dropbox/Truckee River Metabolism/GLDAS Pressure Data",
           "C:/Users/lkatona.BLASZCZAKLB4-PC/Dropbox/Truckee River Metabolism/GLDAS Pressure Data",
         "Lockwood",
         39.50917, 
         -119.6494)

GLDAS_proc("C:/Users/lkatona.BLASZCZAKLB4-PC/Dropbox/Truckee River Metabolism/GLDAS Pressure Data",
           "C:/Users/lkatona.BLASZCZAKLB4-PC/Dropbox/Truckee River Metabolism/GLDAS Pressure Data",
         "Waltham",
         39.55657878, 
         -119.5532362)

