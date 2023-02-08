#' Downloads GLDAS pressure data
#' @description This function downloads GLDAS pressure for a given Latitude and Longitude
#' @param save_dir The save directory for files to be placed in. For example, "C:/myfolder
#' @param Site The site name, for example "FL_ICHE2700"
#' @param Lat The site Latitude
#' @param Lon The site Longitude
#' @param startDate The starting date for the download (YYYY-MM-DD)
#'
#' @return Returns a time series of barometric pressure from the start
#' date to the most recent available data
#' @export

#===============================================================================
#Function for downloading GLDAS pressure data from 2000 - 2022 via data rods
#https://disc.gsfc.nasa.gov/information/tools?title=Hydrology%20Data%20Rods
#Created 12 December 2022
#===============================================================================

DL_GLDAS <- function(save_dir, Site, Lat, Lon, startDate){
#The initial string to build the URL
      http_string <- paste("https://hydro1.gesdisc.eosdis.nasa.gov/daac-bin/access/timeseries.cgi?variable=GLDAS2:GLDAS_NOAH025_3H_v2.1:Psurf_f_inst&startDate=2000-01-01T00&endDate=2022-12-31T23&location=GEOM:POINT(-119,%2050)&type=asc2")
      
    #Separating the date information
      start_split <- strsplit(startDate, "-")[[1]]
      #end_split <- strsplit(endDate, "-")[[1]]

    #Generating the URL
     # url <- paste(http_string, "&startDate=",
     #              start_split[1], "-",start_split[2], "-", start_split[3], "T00",
     #              "&endDate=",
     #              end_split[1], "-",end_split[2], "-", end_split[3], "T23",
     #              "&location=GEOM:POINT(", -119, ", %2050", 
     #              ")&type=asc2", sep = "")
      url <- paste(http_string, Lon, ", ", Lat, ")&startDate=", start_split[1], "-",
                   start_split[2], "-", start_split[3], "T00", "&type=asc2", sep = "")
      url2 <- "https://hydro1.gesdisc.eosdis.nasa.gov/daac-bin/access/timeseries.cgi?variable=GLDAS2:GLDAS_NOAH025_3H_v2.1:Psurf_f_inst&startDate=2000-01-01T00&endDate=2022-12-31T23&location=GEOM:POINT(-119,%2050)&type=asc2"


      #Downloading the data
      destfile <- paste(save_dir,"/", Site, "_GLDAS.asc", sep = "")

      
      
    #Error catch in case the page is inaccessible. A little inelegant at present...
      try_result <- try(download.file(url2, destfile), silent = FALSE)

      if(class(try_result) == "try-error") {file.remove(destfile)}

  } #End DL_NLDAS function
  
  
 DL_GLDAS("C:/Users/lkatona.BLASZCZAKLB4-PC/Desktop/GLDAS test",
          "Reno",
          39.52963, -119.8138,
          "2022-01-01")

## Desktop/Dropbox
DL_GLDAS("C:/Users/lkatona.BLASZCZAKLB4-PC/Dropbox/Truckee River Metabolism/GLDAS Pressure Data",
         "Reno",
         39.52963, -119.8138,
         "1980-01-01")

## site-specific pressure

DL_GLDAS("C:/Users/lkatona.BLASZCZAKLB4-PC/Dropbox/Truckee River Metabolism/GLDAS Pressure Data",
         "Marble Bluff",
         39.7773722, 
         -119.3375222,
         "1980-01-01")

DL_GLDAS("C:/Users/lkatona.BLASZCZAKLB4-PC/Dropbox/Truckee River Metabolism/GLDAS Pressure Data",
         "McCarran",
         39.51722, 
         -119.7397,
         "1980-01-01")

DL_GLDAS("C:/Users/lkatona.BLASZCZAKLB4-PC/Dropbox/Truckee River Metabolism/GLDAS Pressure Data",
         "Mogul",
         39.50713218, 
         -119.931864,
         "1980-01-01")

DL_GLDAS("C:/Users/lkatona.BLASZCZAKLB4-PC/Dropbox/Truckee River Metabolism/GLDAS Pressure Data",
         "North Truckee Drain",
         39.5204672, 
         -119.7010198,
         "1980-01-01")

DL_GLDAS("C:/Users/lkatona.BLASZCZAKLB4-PC/Dropbox/Truckee River Metabolism/GLDAS Pressure Data",
         "Painted Rock",
         39.58463448, 
         -119.4412872,
         "1980-01-01")

DL_GLDAS("C:/Users/lkatona.BLASZCZAKLB4-PC/Dropbox/Truckee River Metabolism/GLDAS Pressure Data",
         "Steamboat Creek",
         39.5129672, 
         -119.712409,
         "1980-01-01")

DL_GLDAS("C:/Users/lkatona.BLASZCZAKLB4-PC/Dropbox/Truckee River Metabolism/GLDAS Pressure Data",
         "Tracy",
         39.57222, 
         -119.4914,
         "1980-01-01")

DL_GLDAS("C:/Users/lkatona.BLASZCZAKLB4-PC/Dropbox/Truckee River Metabolism/GLDAS Pressure Data",
         "Wadsworth",
         39.74556, 
         -119.3222,
         "1980-01-01")

DL_GLDAS("C:/Users/lkatona.BLASZCZAKLB4-PC/Dropbox/Truckee River Metabolism/GLDAS Pressure Data",
         "Lockwood",
         39.50917, 
         -119.6494,
         "1980-01-01")

DL_GLDAS("C:/Users/lkatona.BLASZCZAKLB4-PC/Dropbox/Truckee River Metabolism/GLDAS Pressure Data",
         "Waltham",
         39.55657878, 
         -119.5532362,
         "1980-01-01")