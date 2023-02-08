#' Downloads NLDAS light data
#' @description This function downloads NLDAS incoming shortwave radiation data
#' (w m-2) for a given Latitude and Longitude.
#'
#' @param save_dir The save directory for files to be placed in. For example, "C:/myfolder
#' @param Site The site name, for example "FL_ICHE2700"
#' @param Lat The site Latitude
#' @param Lon The site Longitude
#' @param startDate The starting date for the download (YYYY-MM-DD)
#'
#' @return Returns a time series of incoming shortwave solar radiation from the start
#' date to the most recent available data
#' @export

#===============================================================================
#Function for downloading NLDAS light data via data rods
#https://disc.gsfc.nasa.gov/information/tools?title=Hydrology%20Data%20Rods
#Updated 8/25/2022
#===============================================================================
  DL_NLDAS <- function(save_dir, Site, Lat, Lon, startDate){
    #The initial string to build the URL
      http_string <- paste("http://hydro1.gesdisc.eosdis.nasa.gov/daac-bin/access/timeseries.cgi?variable=NLDAS:NLDAS_FORA0125_H.002:DSWRFsfc&location=GEOM:POINT(")

    #Separating the date information
      start_split <- strsplit(startDate, "-")[[1]]

    #Generating the URL
      url <- paste(http_string, Lon, ", ", Lat, ")&startDate=", start_split[1], "-",
        start_split[2], "-", start_split[3], "T00", "&type=asc2", sep = "")

    #Downloading the data
      destfile <- paste(save_dir, "/", Site, "_NLDAS.asc", sep = "")

    #Error catch in case the page is inaccessible. A little inelegant at present...
      try_result <- try(download.file(url, destfile), silent = FALSE)

      if(class(try_result) == "try-error") {file.remove(destfile)}

  } #End DL_NLDAS function
  

### general Reno area download
### be sure to adjust your directory and download start accordingly  
  
DL_NLDAS("C:/Users/leonk/Dropbox/Blaszczak-Lab-Metabolism-Code-Working-Files",
         "Reno",
         39.52963, -119.8138,
         "2022-01-01")

