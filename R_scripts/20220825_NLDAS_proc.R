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
#Created 8/25/2022
#===============================================================================
  NLDAS_proc <- function(read_dir, save_dir, Site, Lat, Lon){
    #Reading in the table, skipping the first 40 lines of header information
    #and removing the last row which contains a calculated mean value
      setwd(read_dir)
      file_name <- paste(Site, "_NLDAS.asc", sep = "")
      nldas <- read.table(file_name, skip = 40, nrows = length(readLines(file_name)) - 41)
          colnames(nldas) <- c("Date", "hour_raw", "light")

    #Adding in date and time information
      #Extracting the hour information
        nldas[, "Time"] <- as.numeric(substr(nldas[,"hour_raw"], 1, 2))

      #Adding a POSIX time column
        nldas[, "pos_time"] <- as.POSIXct(paste(nldas[, "Date"], " ",
          as.matrix(sprintf("%02d", nldas[, "Time"])), sep = ""), format = "%Y-%m-%d %H",
          tz = "UTC")

      # #Converting into local time
      #   nldas[, "local_time"] <- format(nldas[, "pos_time"], tz = TZ_name)

      #Adding in Year, DOY, and hour information
        nldas[, "Year"] <- as.numeric(format(nldas[, "pos_time"], format = "%Y", tz = "UTC"))
        nldas[, "DOY"] <- as.numeric(format(nldas[, "pos_time"], format = "%j", tz = "UTC"))
        nldas[, "Hour"] <- as.numeric(format(nldas[, "pos_time"], format = "%H", tz = "UTC"))

      #Converting to PAR
        nldas[, "PAR"] <- nldas[, "light"] * 2.114

      #Selecting the final column
        final <- nldas[, c("Year", "DOY", "Hour", "PAR")]

      #Writing the final output
        setwd(save_dir)
        write.csv(final, paste(Site, "_NLDAS_PAR.csv", sep = ""), quote = FALSE, row.names = FALSE)

  } #End NLDAS_proc
  
### general Reno light data processing
### Be sure to adjust directories accordingly and consider whether you want  
### separate directories for downloaded and processed data
  
NLDAS_proc("C:/Users/leonk/Dropbox/Blaszczak-Lab-Metabolism-Code-Working-Files",
           "C:/Users/leonk/Dropbox/Blaszczak-Lab-Metabolism-Code-Working-Files",
           "Reno",
           39.52963, -119.8138)

