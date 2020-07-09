# function to grab rainfall data from ftp and d2w and add it to BaSaR rain data base "rainDB.txt".
updateRainDB <- function(rawdir,
                         rainDBname,
                         tBeg,
                         tEnd,
                         tFalseRainName,
                         login,
                         D2Wsid,
                         summerTime,
                         skip,
                         overwriteOldDB){
  # the function downloads a user-defined time period (days) and adds this data to the corresponding
  # rows of rainDB.
  # data sources are KWB ftp server for BWB gauges (Owd, KöpI, Joh, BlnX, BlnIV, BlnXI) and
  # Nivus device2web (d2w) for KWB gauge in Bruno-B?rgel-Weg
  # time staps in rainDB are always in winter time -> no winter/summer time change
  # times with false or dubious data are NOT automatically adjusted or ignored by the function, it
  # simply downloads whatever is in the source data (ftp, d2w). the user must identify false values
  # and modify them manually in rainDB. then, the time stamps where the false data were identified
  # are entered in tFalseRain. this will make the function avoid replacing those values with the
  # downloaded ones in case the user downloads the same time period again
  
  # column names in internal rainDB come from rainDB on disk. 
  # column name for KWB rain gauge must be 'KWB'
  # first column in all data sources must be time stamps. remaining columns contain rainfall
  # in the different rain gauges
  # decimal separator in BWB and KWB is ',', and is changed to '.' by function
  
  require(dplyr)
  require(RCurl)
  
  # control locale for the name of months in German
  if(Sys.getlocale(category="LC_TIME") != "German_Germany.1252")
    Sys.setlocale(category="LC_TIME", locale="German_Germany.1252")
  
  # read and format text file with time stamps of adjusted rain data
  tFalseRainPath <- paste(rawdir, tFalseRainName, sep='')
  tFalseRain <- scan(tFalseRainPath, what="character", sep="\n", quiet=TRUE)
  tFalseRain <- paste0(tFalseRain, ":00")
  tFalseRain <- format(as.POSIXct(tFalseRain,
                                  format="%d.%m.%Y %H:%M:%S",
                                  tz="Etc/GMT-1"),
                       format="%Y-%m-%d %H:%M")

  # read rainDB
  rdbPath <- paste(rawdir, rainDBname, sep='')
  rainDB <- read.table(rdbPath,
                       sep=";",
                       header=TRUE,
                       encoding="UTF-8",
                       colClasses=c("character", rep("numeric", times=3)),
                       stringsAsFactors=FALSE)
  
  # read BWB gauges for user-defined time window, format data and add to rainDB
  if(skip != "BWB"){
    
    # check ftp server for data availability
    {
      # make user time window
      userTW <- seq(from=as.Date(tBeg, format="%Y%m%d"),
                    to=as.Date(tEnd, format="%Y%m%d"),
                    by=1)+1 # +1 because on ftp server, rain data for day i are in file named i+1

      # read list of available files
      availDates <- unlist(strsplit(getURL('ftp://ftp.kompetenz-wasser.de/',
                                           userpwd=login),
                                    "\r*\n"))
      xx <- as.list(availDates[4:length(availDates)])
      availDates <- unlist(lapply(X=xx, FUN=substr, start=nchar(xx)-14, stop=nchar(xx)-9))
      availDates <- lapply(X=availDates,FUN=as.Date, format="%y%m%d")
      availDates <- do.call("c", availDates)

      # compare user-defined time frame with available dates
      index <- match(userTW, availDates)
      index <- index[!is.na(index)]
      availUser <- availDates[index]

      if(length(index)==0) stop("desired data are unavailable on ftp server")
    }

    # download available data from user time window and add to output data.frame
    {
      # make empty data frame for holding downloaded data from user-defined time window
      downloadedRain <- data.frame()

      # loop over available dates
      for(i in 1:length(availUser)){
        
        # data frame to temporarily store daily data 
        # (since this is how they're stored on ftp server)
        rainDay <- as.data.frame(matrix(nrow=0, 
                                        ncol=ncol(rainDB)-1,
                                        dimnames=list(NULL, 
                                                      names(rainDB)[names(rainDB) != 'KWB'])))

        # re-format date for pasting onto download string
        i2 <- format(availUser[i], "%y%m%d", tz="Etc/GMT-1")
        i2 <- format(availUser[i], "%y%m%d", tz="Europe/Berlin")

        # make download string
        read_url <- paste0('ftp://ftp.kompetenzwasser.de/',
                           "Regenschreiber_",
                           i2,
                           "_0810.txt")

        # read file from ftp server
        url_content <- getURL(read_url,
                              userpwd=login)

        # split content into lines
        content <- strsplit(url_content, split="\n")[[1]]

        # grab and split column names (1st element), removing 1st column name (time stamps)
        cnFTP <- content[1]
        cnFTP <- strsplit(x=cnFTP, split='\t')[[1]][-1]
        
        # remove whitespace from column names on downloaded data and rainDB
        cnFTP <- gsub(pattern=" ", replacement="", x=cnFTP)
        cnRainDB <- gsub(pattern=" ", replacement="", x=names(rainDB)[-1])

        # rempove 1st element (headers)
        content <- content[-1]

        # make data.frame for current day, including only rain gauges
        # contained in the version of rainDB on disk:

        # determine which columns to use (column with KWB rain gauge is left out by this,
        # because it doesn't exist in BWB data)
        useCol <- match(cnRainDB, cnFTP)
        useCol <- useCol[!is.na(useCol)]

        # since BWB data does not always have the same number of rows, loop length
        # must be determined by finding the row number of the last time stamp of
        # the current day

        # make vector of downloaded dates
        downloadedDates <- rep(NA, times=length(content))
        for(j in 1:length(content)){
          contentj           <- content[j]
          downloadedDates[j] <- strsplit(contentj, split="\t")[[1]][1]
        }
        downloadedDates <- as.Date(format(as.POSIXct(downloadedDates,
                                                     format="%d-%b-%y %H:%M:%S"),
                                          "%Y-%m-%d"), format="%Y-%m-%d")

        # current date
        datei <- as.Date(availUser[i], format="%Y-%m-%d")

        # current date. "-1" due to the day shift in the BWB data base
        datei <- as.Date(availUser[i], format="%Y-%m-%d")-1

        # row number of last time stamp of current day
        rowsdatei <- which(downloadedDates==datei)

        # if user wants to download last night's rain (availUser[i] == Sys.Date()),
        # then include all rows in content, otherwise include only rows until end of
        # current day
        if(availUser[i] != Sys.Date()){
          loopRows <- rowsdatei
        } else {
          loopRows <- 1:length(content)
        }

        # split content
        for(j in 1:length(loopRows)){
          contentsplt  <- strsplit(content[loopRows[j]], split="\t")[[1]]
          rowj         <- contentsplt[c(2, useCol)]
          rainDay[j, ] <- rowj
        }

        # remove potential NAs due to missing rows in rainDay
        rainDay <- filter(rainDay, !is.na(dateTime))

        # add new downloaded day
        downloadedRain <- rbind(downloadedRain, rainDay)
      }

      # change decimal separator from ',' to '.'
      downloadedRain <- data.frame(lapply(X=downloadedRain,
                                          FUN=function(a){gsub(pattern=',', 
                                                               replacement='.',
                                                               x=a)}),
                                   stringsAsFactors=FALSE)
      
      # bring time axis of downloaded BWB data to winter time (bringing summer time stamps
      # (tz="Europe/Berlin") to winter time (tz="Etc/GMT-1"). this is not necessary for KWB
      # rain gauge, since logger configuration is always winter time
      
      # for each year, make 5-min. time stamps of summer time in tz Berlin/Europe 
      # using range given by user. needed to shift BWB data to winter time
      tsSummer <- list()
      tsSummer <- apply(X=summerTime,
                        MARGIN=1,
                        FUN=function(a){
                          format(
                            seq(from=as.POSIXct(a['start'],
                                                format="%d-%b-%y %H:%M:%S %z",
                                                tz="Europe/Berlin"), 
                                to=as.POSIXct(a['end'],
                                              format="%d-%b-%y %H:%M:%S %z",
                                              tz="Europe/Berlin"), 
                                by=300),
                            tz="Europe/Berlin",
                            format="%d-%b-%y %H:%M:%S")
                        })
      names(tsSummer) <- summerTime$year
      tsSummer <- lapply(X=tsSummer, FUN=as.character)
      tsSummer <- unlist(tsSummer, use.names=FALSE)
      
      # find summer time stamps in downloaded BWB data (downloadedRain), 
      # checking against the summer time stamps of the corresponding years (tsSummer)
      
      # find year(s) of downloadedRain
      yearsdownloaded <- as.character(unique(format(as.POSIXct(downloadedRain$dateTime,
                                                               tz="Europe/Berlin",
                                                               format="%d-%b-%y %H:%M:%S"),
                                                    format="%Y")))
      
      # make index vector for all time stamps in downloadedRain
      indexAll    <- 1:nrow(downloadedRain)
      
      # try to match for summer time stamps in downloadedRain
      indexSummer <- match(tsSummer, downloadedRain$dateTime)
      indexSummer <- indexSummer[!is.na(indexSummer)]
      
      # did user download any summer time stamps?
      if(length(indexSummer) > 0){
        
        # if so, winter indexes are all indexes that are not summer. this includes the case
        # where all indexes are summer, in which case indexWinter has length 0 (integer(0))
        indexWinter <- indexAll[-indexSummer]
        
      } else { # if there are no summer indexes, there are only winter indexes
        
        indexWinter <- indexAll
      }
      
      # append "+0200" to summer time stamps and "+0100" to winter time stamps, in preparation
      # for conversion to tz="Etc/GMT-1"
      
      # append "+0200" to all summer time stamps, if they exist
      # (downloaded data may be only in winter)
      if(length(indexSummer) > 0){
        downloadedRain$dateTime[indexSummer] <-
          paste(downloadedRain$dateTime[indexSummer], "+0200")
      }
      
      # append "+0100" to all winter time stamps, if they exist
      # (downloaded data may be only in summer)
      if(length(indexWinter) > 0){
        downloadedRain$dateTime[indexWinter] <-
          paste(downloadedRain$dateTime[indexWinter], "+0100")
      }
      
      # convert to tz="Etc/GMT-1" and re-format to remove seconds (to match dateTime in rainDB)
      
      # format as Etc/GMT-1 to remove winter/summer time shift
      downloadedRain$dateTime <- format(as.POSIXct(downloadedRain$dateTime,
                                                   format="%d-%b-%y %H:%M:%S %z"),
                                        tz="Etc/GMT-1")
      
      # format dateTime as Y-m-d H:M
      downloadedRain$dateTime <- as.character(format(as.POSIXct(downloadedRain$dateTime,
                                                                tz="Etc/GMT-1",
                                                                format="%Y-%m-%d %H:%M:%S"),
                                                     tz="Etc/GMT-1",
                                                     format="%Y-%m-%d %H:%M"))
      
      # add downloadedRain to rainDB. ignore rows with false rain (corrected manually 
      # in rainDB):
      
      # match columns in downloadedRain and rainDB, removing dateTime column
      useCol2 <- match(names(downloadedRain)[-1], names(rainDB))
      
      # did user download time window including tFalseRain?
      indexFalse <- match(tFalseRain, downloadedRain$dateTime)
      indexFalse <- indexFalse[!is.na(indexFalse)]
      
      # if so, remove those rows from the downloaded data and add the remaining rows 
      # to rainDB
      if(length(indexFalse) > 0){ 
        downloadedRain <- downloadedRain[-indexFalse, ]
        index <- match(downloadedRain$dateTime, rainDB$dateTime)
        rainDB[index, useCol2] <- downloadedRain[, -1]
        
        # if not, simply add downloaded data to rainDB
      } else {               
        index <- match(downloadedRain$dateTime, rainDB$dateTime)
        rainDB[index, useCol2] <- downloadedRain[, -1]
      }
    }
  }
  
  # read KWB rain gauge in Bruno-Bürgel-Weg for user-defined time window, format data and add to rainDB
  if(skip != "KWB")
  {
    # download d2w data
    t2 <- as.POSIXct(tEnd, format="%Y%m%d", tz="Etc/GMT-1")
    t1 <- as.POSIXct(tBeg, format="%Y%m%d", tz="Etc/GMT-1")
    tPeriod <- as.numeric(t2 - t1)
    tPeriod <- tPeriod+1
    
    url <- paste0("https://www.nivulog.nivus-d2w.com/download.tsv?","sid=",
                  D2Wsid,
                  "&start=",
                  format(t1, format="%d.%m.%Y", tz="Etc/GMT-1"),
                  "%2000:00:00&",
                  "periode=",
                  tPeriod,
                  "&mode=0&separator=9&nrformat=-1&timeformat=-1&dateformat=-1&sitenames=0&site0=A8FE7F82188FFB73&tag0=00005200&field0=ch1&cont0=6&tp0=-5&units0=")
    tempFile <- paste0(rawdir, "temp_d2w.txt")
    
    # download data onto temporary file
    download.file(url, tempFile, quiet=TRUE)
    
    # check data downloaded from d2w. use first item of downloaded file to check
    char1 <- scan(tempFile, what="character", nmax=1, quiet=TRUE)
    d2wOK <- grepl(pattern="?at*/*", x = char1)
    
    # if data downloaded from d2w are ok, read and format downloaded file, and add data to 
    # rainDB
    if(d2wOK){
      
      # read and delete temporary file
      tempKWB <- read.table(tempFile,
                            header=TRUE,
                            sep="\t",
                            colClasses=c("character", "numeric"),
                            col.names=c('dateTime', 'KWB'),
                            dec=",",
                            stringsAsFactors=FALSE)
      file.remove(tempFile)
      
      # format time stamps
      tempKWB$dateTime <- as.character(format(as.POSIXct(as.character(tempKWB$dateTime),
                                                     format="%d.%m.%Y %H:%M:%S",
                                                     tz="Etc/GMT-1"),
                                          "%Y-%m-%d %H:%M"))
      
      # add tempBBW to rainDB. ignore rows with false rain (were corrected manually in 
      # rainDB):
      
      # match columns in downloadedRain and rainDB, removing dateTime column
      useCol3 <- match(names(tempKWB)[-1], names(rainDB))
      
      # did user download time window including tFalseRain?
      indexFalse <- match(tFalseRain, tempKWB$dateTime)
      indexFalse <- indexFalse[!is.na(indexFalse)]
      
      # if so, remove those rows from the downloaded data and add the remaining 
      # rows to rainDB
      if(length(indexFalse) > 0){
        tempKWB  <- tempKWB[-indexFalse, ]
        index <- match(tempKWB$dateTime, rainDB$dateTime)
        rainDB[index, useCol3] <- tempKWB[, -1]
        
        # if not, simply add downloaded data to rainDB
      } else {               
        index <- match(tempKWB$dateTime, rainDB$dateTime)
        index <- index[!is.na(index)]
        rainDB[index, useCol3] <- tempKWB[, -1]
      }
      
    } else {
      stop("file downloaded from d2w seems strange...please check temp_d2w.txt\nmaybe new D2Wsid required")
    }
  }
  
  # write new rainDB
  if(overwriteOldDB)
  {
    write.table(rainDB,
                file=paste0(rawdir, rainDBname),
                quote=FALSE,
                sep=";",
                row.names=FALSE,
                fileEncoding="UTF-8")
  } else {
    write.table(rainDB,
                file=paste0(rawdir, "rainDB_new.txt"),
                quote=FALSE,
                sep=";",
                row.names=FALSE,
                fileEncoding="UTF-8")
  }
}

# read BaSaR rainfall data base
readRain <- function(rawdir, rainDBname){
  
  # make path to file
  rdbPath <- paste(rawdir, rainDBname, sep='')
  
  # check that 1st column is dateTime
  dateTime1 <- scan(rdbPath, what="character", nmax=1, quiet=TRUE)
  if(strsplit(dateTime1, split=';')[[1]][1] != 'dateTime')
    stop('First column of rain data file must contain time stamps (%Y-%m-%d %H:%M)\nand have column name "dateTime"')
  
  rain <- read.table(rdbPath,
                     sep=";",
                     header=TRUE,
                     encoding="UTF-8",
                     colClasses="character")
  
  rain$dateTime <- as.POSIXct(rain$dateTime,
                              format="%Y-%m-%d %H:%M",
                              tz="Etc/GMT-1")
  
  rain[2:ncol(rain)] <- apply(rain[2:ncol(rain)], FUN=as.numeric, MARGIN=2)
  
  return(rain)
}

# look at rainfall for each site, multiple gauges
checkRain <- function(rainData, tBeg, tEnd, dt, dy, gauges, cols, pch, lty){
  # filter data
    tBeg <- as.POSIXct(tBeg, format="%Y-%m-%d %H:%M", tz="Etc/GMT-1")
    tEnd <- as.POSIXct(tEnd, format="%Y-%m-%d %H:%M", tz="Etc/GMT-1")
    rainSel <- rainData[rainData$dateTime >= tBeg & rainData$dateTime <= tEnd, 
                        c('dateTime', gauges)]
  
    # make axes
    tAx <- seq(tBeg, tEnd, by=dt)
    rainMax <- max(rainSel[2:ncol(rainSel)], na.rm=TRUE)
    rainAx <- seq(0, rainMax, by=dy)
  
    # plot
    windows(height=4, width=7)
    par(mar=c(3,4,1,3))
    plot(rainSel$dateTime, rainSel[[2]], xlim=c(tBeg, tEnd), type="p", pch=NA,
         axes=FALSE, xlab="", ylab="", main=,
         ylim=c(0, rainMax))
    
    for(i in 1:length(gauges)){
      lines(rainSel[, c('dateTime', gauges[i])],
            col=cols[i], pch=pch[i])
    }
    legend(x=par('usr')[2]-(par('usr')[2]-par('usr')[1])*0.3, 
           y=par('usr')[4]-(par('usr')[4]-par('usr')[3])*0.1,
           legend=gauges,
           col=col,
           lty=lty,
           pch=pch)
    axis(1, at=tAx, labels=format(tAx, format="%H:%M\n%d-%m"), padj=.45, cex.axis=1.2)
    axis(2, las=2, at=rainAx, labels=round(rainAx, digits=1))
    mtext('Rainfall [mm/5min.]', side=2, line=2.5)
    box()
}

# download temperature from DWD
updateWeatherDB <- function(rawdir, dbName, 
                            dwdStationID, dwdCols,
                            overwriteOldDB){
  
  # as in rainDB, first column must be time stamps
  # dwdCols must be in same order as in target data base (db)
  
  # set working directory
  setwd(rawdir)
  
  # read db on disk
  db <- read.table(dbName, 
                   header=TRUE,
                   sep=';',
                   colClasses='character')
  
  # format columns
  db[2:ncol(db)] <- apply(X=db[2:ncol(db)], 
                  MARGIN=2,
                  FUN=as.numeric)
  
  # download data from DWD ftp Server
  if(grepl(pattern='temp', x=dbName)){
    url <- paste('ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/10_minutes/air_temperature/recent/10minutenwerte_TU_00',
                 dwdStationID, '_akt.zip',
                 sep='')
  } else {
    if(grepl(pattern='wind', x=dbName)){
      url <- paste('ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/10_minutes/wind/recent/10minutenwerte_wind_00', 
                   dwdStationID, '_akt.zip',
                   sep='')
    } else {
      if(grepl(pattern='solar', x=dbName)){
        url <- paste('ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/10_minutes/solar/recent/10minutenwerte_SOLAR_00',
                     dwdStationID, '_akt.zip',
                     sep='')
      } else {
        stop('dbName must contain either "wind", "temp" or "solar"')
      }
    }
  }
  
  destfile <- 'download.zip'
  download.file(url, destfile, quiet=TRUE)
  unzip('download.zip', files = NULL, list = FALSE, overwrite = TRUE,
        junkpaths = FALSE, exdir = getwd(), unzip = "internal",
        setTimes = FALSE)
  file.remove('download.zip')
  unzipped = list.files(pattern="produkt_")
  
  # read downloaded table 
  downloaded <- read.table(unzipped,
                           header=TRUE,
                           sep=';',
                           colClasses='character',
                           stringsAsFactors=FALSE)
  
  # remove unzipped file
  file.remove(unzipped)
  
  # leave only user-selected columns in downloaded
  downloaded <- downloaded[, dwdCols]
  
  # 
  names(downloaded) <- names(db)
  
  # format dateTime
  downloaded[[1]] <- as.character(format(as.POSIXct(downloaded[[1]],
                                                    format="%Y%m%d%H%M", 
                                                    tz="Etc/GMT-1"), 
                                         "%Y-%m-%d %H:%M", 
                                         tz="Etc/GMT-1"))
  
  # match time stamps between db on disk and downloaded in order to
  # keep only the rows in donwloaded that match db
  rowIndex <- match(db$dateTime, downloaded[[1]])
  rowIndex <- rowIndex[!is.na(rowIndex)]
  downloaded <- downloaded[rowIndex, ]
  
  # assign the new downloaded to the corresponding rows in db
  index <- match(downloaded$dateTime, db$dateTime)
  index <- index[!is.na(index)]
  db[index, -1] <- downloaded[, -1]
  
  # write new tempDB
  if(overwriteOldDB){
    write.table(db,
                file=paste(rawdir, dbName, sep=''),
                quote=FALSE,
                sep=";",
                row.names=FALSE,
                fileEncoding="UTF-8")
  } else {
    write.table(db,
                file=paste(rawdir, 'new_',  dbName, sep=''),
                quote=FALSE,
                sep=";",
                row.names=FALSE,
                fileEncoding="UTF-8")
  }
}




# plot and compute statistics for weather data, filtering out time intervals without rain
# within the event. tBeg and tEnd are the start and end time of the full event, not the
# individual parts
checkWeather <- function(tBeg, tEnd, dt,
                         rainDB, windDB, tempDB,
                         rainGaugeBBW, rainGaugeBBR,
                         rainScale)
{
  # format start and end times
  tEnd <- as.POSIXct(tEnd, format = "%Y-%m-%d %H:%M", tz="Etc/GMT-1")
  tBeg <- as.POSIXct(tBeg, format = "%Y-%m-%d %H:%M", tz="Etc/GMT-1")

  # format wind and temperature dateTime
  {
    windDB$dateTime <- as.POSIXct(windDB$dateTime,
                                  tz="Etc/GMT-1",
                                  format="%Y-%m-%d %H:%M:%S")
    tempDB$dateTime <- as.POSIXct(tempDB$dateTime,
                                  tz="Etc/GMT-1",
                                  format = "%Y-%m-%d %H:%M:%S")
  }

  # grab only desired event
  {
    rainEvent    <- filter(rainDB, dateTime >= tBeg & dateTime <= tEnd)
    windEvent    <- filter(windDB, dateTime >= tBeg & dateTime <= tEnd)
    tempEvent    <- filter(tempDB, dateTime >= tBeg & dateTime <= tEnd)
    rainBBWevent <- dplyr::select(rainEvent, dateTime, rainGaugeBBW)
    rainBBRevent <- dplyr::select(rainEvent, dateTime, rainGaugeBBR)
    windBBRevent <- dplyr::select(windEvent, dateTime, speed=speed_BBR, direction=direction_BBR)
    windBBWevent <- dplyr::select(windEvent, dateTime, speed=speed_BBW, direction=direction_BBW)
    tempBBRevent <- dplyr::select(tempEvent, dateTime, tBBR=temperature_BBR)
    tempBBWevent <- dplyr::select(tempEvent, dateTime, tBBW=temperature_BBW)
  }

  # bring all data to same time axis though linear interpolation
  {
    intWspeedBBRevent <- as.data.frame(approx(x=windBBRevent$dateTime,
                                              y=windBBRevent$speed,
                                              xout=rainBBRevent$dateTime))
    intWspeedBBWevent <- as.data.frame(approx(x=windBBWevent$dateTime,
                                              y=windBBWevent$speed,
                                              xout=rainBBWevent$dateTime))
    intWdirBBRevent <- as.data.frame(approx(x=windBBRevent$dateTime,
                                            y=windBBRevent$direction,
                                            xout=rainBBRevent$dateTime))
    intWdirBBWevent <- as.data.frame(approx(x=windBBWevent$dateTime,
                                            y=windBBWevent$direction,
                                            xout=rainBBWevent$dateTime))
    intTempBBRevent <- as.data.frame(approx(x=tempBBRevent$dateTime,
                                            y=tempBBRevent$tBBR,
                                            xout=rainBBRevent$dateTime))
    intTempBBWevent <- as.data.frame(approx(x=tempBBWevent$dateTime,
                                            y=tempBBWevent$tBBW,
                                            xout=rainBBWevent$dateTime))
  }

  # grab temperature and wind from rows where rain>0
  {
    indexBBR <- which(pull(rainBBRevent, rainGaugeBBR) > 0)
    wsBBR    <- intWspeedBBRevent[indexBBR, ]
    wdBBR    <- intWdirBBRevent[indexBBR, ]
    tBBR     <- intTempBBRevent[indexBBR, ]

    indexBBW <- which(pull(rainBBWevent, rainGaugeBBW) > 0)
    wsBBW    <- intWspeedBBWevent[indexBBW, ]
    wdBBW    <- intWdirBBWevent[indexBBW, ]
    tBBW     <- intTempBBWevent[indexBBW, ]
  }

  # make statistics of these data
  {
    cat("\nBBR:\n")
    xx <- sum(pull(rainBBRevent, rainGaugeBBR), na.rm=TRUE)
    cat("rain height =", round(xx, digits=1), "mm\n")

    xx <- mean(wsBBR$y, na.rm=TRUE)
    sdxx <- sd(wsBBR$y, na.rm=TRUE)
    cat("wind speed =", round(xx, digits=1), "+/-", round(sdxx, digits=2), "m/s\n")

    xx <- mean(wdBBR$y, na.rm=TRUE)
    sdxx <- sd(wdBBR$y, na.rm=TRUE)
    cat("wind dir. =", round(xx, digits=1), "+/-", round(sdxx, digits=2), "deg\n")


    xx <- mean(tBBR$y, na.rm=TRUE)
    sdxx <- sd(tBBR$y, na.rm=TRUE)
    cat("air temp. =", round(xx, digits=1), "+/-", round(sdxx, digits=2), "degC\n")

    cat("\nBBW:\n")
    xx <- sum(pull(rainBBWevent, rainGaugeBBW), na.rm=TRUE)
    cat("rain height =", round(xx, digits=1), "mm\n")

    xx <- mean(wsBBW$y, na.rm=TRUE)
    sdxx <- sd(wsBBW$y, na.rm=TRUE)
    cat("wind speed =", round(xx, digits=1), "+/-", round(sdxx, digits=2), "m/s\n")

    xx <- mean(wdBBW$y, na.rm=TRUE)
    sdxx <- sd(wdBBW$y, na.rm=TRUE)
    cat("wind dir. =", round(xx, digits=1), "+/-", round(sdxx, digits=2), "deg\n")

    xx <- mean(tBBW$y, na.rm=TRUE)
    sdxx <- sd(tBBW$y, na.rm=TRUE)
    cat("air temp. =", round(xx, digits=1), "+/-", round(sdxx, digits=2), "degC\n")
  }

  # make plots
  {
    # sort data to avoid inexplicable line connecting last and first point...
    windBBRevent <- windBBRevent[order(windBBRevent$dateTime), ]
    windBBWevent <- windBBWevent[order(windBBWevent$dateTime), ]

    scaleSpeed <- 360/max(c(windBBRevent$speed, windBBWevent$speed), na.rm=TRUE)
    tAx        <- seq(tBeg, tEnd, by=dt)

    par(mar=c(3,3.5,1,5), mfcol=c(2,1))

    # BBR
    plot(windBBRevent$dateTime, windBBRevent$direction, xlim=c(tBeg-0.01*(tEnd-tBeg),
                                                               tEnd+0.05*(tEnd-tBeg)),
         type="p", pch=20, axes=FALSE, xlab="", ylab="",
         main=paste("BBR, DWD Tegel,", rainGaugeBBR),
         ylim=c(0, 360)); box()
    addRain(raindat=rainBBRevent, ymax=360, scale=rainScale, color="grey",
            rainGauge=rainGaugeBBR)
    lines(windBBRevent$dateTime, windBBRevent$direction, pch=20, type="p")
    lines(windBBRevent$dateTime, windBBRevent$speed*scaleSpeed, pch=20, type="p", col="red")
    axis(1, at=tAx, labels=format(tAx, format="%H:%M\n%d-%m"), padj=.25, cex.axis=0.8)
    axis(2, las=2, at=seq(0, 360, by=90), cex.axis=0.8)
    axis(4, las=3.5, at=seq(0, 360, length=6), cex.axis=0.8, col="red", col.axis="red",
         lab=round(seq(0, 360, length=6)/scaleSpeed, digits=0), mgp = c(0,2.5,2.8))
    mtext("Wind Dir. [grad]", side=2, line=2.5)
    mtext("W_Speed [m/s]", side=4, line=1, col="red", padj = .25, adj = 0)
    abline(h=c(45, 135, 225, 315), lty=2, col="grey")
    text(c("N", "E", "S", "W", "N"), x=tEnd-0.05*(tBeg-tEnd), y=c(20, 90, 180, 270, 350))
    eq <-
      bquote(paste(h[N]   == .(sum(pull(rainBBRevent, rainGaugeBBR), na.rm=TRUE)), " mm, ",
                   wDir   == .(round(mean(wdBBR$y, na.rm=TRUE), digits=0)), "(",
                   .(round(sd(wdBBR$y, na.rm=TRUE), digits=0)), ") deg, ",
                   wSpeed == .(round(mean(wsBBR$y, na.rm=TRUE), digits=1)), "(",
                   .(round(sd(wsBBR$y, na.rm=TRUE), digits=1)),") m/s"))
    #text(x=tEnd, y=10, eq, adj=1)

    # BBW
    plot(windBBWevent$dateTime, windBBWevent$direction, xlim=c(tBeg-0.01*(tEnd-tBeg),
                                                               tEnd+0.05*(tEnd-tBeg)),
         type="p", pch=20, axes=FALSE, xlab="", ylab="",
         main=paste("BBW, DWD Sch?nefeld,", rainGaugeBBW),
         ylim=c(0, 360)); box()
    addRain(raindat=rainBBWevent, ymax=360, scale=rainScale, color="grey",
            rainGauge=rainGaugeBBW)
    lines(windBBWevent$dateTime, windBBWevent$direction, pch=20, type="p")
    lines(windBBWevent$dateTime, windBBWevent$speed*scaleSpeed, type="p", pch=20,
          col="red")
    axis(1, at=tAx, labels=format(tAx, format="%H:%M\n%d-%m"), padj=.25, cex.axis=0.8)
    axis(2, las=2, at=seq(0, 360, by=90), cex.axis=0.8)
    axis(4, las=3.5, at=seq(0, 360, length=6), cex.axis=0.8, col="red", col.axis="red",
         lab=round(seq(0, 360, length=6)/scaleSpeed, digits=0), mgp = c(0,2.5,2.8))
    mtext("Wind Dir. [grad]", side=2, line=2.5)
    mtext("W_Speed [m/s]", side=4, line=1, col="red", padj = .25, adj = 0)
    abline(h=c(45, 135, 225, 315), lty=2, col="grey")
    text(c("N", "E", "S", "W", "N"), x=tEnd-0.05*(tBeg-tEnd), y=c(20, 90, 180, 270, 350))
    eq <-
      bquote(paste(h[N]   == .(sum(pull(rainBBWevent, rainGaugeBBW), na.rm=TRUE)), " mm, ",
                   wDir   == .(round(mean(wdBBW$y, na.rm=TRUE), digits=0)), "(",
                   .(round(sd(wdBBW$y, na.rm=TRUE), digits=0)), ") deg, ",
                   wSpeed == .(round(mean(wsBBW$y, na.rm=TRUE), digits=1)), "(",
                   .(round(sd(wsBBW$y, na.rm=TRUE), digits=1)),") m/s"))
    #text(x=tEnd, y=10, eq, adj=1)
  }
}


# read wind data base
readWind <- function()
{
  setwd("//Medusa/Projekte$/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_Daten/RAW/_KlimaDWD/Wind")


  windData <- tbl_df(read.table("windData.txt", header=TRUE,
                                dec=",", sep =";",
                                colClasses=c("character", rep("numeric", times=6))))

  windData$dateTime <- as.POSIXct(windData$dateTime,
                                  format="%Y-%m-%d %H:%M",
                                  tz="Etc/GMT-1")

  return(windData)
}

# read temperature data base
readTemp <- function()
{
  setwd("//Medusa/Projekte$/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_Daten/RAW/_KlimaDWD/Temperature")

  tempdData <- tbl_df(read.table("tempData.txt", header=TRUE,
                                 dec=",", sep =";",
                                 colClasses=c("character", rep("numeric", times=6))))

  tempdData$dateTime <- as.POSIXct(tempdData$dateTime,
                                   format="%Y-%m-%d %H:%M",
                                   tz="Etc/GMT-1")

  return(tbl_df(tempdData))
}

# read solar data base
readSolar <- function()
{
  setwd("//Medusa/Projekte$/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_Daten/RAW/_KlimaDWD/solar")

  solarData <- tbl_df(read.table("solarData.txt", header=TRUE,
                                 dec=".", sep =";",
                                 colClasses = c("character",
                                                rep("numeric", times=10)),
                                 na.strings = "-999"))

  solarData$dateTime <- as.POSIXct(solarData$dateTime,
                                   format="%Y-%m-%d %H:%M",
                                   tz="Etc/GMT-1")

  return(solarData)
}

# add rain to existing plot as upside-down bars
addRain <- function(raindat, ymax, scale, color, rainGauge)
{
  for(i in 1:(nrow(raindat)-1))
  {
    x0 <- raindat[[1]][i]
    x1 <- raindat[[1]][i+1]
    y1 <- pull(raindat, rainGauge)[i]

    polygon(x=c(x0, x0, x1, x1),
            y=c(ymax, -y1*scale + ymax, -y1*scale + ymax, ymax),
            col=color, border=color)
  }

  iNmax  <- max(pull(raindat, rainGauge), na.rm=TRUE)

  if(iNmax < 0.5)
  {
    iNby <- 0.1

  } else {

    if(iNmax < 1)
    {
      iNby <- 0.2

    } else {

      if(iNmax < 2)
      {

        iNby <- 0.5

      } else {

        if(iNmax < 5)
        {
          iNby <- 1

        } else {

          iNby <- 2
        }
      }
    }
  }

  rAx    <- -seq(0, iNmax, by=iNby)*scale + ymax
  rAxLab <- round(seq(0, iNmax, by=iNby), digits=1)

  axis(4, las=2, at=rAx, labels=rAxLab, hadj=0.3)
  mtext(expression(paste(i[N], " [mm/5min.]")), side=4, line=1.8, at=ymax, adj=1, cex=0.8)
}

# compute angle of attack of wind to facade
angleAttack <- function(facadeOrientation, windDir){

  dalpha <- abs(windDir - facadeOrientation)

  # two cases = dalpha < 180 of dalpha > 180
  if(dalpha < 180){

    aa <- ifelse(dalpha>=90, 0, abs(90-dalpha))

  } else {

    # for dalpha > 180, subtract 360 to bring alpha to first quadrant of cartesian plane
    dalpha2 <- abs(dalpha - 360)
    aa <- ifelse(dalpha2 >= 90, 0, abs(90-dalpha2))
  }

  return(aa)
}



