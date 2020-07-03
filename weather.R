# function to grab rainfall data from ftp and d2w and add it to BaSaR rain data base "rainDB.txt".
updateRainDB <- function(tBeg, tEnd, 
                         D2Wsid, 
                         removeTempBBW, 
                         overwriteOldrainDB,
                         skip)
{
  require(dplyr)
  require(RCurl)
  require(curl)
  
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
  
  if(Sys.getlocale(category="LC_TIME") != "German_Germany.1252")
    Sys.setlocale(category="LC_TIME", locale="German_Germany.1252")
  
  # make raw directory
  # rawdir <- "//Medusa/Projekte$/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_Daten/RAW/_Regen/"
  rawdir <- "c:/kwb/BaSaR/_Daten/RAW/_Regen/"
  
  # read and format text file with time stamps of adjusted rain data
  {
    #tFalseRain <- "//Medusa/projekte$/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_Daten/VAL/REGEN/tFalseRain.txt"
    tFalseRain <- "c:/kwb/BaSaR/_Daten/VAL/REGEN/tFalseRain.txt"
    tFalseRain <- scan(tFalseRain, what="character", sep="\n", quiet=TRUE)
    tFalseRain <- paste0(tFalseRain, ":00")
    tFalseRain <- format(as.POSIXct(tFalseRain, 
                                    format="%d.%m.%Y %H:%M:%S", 
                                    tz="Etc/GMT-1"),
                         format="%Y-%m-%d %H:%M")
  }
  
  # read rainDB
  {
    rainDB <- read.table(paste0(rawdir,"rainDB.txt"), 
                         sep=";", 
                         header=TRUE, 
                         encoding="UTF-8",
                         colClasses=c("character", rep("numeric", times=7)))
  }
  
  # read BWB gauges for user-defined time window, format data and add to rainDB
  if(skip != "BWB")
  {
    # check ftp server for data availability
    {
      # make user time window
      userTW <- seq(from=as.Date(tBeg, format="%Y%m%d"),
                    to=as.Date(tEnd, format="%Y%m%d"),
                    by=1)+1 # +1 because on ftp server, rain data for day i are in file named i+1
      
      # read list of available files
      availDates <- unlist(strsplit(getURL("xxxxxxxx", 
                                           userpwd="xxxxx:xxxxx"),
                                    "\r*\n"))
      xx         <- as.list(availDates[4:length(availDates)])
      availDates <- unlist(lapply(X=xx, FUN=substr, start=nchar(xx)-14, stop=nchar(xx)-9))
      availDates <- lapply(X=availDates,FUN=as.Date, format="%y%m%d")
      availDates <- do.call("c", availDates)
      
      # compare user-defined time frame with available dates
      index     <- match(userTW, availDates)
      index     <- index[!is.na(index)]
      availUser <- availDates[index]
      
      if(length(index)==0) stop("desired data are unavailable on ftp server")
    }
    
    # download available data from user time window and build file
    {
      # make empty data frame for holding downloaded data from user-defined time window
      downloadedRain <- data.frame()
      
      # loop over 
      for(i in 1:length(availUser))
      {
        # to store daily data (since this is how they're stored on ftp server)
        rainDay <- data.frame(dateTime=NA,
                              Owd=NA,
                              KöpI=NA,
                              Joh=NA,
                              BlnX=NA,
                              BlnIV=NA,
                              BlnXI=NA)
        
        # re-format date for pasting onto download string
        i2 <- format(availUser[i], "%y%m%d", tz="Etc/GMT-1")
        i2 <- format(availUser[i], "%y%m%d", tz="Europe/Berlin")
        
        # make download string
        read_url <- paste0("xxxxx", 
                           "xxxxxx",
                           i2,
                           "_0810.txt" )
        
        # read file from ftp server
        url_content <- getURL(read_url, 
                              userpwd="xxxxxx:xxxxxx")
        
        # split content into lines
        content <- strsplit(url_content, split="\n")[[1]]
        
        # rempove 1st element (headers)
        content <- content[2:length(content)]
        
        # make decent data frame for current day with BaSaR-relevant rain gauges:
        # BBW: Owd, KöpI, Joh
        # BBW: Owd, KöpI, Joh
        # BBR: BlnX, BlnIV, BlnXI
        
        # since BWB data does not always have the same number of rows, loop length
        # must be determined by finding the row number of the last time stamp of
        # the current day
        
        # make vector of downloaded dates
        downloadedDates <- rep(NA, times=length(content))
        for(j in 1:length(content))
        {
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
        # then include all rows in content, otherwise include only until end of current
        # day
        if(availUser[i] != Sys.Date())
        {
          loopRows <- rowsdatei 
        } else {
          loopRows <- 1:length(content)
        }
        
        # split content
        for(j in 1:length(loopRows)) 
        {
          contentsplt  <- strsplit(content[loopRows[j]], split="\t")[[1]]
          rowj         <- contentsplt[c(1, 35, 29, 26, 39, 37, 38)]
          rainDay[j, ] <- rowj
        }
        
        # remove potential NAs due to missing rows in rainDay
        rainDay <- filter(rainDay, !is.na(dateTime))
        
        # add new downloaded day
        downloadedRain <- rbind(downloadedRain, rainDay)
      }
      
      # format decimal point
      for(j in 2:7)
      {
        downloadedRain[[j]] <- as.numeric(gsub(",", ".", downloadedRain[[j]]))
      }
      
      # bring time axis of downloaded BWB data to winter time (bringing summer time stamps 
      # (tz="Europe/Berlin") to winter time (tz="Etc/GMT-1"). this is not necessary for KWB 
      # rain gauge, since logger configuration is always winter time
      {
        # make list of 5-min. time stamps of summer time in Berlin in 2017, 2018, 2019 and 2020
        # in Berlin/Europe time zone, i.e., containing the shift, to match the BWB data
        {
          tsSummer <- list(
            sum2017=format(seq(from=as.POSIXct("26-Mrz-17 03:00:00 +0200", 
                                               format="%d-%b-%y %H:%M:%S %z",
                                               tz="Europe/Berlin"),
                               to=as.POSIXct("29-Okt-17 02:55:00 +0200",
                                             format="%d-%b-%y %H:%M:%S %z",
                                             tz="Europe/Berlin"),
                               by=300),
                           format="%d-%b-%y %H:%M:%S"),
            sum2018=format(seq(from=as.POSIXct("25-Mrz-18 03:00:00 +0200", 
                                               format="%d-%b-%y %H:%M:%S %z",
                                               tz="Europe/Berlin"),
                               to=as.POSIXct("28-Okt-18 02:55:00 +0200", 
                                             format="%d-%b-%y %H:%M:%S %z",
                                             tz="Europe/Berlin"),
                               by=300),
                           format="%d-%b-%y %H:%M:%S"),
            sum2019=format(seq(from=as.POSIXct("31-Mrz-19 03:00:00 +0200", 
                                               format="%d-%b-%y %H:%M:%S %z",
                                               tz="Europe/Berlin"),
                               to=as.POSIXct("27-Okt-19 02:55:00 +0200", 
                                             format="%d-%b-%y %H:%M:%S %z",
                                             tz="Europe/Berlin"),
                               by=300),
                           format="%d-%b-%y %H:%M:%S"),
            sum2020=format(seq(from=as.POSIXct("29-Mrz-20 03:00:00 +0200", 
                                               format="%d-%b-%y %H:%M:%S %z",
                                               tz="Europe/Berlin"),
                               to=as.POSIXct("25-Okt-20 02:55:00 +0200", 
                                             format="%d-%b-%y %H:%M:%S %z",
                                             tz="Europe/Berlin"),
                               by=300),
                           format="%d-%b-%y %H:%M:%S"))
          }
        
        # find summer time stamps in downloaded BWB data (downloadedRain), checking against the summer
        # time stamps of the corresponding years (tsSummer2)
        {
          # find year(s) of downloadedRain
          yearsdownloaded <- unique(format(as.POSIXct(downloadedRain$dateTime, 
                                                      tz="Europe/Berlin", 
                                                      format="%d-%b-%y %H:%M:%S"),
                                           format="%Y"))
          
          # use only tsSummer element(s) for the years in downloadedRain
          tsSummer2 <- tsSummer[paste0("sum", yearsdownloaded)]
          
          # convert tsSummer2 into one character vector of summer time stamps
          tsSummer2 <- unlist(tsSummer2)
          
          # make index vector for all time stamps in downloadedRain
          indexAll    <- 1:nrow(downloadedRain)
          
          # try to match for summer time stamps in downloadedRain
          indexSummer <- match(tsSummer2, downloadedRain$dateTime)
          indexSummer <- indexSummer[!is.na(indexSummer)]
          
          # did user download any summer time stamps?
          if(length(indexSummer) > 0){
            
            # if so, winter indexes are all indexes that are not summer. this includes the case
            # where all indexes are summer, in which case indexWinter has length 0 (integer(0))
            indexWinter <- indexAll[-indexSummer]
            
          } else { # if there are no summer indexes, there are only winter indexes
            
            indexWinter <- indexAll
          }
        }
        
        # append "+0200" to summer time stamps and "+0100" to winter time stamps, in preparation
        # for conversion to tz="Etc/GMT-1"
        {
          # append "+0200" to all summer time stamps, if they exist 
          # (downloaded data may be only in winter)
          if(length(indexSummer) > 0)
          {
            downloadedRain$dateTime[indexSummer] <- 
              paste(downloadedRain$dateTime[indexSummer], "+0200")
          }
          
          # append "+0100" to all winter time stamps, if they exist
          # (downloaded data may be only in summer)
          if(length(indexWinter) > 0)
          {
            downloadedRain$dateTime[indexWinter] <- 
              paste(downloadedRain$dateTime[indexWinter], "+0100")
          }
        }
        
        # convert to tz="Etc/GMT-1" and re-format to remove seconds (to match dateTime in rainDB)
        {
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
        }
      }
    }
    
    # add downloadedRain to rainDB. ignore rows with false rain (corrected manually in rainDB)
    {
      # did user download time window including tFalseRain?
      index1 <- match(tFalseRain, downloadedRain$dateTime) 
      index1 <- index1[!is.na(index1)]
      
      if(length(index1) > 0) # if so, remove those rows from the downloaded data
      {                      # and add the remaining rows to rainDB
        
        downloadedRain     <- downloadedRain[-index1, ]
        index              <- match(downloadedRain$dateTime, rainDB$dateTime)
        rainDB[index, 2:7] <- downloadedRain[, 2:7]
        
      } else {               # if not, simply add downloaded data to rainDB
        
        index              <- match(downloadedRain$dateTime, rainDB$dateTime)
        rainDB[index, 2:7] <- downloadedRain[, 2:7]
      }
    }
  }
  
  # read KWB rain gauge in Bruno-Bürgel-Weg for user-defined time window, format data and add to rainDB
  if(skip != "KWB")
  {
    # download d2w data
    {
      t2       <- as.POSIXct(tEnd, format="%Y%m%d", tz="Etc/GMT-1")
      t1       <- as.POSIXct(tBeg, format="%Y%m%d", tz="Etc/GMT-1")
      tPeriod  <- as.numeric(t2 - t1)
      tPeriod  <- tPeriod+1
      
      url      <- paste0("xxxxx","sid=",
                         D2Wsid,
                         "&start=",
                         format(t1, format="%d.%m.%Y", tz="Etc/GMT-1"),
                         "%2000:00:00&",
                         "periode=",
                         tPeriod,
                         "xxxxxxx")
      tempFile <- paste0(rawdir, "temp_d2w.txt")
      
      # download data onto temporary file
      download.file(url, tempFile)
    }
    
    # check data downloaded from d2w. use first item of downloaded file to check
    {
      char1 <- scan(tempFile, what="character", nmax=1)
      d2wOK <- grepl(pattern="?at*/*", x = char1)
    }
    
    # if data downloaded from d2w are ok, read and format downloaded file, and add data to rainDB
    {
      if(d2wOK){
        
        # read and delete temporary file
        tempBBW <- read.table(tempFile, 
                              header=TRUE, 
                              sep="\t", 
                              colClasses=c("character", "numeric"),
                              dec=",")
        if(removeTempBBW) file.remove(tempFile)
        
        # format time stamps
        tempBBW[[1]]     <- as.character(format(as.POSIXct(as.character(tempBBW[[1]]),
                                                           format="%d.%m.%Y %H:%M:%S",
                                                           tz="Etc/GMT-1"),
                                                "%Y-%m-%d %H:%M"))
        
        # add tempBBW to rainDB. ignore rows with false rain (were corrected manually in rainDB)
        {
          # did user download time window including tFalseRain?
          index1 <- match(tFalseRain, tempBBW[[1]]) 
          index1 <- index1[!is.na(index1)]
          
          if(length(index1) > 0) # if so, remove those rows from the downloaded data
          {                      # and add the remaining rows to rainDB
            tempBBW          <- tempBBW[-index1, ]
            index            <- match(tempBBW[[1]], rainDB$dateTime)
            rainDB[index, 8] <- tempBBW[, 2]
            
          } else {               # if not, simply add downloaded data to rainDB
            
            index            <- match(tempBBW[[1]], rainDB$dateTime)
            index            <- index[!is.na(index)]
            rainDB[index, 8] <- tempBBW[, 2]
          }
        }
        
      } else {
        
        stop("file downloaded from d2w seems strange...please check temp_d2w.txt")
        
      }
    }
  }

  # write new rainDB
  {
    if(overwriteOldrainDB)
    {
      write.table(rainDB, 
                  file=paste0(rawdir, "rainDB.txt"),
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
}


# make IDF curves based on rainfall time series; durations in sec, returnPeriods in years,
# minimum value > 1 year
# years must be in increasing order (not necessarily continuous)
# dt = time step in rainfall data, miu0 and beta0 = initial values for parameter estimation in
# gumbel distribution; don't use miu0=0 and/or beta0=0!
makeIDF <- function(rain, gaugeName, durations, years, returnPeriods, dt,
                    miu0, beta0)
{
  # run sliding window for each duration through time series and get maximum intensity
  {
    rain$year <- lubridate::year(rain[[1]])
    hNdur     <- matrix(ncol=length(durations), nrow=length(years),
                        dimnames=list(years, paste0(durations/60, "min")))
    
    for(i in 1:length(years)){
      
      for(j in 1:length(durations)){
        
        yeari       <- pull(filter(rain, year == years[i]), gaugeName)
        xx          <- max(zoo::rollsum(x=yeari,
                                        k=durations[j]/(dt),
                                        fill=NA),
                           na.rm=TRUE)
        hNdur[i, j] <- xx
      }
    }
  }
  
  # compute mean intensities for each duration (output in mm/hour)
  {
    iNdur     <- matrix(ncol=length(durations), nrow=length(years),
                        dimnames=list(years, paste0(durations/60, "min")))
    
    for(j in 1:length(durations)) {iNdur[, j] <- hNdur[, j]/durations[j]*3600}
  }
  
  # define gumbel
  {
    dgumbel <<- function(x, miu, beta) 1/beta*exp(-((x-miu)/beta + exp(-((x-miu)/beta))))
    pgumbel <<- function(q, miu, beta) exp(-exp(-(q-miu)/beta))
    qgumbel <<- function(p, miu, beta) miu - beta*log(-log(p))
  }
  
  # make theoretical IDF distribution
  {
    IDFtable <- matrix(ncol=length(durations), nrow=length(returnPeriods),
                       dimnames=list(paste0(returnPeriods, "years"),
                                     paste0(durations/60, "min")))
    
    pexceed  <- 1/returnPeriods
    
    for(j in 1:length(durations))
    {
      xx            <- iNdur[, j]
      fitgumbel     <- fitdist(xx, "gumbel", start=list(miu=miu0, beta=beta0))
      miu           <- fitgumbel$estimate[1]
      beta          <- fitgumbel$estimate[2]
      iNi           <- qgumbel(1-pexceed, miu=miu, beta=beta)
      IDFtable[, j] <- iNi
    }
  }
  
  output <- list(duration_hN=hNdur,
                 duration_iN=iNdur,
                 IDFtable=IDFtable)
  return(output)
}

# make frequency analysis for rainfall, including homogeneity test
# years must be in increasing order (not necessarily continuous)
rainFreq <- function(rain, gaugeName, years, miu0, beta0)
{
  # make series of annual maxima
  {
    annualMax  <- matrix(nrow=length(years), ncol=2,
                         dimnames=list(years, c("maxhN_mm", "Dur_min")))
    
    rain$year <- year(rain$From)
    
    for(i in 1:length(years))
    {
      # get events for year i
      yeari   <- filter(rain, year == years[i])
      eventsi <- getEvents(yeari, seriesName=gaugeName)
      
      # add statistics to events of year i
      for (j in 1:nrow(eventsi))
      {
        rainSel       <-  rain[rain$From >= eventsi$tBeg[j] &
                                 rain$From <= eventsi$tEnd[j], ]
        eventsi$hN[j] <- sum(pull(rainSel, gaugeName), na.rm=TRUE)
      }
      
      # find year's maximum hN and store in annualMax
      maxi <- filter(eventsi, hN==max(hN))
      annualMax[i, ] <- c(maxi$hN, maxi$dur/60)
    }
    annualMax <<- annualMax
  }
  
  # homogeneity test: cumulative deviations from the mean (Buishand,  1982)
  {
    xi   <- annualMax[, 1]
    xbar <- mean(xi)
    sk   <- rep(0, times=length(xi)+1)
    s    <- sd(xi)
    n    <- length(sk)-1
    
    for(i in 2:length(xi)) {sk[i] <- sk[i-1] + xi[i] - xbar}
    
    Q          <- max(sk/s)
    testTableQ <- data.frame(n=c(10, 20, 30, 40, 50, 100),
                             Q_sqrt_n_90=c(1.05, 1.1, 1.12, 1.13, 1.14, 1.17),
                             Q_sqrt_n_95=c(1.14, 1.22, 1.24, 1.26, 1.27, 1.29),
                             Q_sqrt_n_99=c(1.29, 1.42, 1.46, 1.5, 1.52, 1.55))
    Q90        <- approx(x=testTableQ$n, y=testTableQ$Q_sqrt_n_90, xout=n)$y*sqrt(n)
    Q95        <- approx(x=testTableQ$n, y=testTableQ$Q_sqrt_n_95, xout=n)$y*sqrt(n)
    Q99        <- approx(x=testTableQ$n, y=testTableQ$Q_sqrt_n_99, xout=n)$y*sqrt(n)
  }
  
  # define gumbel
  {
    dgumbel <<- function(x, miu, beta) 1/beta*exp(-((x-miu)/beta + exp(-((x-miu)/beta))))
    pgumbel <<- function(q, miu, beta) exp(-exp(-(q-miu)/beta))
    qgumbel <<- function(p, miu, beta) miu - beta*log(-log(p))
  }
  
  # fit gumbel
  {
    xx        <- annualMax[, 1]
    fitgumbel <- fitdist(xx, "gumbel", start=list(miu=miu0, beta=beta0))
    miu       <- fitgumbel$estimate[1]
    beta      <- fitgumbel$estimate[2]
    miuse     <- fitgumbel$sd[1]
    betase    <- fitgumbel$sd[2]
    gofit     <- gofstat(fitgumbel)
  }
  
  # print gumbel parameters
  {
    print(paste0("miu = ", round(miu, digits=2),
                 ", se= ", round(miuse, digits=2)),
          quote=FALSE)
    print(paste0("beta = ", round(beta, digits=2),
                 ", se = ", round(betase, digits=2)),
          quote=FALSE)
    print(paste0("p-value chi-squared = ",
                 round(gofit$chisqpvalue, digits=2)),
          quote=FALSE)
  }
  
  # plot annual max hN vs. return period and homogeneity test
  {
    par(mfcol=c(1,2))
    
    xx <- sort(annualMax[, 1])
    Fx <- (1:length(xx))/(1 + length(xx))
    xpred <- seq(0, 200, by=.1)
    plot(xx, 1/(1-Fx), ylab="T [years]",
         xlab=expression(paste("Annual. max. ", h[N], " [mm]")),
         ylim=c(0, length(years)),
         xlim=c(0, max(xx)),
         main="Event depth vs. Return Period")
    for(i in 1:500)
    {
      miui  <- runif(n=1, min=miu - miuse, miu + miuse)
      betai <- runif(n=1, min=beta - betase, beta + betase)
      lines(xpred, 1/(1-pgumbel(xpred, miu=miui, beta=betai)), col="grey")
    }
    lines(xpred, 1/(1-pgumbel(xpred, miu=miu, beta=beta)))
    points(xx, 1/(1-Fx), pch=20, cex=2)
    
    plot(c(years[1]-1, years), c(0, sk[2:length(sk)]/s),
         type="b", pch=20, cex=1.5,
         ylim=c(-5, 5),
         ylab="rescaled cumulative deviation",
         xlab="years",
         main="Q-Homogeneity test (Buishand,  1982)")
    abline(h=0, col="red")
    abline(h=c(Q90, Q95, Q99, -Q90, -Q95, -Q99), lty=c(1, 2, 3, 1, 2, 3))
    legend(x=years[1]-1, y=5,
           legend=c("90%", "95%", "99%"),
           lty=c(1,2,3))
  }
  
}

# read BaSaR rainfall data base
readRain <- function()
{
  # setwd("xxxxx")
  setwd("c:/kwb/BaSaR/_Daten/RAW/_Regen")
  rain <- read.table("rainDB.txt",
                     sep=";",
                     header=TRUE,
                     encoding="UTF-8",
                     colClasses=c("character", rep("numeric", times=7)))
  
  rain$dateTime <- as.POSIXct(rain$dateTime,
                              format="%Y-%m-%d %H:%M",
                              tz="Etc/GMT-1")
  return(dplyr::tbl_df(rain))
}

# download temperature from DWD
download_tempData <- function() #BBW=427; BBR=430
{
  ###BBW
  
  #download data from DWD ftp Server
  setwd("//Medusa/Projekte$/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_Daten")
  url <- "ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/10_minutes/air_temperature/recent/10minutenwerte_TU_00427_akt.zip"
  destfile <- "tempBBWnew.zip"
  download.file(url, destfile, quiet = FALSE)
  #unzip BBW data
  unzip("tempBBWnew.zip", files = NULL, list = FALSE, overwrite = TRUE,
        junkpaths = FALSE, exdir = "//Medusa/Projekte$/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_Daten/OTHER/Temperature", unzip = "internal",
        setTimes = FALSE)
  #read table BBW
  setwd("//Medusa/Projekte$/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_Daten/OTHER/Temperature")
  filelist = list.files(pattern="*.txt$")
  datalist <- list()
  for (i in 1:length(filelist))
  {
    datalist[[i]]<-read.delim2(filelist[i])
  }
  tempBBWnew  <- do.call("rbind" ,datalist)
  
  
  #clean data set
  tempBBWnew <- as.data.frame(tempBBWnew, row.names = NULL,
                              responseName = "Freq", stringsAsFactors = F,header=FALSE,
                              sep = ";", base = list(LETTERS))
  
  tempBBWnew <- tidyr::separate(tempBBWnew, col=STATIONS_ID.MESS_DATUM...QN.PP_10.TT_10.TM5_10.RF_10.TD_10.eor, sep = ";", into =c( "id" , "dateTime", "QN", "PP_10", "temperature","TM5_10","humidity", "TD_10", "eor") , remove = TRUE, convert = FALSE , extra = "warn", fill = "warn")
  tempBBWnew$id <- tempBBWnew$eor <- tempBBWnew$TM5_10 <- tempBBWnew$TD_10 <- tempBBWnew$PP_10 <- NULL
  tempBBWnew$dateTime <- format(as.POSIXct(tempBBWnew$dateTime, format= "%Y%m%d%H%M", tz="Etc/GMT-1"), "%Y-%m-%d %H:%M:%S", tz="Etc/GMT-1")
  tempBBWnew$temperature <- as.numeric(as.character(tempBBWnew$temperature))
  tempBBWnew$humidity <-  as.numeric(as.character(tempBBWnew$humidity))
  tempBBWnew$QN <- as.numeric(as.character(tempBBWnew$QN))
  
  #delete unnessecary temporary files
  setwd("//Medusa/Projekte$/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_Daten")
  file.remove("tempBBWnew.zip")
  setwd("//Medusa/Projekte$/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_Daten/OTHER/Temperature")
  filelist = list.files(pattern="*.txt$")
  file.remove(paste0(filelist))
  
  ###BBR
  
  #download data from DWD ftp Server
  setwd("//Medusa/Projekte$/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_Daten")
  url <- "ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/10_minutes/air_temperature/recent/10minutenwerte_TU_00430_akt.zip"
  destfile <- "tempBBRnew.zip"
  download.file(url, destfile, quiet = FALSE)
  #unzip BBR data
  unzip("tempBBRnew.zip", files = NULL, list = FALSE, overwrite = TRUE,
        junkpaths = FALSE, exdir = "//Medusa/Projekte$/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_Daten/OTHER/Temperature", unzip = "internal",
        setTimes = FALSE)
  #read table BBR
  setwd("//Medusa/Projekte$/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_Daten/OTHER/Temperature")
  filelist = list.files(pattern="*.txt$")
  datalist <- list()
  for (i in 1:length(filelist))
  {
    datalist[[i]]<-read.delim2(filelist[i])
  }
  tempBBRnew  <- do.call("rbind" ,datalist)
  
  
  #clean data set
  tempBBRnew <- as.data.frame(tempBBRnew, row.names = NULL,
                              responseName = "Freq", stringsAsFactors = F,header=FALSE,
                              sep = ";", base = list(LETTERS))
  
  tempBBRnew <- tidyr::separate(tempBBRnew, col=STATIONS_ID.MESS_DATUM...QN.PP_10.TT_10.TM5_10.RF_10.TD_10.eor, sep = ";", into =c( "id" , "dateTime", "QN", "PP_10", "temperature","TM5_10","humidity", "TD_10", "eor") , remove = TRUE, convert = FALSE , extra = "warn", fill = "warn")
  tempBBRnew$id <- tempBBRnew$eor <- tempBBRnew$TM5_10 <- tempBBRnew$TD_10 <- tempBBRnew$PP_10 <- NULL
  tempBBRnew$dateTime <- format(as.POSIXct(tempBBRnew$dateTime, format= "%Y%m%d%H%M", tz="Etc/GMT-1"), "%Y-%m-%d %H:%M:%S", tz="Etc/GMT-1")
  tempBBRnew$temperature <- as.numeric(as.character(tempBBRnew$temperature))
  tempBBRnew$humidity <-  as.numeric(as.character(tempBBRnew$humidity))
  tempBBRnew$QN <- as.numeric(as.character(tempBBRnew$QN))
  
  #delete unnessecary temporary files
  setwd("//Medusa/Projekte$/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_Daten")
  file.remove("tempBBRnew.zip")
  setwd("//Medusa/Projekte$/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_Daten/OTHER/Temperature")
  filelist = list.files(pattern="*.txt$")
  file.remove(paste0(filelist))
  
  
  #read old data and connect with new
  setwd("//Medusa/Projekte$/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_Daten/RAW/_KlimaDWD/Temperature")
  tempDatanew <- dplyr::right_join(tempBBRnew, tempBBWnew, by="dateTime")
  names(tempDatanew) <- c("dateTime", "QN_BBR", "temperature_BBR", "humidity_BBR", "QN_BBW", "temperature_BBW", "humidity_BBW")
  
  tempData <- read.delim2("tempData.txt", sep=";", dec =",")
  tempData$dateTime <- format(as.POSIXct(tempData$dateTime, format= "%Y-%m-%d %H:%M:%S", tz="Etc/GMT-1"), "%Y-%m-%d %H:%M:%S", tz="Etc/GMT-1")
  tempData <- dplyr::union(tempData, tempDatanew)
  tempData[!duplicated(tempData$dateTime), ]
  write.table(tempData, file = "tempData.txt",dec=",", sep = ";", quote = F, row.names = F)
  
  
}

# download wind from DWD
download_windData <- function() #BBW=427; BBR=430
{
  ###BBW
  
  #download data from DWD ftp Server
  setwd("//Medusa/Projekte$/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_Daten")
  url <- "ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/10_minutes/wind/recent/10minutenwerte_wind_00427_akt.zip"
  destfile <- "windBBWnew.zip"
  download.file(url, destfile, quiet = FALSE)
  #unzip BBW data
  unzip("windBBWnew.zip", files = NULL, list = FALSE, overwrite = TRUE,
        junkpaths = FALSE, exdir = "//Medusa/Projekte$/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_Daten/OTHER/Wind", unzip = "internal",
        setTimes = FALSE)
  #read table BBW
  setwd("//Medusa/Projekte$/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_Daten/OTHER/Wind")
  filelist = list.files(pattern="*.txt$")
  datalist <- list()
  for (i in 1:length(filelist))
  {
    datalist[[i]]<-read.delim2(filelist[i])
  }
  windBBWnew  <- do.call("rbind" ,datalist)
  
  
  #clean data set
  windBBWnew <- as.data.frame(windBBWnew, row.names = NULL,
                              responseName = "Freq", stringsAsFactors = F,header=FALSE,
                              sep = ";", base = list(LETTERS))
  
  windBBWnew <- tidyr::separate(windBBWnew, col=STATIONS_ID.MESS_DATUM...QN.FF_10.DD_10.eor  , sep = ";", into =c( "id" , "dateTime", "QN", "speed", "direction","eor") , remove = TRUE, convert = FALSE , extra = "warn", fill = "warn")
  windBBWnew$eor <- windBBWnew$id <- NULL
  windBBWnew$dateTime <- format(as.POSIXct(windBBWnew$dateTime, format= "%Y%m%d%H%M", tz="Etc/GMT-1"), "%Y-%m-%d %H:%M:%S", tz="Etc/GMT-1")
  windBBWnew$speed <- as.numeric(as.character(windBBWnew$speed))
  windBBWnew$direction <-  as.numeric(as.character(windBBWnew$direction))
  windBBWnew$QN <- as.numeric(as.character(windBBWnew$QN))
  
  #delete unnessecary files
  setwd("//Medusa/Projekte$/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_Daten")
  file.remove("windBBWnew.zip")
  setwd("//Medusa/Projekte$/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_Daten/OTHER/Wind")
  filelist = list.files(pattern="*.txt$")
  file.remove(paste0(filelist))
  
  
  ###BBR
  
  #download data from DWD ftp Server
  setwd("//Medusa/Projekte$/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_Daten")
  url <- "ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/10_minutes/wind/recent/10minutenwerte_wind_00430_akt.zip"
  destfile <- "windBBRnew.zip"
  download.file(url, destfile, quiet = FALSE)
  #unzip BBR data
  unzip("windBBRnew.zip", files = NULL, list = FALSE, overwrite = TRUE,
        junkpaths = FALSE, exdir = "//Medusa/Projekte$/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_Daten/OTHER/Wind", unzip = "internal",
        setTimes = FALSE)
  #read table BBR
  setwd("//Medusa/Projekte$/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_Daten/OTHER/Wind")
  filelist = list.files(pattern="*.txt$")
  datalist <- list()
  for (i in 1:length(filelist))
  {
    datalist[[i]]<-read.delim2(filelist[i])
  }
  windBBRnew  <- do.call("rbind" ,datalist)
  
  
  #clean data set
  windBBRnew <- as.data.frame(windBBRnew, row.names = NULL,
                              responseName = "Freq", stringsAsFactors = F,header=FALSE,
                              sep = ";", base = list(LETTERS))
  
  windBBRnew <- tidyr::separate(windBBRnew, col=STATIONS_ID.MESS_DATUM...QN.FF_10.DD_10.eor  , sep = ";", into =c( "id" , "dateTime", "QN", "speed", "direction","eor") , remove = TRUE, convert = FALSE , extra = "warn", fill = "warn")
  windBBRnew$eor <- windBBRnew$id <- NULL
  windBBRnew$dateTime <- format(as.POSIXct(windBBRnew$dateTime, format= "%Y%m%d%H%M", tz="Etc/GMT-1"), "%Y-%m-%d %H:%M:%S", tz="Etc/GMT-1")
  windBBRnew$speed <- as.numeric(as.character(windBBRnew$speed))
  windBBRnew$direction <-  as.numeric(as.character(windBBRnew$direction))
  windBBRnew$QN <- as.numeric(as.character(windBBRnew$QN))
  
  #delete unnessecary files
  setwd("//Medusa/Projekte$/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_Daten")
  file.remove("windBBRnew.zip")
  setwd("//Medusa/Projekte$/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_Daten/OTHER/Wind")
  filelist = list.files(pattern="*.txt$")
  file.remove(paste0(filelist))
  
  
  #load old data and bind with new
  setwd("//Medusa/Projekte$/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_Daten/RAW/_KlimaDWD/Wind")
  windDatanew <- dplyr::left_join(windBBRnew, windBBWnew, by="dateTime")
  names(windDatanew) <- c("dateTime", "QN_BBR", "speed_BBR", "direction_BBR", "QN_BBW", "speed_BBW", "direction_BBW")
  
  windData <- read.delim2("windData.txt", sep=";", dec =",")
  windData$dateTime <- format(as.POSIXct(windData$dateTime, format= "%Y-%m-%d %H:%M:%S", tz="Etc/GMT-1"), "%Y-%m-%d %H:%M:%S", tz="Etc/GMT-1")
  windData <- dplyr::union(windData, windDatanew)
  windData[!duplicated(windData$dateTime), ]
  write.table(windData, file = "windData.txt",dec=",", sep = ";", quote = F, row.names = F)
  
}

# download solar from DWD
download_solarData <- function() #BBW=427; BBR=430
{
  ###BBW
  
  #download data from DWD ftp Server
  setwd("//Medusa/Projekte$/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_Daten")
  url <- "ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/10_minutes/solar/recent/10minutenwerte_SOLAR_00427_akt.zip"
  destfile <- "solarBBWnew.zip"
  download.file(url, destfile, quiet = FALSE)
  
  #unzip BBW data
  unzip("solarBBWnew.zip", files = NULL, list = FALSE, overwrite = TRUE,
        junkpaths = FALSE,
        exdir = "//Medusa/Projekte$/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_Daten/OTHER/solar", unzip = "internal",
        setTimes = FALSE)
  
  #read table BBW
  setwd("//Medusa/Projekte$/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_Daten/OTHER/solar")
  filelist = list.files(pattern="*.txt$")
  datalist <- list()
  for (i in 1:length(filelist))
  {
    datalist[[i]]<-read.delim2(filelist[i])
  }
  solarBBWnew  <- do.call("rbind" ,datalist)
  
  #clean data set
  solarBBWnew <- as.data.frame(solarBBWnew, row.names = NULL,
                               responseName = "Freq", stringsAsFactors = F,header=FALSE,
                               sep = ";", base = list(LETTERS))
  
  solarBBWnew <- tidyr::separate(solarBBWnew,
                                 col=STATIONS_ID.MESS_DATUM...QN.DS_10.GS_10.SD_10.LS_10.eor,
                                 sep = ";",
                                 into =c( "id" ,
                                          "dateTime",
                                          "QN_BBW",
                                          "direct_BBW",
                                          "global_BBW",
                                          "sunh_BBW",
                                          "atmos_Gegenstrahlung_BBW",
                                          "eor") ,
                                 remove = TRUE,
                                 convert = FALSE,
                                 extra = "warn",
                                 fill = "warn")
  solarBBWnew$dateTime <- format(as.POSIXct(solarBBWnew$dateTime, format= "%Y%m%d%H%M", tz="Etc/GMT-1"), "%Y-%m-%d %H:%M:%S", tz="Etc/GMT-1")
  solarBBWnew$eor <- solarBBWnew$id <- NULL
  
  #delete unnessecary temporary files
  setwd("//Medusa/Projekte$/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_Daten")
  file.remove("solarBBWnew.zip")
  setwd("//Medusa/Projekte$/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_Daten/OTHER/solar")
  filelist = list.files(pattern="*.txt$")
  file.remove(paste0(filelist))
  
  ###BBR
  
  #download data from DWD ftp Server
  setwd("//Medusa/Projekte$/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_Daten")
  url <- "ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/10_minutes/solar/recent/10minutenwerte_SOLAR_00430_akt.zip"
  destfile <- "solarBBRnew.zip"
  download.file(url, destfile, quiet = FALSE)
  
  #unzip BBR data
  unzip("solarBBRnew.zip", files = NULL, list = FALSE, overwrite = TRUE,
        junkpaths = FALSE, exdir = "//Medusa/Projekte$/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_Daten/OTHER/solar", unzip = "internal",
        setTimes = FALSE)
  
  #read table BBR
  setwd("//Medusa/Projekte$/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_Daten/OTHER/solar")
  filelist = list.files(pattern="*.txt$")
  datalist <- list()
  for (i in 1:length(filelist))
  {
    datalist[[i]]<-read.delim2(filelist[i])
  }
  solarBBRnew  <- do.call("rbind" ,datalist)
  
  #clean data set
  solarBBRnew <- as.data.frame(solarBBRnew, row.names = NULL,
                               responseName = "Freq", stringsAsFactors = F,header=FALSE,
                               sep = ";", base = list(LETTERS))
  
  solarBBRnew <- tidyr::separate(solarBBRnew,
                                 col=STATIONS_ID.MESS_DATUM...QN.DS_10.GS_10.SD_10.LS_10.eor,
                                 sep = ";",
                                 into =c( "id" ,
                                          "dateTime",
                                          "QN_BBR",
                                          "direct_BBR",
                                          "global_BBR",
                                          "sunh_BBR",
                                          "atmos_Gegenstrahlung_BBR",
                                          "eor"),
                                 remove = TRUE,
                                 convert = FALSE,
                                 extra = "warn",
                                 fill = "warn")
  solarBBRnew$dateTime <- format(as.POSIXct(solarBBRnew$dateTime,
                                            format= "%Y%m%d%H%M",
                                            tz="Etc/GMT-1"),
                                 "%Y-%m-%d %H:%M:%S", tz="Etc/GMT-1")
  solarBBRnew$eor <- solarBBRnew$id <- NULL
  
  #delete unnessecary temporary files
  setwd("//Medusa/Projekte$/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_Daten")
  file.remove("solarBBRnew.zip")
  setwd("//Medusa/Projekte$/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_Daten/OTHER/solar")
  filelist = list.files(pattern="*.txt$")
  file.remove(paste0(filelist))
  
  #load old data and bind with new
  setwd("//Medusa/Projekte$/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_Daten/RAW/_KlimaDWD/solar")
  solarDatanew <- dplyr::right_join(solarBBRnew, solarBBWnew, by="dateTime")
  
  for(i in 1:ncol(solarDatanew)){
    solarDatanew[[i]] <- as.character(solarDatanew[[i]])
  }
  
  
  solarData <- read.delim2("solarData.txt", sep=";", dec =",")
  solarData$dateTime <- format(as.POSIXct(solarData$dateTime, format= "%Y-%m-%d %H:%M:%S", tz="Etc/GMT-1"), "%Y-%m-%d %H:%M:%S", tz="Etc/GMT-1")
  
  for(i in 1:ncol(solarData)){
    solarData[[i]] <- as.character(solarData[[i]])
  }
  
  solarData <- dplyr::union(solarData, solarDatanew)
  solarData[!duplicated(solarData$dateTime), ]
  write.table(solarData, file = "solarData.txt",dec=",", sep = ";", quote = F, row.names = F)
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

# look at rainfall for each site, multiple gauges
checkRain <- function(rainDB, tBeg, tEnd, dt, diN)
{
  # filter data
  {
    tBeg    <- as.POSIXct(tBeg, format="%Y-%m-%d %H:%M", tz="Etc/GMT-1")
    tEnd    <- as.POSIXct(tEnd, format="%Y-%m-%d %H:%M", tz="Etc/GMT-1")
    rainSel <- filter(rainDB, dateTime >= tBeg & dateTime <= tEnd)
  }
  
  # make axes
  {
    tAx <- seq(tBeg, tEnd, by=dt)
    
    # find maximum of all rain gauges
    rainMax <- 0
    for(i in 2:ncol(rainSel))
    {
      newMax  <- max(pull(rainSel, i), na.rm=TRUE)
      rainMax <- ifelse(newMax > rainMax, newMax, rainMax)
    }
    
    # build axis
    rainAx <- seq(0, rainMax, by=diN)
  }
  
  # plot BlnX
  {
    par(mar=c(3,4,1,3), mfcol=c(3,2))
    plot(rainSel$dateTime, rainSel$BlnX, xlim=c(tBeg, tEnd), type="o", pch=20,
         axes=FALSE, xlab="", ylab="", main=,
         ylim=c(0, rainMax)); box()
    eq <- bquote(paste("BlnX, ", h[N]   == .(sum(pull(rainSel, BlnX), na.rm=TRUE)), " mm"))
    text(x=tEnd-0.01*(tEnd-tBeg), y=rainMax-0.1*rainMax, eq, adj=1, cex=1.5)
    axis(1, at=tAx, labels=format(tAx, format="%H:%M\n%d-%m"), padj=.45, cex.axis=1.2)
    axis(2, las=2, at=rainAx, labels=round(rainAx, digits=1))
    mtext(expression(paste(h[N], " [mm/5min.]")), side=2, line=2)
  }
  
  # plot BlnXI
  {
    par(mar=c(3,4,1,3))
    plot(rainSel$dateTime, rainSel$BlnXI, xlim=c(tBeg, tEnd), type="o", pch=20,
         axes=FALSE, xlab="", ylab="", main=,
         ylim=c(0, rainMax)); box()
    eq <- bquote(paste("BlnXI, ", h[N]   == .(sum(pull(rainSel, BlnXI), na.rm=TRUE)), " mm"))
    text(x=tEnd-0.01*(tEnd-tBeg), y=rainMax-0.1*rainMax, eq, adj=1, cex=1.5)
    axis(1, at=tAx, labels=format(tAx, format="%H:%M\n%d-%m"), padj=.45, cex.axis=1.2)
    axis(2, las=2, at=rainAx, labels=round(rainAx, digits=1))
    mtext(expression(paste(h[N], " [mm/5min.]")), side=2, line=2)
  }
  
  # plot BlnIV
  {
    par(mar=c(3,4,1,3))
    plot(rainSel$dateTime, rainSel$BlnIV, xlim=c(tBeg, tEnd), type="o", pch=20,
         axes=FALSE, xlab="", ylab="", main=,
         ylim=c(0, rainMax)); box()
    eq <- bquote(paste("BlnIV, ", h[N]   == .(sum(pull(rainSel, BlnIV), na.rm=TRUE)), " mm"))
    text(x=tEnd-0.01*(tEnd-tBeg), y=rainMax-0.1*rainMax, eq, adj=1, cex=1.5)
    axis(1, at=tAx, labels=format(tAx, format="%H:%M\n%d-%m"), padj=.45, cex.axis=1.2)
    axis(2, las=2, at=rainAx, labels=round(rainAx, digits=1))
    mtext(expression(paste(h[N], " [mm/5min.]")), side=2, line=2)
  }
  
  # plot Owd
  {
    par(mar=c(3,4,1,3))
    plot(rainSel$dateTime, rainSel$Owd, xlim=c(tBeg, tEnd), type="o", pch=20,
         axes=FALSE, xlab="", ylab="", main=,
         ylim=c(0, rainMax)); box()
    addRain(raindat = rainSel, ymax=rainMax, scale=1, color="grey", rainGauge="BBW")
    eq <- bquote(paste("Owd, ", h[N]   == .(sum(pull(rainSel, Owd), na.rm=TRUE)), " mm"))
    text(x=tEnd-0.01*(tEnd-tBeg), y=rainMax-0.1*rainMax, eq, adj=1, cex=1.5)
    eq <- bquote(paste("BBW, ", h[N]   == .(sum(pull(rainSel, BBW), na.rm=TRUE)), " mm"))
    text(x=tEnd-0.01*(tEnd-tBeg), y=rainMax-0.3*rainMax, eq, adj=1, cex=1.5)
    axis(1, at=tAx, labels=format(tAx, format="%H:%M\n%d-%m"), padj=.45, cex.axis=1.2)
    axis(2, las=2, at=rainAx, labels=round(rainAx, digits=1))
    mtext(expression(paste(h[N], " [mm/5min.]")), side=2, line=2)
    lines(rainSel$dateTime, rainSel$Owd, type="o", pch=20)
  }
  
  # plot KöpI
  {
    par(mar=c(3,4,1,3))
    plot(rainSel$dateTime, rainSel$KöpI, xlim=c(tBeg, tEnd), type="o", pch=20,
         axes=FALSE, xlab="", ylab="", main=,
         ylim=c(0, rainMax)); box()
    addRain(raindat = rainSel, ymax=rainMax, scale=1, color="grey", rainGauge="BBW")
    eq <- bquote(paste("KöpI, ", h[N]   == .(sum(pull(rainSel, KöpI), na.rm=TRUE)), " mm"))
    text(x=tEnd-0.01*(tEnd-tBeg), y=rainMax-0.1*rainMax, eq, adj=1, cex=1.5)
    eq <- bquote(paste("BBW, ", h[N]   == .(sum(pull(rainSel, BBW), na.rm=TRUE)), " mm"))
    text(x=tEnd-0.01*(tEnd-tBeg), y=rainMax-0.3*rainMax, eq, adj=1, cex=1.5)
    axis(1, at=tAx, labels=format(tAx, format="%H:%M\n%d-%m"), padj=.45, cex.axis=1.2)
    axis(2, las=2, at=rainAx, labels=round(rainAx, digits=1))
    mtext(expression(paste(h[N], " [mm/5min.]")), side=2, line=2)
    lines(rainSel$dateTime, rainSel$KöpI, type="o", pch=20)
  }
  
  # plot Joh
  {
    par(mar=c(3,4,1,3))
    plot(rainSel$dateTime, rainSel$Joh, xlim=c(tBeg, tEnd), type="o", pch=20,
         axes=FALSE, xlab="", ylab="", main=,
         ylim=c(0, rainMax)); box()
    addRain(raindat = rainSel, ymax=rainMax, scale=1, color="grey", rainGauge="BBW")
    eq <- bquote(paste("Joh, ", h[N]   == .(sum(pull(rainSel, Joh), na.rm=TRUE)), " mm"))
    text(x=tEnd-0.01*(tEnd-tBeg), y=rainMax-0.1*rainMax, eq, adj=1, cex=1.5)
    eq <- bquote(paste("BBW, ", h[N]   == .(sum(pull(rainSel, BBW), na.rm=TRUE)), " mm"))
    text(x=tEnd-0.01*(tEnd-tBeg), y=rainMax-0.3*rainMax, eq, adj=1, cex=1.5)
    axis(1, at=tAx, labels=format(tAx, format="%H:%M\n%d-%m"), padj=.45, cex.axis=1.2)
    axis(2, las=2, at=rainAx, labels=round(rainAx, digits=1))
    mtext(expression(paste(h[N], " [mm/5min.]")), side=2, line=2)
    lines(rainSel$dateTime, rainSel$Joh, type="o", pch=20)
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



