# read roof runoff
readTipbucket <- function(path, dateFormat, timeZone){
  
  require(dplyr)
  
  # read available files
  setwd(path)
  avFiles <- list.files(path, pattern=".csv$")
  avData  <- lapply(avFiles,
                    read.table,
                    header=FALSE,
                    skip=28,
                    sep=";",
                    quote="\"",
                    dec=".",
                    colClasses=c("numeric", "character", "character"),
                    col.names=c("id", "dateTime", "Q"),
                    stringsAsFactors=FALSE)
  
  # element names = file names
  names(avData) <- avFiles
  
  # make table with start and end dateTimes of each available data file
  timeWindows <- as.data.frame(matrix(nrow=0, ncol=3))
  names(timeWindows) <- c("file", "start", "end")
  
  for(i in 1:length(avData)){
    
    # format all time columns as posixct
    avData[[i]]$dateTime <- as.POSIXct(avData[[i]]$dateTime,
                                       format='%d.%m.%Y %H:%M',
                                       tz='Etc/GMT-1')
    
    # strip whitespace from discharge values
    avData[[i]]$Q <- sub(pattern='\\s*', replacement='', x=avData[[i]]$Q)

    # grab file name, start and end of each file
    rowi <- data.frame(file=names(avData)[i],
                       start=min(avData[[i]]$dateTime),
                       end=max(avData[[i]]$dateTime))
    
    timeWindows <- rbind(timeWindows, rowi)
  }
  
  timeWindows$start <- as.POSIXct(timeWindows$start, tz=timeZone,
                                  format=dateFormat)
  timeWindows$end <- as.POSIXct(timeWindows$end, tz=timeZone,
                                format=dateFormat)
  
  # During the monitoring, it was necessary to adjust the logger's clock several times (the
  # clock drifted 1-2 min. per month more or less). Adjustment was made by making the logger
  # time equal to our cell phones' time. Since we use continuous logging ("Ringspeicher"), every
  # data file downloaded from the logger contains all the data since the beginning of the record
  # until the moment it was downloaded (same as in PCM).
  # Every time the logger's clock was adjusted, all the time stamps shifted -> when data was down-
  # loaded the next time, the new file had slightly shitfed time stamps even though it is the
  # exact same record ( -> it should have the same time stamps during the overlapping period).
  # For this reason, column "start" in "timeWindows" has several times on the same day, which
  # is just due to clock adjustment; apart from the slightly shifted time stamps, the time series
  # inside the file is exactly the same.
  # In order to build the continuous time series, I simply added a new column to timeWindows
  # containing the day ("startDay") and time = 00:00:00, so as to make a common starting point
  # for all the files coming from the same day (as if we had never adjusted the logger's clock).
  # This allows binding the data incrementally in time, using only the largest file (the last
  # before logger reset).
  timeWindows$startDay <- as.POSIXct(paste(as.Date(timeWindows$start),
                                           "00:00:00"),
                                     format="%Y-%m-%d %H:%M:%S",
                                     tz="Etc/GMT-1")
  
  timeWindows$durations <- (as.numeric(timeWindows$end) - 
                              as.numeric(timeWindows$startDay))
  
  
  # identify which files are needed for covering full time period by grabbing the file
  # with the maximum duration for each start date
  startDates <- unique(timeWindows$startDay)
  
  baseData <- character(0)
  
  for(i in startDates){
    index        <- which(timeWindows$startDay == i)
    timeWindowsi <- timeWindows[index, ]
    baseData <- c(baseData,
                  timeWindowsi$file[timeWindowsi$durations ==
                                      max(timeWindowsi$durations)])
  }
  
  # make single table using baseData
  runoffTimeSeries <-  do.call("rbind", avData[baseData])
  runoffTimeSeries <- runoffTimeSeries[, 2:3]
  
  # handle data points > measurement limit:
  
  # find data points with non-digit characters
  ndc <- !grepl(pattern='^[0123456789]+$', x=runoffTimeSeries$Q)
  
  if(sum(ndc)>0){
    runoffTimeSeries$Q[ndc] <- '-9999'
  }
  runoffTimeSeries$Q <- as.integer(runoffTimeSeries$Q)
  runoffTimeSeries$Q[ndc] <- NA_integer_
  
  # format dateTime as posixct
  runoffTimeSeries$dateTime <- as.POSIXct(runoffTimeSeries$dateTime,
                                          format=dateFormat,
                                          tz=timeZone)
  
  # remove row names
  rownames(runoffTimeSeries) <- NULL
  
  # return time series
  return(runoffTimeSeries)
}

# compute runoff volume (Q data in l/s), times in dateTime as.POSIXct
computeVol <- function(dischargeData, Qcolumn, tBeg, tEnd){
  
  require(dplyr)
  
  tBeg <- as.POSIXct(tBeg, format="%Y-%m-%d %H:%M", tz="Etc/GMT-1")
  tEnd <- as.POSIXct(tEnd, format="%Y-%m-%d %H:%M", tz="Etc/GMT-1")
  
  Qsel <- dplyr::filter(dischargeData, (dateTime >= tBeg) & 
                          (dateTime <= tEnd) & (!is.na(Qcolumn)))
  
  AA <- dplyr::pull(Qsel, Qcolumn)[2:(nrow(Qsel))]
  aa <- dplyr::pull(Qsel, Qcolumn)[1:(nrow(Qsel)-1)]
  hh <- (as.numeric(Qsel$dateTime[2:(nrow(Qsel))]) - 
           as.numeric(Qsel$dateTime[1:(nrow(Qsel)-1)]))/3600
  Vtot <- sum((AA + aa)/2*hh, na.rm=TRUE)
  
  return(Vtot)
}

# plot event
plotEvent <- function(tBeg, tEnd, dt, 
                      inflowQ, outflowQ, 
                      rainData, rainGauge,
                      rainScale){
  
  # format tBeg and tEnd
  tBeg <- as.POSIXct(tBeg, format="%Y-%m-%d %H:%M", tz="Etc/GMT-1")
  tEnd <- as.POSIXct(tEnd, format="%Y-%m-%d %H:%M", tz="Etc/GMT-1")
  
  # filter data
  inflowQsel <- inflowQ[(inflowQ$dateTime >= tBeg) & (inflowQ$dateTime <= tEnd), ]
  outflowQsel <- outflowQ[(outflowQ$dateTime >= tBeg) & (outflowQ$dateTime <= tEnd), ]
  rainSel <- rainData[(rainData$dateTime >= tBeg) & (rainData$dateTime <= tEnd), ]
  
  # compute runoff volume
  zuV <- computeVol(dischargeData=Qzu, 
                    Qcolumn='Q', 
                    tBeg=tBeg,
                    tEnd=tEnd)
  abV <- computeVol(dischargeData=Qab, 
                    Qcolumn='Q', 
                    tBeg=tBeg,
                    tEnd=tEnd)
  
  # make plot
  tAx  <- seq(tBeg, tEnd, by=dt)
  Qmax <- max(inflowQsel$Q, na.rm=TRUE)
  
  windows(height=5, width=8)
  par(mar=c(3, 5, 2, 5))
  plot(inflowQsel$dateTime, inflowQsel$Q, xlim=c(tBeg, tEnd), type="o", pch=20,
       axes=FALSE, xlab="", ylab="", ylim=c(0, 1.1*Qmax))
  lines(outflowQsel$dateTime, outflowQsel$Q, col='red', type='o', pch=20)
  addRain(raindat=rainSel, ymax=1.1*Qmax, scale=rainScale, color="grey", rainGauge)
  legend(x=tEnd-3600*12,
         y=Qmax,
         legend = c('Zu', 'Ab'), lty=1, col=c('black', 'red'),
         cex=0.8)
  axis(1, at=tAx, labels=format(tAx, format="%H:%M\n%d-%m"), padj=.25, cex.axis=0.8)
  axis(2, las=2)
  mtext("Q [l/h]", side=2, line=3.5, cex=1.25)
  eq <- bquote(paste(h[N] == .(sum(rainSel[, rainGauge]), na.rm=TRUE), " mm"))
  text(x=tEnd-3600, y=0.7*Qmax, eq, adj=1)
  eq <- bquote(paste(V[Zu] == .(zuV), " l"))
  text(x=tEnd-3600, y=0.6*Qmax, eq, adj=1)
  eq <- bquote(paste(V[Ab] == .(abV), " l"))
  text(x=tEnd-3600, y=0.5*Qmax, eq, adj=1)
  box()
}