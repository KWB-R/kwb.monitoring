# read roof runoff.
readTipbucket <- function(path, dateFormat, timeZone)
{
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
  
  names(avData) <- avFiles
  
  # make table with start and end dateTimes of each available data file
  timeWindows <- dplyr::tbl_df(as.data.frame(matrix(nrow=0, ncol=3)))
  names(timeWindows) <- c("file", "start", "end")
  
  for(i in 1:length(avData)){
    
    rowi <- data.frame(file=names(avData)[i],
                       start=min(avData[[i]]$dateTime),
                       end=max(avData[[i]]$dateTime))
    
    timeWindows <- rbind(timeWindows, rowi)
  }
  
  timeWindows$file  <- as.character(timeWindows$file)
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
  runoffTimeSeries <- dplyr::tbl_df(runoffTimeSeries[, 2:3])
  
  # handle data points > measurement limit:
  
  # find data points with non-digit characters
  ndc <- which(!grepl(pattern='^[0123456789]+$', 
                      x=runoffTimeSeries$Q))
  if(length(ndc)>0){
    runoffTimeSeries$Q[ndc] <- '-9999'
  }
  runoffTimeSeries$Q <- as.integer(runoffTimeSeries$Q)
  runoffTimeSeries$Q[ndc] <- NA_integer_
  
  # format dateTime as posixct
  runoffTimeSeries$dateTime <- as.POSIXct(runoffTimeSeries$dateTime,
                                          format=dateFormat,
                                          tz=timeZone)
  
  # return time series
  return(runoffTimeSeries)
}

# plot roof event
# *****************switch for which one to plot, or both together
# *****************adjust names -> not 'roof'
plotRoofEvent <- function(site, tBeg, tEnd, roofData, rainData, rainGauge,
                          rainScale, dt, Qmax)
{
  tBeg <- as.POSIXct(tBeg, format="%Y-%m-%d %H:%M", tz="Etc/GMT-1")
  tEnd <- as.POSIXct(tEnd, format="%Y-%m-%d %H:%M", tz="Etc/GMT-1")
  
  # filter data  
  roofSel <- dplyr::filter(roofData, dateTime >= tBeg & dateTime <= tEnd)
  rainSel <- dplyr::filter(rainData, dateTime >= tBeg & dateTime <= tEnd)
  
  # compute roof runoff volume
  AA   <- roofSel$Q[2:(nrow(roofSel))]
  aa   <- roofSel$Q[1:(nrow(roofSel)-1)]
  hh   <- as.numeric(roofSel$dateTime[2:(nrow(roofSel))] -
                       roofSel$dateTime[1:(nrow(roofSel)-1)])/60
  Vtot <- sum((AA + aa)/2*hh)
  
  # make plot
  tAx  <- seq(tBeg, tEnd, by=dt)
  
  par(mar=c(3,5,2,5))
  plot(roofSel$dateTime, roofSel$Q, xlim=c(tBeg, tEnd), type="o", pch=20,
       axes=FALSE, xlab="", ylab="", main=paste0(site, ", Gauge ", rainGauge),
       ylim=c(0, Qmax))
  addRain(raindat=rainSel, ymax=Qmax, scale=rainScale, color="grey", rainGauge)
  lines(roofSel$dateTime, roofSel$Q, type="o", pch=20)
  axis(1, at=tAx, labels=format(tAx, format="%H:%M\n%d-%m"), padj=.25, cex.axis=0.8)
  axis(2, las=2)
  mtext(expression(paste(Q[roof], " [l/h]")), side=2, line=3, cex=1.5)
  box()
  
  eq <- bquote(paste(h[N] == .(sum(pull(rainSel, rainGauge), na.rm=TRUE)), " mm"))
  text(x=tEnd-3600, y=Qmax-0.1*Qmax, eq, adj=1)
  
  eq <- bquote(paste(V[roof] == .(Vtot), " l"))
  text(x=tEnd-3600, y=Qmax-0.2*Qmax, eq, adj=1)
}

# compute runoff volume (Q data in l/s), times in dateTime as.POSIXct
computeVol <- function(dischargeData, Qcolumn, tBeg, tEnd)
{
  tBeg <- as.POSIXct(tBeg, format="%Y-%m-%d %H:%M", tz="Etc/GMT-1")
  tEnd <- as.POSIXct(tEnd, format="%Y-%m-%d %H:%M", tz="Etc/GMT-1")
  
  Qsel <- filter(dischargeData, dateTime >= tBeg & dateTime <= tEnd & !is.na(Qcolumn))
  
  AA   <- pull(Qsel, Qcolumn)[2:(nrow(Qsel))]
  aa   <- pull(Qsel, Qcolumn)[1:(nrow(Qsel)-1)]
  hh   <- as.numeric(Qsel$dateTime[2:(nrow(Qsel))] -
                       Qsel$dateTime[1:(nrow(Qsel)-1)])*60
  Vtot <- sum((AA + aa)/2*hh)
  
  return(Vtot)
}


# compute SPECIFIC loads [emitted mass/m2] for all substances at all measurement points
# detLimit = "zero", "half", "detLim" -> switch for treatment of data at detection limit
# load units are: microgram if concentration in microgram/l, or miligram if conc. in mg/l
computeLoads <- function(site, 
                         detLimit,
                         outFile)
{
  require(readxl)
  
  # make specific runoff for all points
  {
    specQf <- specQfacade(site=site)
    specQr <- specQroof(site=site)
    specQs <- specQsite(site=site)
  }
  
  # grab and format concentration data
  {
    rawdir <- "//medusa/projekte$/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring"
    
    cc <-  readxl::read_xlsx(path = paste0(rawdir,"/Felddatenbank_Konzentrationen_BaSaR.xlsx"),
                             sheet = site,
                             col_names = T,
                             skip = 5,
                             col_types = "text",
                             na=c("","na"),
                             .name_repair = "minimal")
    
    cc$tBegRain <- as.POSIXct(cc$tBegRain, format="%d.%m.%Y %H:%M", tz="Etc/GMT-1")
    
    # Kanal
    ccKanal <- cc[, c(1, 17:61)] # col 1 = tBegRain
    ccKanal <- ccKanal[rowSums(!is.na(ccKanal)) > 1, ] # remove rows with only NAs
    
    # facade
    ccFacade <- cc[, c(1, 66, 72:115)] # col 1 = tBegRain, col 66 = analyzed side (N, O, W, S)
    ccFacade <- ccFacade[rowSums(!is.na(ccFacade)) > 2, ] # remove rows with only NAs
    
    # roof
    ccRoof <- cc[, c(1, 122:165)] # col 1 = tBegRain
    ccRoof <- ccRoof[rowSums(!is.na(ccRoof)) > 1, ] # remove rows with only NAs
  }
  
  # make facade loads and concentrations
  {
    # make table of analyzed events from specQf, with same format as concentrations table
    {
      specQfAn            <- specQf[!is.na(specQf$`LIMS-Nr`), ]
      specQfAn2           <- tbl_df(data.frame(matrix(nrow=0, ncol=ncol(specQfAn))))
      colnames(specQfAn2) <- colnames(specQfAn)
      currentRow          <- 0
      
      for(i in 1:nrow(specQfAn)){
        spl <- strsplit(specQfAn$analysierte_Flasche[i], split=",")[[1]]
        nAnFli <- length(spl)
        
        for(j in 1:nAnFli){
          currentRow <- currentRow+1
          specQfAn2  <- rbind(specQfAn2, specQfAn[i, ])
          spl[j]     <- gsub(" ", "", spl[j])
          spl[j]     -> specQfAn2$analysierte_Flasche[currentRow]
        }
      }
    }
    
    # match concentrations and specific Q using tBegRain and analyzed side to ensure
    # they are in the same order
    {
      idQ   <- paste(as.character(specQfAn2$tBegRain),
                     as.character(specQfAn2$analysierte_Flasche))
      
      idC   <- paste(as.character(ccFacade$tBegRain),
                     as.character(ccFacade$beprobte_Flasche))
      
      index <- match(idC, idQ)
      
      
      # leave only rows with both specQ and concentration
      specQfAn2 <- specQfAn2[index, ]
    }
    
    # make table for holding loads
    {
      colsSpecQf <- c(1:7, 10, 19:22)
      
      specLoadf <- tbl_df(data.frame(matrix(nrow=0,
                                            ncol=2+length(colsSpecQf) +
                                              ncol(ccFacade[3:ncol(ccFacade)]))))
      
      colnames(specLoadf) <- c("tBegRain",
                               "tEndRain",
                               "Regenh?he_mm",
                               "Anzahl_Ereignisse",
                               "Regenschreiber",
                               "Lufttemperatur_gradC_mean",
                               "Lufttemperatur_gradC_SD",
                               "analysierte_Flasche",
                               "Wind_v_mean_m_s",
                               "Wind_v_StAbw",
                               "Windrichtung_mean_Grad",
                               "Windrichtung_StAbw",
                               "areaRinne_m2",
                               "specQ",
                               colnames(ccFacade[3:ncol(ccFacade)]))
    }
    
    # run through events and compute loads
    {
      for(i in 1:nrow(ccFacade)){
        
        # find corresponding specQ
        specQfi <- dplyr::select(specQfAn2,
                                 contains(paste0("specQ",
                                                 ccFacade$beprobte_Flasche[i])))
        specQfi <- specQfi[[1]][i]
        
        # find corresponding area above Rinne
        areaRinnei <- dplyr::select(specQfAn2, contains(paste0("Fl?cheRinne",
                                                               ccFacade$beprobte_Flasche[i],
                                                               "_m2")))
        areaRinnei <- areaRinnei[[1]][i]
        
        # make vector with concentrations for all substances
        ccFacadei  <- unlist(c(ccFacade[i, 3:ncol(ccFacade)]))
        ccFacadei2 <- numeric()
        
        # loop through substances and compute loads
        for(j in 1:length(ccFacadei)){
          
          stoffj <- unlist(strsplit(ccFacadei[j], split = "<"))
          
          if(length(stoffj)>1){
            
            stoffj <- stoffj[2]
            stoffj <- as.numeric(gsub(pattern=",", replacement=".", x=stoffj))
            
            # for values in detection limit, use switch (zero, half or full detection limit)
            stoffj <- ifelse(detLimit=="zero", 0,
                             ifelse(detLimit=="half", stoffj*0.5,
                                    stoffj))
          } else {
            
            stoffj <- as.numeric(stoffj)
          }
          
          ccFacadei2 <- c(ccFacadei2, stoffj)
        }
        
        names(ccFacadei2) <- names(ccFacadei)
        
        specLoadfi  <- as.data.frame(t(specQfi*ccFacadei2))
        namesAll    <- c(names(specQfAn2[, colsSpecQf]), "areaRinne_m2", "specQ",
                         names(specLoadfi))
        rowi        <- cbind(specQfAn2[i, colsSpecQf], areaRinnei, specQfi, specLoadfi)
        names(rowi) <- namesAll
        specLoadf   <- rbind(specLoadf, rowi)
      }
      
      specLoadf <- tbl_df(specLoadf)
    }
    
    # make facade concentrations table with same format as loads table
    {
      concf <- specLoadf[, 1:14]
      concf <- tbl_df(cbind(concf, ccFacade[3:ncol(ccFacade)]))
      colnames(concf) <- colnames(specLoadf)
    }
  }
  
  # make roof loads and concentrations
  {
    # make table for holding results
    {
      colsSpecQr <- c(1:11)
      
      specLoadr <- tbl_df(data.frame(matrix(nrow=0,
                                            ncol=length(colsSpecQr) +
                                              ncol(ccRoof[2:ncol(ccRoof)]))))
      colnames(specLoadr) <- c("tBegRain",
                               "tEndRain",
                               "Regenh?he_mm",
                               "Anzahl_Ereignisse",
                               "Regenschreiber",
                               "Lufttemperatur_gradC_mean",
                               "Lufttemperatur_gradC_SD",
                               "tBegHydraul",
                               "tEndHydraul",
                               "Abflussvol_l",
                               "Abflussvol_aus_Regression",
                               colnames(ccRoof[2:ncol(ccRoof)]))
    }
    
    # match concentrations and specific Q using tBegRain
    {
      idQ   <- as.character(specQr$tBegRain)
      idC   <- as.character(ccRoof$tBegRain)
      index <- match(idC, idQ)    
      
      # leave only rows with both specQ and concentration
      specQr <- specQr[index, ]
    }
    
    # run through events and compute loads
    {
      for(i in 1:nrow(ccRoof)){
        
        # make vector with concentrations for all substances
        ccRoofi  <- unlist(c(ccRoof[i, 2:ncol(ccRoof)]))
        ccRoofi2 <- numeric()
        
        for(j in 1:length(ccRoofi)){
          
          stoffj <- unlist(strsplit(ccRoofi[j], split = "<"))
          
          if(length(stoffj)>1){
            
            stoffj <- stoffj[2]
            stoffj <- as.numeric(gsub(pattern=",", replacement=".", x=stoffj))
            stoffj <- ifelse(detLimit=="zero", 0,
                             ifelse(detLimit=="half", stoffj*0.5,
                                    stoffj))
          } else {
            
            stoffj <- as.numeric(stoffj)
          }
          
          ccRoofi2 <- c(ccRoofi2, stoffj)
        }
        
        names(ccRoofi2) <- names(ccRoofi)
        
        specLoadri  <- as.data.frame(t(specQr$specQ[i]*ccRoofi2))
        namesAll    <- c(names(specQr[, colsSpecQr]), names(specLoadri))
        rowi        <- cbind(specQr[i, colsSpecQr], (specLoadri))
        names(rowi) <- namesAll
        specLoadr   <- rbind(specLoadr, rowi)
      }
      
      specLoadr <- tbl_df(specLoadr)
    }
    
    # make roof concentrations table with same format as loads table
    {
      concr <- specLoadr[, 1:11]
      concr <- tbl_df(cbind(concr, ccRoof[2:ncol(ccRoof)]))
      colnames(concr) <- colnames(specLoadr)
    }
  }
  
  # make Kanal loads and concentrations
  {
    # make table for holding loads
    {
      colsSpecQs <- c(1:10)
      
      specLoads <- tbl_df(data.frame(matrix(nrow=0,
                                            ncol=length(colsSpecQs) +
                                              ncol(ccKanal[2:ncol(ccKanal)]))))
      colnames(specLoads) <- c("tBegRain",
                               "tEndRain",
                               "Regenh?he_mm",
                               "Anzahl_Ereignisse",
                               "Regenschreiber",
                               "Lufttemperatur_gradC_mean",
                               "Lufttemperatur_gradC_SD",
                               "tBegHydraul",
                               "tEndHydraul",
                               "Abflussvol_l",
                               colnames(ccKanal[2:ncol(ccKanal)]))
    }
    
    # match concentrations and specific Q using tBegRain
    {
      idQ   <- as.character(specQs$tBegRain)
      idC   <- as.character(ccKanal$tBegRain)
      index <- match(idC, idQ)
      
      # leave only rows with both specQ and concentration
      specQs <- specQs[index, ]
    }
    
    # run through events and compute loads
    {
      for(i in 1:nrow(ccKanal)){
        
        # make vector with concentrations for all substances
        ccKanali  <- unlist(c(ccKanal[i, 2:ncol(ccKanal)]))
        ccKanali2 <- numeric()
        
        for(j in 1:length(ccKanali)){
          
          stoffj <- unlist(strsplit(ccKanali[j], split = "<"))
          
          if(length(stoffj)>1){
            
            stoffj <- stoffj[2]
            stoffj <- as.numeric(gsub(pattern=",", replacement=".", x=stoffj))
            stoffj <- ifelse(detLimit=="zero", 0,
                             ifelse(detLimit=="half", stoffj*0.5,
                                    stoffj))
          } else {
            
            stoffj <- as.numeric(stoffj)
          }
          
          ccKanali2 <- c(ccKanali2, stoffj)
        }
        
        names(ccKanali2) <- names(ccKanali)
        
        specLoadsi  <- as.data.frame(t(specQs$specQ[i]*ccKanali2))
        namesAll    <- c(names(specQs[, colsSpecQs]), names(specLoadsi))
        rowi        <- cbind(specQs[i, colsSpecQs], (specLoadsi))
        names(rowi) <- namesAll
        specLoads   <- rbind(specLoads, rowi)
      }
      
      specLoads <- tbl_df(specLoads)
    }
    
    # make Kanal concentrations table with same format as loads table
    {
      concs <- specLoads[, 1:10]
      concs <- tbl_df(cbind(concs, ccKanal[2:ncol(ccKanal)]))
      colnames(concs) <- colnames(specLoads)
    }
    
  }
  
  # write result tables
  {
    # loads
    outFileFacade_load <- paste0(rawdir, "/_DatenAnalyse/", outFileFacade, "_load.txt")
    outFileRoof_load   <- paste0(rawdir, "/_DatenAnalyse/", outFileRoof, "_load.txt")
    outFileSewer_load  <- paste0(rawdir, "/_DatenAnalyse/", outFileSewer, "_load.txt")
    
    write.table(specLoadf, file=outFileFacade_load, sep=";", quote=FALSE, row.names=FALSE)
    write.table(specLoadr, file=outFileRoof_load, sep=";", quote=FALSE, row.names=FALSE)
    write.table(specLoads, file=outFileSewer_load, sep=";", quote=FALSE, row.names=FALSE)
    
    # concentrations
    outFileFacade_conc <- paste0(rawdir, "/_DatenAnalyse/", outFileFacade, "_conc.txt")
    outFileRoof_conc   <- paste0(rawdir, "/_DatenAnalyse/", outFileRoof, "_conc.txt")
    outFileSewer_conc   <- paste0(rawdir, "/_DatenAnalyse/", outFileSewer, "_conc.txt")
    
    write.table(concf, file=outFileFacade_conc, sep=";", quote=FALSE, row.names=FALSE)
    write.table(concr, file=outFileRoof_conc, sep=";", quote=FALSE, row.names=FALSE)
    write.table(concs, file=outFileSewer_conc, sep=";", quote=FALSE, row.names=FALSE)
  }
}
