# fill hydraulics with saved rating curve file
fillHydraulics <- function(
  hydraulics, raw, rainData, rainScale, rainGauge, QmessZeroToNA, modelFile,
  tBeg, tEnd, dtPlot, Hgaps, hKrit, hKritMin, hSamplerOn, hSamplerOff, makePlot,
  ratingCurveDir
)
{
  # format time stamps
  {
    # Define helper function
    as_posix <- function(x) {
      as.POSIXct(x, format = "%Y-%m-%d %H:%M", tz = "Etc/GMT-1")
    }
    
    tBeg <- as_posix(tBeg)
    tEnd <- as_posix(tEnd)
    
    tax  <- seq(tBeg, tEnd, by = dtPlot)
  }
  
  # grab only event data and fill H gaps with user-defined data
  {
    times <- kwb.utils::selectColumns(hydraulics, "dateTime")
    hydraulicEvent <- hydraulics[times >= tBeg & times <= tEnd, ]
    
    if (! is.na(Hgaps)) {
      hydraulicEvent$H[is.na(hydraulicEvent$H)] <- Hgaps
    }
    
    # Select rows from rain data that are within the rain event 
    times <- kwb.utils::selectColumns(rainData, "dateTime")
    rainSel <- rainData[times >= tBeg & times <= tEnd, ]
  }
  
  # if we are working with validated data, temporarily change column names for H 
  # and Q to Hcorr and Qcorr
  {
    names(hydraulicEvent)[c(3, 5)] <- c("H", "Q")
  }
  
  # fill H through linear interpolation (here I assume only very small gaps in H)
  {
    fullH <- kwb.utils::selectColumns(hydraulicEvent, c("dateTime", "H"))
    
    fullH <- fullH[stats::complete.cases(fullH), ]
    
    filledH <- stats::approx(
      x = fullH$dateTime, y = fullH$H, xout = hydraulicEvent$dateTime, rule = 2
    )
    
    filledH$y -> hydraulicEvent$Hfilled
  }
  
  # read model parameters
  {
    inFile <- file.path(ratingCurveDir, paste0(modelFile, ".txt"))
    
    modPar <- scan(file=inFile)
    
    if (length(modPar) == 8) {
      
      a1 <- modPar[1]
      b1 <- modPar[2]
      c1 <- modPar[3]
      
      a2 <- modPar[4]
      b2 <- modPar[5]
      c2 <- modPar[6]
      
      breakh <- modPar[7]
      HQzero <- modPar[8]
      
    } else {
      
      # res1$par, HQzero
      a1 <- modPar[1]
      b1 <- modPar[2]
      c1 <- modPar[3]
      
      HQzero <- modPar[4]
    }
  }
  
  # make predicted Q time series using rating curve. as above, H is converted from m 
  # to cm, since this is how the model is implemented above
  {
    f <- function(a, b, c, d) {
      a * (b * 100 + c)^d
    }
    
    hydraulicEvent$Qpred <- NA
    
    # use fitted model to predict Q
    if (length(modPar) == 8) {
      
      Hpred <- hydraulicEvent$Hfilled[hydraulicEvent$Hfilled < breakh]

      hydraulicEvent$Qpred[hydraulicEvent$Hfilled < breakh] <- f(
        a = modPar[1], b = Hpred, c = modPar[2], d = modPar[3]
      )
      
      Hpred <- hydraulicEvent$Hfilled[hydraulicEvent$Hfilled >= breakh]
      
      hydraulicEvent$Qpred[hydraulicEvent$Hfilled >= breakh] <- f(
        a = modPar[4], b = Hpred, c = modPar[5], d = modPar[6]
      )
      
    } else {
      
      Hpred <- hydraulicEvent$Hfilled
      
      hydraulicEvent$Qpred <- f(
        a = modPar[1], b = Hpred, c = modPar[2], d = modPar[3]
      )
    }
    
    # Q=0 below HQzero
    0 -> hydraulicEvent$Qpred[hydraulicEvent$Hfilled <= HQzero]
    
    # Q values between HQzero and first point of fitted curve are joined linearly,
    # if there are NAs for Qpred below a certain H threshold:
    xNA <- which(hydraulicEvent$Hfilled>HQzero)
    
    if (length(xNA) > 0) {
      
      # grab the minimum water level with existing Qpred that is larger than HQzero
      Qpred <- kwb.utils::selectColumns(hydraulicEvent, "Qpred")
      Hfilled <- kwb.utils::selectColumns(hydraulicEvent, "Hfilled")
      
      HminQnotNA  <- hydraulicEvent[! is.na(Qpred) & Hfilled > HQzero, ]
      
      min_h <- min(HminQnotNA$Hfilled, na.rm = TRUE)
      
      HminQnotNA2 <- HminQnotNA[HminQnotNA$Hfilled == min_h, ]
      
      H0 <- HQzero
      H1 <- HminQnotNA2$Hfilled[1]
      Q0 <- 0
      Q1 <- HminQnotNA2$Qpred[1]
      mm <- (Q1 - Q0) / (H1 - H0)
      
      index <- which(is.na(hydraulicEvent$Qpred))
      
      hydraulicEvent$Qpred[index] <- Q0 + hydraulicEvent$Hfilled[index]*mm
    }
    
    # separate actual measured discharges from those computed by the PCM
    hydraulicEvent$QcompPCM <- NA
    indices <- which(hydraulicEvent$H <= hKritMin)
    hydraulicEvent$QcompPCM[indices] <- hydraulicEvent$Q[indices]
    
    hydraulicEvent$QmessPCM <- NA
    indices <- which(hydraulicEvent$H > hKritMin)
    hydraulicEvent$QmessPCM[indices] <- hydraulicEvent$Q[indices]
    
    # create column for storing Qfilled
    hydraulicEvent$Qfilled <- NA
    
    # make dubious Q=0 measurements to NA
    if (QmessZeroToNA) {
      hydraulicEvent$QmessPCM[hydraulicEvent$QmessPCM <= 0] <- NA
    }
    
    # combine measured and predicted Q values: wherever Qmeasured is not available
    # Qfilled=Qpredicted, otherwise Q=Qmeasured
    hydraulicEvent$Qfilled <- ifelse(
      is.na(hydraulicEvent$QmessPCM),
      hydraulicEvent$Qpred, 
      hydraulicEvent$QmessPCM
    )
  }
  
  # plot filled hydraulic event
  {
    if (makePlot) {
  
      maxQ <- 1.2 * max(hydraulicEvent$Qfilled, na.rm = TRUE)
      maxH <- 1.2 * max(hydraulicEvent$H, na.rm = TRUE)

      graphics::par(mar = c(4, 4, 0.2, 3), mfcol = c(2, 1))
      
      graphics::plot(
        hydraulicEvent$dateTime, hydraulicEvent$QmessPCM, xlim = c(tBeg, tEnd), 
        ylim = c(0, maxQ), xaxt = "n", las = 2, ylab = "Q [l/s]", xlab = ""
      )
      
      addRain(
        raindat = rainSel, ymax = maxQ, scale = rainScale, color = "grey", 
        rainGauge
      )
      
      graphics::points(
        hydraulicEvent$dateTime, hydraulicEvent$QcompPCM, col = "grey"
      )
      
      graphics::lines(
        hydraulicEvent$dateTime, hydraulicEvent$Qfilled, col = "red", lwd = 2
      )
      
      graphics::axis(
        1, at = tax, lab = format(tax, format = "%m-%d\n%H:%M"), padj = 0.1, 
        cex.axis = 1, las = 2
      )
      
      graphics::abline(h = 0, col = "grey")
      
      graphics::legend(
        x = tEnd - (tEnd - tBeg) * 0.20, y = 0.99 * maxQ, 
        legend = c("QmessPCM", "QcompPCM", "QFilled"), pch = c(1, 1, NA), 
        lty = c(NA, NA, 1), col = c("black", "grey", "red")
      )
      
      graphics::plot(
        hydraulicEvent$dateTime, hydraulicEvent$H, xlim = c(tBeg, tEnd), 
        ylim = c(0, maxH), xaxt = "n", las = 2, ylab = "H [m]", xlab = ""
      )
      
      graphics::lines(
        hydraulicEvent$dateTime, hydraulicEvent$Hfilled, col = "red", lwd = 2
      )
      
      hKritLine <- seq(tBeg - 3600, tEnd + 3600, by = 500)
      
      graphics::points(
        x = hKritLine, y = rep(hKrit, times = length(hKritLine)), pch = 20, 
        col = "blue", type = "p", cex = 0.75
      )
      
      graphics::abline(h = hKritMin, lty = 1, col = "blue")
      
      graphics::abline(h = c(hSamplerOn, hSamplerOff), lty = c(2, 3))
      
      graphics::abline(h = 0, col = "grey")
      
      #legend(x=tEnd - (tEnd-tBeg)*0.20, y=maxH,
      #       legend=c("HmessPCM", "Hfilled", "hKrit", "hKritMin", "hSamplerOn", "hSamplerOff"),
      #       pch=c(    1,          NA,        20,     NA,          NA,           NA),
      #       lty=c(    NA,         1,         NA,     1,           2,            3),
      #       col=c(    "black",    "red",    "blue",  "blue",     "black",      "black"    ))
      
      graphics::axis(
        1, at = tax, lab = format(tax, format = "%m-%d\n%H:%M"), padj = 0.1, 
        cex.axis = 1, las = 2
      )
    }
  }
  
  # if we are working with validated data, undo changes in column names made at the begginning
  {
    if (! raw) {
      names(hydraulicEvent)[c(3, 5)] <- c("Hcorr", "Qcorr")
    }
  }
  
  hydraulicEvent
}
