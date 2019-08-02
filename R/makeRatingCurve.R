# fill intervals with low Q quality (due to low H) using rating curve (stage-discharge relationship)
makeRatingCurve <- function(hydraulicEvent, raw,
                            Qmin, hKritMin, HQzero,
                            hSplit,
                            a0, b0, c0,
                            modelFile)
{
  # if we are working with validated data, temporarily change column names of H and Q to Hcorr and Qcorr
  if(!raw){c("H", "Q") -> names(hydraulicEvent)[c(3, 5)]} 
  
  # filter data to remove:
  # - Q and H below measurement limit of v sensor (hKritMin = 0.03 m), and
  # - any remaining points above (and close to) hKritMin where Q is unreliable (Q < Qmin)
  {
    H <- kwb.utils::selectColumns(hydraulicEvent, "H")
    Q <- kwb.utils::selectColumns(hydraulicEvent, "Q")
    
    hq <- hydraulicEvent[H > hKritMin & Q > Qmin, ]
    hq <- hq[! is.na(hq$H) & ! is.na(hq$Q), ]
    
    # get maxima of Q and H  
    maxH <- max(hq$H, na.rm=TRUE) + 0.2*max(hq$H, na.rm=TRUE)
    maxQ <- max(hq$Q, na.rm=TRUE) + 0.2*max(hq$Q, na.rm=TRUE)
    
    # split remaining data, H < pipe radius, H > pipe radius
    is_below <- hq$H <= hSplit
    hq1 <- hq[is_below, ]
    hq2 <- hq[! is_below, ]
  }
  
  # fit rating curve model: Q = a*(H*100 + b)^c to both halves. H is converted from m
  # to cm to make convergence easier
  {
    ssq <- function(data, par)
    {
      a=par[1]
      b=par[2]
      c=par[3]
      with(data, sum((Q - (a*(H*100 + b)^c))^2))
    }
    
    res1 <- stats::optim(par=c(a0, b0, c0), fn=ssq, data=hq1)
    
    if(nrow(hq2) > 0)
    {
      res2 <- stats::optim(par=c(res1$par[1], res1$par[2], res1$par[3]), fn=ssq, data=hq2)
    }
  }
  
  # find intersection of both curves
  {
    Hpred  <- seq(0, maxH, by=0.001)
    Qpred1 <- res1$par[1]*(Hpred*100 + res1$par[2])^res1$par[3]
    xx     <- data.frame(Qpred=Qpred1, Hpred)
    
    if(nrow(hq2) > 0)
    {
      Qpred2 <- res2$par[1]*(Hpred*100 + res2$par[2])^res2$par[3]
      xx     <- data.frame(Hpred, 
                           Qpred1, 
                           Qpred2,
                           sqd=(Qpred1 - Qpred2)^2)
      joinPt <- xx[order(xx$sqd)[2], ] # potential issue: what if there are more intersection points?
      breakh <- joinPt$Hpred
      Hpred1 <- seq(0, breakh, by=0.001)
      Hpred2 <- seq(breakh, maxH, by=0.001)
      Qpred1 <- res1$par[1]*(Hpred1*100 + res1$par[2])^res1$par[3]
      Qpred2 <- res2$par[1]*(Hpred2*100 + res2$par[2])^res2$par[3]
      xx     <- rbind(cbind(Qpred=Qpred1, Hpred=Hpred1),
                      cbind(Qpred2, Hpred2))
    }
  }
  
  # Q values below HQzero are set to zero
  {
    xx[xx[, 2]<= HQzero, 1] <- 0
  }
  
  # Q values between HQzero and first point of fitted curve are joined linearly
  {
    xna    <- which(is.na(xx[, 1]))
    lastNA <- ifelse(length(xna) > 0, max(xna), NA)
    
    if(!is.na(lastNA))
    {
      H0     <- HQzero
      H1     <- xx[lastNA+1, "Hpred"]
      Q0     <- 0
      Q1     <- xx[lastNA+1, "Qpred"]
      mm     <- (Q1 - Q0)/(H1 - H0)
      
      xx[is.na(xx[, 1]), "Qpred"] <- xx[is.na(xx[, 1]), "Hpred"]*mm
    }
  }
  
  # compute rmse
  {
    Qobs <- hq$Q
    
    if(nrow(hq2) > 0)
    {
      H <- hq$H
      is_below <- H < breakh
      
      h1 <- H[is_below]
      Qmod1 <- res1$par[1]*(h1*100 + res1$par[2])^res1$par[3]
      
      h2 <- H[! is_below]
      Qmod2 <- res2$par[1]*(h2*100 + res2$par[2])^res2$par[3]
      Qmod  <- c(Qmod1, Qmod2)
      
    } else {
      
      Qmod <- res1$par[1]*(hq$H*100 + res1$par[2])^res1$par[3]
    }
    
    rmse  <- (sum((Qobs - Qmod)^2)/length(Qobs))^0.5
  }
  
  # plot H-Q data and resulting rating curve
  {
    graphics::par(mar=c(4,4.5,1,1), mfrow=c(1,1))
    graphics::plot(hydraulicEvent$Q, hydraulicEvent$H, pch=20, col="grey", cex=1.25, axes=FALSE,
         xlim=c(0, maxQ), ylim=c(0, maxH), 
         xlab="Q [l/s]", ylab="H [m]", main=format(hydraulicEvent$dateTime[1], format="%Y-%m-%d"))
    graphics::abline(h=hKritMin, lty=2)
    graphics::abline(h=HQzero, lty=2)
    graphics::abline(h=hSplit, lty=3)
    graphics::points(hq$Q, hq$H, cex=0.1, pch=20)
    graphics::lines(xx[, 1], xx[, 2], col="blue")
    graphics::axis(1, at=seq(0, maxQ, length=7), labels=round(seq(0, maxQ, length=7), digits=3))
    graphics::axis(2, at=seq(0, maxH, length=7), labels=round(seq(0, maxH, length=7), digits=3), las=2)
    graphics::box()
    graphics::text(x=maxQ, y=hKritMin+0.2*hKritMin, labels="hKritMin", adj=1)
    graphics::text(x=maxQ, y=HQzero+0.2*hKritMin, labels="HQzero", adj=1)
    graphics::text(x=0, y=maxH, labels=paste("rmse =", round(rmse, digits=3), "L/s"), adj=0)
  }
  
  # write out model parameters
  {
    if(!is.na(modelFile))
    {
      outFile <- paste0("//Medusa/Projekte$/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_DatenAnalyse/ratingCurves/",
                        modelFile, ".txt")
      
      if(nrow(hq2) > 0)
      {
        outData <- c(res1$par, res2$par, breakh, HQzero)
        
      } else {
        
        outData <- c(res1$par, HQzero)
      }
      
      write(outData, file=outFile, ncolumns=length(outData))
      
      # print out model information
      print(paste("rmse =", round(rmse, digits=3), "L/s"), quote=FALSE)
      print(paste("model file", modelFile, "written to '.../_DatenAnalyse/ratingCurves'"), 
            quote=FALSE)
    }
  }
}
