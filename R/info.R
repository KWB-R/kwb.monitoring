# formatSettings ---------------------------------------------------------------

#' Format Settings
#' 
#' @param settings list of settings
#' @param settingNames names of the settings, by default: 
#'   \code{names(settings)}
#' @param do.stop passed to \code{kwb.monitoring:::get_H_threshold}, 
#'   \code{kwb.monitoring:::get_Q_threshold}, 
#'   \code{kwb.monitoring:::get_V_threshold}
#'   
formatSettings <- function(
  settings, settingNames = names(settings), do.stop = FALSE
)
{  
  textlines <- character()
  
  if (!is.null(settings$Hthresholds)) {
    textlines <- c(
      textlines, 
      sprintf("Water level threshold: %0.2f m", 
              get_H_threshold(settings, do.stop))
    )
  }
  
  if (!is.null(settings$Qthresholds)) {
    textlines <- c(
      textlines,
      sprintf("Flow intensity threshold: %0.2f L/s", 
              get_Q_threshold(settings, do.stop))
    )
  }
  
  if (!is.null(settings$Vthresholds)) {
    textlines <- c(
      textlines, 
      sprintf("Volume threshold: %0.1f m3", 
              get_V_threshold(settings, do.stop))
    )
  }
  
  if ("evtSepTime" %in% settingNames) {
    textlines <- c(
      textlines, 
      sprintf("New event after pause of: %0.2f h", settings$evtSepTime/3600))
  }
  if ("durationThreshold" %in% settingNames) {
    textlines <- c(
      textlines, 
      sprintf("Min. event duration : %s min", settings$durationThreshold))
  }  
  
  if ("replaceMissingQMethod" %in% settingNames) {
    method <- ifelse(settings$replaceMissingQMethod == "interpolate", 
                     "interpolation", "H-Q-regression")
    textlines <- c(textlines, "", sprintf("Missing Q-values -> %s", method))
  }
  
  if ("sampleEventMethod" %in% settingNames) {
    x <- ifelse(settings$sampleEventMethod == "centres", "", " limits")
    textlines <- c(textlines, sprintf("sampling at %s%s of time intervals", 
                                      settings$sampleEventMethod, x))
  }
  
  paste(textlines, collapse = "\n")
}

# formatBottleEnumeration ------------------------------------------------------

formatBottleEnumeration <- function(caption, bottles, na.text)
{
  if (is.null(bottles) || all(is.na(bottles))) {
    infoText <- na.text
  }
  else {
    infoText <- paste(bottles, collapse = ", ")
  }
  
  sprintf("%s: %s", caption, infoText) 
}

# formatVolumeCompositeSample --------------------------------------------------

formatVolumeCompositeSample <- function(
  volumeCompositeSample, precisionLevel = NULL
)
{
  if (is.null(precisionLevel)) {
    precisionLevel <- 1
  }
  
  dataFrame <- kwb.utils::removeColumns(
    volumeCompositeSample[volumeCompositeSample$useBottle, ], 
    c("V.m3", "useBottle", "ratioNormed")
  )
  
  dataFrame <- addSumRow(dataFrame)

  dataFrame$V.used <- round(dataFrame$V.used, ifelse(precisionLevel==1, 1, 3))
  dataFrame$ratio <- round(dataFrame$ratio * 100, 1)
  dataFrame$V.bottle.mL <- round(dataFrame$V.bottle.mL)
  
  dataFrame <- kwb.utils::renameColumns(dataFrame, list(
    V.used = "V flow (m3)",
    ratio = "ratio (%)",
    V.bottle.mL = "V bottle (mL)"
  ))
  
  dataFrame
}

# addSumRow --------------------------------------------------------------------

#' Add Sum Row
#' 
#' @param x matrix or data frame to which a sum row is added as its last row
#' 
addSumRow <- function(x)
{
  x <- rbind(x, NA, colSums(x, na.rm=TRUE))
  x[nrow(x), 1] <- "Total"
  x
}

# formatEvent ------------------------------------------------------------------

#' Format Event
#' 
#' @param event one row of event data frame as returned by 
#'   \code{\link[kwb.event]{hsEvents}}
#' @param eventNumber number of the event
#' @param precisionLevel 1 (less precise) or 2 (more precise)
#' 
formatEvent <- function(event, eventNumber = 1, precisionLevel = NULL)
{
  if (is.null(precisionLevel)) {
    precisionLevel <- 1
  }
  
  event <- kwb.event::hsEventsToUnit(event, "h")
  
  paste(
    sprintf("Event #%d", eventNumber), 
    "",
    sprintf("From: %s", format(event$tBeg, "%Y-%m-%d %H:%M")),
    sprintf("To:   %s", format(event$tEnd, "%Y-%m-%d %H:%M")),
    "",
    sprintf(
      "Duration:     %s h", 
      formatFloatingPointNumber(event$dur, 10, ifelse(precisionLevel == 1, 1, 3))
    ),
    sprintf("Pause before: %12s", formatDurationDaysHours(event$pBefore)),
    sep = "\n")
}

# formatEventRelation ----------------------------------------------------------

formatEventRelation <- function(
  mergedEventAndStat, V.event, V.sampled, precisionLevel = NULL
)
{
  if (is.null(precisionLevel)) {
    precisionLevel <- 1
  }
  
  paste(
    sprintf(
      "Pause before event: %s", 
      formatDurationDaysHours(
        kwb.event::hsEventsToUnit(mergedEventAndStat, tUnit = "h")$pBefore
      )
    ),
    
    sprintf(
      "V event:             %s m3", 
      formatFloatingPointNumber(V.event, 5, ifelse(precisionLevel == 1, 1, 3))
    ),
    
    sprintf(
      "V sampled:           %s m3", 
      formatFloatingPointNumber(V.sampled, 5, ifelse(precisionLevel == 1, 1, 3))
    ),
    
    sprintf(
      "V sampled / V event: %s %%", 
      formatFloatingPointNumber(V.sampled/V.event * 100, 5, 1)
    ),
    
    sep = "\n")  
}

# formatDuration ---------------------------------------------------------------

formatDuration <- function(duration.s, time.format = .defaultTimeFormat())
{  
  if (is.na(duration.s)) {
    return("-")  
  }
  
  time.formatted <- format(
    as.POSIXct(duration.s, tz="UTC", origin="1900-01-01"), format = time.format
  )
  
  days <- floor(duration.s / 86400)
  
  sprintf("%d days %s", days, time.formatted)
}

# .defaultTimeFormat -----------------------------------------------------------

.defaultTimeFormat <- function()
{
  return ("%H:%M:%S")
}

# formatDurationDaysHours ------------------------------------------------------

formatDurationDaysHours <- function(duration.h)
{  
  if (isNullOrEmpty(duration.h)) {
    return("")
  }
  
  days <- floor(duration.h / 24)
  hours <- duration.h - days * 24
  
  duration <- sprintf("%0.0f d %0.1f h", days, hours)  
  
  lessOneDay <- which(days == 0)  
  duration[lessOneDay] <- sprintf("%0.1f h", hours[lessOneDay])
  
  duration[is.na(duration.h)] <- ""

  duration
}

# formatEventStatistics --------------------------------------------------------

#' Format Event Statistics
#' 
#' @param eventStatistics date frame with columns \emph{V.m3}, \emph{H.max},
#'   \emph{Q.max}, \emph{Q.raw.na},
#' @param precisionLevel number of digits after decimal point
#' 
formatEventStatistics <- function(eventStatistics, precisionLevel = NULL)
{
  if (is.null(precisionLevel)) {
    precisionLevel <- 1
  }
  
  numberLength <- 6
  
  paste(
    sprintf("Volume:            %s m3", 
            formatFloatingPointNumber(eventStatistics$V.m3, numberLength, 
                                      ifelse(precisionLevel == 1, 1, 3))),
    
    sprintf("Max. H:            %s m", 
            formatFloatingPointNumber(eventStatistics$H.max, numberLength, 
                                      ifelse(precisionLevel == 1, 2, 3))),
    
    sprintf("Max. Q:            %s L/s", 
            formatFloatingPointNumber(eventStatistics$Q.max, numberLength, 
                                      ifelse(precisionLevel == 1, 1, 3))),
    
    sprintf("Availability of Q: %s %%", 
            formatFloatingPointNumber(eventStatistics$Q.available, 
                                      numberLength, 1)),
    
    sep="\n")
}

# formatFloatingPointNumber ----------------------------------------------------

formatFloatingPointNumber <- function(
  x, 
  totalLength = 0, 
  precision = 5
)
{
  formatString <- paste("%", totalLength, ".", precision, "f", sep = "")
  sprintf(formatString, x)
}

# formatEventStatisticsTable ---------------------------------------------------

#' Format Event Statistics Table
#' 
#' @param eventStatisticsExtended list of event properties
#' @param precisionLevel 1 (less precise) or 2 (more precise)
#' 
formatEventStatisticsTable <- function(
  eventStatisticsExtended, precisionLevel = NULL
)
{
  if (is.null(precisionLevel)) {
    precisionLevel <- 1
  }
  
  cat("precisionLevel:", precisionLevel, "\n")
  
  x <- eventStatisticsExtended
  x$tBeg <- format(x$tBeg, format = "%Y-%m-%d %H:%M")
  x$dur <- formatFloatingPointNumber(x$dur, 0, ifelse(precisionLevel == 1, 1, 3)) 
  x$pBefore <- formatDurationDaysHours(x$pBefore)
  x$V.m3  <- formatFloatingPointNumber(x$V.m3, 0, ifelse(precisionLevel == 1, 1, 3)) 
  x$Q.max <- formatFloatingPointNumber(x$Q.max, 0, ifelse(precisionLevel == 1, 1, 3)) 
  x$H.max <- formatFloatingPointNumber(x$H.max, 0, ifelse(precisionLevel == 1, 2, 3)) 
  x$Q.available <- formatFloatingPointNumber(x$Q.available, 0, 1) 
  
  columns <- c(
    "eventNumber", "tBeg", "dur", "pBefore", "V.m3", "Q.max", "H.max", 
    "Q.available"
  )
  
  kwb.utils::checkForMissingColumns(x, columns)
  
  x <- x[, columns]
  
  kwb.utils::renameColumns(x, list(
    eventNumber = "Event", 
    tBeg = "Start",
    dur = "Duration (h)",
    pBefore = "Pause before",
    V.m3 = "Volume (m3)",
    Q.max = "Q max (L/s)",
    H.max = "H max (m)",
    Q.available = "Q availability (%)"
  ))
}
