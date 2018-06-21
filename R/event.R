# getAllTypesOfEvents ----------------------------------------------------------

#' Get All Types of Events
#' 
#' @param hydraulicData data frame containing hydraulic data that is passed to
#'   \code{\link{getHydraulicEvents}}
#' @param settings list of settings, passed to \code{\link{getSamplerEvents}} 
#'   and containing element \code{tstep.fill.s}
#' @param FUN.readSamplerFile e.g. kwb.ogre::readOgreSamplerFileByName or 
#'   kwb.dswt::readDswtSamplerFileByName
#' 
getAllTypesOfEvents <- function(
  hydraulicData = NULL, settings, FUN.readSamplerFile
) 
{
  hydraulicEvents <- if (! is.null(hydraulicData)) {
    
     getHydraulicEvents(hydraulicData, settings)
    
  } else {
    
    NULL
  }

  if (isNullOrEmpty(hydraulicEvents)) {
    
    clean_warning(
      "There are no hydraulic events according to the defined criteria ",
      "(or hydraulicData was NULL)!"
    )
  }
  
  samplingEvents <- getSamplerEvents(settings, FUN.readSamplerFile)
  
  if (isNullOrEmpty(samplingEvents)) {
    
    clean_warning("There are no sampling events!")
  }
  
  mergedEvents <- getMergedEvents(
    hydraulicEvents = hydraulicEvents,
    samplerEvents = samplingEvents$samplerEvents,
    signalWidth = settings$tstep.fill.s # = hsSigWidth(hydraulicEvents)
  )
  
  #cat("time unit of mergedEvents:", .getCurrentTimeUnitOrStop(mergedEvents), "\n")
  #cat("time unit of hydraulicEvents:", .getCurrentTimeUnitOrStop(hydraulicEvents), "\n")
  
  # set pauses of merged events according to the pauses of the hydraulic events
  # that are fully contained in the merged events
  if (! isNullOrEmpty(mergedEvents)) {
    
    mergedEvents <- setPausesOfMergedEvents(hydraulicEvents, mergedEvents)  
  }

  c(samplingEvents, list(hydraulic = hydraulicEvents, merged = mergedEvents))
}

# getHydraulicEvents -----------------------------------------------------------

#' Get Hydraulic Events
#' 
#' @param hydraulicData data frame with columns ...
#' @param settings settings as returned by ...
#' @param eventSettings default: \code{settings$event[[settings$station]]}
#' @param columnQ name of column containing water flows. Default: "Q"
#' @param columnH name of column containing water levels. Default: "H"
#' 
getHydraulicEvents <- function 
(
  hydraulicData,
  settings,
  eventSettings = settings$event[[settings$station]],  
  columnQ = "Q",
  columnH = "H"
)
{
  thresholdQ <- get_Q_threshold(settings, do.stop = FALSE)
  thresholdH <- get_H_threshold(settings, do.stop = FALSE)
  
  indices <- getIndicesWithinEvents(
    hydraulicData = hydraulicData, 
    eventSettings = eventSettings,
    thresholds = c("H" = unname(thresholdH), "Q" = unname(thresholdQ))
  )
  
  if (isNullOrEmpty(indices)) {
    
    rangeQ <- range(hydraulicData[[columnQ]], na.rm = TRUE)
    rangeH <- range(hydraulicData[[columnH]], na.rm = TRUE)
    
    warning(
      "*** The event conditions are never met.\n",
      sprintf("  Range of Q values: [%f, %f]\n", rangeQ[1], rangeQ[2]),
      sprintf("  Range of H values: [%f, %f]\n", rangeH[1], rangeH[2]),
      sprintf("  Q-threshold: %f\n", thresholdQ), 
      sprintf("  H-threshold: %f\n", thresholdH)
    )
    
    return(invisible(NULL))
  }
  
  hydraulicEvents.all <- kwb.event::hsEvents(
    tseries = hydraulicData[indices, "DateTime"], 
    evtSepTime = settings$evtSepTime, 
    signalWidth = settings$tstep.fill.s, 
    tUnit = "min"
  )
  
  selection <- hydraulicEvents.all$dur > settings$durationThreshold
  
  kwb.utils::removeColumns(hydraulicEvents.all[selection, ], c("iBeg", "iEnd"))
}

# getIndicesWithinEvents -------------------------------------------------------

#' Get Indices Within Events
#' 
#' @param hydraulicData data frame with a columns \emph{DateTime} and two
#'   columns named as given in \emph{columnH}, \emph{columnQ}
#' @param eventSettings data frame with columns \emph{tBeg}, \emph{tEnd} (begin
#'   and end of period in which thresholds are valid), \emph{Hthreshold},
#'   \emph{Qthreshold}
#' @param thresholds default H and Q thresholds to be applied for time intervals
#'   for which no special thresholds are defined
#' @param columns optional. Named vector of character with column names for
#'   \code{H} and \code{Q}
#' 
getIndicesWithinEvents <- function(
  hydraulicData,
  eventSettings = NULL,
  thresholds = c(H = NA, Q = NA),
  columns = c(H = "H", Q = "Q")
)
{
  # start with an empty vector of integer
  indices <- integer()
  
  # vector of boolean indicating for each row if the default thresholds are to
  # be applied; start with all TRUE and set to FALSE at indices within time
  # intervals for which special thresholds are defined
  apply.default <- rep(TRUE, times = nrow(hydraulicData))
  
  if (is.null(eventSettings)) {
    cat("No time dependent event criteria defined.", 
        "Global thresholds Hthreshold =", thresholds["H"], 
        "and Qthreshold =", thresholds["Q"], "are used.")
  }
  else {
    
    H <- hydraulicData[[columns["H"]]]
    Q <- hydraulicData[[columns["Q"]]]
    
    for (i in seq_len(nrow(eventSettings))) {
      
      timeInterval <- lubridate::interval(
        eventSettings$tBeg[i], eventSettings$tEnd[i]
      )
      
      in.interval <- (hydraulicData$DateTime %within% timeInterval)
      
      newIndices <- which(
        in.interval & 
          (H > eventSettings$Hthreshold[i]) &
          (Q > eventSettings$Qthreshold[i])
      )
      
      indices <- c(indices, newIndices)
      apply.default <- apply.default & ! in.interval
    }    
  }
  
  # Add indices of non specifically considered time intervals where H and Q are
  # above the default thresholds
  if (any(apply.default)) {
    indices <- c(
      indices, 
      whichAboveThresholds(
        hydraulicData, 
        indices = which(apply.default), 
        thresholds = thresholds
      )
    )    
  }
  
  sort(unique(indices))
}

# getSamplerEvents -------------------------------------------------------------

#' Get Sampler Events
#' 
#' Get sample event information from all available auto sampler files
#' 
#' @param settings passed to \code{\link{getSampleInformation}}
#' @param FUN.readSamplerFile passed to \code{\link{getSampleInformation}}
#' @param warn passed to \code{\link{availableAutoSamplerFiles}}
#' 
getSamplerEvents <- function(settings, FUN.readSamplerFile, warn = TRUE)
{
  samplerFiles <- availableAutoSamplerFiles(
    dictionary = settings$dictionary, warn = warn
  )
  
  if (length(samplerFiles) > 0) {
    
    getSampleInformation(
      sampleEventIndices = seq_len(length(samplerFiles)), 
      FUN.readSamplerFile = FUN.readSamplerFile,
      settings = settings
    )
    
  } else {
    
    NULL
    
  }
}

# getMergedEvents --------------------------------------------------------------

#' Merge hydraulic events and sampler events
#' 
#' @param hydraulicEvents data frame containing information on hydraulic events
#' @param samplerEvents data frame containing information on sampler events
#' @param signalWidth passed to \code{\link[kwb.event]{toEvents}}
#' 
getMergedEvents <- function(hydraulicEvents, samplerEvents, signalWidth)
{
  template <- "No %sEvents given to getMergedEvents(). Returning %sEvents."
  
  if (is.null(hydraulicEvents)) {
    
    clean_warning(sprintf(template, "hydraulic", "sampler"))

    return (samplerEvents)
  }
  
  if (is.null(samplerEvents)) {
    
    clean_warning(sprintf(template, "sampler", "hydraulic"))

    return (hydraulicEvents)
  }
  
  # to make it run now: modify duration in sampler events so that the signal
  # width is the same; @TODO: adapt hsMergeEvents!
  
  samplerEvents.min <- kwb.event::toEvents(
    kwb.utils::removeColumns(samplerEvents, "dur"), 
    signalWidth = signalWidth,
    timeUnit = "min"
  )
  
  kwb.event::hsMergeEvents(hydraulicEvents, samplerEvents.min)
}

# setPausesOfMergedEvents ------------------------------------------------------

#' Update Pauses of Merged Events
#' 
#' @param hydraulicEvents data frame containing information on hydraulic events
#' @param mergedEvents data frame containing information on "merged" events
#' @param dbg logical. If \code{TRUE}, debug messages are shown
#' 
#' @return \emph{mergedEvents} with updated pauses \emph{pBefore} and
#'   \emph{pAfter}
#'   
setPausesOfMergedEvents <- function(hydraulicEvents, mergedEvents, dbg = FALSE)
{
  numberOfEvents <- nrow(mergedEvents)
                         
  for (i in seq_len(numberOfEvents)) {
    
    kwb.utils::catIf(dbg, "i =", i, "/", numberOfEvents, "...\n")
    
    indices <- kwb.event::indicesOfEventsContainedInEvent(
      hydraulicEvents, mergedEvents[i, ]
    )
    
    if (isNullOrEmpty(indices)) {
      
      warning("No hydraulic event fully contained in merged event ", i)
      
    } else {

      kwb.utils::printIf(dbg, indices)        

      if (!is.null(mergedEvents$pBefore)) {
        
        first_index <- indices[1]
        mergedEvents$pBefore[i] <- hydraulicEvents$pBefore[first_index]
      }
      
      if (! is.null(mergedEvents$pAfter)) {
        
        last_index <- kwb.utils::lastElement(indices)
        mergedEvents$pAfter[i] <- hydraulicEvents$pAfter[last_index]
      }
    }
  }
  
  mergedEvents
}

# addStatisticsToEvents --------------------------------------------------------

#' Add Statistics to Events
#' 
#' @param events list with elements \code{hydraulic} and \code{merged} each
#'   of which is a data frame containing event information
#' @param hydraulicData passed to \code{\link{getStatisticsByEvent}}
#' 
addStatisticsToEvents <- function(events, hydraulicData)
{
  eventsAndStat <- list()
  
  statistics <- getStatisticsByEvent(hydraulicData, events$hydraulic)

  if (!is.null(statistics)) {
    eventsAndStat$hydraulic <- kwb.utils::hsRestoreAttributes(
      cbind(events$hydraulic, statistics),
      attributes(events$hydraulic))
  }
  
  # Add statistics to "merged" events
  statistics <- getStatisticsByEvent(hydraulicData, events$merged)

  if (! is.null(statistics)) {
    eventsAndStat$merged <- kwb.utils::hsRestoreAttributes(
      cbind(events$merged, statistics), 
      attributes(events$merged))    
  }
  
  eventsAndStat
}

# getStatisticsByEvent ---------------------------------------------------------

#' Get Statistics by Event
#' 
#' @param hydraulicData data frame containing hydraulic data
#' @param events data frame containing information on events, passed to 
#'   \code{\link[kwb.event]{hsEventNumber}}
#' 
getStatisticsByEvent <- function(hydraulicData, events)
{
  if (is.null(events)) {
    warning("There are no events of which statistics could be calculated.")
    return(NULL)
  }
  
  columnName <- "eventNumber"
  
  hydraulicData[[columnName]] <- kwb.event::hsEventNumber(
    hydraulicData$DateTime, events
  )
  
  x <- getStatistics(hydraulicData, groupByColumn = columnName)
  
  if (is.null(x)) {
    return(NULL)
  }
  
  # There must be a unique time step
  timeStep.s <- findUniqueTimeStepOrStop(hydraulicData$DateTime)
  
  x$V.m3 <- x$Q.sum * timeStep.s / 1000 # L/s * s -> m3
  x$Q.available <- 100 - x$Q.raw.na / x$Q.length * 100
  
  # guarantee that all event numbers are contained in the returned data frame
  x <- merge(data.frame(eventNumber = 1:nrow(events)), x, all.x=TRUE)
  
  x
}

# getStatistics ----------------------------------------------------------------

getStatistics <- function(hydraulicData, groupByColumn)
{
  if (all(is.na(hydraulicData[[groupByColumn]]))) {
    warning(sprintf("All values in column \"%s\" are NA.", groupByColumn))
    return(NULL)
  }
  
  groupBy <- list(hydraulicData[[groupByColumn]])
  names(groupBy) <- groupByColumn
  
  num.na <- function(x) sum(is.na(x))
  
  columns <- c("Q.raw", "Q", "H", "H.raw")
  kwb.utils::checkForMissingColumns(hydraulicData, columns)
  
  hydraulicData.subset <- hydraulicData[, columns]
  
  stat.max <- stats::aggregate(
    hydraulicData.subset, by = groupBy, FUN = max
  )
  
  stat.na <- stats::aggregate(
    hydraulicData.subset, by = groupBy, FUN = num.na
  )
  
  stat.length <- stats::aggregate(
    hydraulicData.subset[, "Q", drop = FALSE], by = groupBy, FUN = length
  )
  
  stat.sum <- stats::aggregate(
    hydraulicData.subset[, "Q", drop = FALSE], by = groupBy, FUN = sum
  )
  
  appendToAllButFirstColumnName <- function(x, postfix) {
    names(x)[-1] <- paste(names(x)[-1], postfix, sep=".")
    x
  }
  
  x <- appendToAllButFirstColumnName(stat.max, "max")
  x <- merge(x, appendToAllButFirstColumnName(stat.na, "na"))  
  x <- merge(x, appendToAllButFirstColumnName(stat.sum, "sum"))
  x <- merge(x, appendToAllButFirstColumnName(stat.length, "length"))
  
  x
}

# mergeParallelRainEventStat ---------------------------------------------------

#' Merge Parallel Rain Event Statistics
#' 
#' @param hydraulicEvents data frame containing information on hydraulic events,
#'   with columns \code{eventNumber, tBeg}
#' @param rainEvents list of data frames containing information on rain events,
#'   with one list entry per rain gauge of which one is named as given in
#'   \code{seriesName}
#' @param rainData passed to \code{\link[kwb.event]{getEventStatistics}}
#' @param seriesName name of rain gauge
#' @param offset time in seconds by which tBeg is shifted backwards
#' @param plot.merged.event.info if \code{TRUE} (default), 
#'   \code{\link[kwb.event]{plotMergedEventInfoForValidation}} is called
#' 
mergeParallelRainEventStat <- function(
  hydraulicEvents, 
  rainEvents,
  rainData,
  seriesName,
  offset = 0, 
  plot.merged.event.info = TRUE
)
{
  hydraulicEvents <- kwb.utils::renameColumns(hydraulicEvents, list(
    eventNumber = "event"
  ))
  
  hydraulicEvents$tBeg <- selectColumns(hydraulicEvents, "tBeg") - offset
  
  events <- list(
    hydraulic = hydraulicEvents, 
    rain = selectElements(rainEvents, seriesName)
  )
  
  referenceName <- "hydraulic"
  partnerName <- "rain"
  
  eventRelations <- kwb.event::getEventRelations(
    events, referenceName = referenceName, partnerName = partnerName
  )
  
  if (!isNullOrEmpty(eventRelations)) {
    
    mergedEvents <- kwb.event::getParallelEventNotEndingAfter(
      events1 = selectElements(events, referenceName), 
      events2 = selectElements(events, partnerName), 
      eventRelations = eventRelations,
      extended = TRUE
    )
    
    if (plot.merged.event.info) {
      
      kwb.event::plotMergedEventInfoForValidation(mergedEvents)    
    }
    
    if (!is.null(mergedEvents)) {
      
      selected <- ! is.na(selectColumns(mergedEvents, "tBeg.merged"))
      columns <- c("event1", "tBeg.merged", "tEnd.merged")
      events.valid <- kwb.utils::renameColumns(
        selectColumns(mergedEvents[selected, ], columns),
        list(tBeg.merged = "tBeg", tEnd.merged = "tEnd")
      )    
    }
    
    events.valid.stat <- kwb.event::getEventStatistics(
      dataFrame = rainData, 
      seriesName = seriesName, 
      events = events.valid, 
      functions = c("sum", "max", "mean"), # , "length"),
      eventNumbers = events.valid$event1
    )
    
    events.valid.stat <- merge(
      events.valid.stat, 
      mergedEvents[c("event1", "tBeg.merged", "tEnd.merged")], 
      by.x = "event", 
      by.y = "event1",
      all.y = TRUE
    )
    
    names(events.valid.stat)[-1] <- paste(
      seriesName, names(events.valid.stat)[-1], 
      sep = "."
    )
    
    extendedEvents <- merge(hydraulicEvents, events.valid.stat, by = "event", all.x = TRUE)
  }
  else {
    warning("There are no intersections between ", referenceName, " events ",
             "and ", partnerName, " events at ", seriesName, "!")
    
    extendedEvents <- hydraulicEvents
  }
  
  kwb.utils::hsRestoreAttributes(x = extendedEvents, attribs = attributes(hydraulicEvents))
}
