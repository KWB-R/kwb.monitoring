# readAndJoinSamplerFiles ------------------------------------------------------

#' Read Multiple Auto-Sampler Files
#' 
#' @param samplerFiles vector of paths to sampler files
#' @param FUN.readSamplerFile function to be used for reading the sampler file
#' @param bottlesToConsider vector of bottle numbers to consider. Defaults to NA
#'   meaning that information on all bottles are to be returened.
#' @param \dots further arguments passed to \code{FUN.readSamplerFile}
#' 
#' @return data frame with columns \emph{file}, \emph{myDateTime},
#'   \emph{sample}, \emph{bottle}, \emph{volume}, \emph{unit}, \emph{result}
#'   
readAndJoinSamplerFiles <- function(
  samplerFiles, FUN.readSamplerFile, bottlesToConsider = NA, ...
)
{
  do.call(rbind, lapply(samplerFiles, function(samplerFile) {
    FUN.readSamplerFile(samplerFile, bottlesToConsider = bottlesToConsider, ...)
  }))
}

# bottleEventsToSamplerEvents --------------------------------------------------

#' Bottle Events to Sampler Events
#' 
#' @param bottleEvents data frame with columns \emph{samplerFile}
#' @param signalWidth passed to \code{\link[kwb.event]{toEvents}}
#' 
bottleEventsToSamplerEvents <- function(bottleEvents, signalWidth)
{
  samplerEvents <- NULL
  columns <- c("tBeg", "tEnd")
  
  for (samplerFile in unique(bottleEvents$samplerFile)) {
    
    rows <- which(bottleEvents$samplerFile == samplerFile)
    bottleEvents.sample <- bottleEvents[rows, ]
    
    samplerEvent <- kwb.event::hsJoinEvents(
      bottleEvents.sample, 
      1:nrow(bottleEvents.sample)
    )
    
    samplerEvent <- data.frame(
      samplerFile = samplerFile,
      samplerEvent[, c("tBeg", "tEnd")], 
      stringsAsFactors = FALSE)
    
    samplerEvents <- rbind(samplerEvents, samplerEvent)    
  }
  
  rownames(samplerEvents) <- 1:nrow(samplerEvents)
  kwb.event::toEvents(samplerEvents, signalWidth = signalWidth, timeUnit = "min")      
}

# filterForRelevantBottles -----------------------------------------------------

#' Filter Data Frame by Bottle Number
#' 
#' @param sampleDataExtended dataFrame with column \emph{bottle}
#' @param bottlesToConsider vector of bottle numbers to be considered
#'  
#' @return data frame with rows in which column \emph{bottle} is one of the
#'   numbers given in \emph{bottlesToConsider}
#' 
filterForRelevantBottles <- function(sampleDataExtended, bottlesToConsider)
{
  kwb.utils::checkForMissingColumns(sampleDataExtended, "bottle")
  
  if (!is.null(bottlesToConsider) & 
        ! all(is.na(bottlesToConsider))) {
    cat("Filtering for bottles", kwb.utils::commaCollapsed(bottlesToConsider), "... ")
    indices <- which(sampleDataExtended$bottle %in% bottlesToConsider)
    sampleDataExtended <- sampleDataExtended[indices, ]
    cat("ok.\n")
  }
  
  sampleDataExtended
}

# filterSampleEventsForFilename ------------------------------------------------

#' Filter Sample Events for Filename
#' 
#' @param events list with required elements \emph{samplingEvents},
#'   \emph{bottleEvents}, \emph{samplerEvents}
#' @param fileName file name to be filtered for in column \emph{samplerFile} of
#'   each of the data frames \emph{events$samplingEvents},
#'   \emph{events$bottleEvents} and \emph{events$samplerEvents}
#'   
#' @return list with elements \emph{samplingEvents}, \emph{bottleEvents}, 
#'   \emph{samplerEvents}
#' 
filterSampleEventsForFilename <- function(events, fileName)
{
  sampleInformation <- list()
  
  for (eventType in c("samplingEvents", "bottleEvents", "samplerEvents")) {
    rows <- which(events[[eventType]]$samplerFile == fileName)
    sampleInformation[[eventType]] <- events[[eventType]][rows, ]      
  }  
  
  sampleInformation
}

# getSampleInformation ---------------------------------------------------------

#' Get Sample Information
#' 
#' @param dictionary default: settings$dictionary
#' @param sampleEventSeparationTime default: settings$sampleEventSeparationTime
#' @param sampleEventIndices default: -1
#' @param bottlesToConsider default: settings$bottlesToConsider
#' @param method one of the methods supported by
#'   \code{\link{sampleDataToSamplingEvents}}. default:
#'   settings$sampleEventMethod
#' @param FUN.readSamplerFile function to be used to read an auto sampler file
#' @param settings list of settings, as returned by \code{\link{configure}}
#' @param signalWidth signal width in seconds. Default: 1
#'   
#' @return list with elements \emph{sampleTimes}, \emph{samplingEvents},
#'   \emph{bottle events} (data frame with columns...)
#' 
getSampleInformation <- function
(
  dictionary = settings$dictionary,
  sampleEventSeparationTime = settings$sampleEventSeparationTime,
  sampleEventIndices = -1,
  bottlesToConsider = settings$bottlesToConsider,
  method = settings$sampleEventMethod,
  FUN.readSamplerFile,
  settings = NULL,
  signalWidth = 1
)
{
  samplerFiles <- availableAutoSamplerFiles(dictionary = dictionary)
  
  if (!all(is.na(sampleEventIndices))) {    
    
    samplerFiles <- kwb.utils::getByPositiveOrNegativeIndex(
      elements = samplerFiles, 
      index = sampleEventIndices
    )
  }
  
  sampleData <- readAndJoinSamplerFiles(
    samplerFiles = samplerFiles, 
    FUN.readSamplerFile = FUN.readSamplerFile,
    bottlesToConsider = bottlesToConsider,
    siteCode = kwb.utils::defaultIfNULL(settings$station, NA)
  )
  
  if (isNullOrEmpty(sampleData)) {
    
    message("\n\n*** No sample data for sampleEventIndex = ", 
            sampleEventIndices, "!\n\n")
    
    list(samplingEvents = NULL, bottleEvents = NULL, samplerEvents = NULL)
  } 
  else {
    sampleDataToSampleInformation(
      sampleData = sampleData, 
      sampleEventSeparationTime = sampleEventSeparationTime,
      method = method,
      signalWidth = signalWidth
    )
  }
}

# getAllSamplerEvents ----------------------------------------------------------

#' Get All Sampler Events
#' 
#' @param rootDirectory passed to \code{availableAutoSamplerFiles2}
#' @param dictionaryFile passed to \code{availableAutoSamplerFiles2}
#' @param stations vector of monitoring station names, each of which is passed
#'   to \code{availableAutoSamplerFiles2}
#' @param FUN.readSamplerFile passed to \code{getAllSamplerEventsFromFiles}
#' @param bottlesToConsider passed to \code{getAllSamplerEventsFromFiles}
#' @param sampleEventSeparationTime passed to
#'   \code{getAllSamplerEventsFromFiles}
#' @param method passed to \code{getAllSamplerEventsFromFiles}
#' @param signalWidth passed to \code{getAllSamplerEventsFromFiles}
#' 
getAllSamplerEvents <- function(
  rootDirectory, 
  dictionaryFile, 
  stations, 
  FUN.readSamplerFile,
  bottlesToConsider = NA,
  sampleEventSeparationTime = 3600,
  method = "centre",
  signalWidth = 60  
)
{
  allSamplerEvents <- list()
  details <- list()
  
  for (station in stations)
  {
    samplerFiles <- availableAutoSamplerFiles2(
      rootDirectory, station, dictionaryFile
    )
    
    samplerEvents <- getAllSamplerEventsFromFiles(
      samplerFiles, 
      FUN.readSamplerFile = FUN.readSamplerFile, 
      bottlesToConsider = bottlesToConsider, 
      sampleEventSeparationTime = sampleEventSeparationTime, 
      method = method,
      signalWidth = signalWidth
    )
    
    details[[station]] <- samplerEvents
    
    elementName <- paste("S", station, sep="_")
    
    allSamplerEvents[[elementName]] <- samplerEvents$samplerEvents    
  }
  
  structure(allSamplerEvents, details = details)
}

# availableAutoSamplerFiles2 ---------------------------------------------------

#' Available Auto Sampler Files 2
#' 
#' @param rootDirectory passed as \code{RAW_DIR} to \code{\link{pathDictionary}}
#' @param station passed as \code{STATION} to \code{\link{pathDictionary}}
#' @param dictionaryFile passed to \code{\link{pathDictionary}}
#' 
availableAutoSamplerFiles2 <- function(rootDirectory, station, dictionaryFile)
{
  availableAutoSamplerFiles(
    dictionary = pathDictionary(
      dictionaryFile = dictionaryFile, 
      RAW_DIR = rootDirectory, 
      STATION = station
    )
  )
}

# availableAutoSamplerFiles ----------------------------------------------------

#' Available "sample_log"-Files
#' 
#' @param sampleDirectory directory in which to look for sample files. By
#'   default the directory is looked up in the \emph{dictionary} at keyword:
#'   SAMPLE_DIR
#' @param pattern file name pattern to which file names are matched. By default
#'   pattern is looked up in the \emph{dictionary} at keyword:
#'   SAMPLE_CSV_PATTERN
#' @param dictionary dictionary (list) with elements \emph{SAMPLE_DIR} and 
#'   \emph{SAMPLE_CSV_PATTERN}
#' @param warn if TRUE, a warning is given if there are no sample files
#' 
availableAutoSamplerFiles <- function(
  sampleDirectory = getOrCreatePath("SAMPLE_DIR", dictionary),
  pattern = do_resolve("SAMPLE_CSV_PATTERN", dictionary),
  dictionary = NULL, 
  warn = TRUE
)
{
  if (! file.exists(sampleDirectory)) {
    
    clean_stop(sprintf("There is no folder \"%s\"!", sampleDirectory))
  }
  
  sampleFiles <- dir(sampleDirectory, pattern, full.names=TRUE)  
  
  if (warn && isNullOrEmpty(sampleFiles)) {
    
    clean_warning(sprintf(
      "There are no sample files (matching \"%s\") in:\n  \"%s\"",
      pattern, sampleDirectory
    ))
  }
  
  sampleFiles
}

# getAllSamplerEventsFromFiles -------------------------------------------------

getAllSamplerEventsFromFiles <- function(
  samplerFiles,
  FUN.readSamplerFile,
  bottlesToConsider = settings$bottlesToConsider,
  sampleEventSeparationTime = settings$sampleEventSeparationTime,
  method = settings$method,
  signalWidth = 60,
  settings = NULL
)
{
  sampleData <- readAndJoinSamplerFiles(
    samplerFiles = samplerFiles, 
    FUN.readSamplerFile = FUN.readSamplerFile,
    bottlesToConsider = bottlesToConsider,
    siteCode = kwb.utils::defaultIfNULL(settings$station, NA)
  )
  
  if (isNullOrEmpty(sampleData)) {
    
    cat(sprintf("\n\n*** No sample data in files '%s'!\n\n", 
                paste(samplerFiles, collapse = ", ")))
    
    samplerEvents <- list(
      samplingEvents = NULL,
      bottleEvents = NULL,
      samplerEvents = NULL)
  } 
  else {
    
    samplerEvents <- sampleDataToSampleInformation(
      sampleData, 
      sampleEventSeparationTime = sampleEventSeparationTime,
      method = method,
      signalWidth = signalWidth)
  }
  
  samplerEvents
}

# sampleDataToSampleInformation ------------------------------------------------
#' @importFrom kwb.datetime hsToPosix
sampleDataToSampleInformation <- function(
  sampleData, 
  sampleEventSeparationTime = NA,
  method, 
  signalWidth
)
{
  if (!is.na(sampleEventSeparationTime)) {
    
    # Split files into sampled events if there are more than 
    # sampleEventSeparationTime between two samples
    sampleTimes <- hsToPosix(sampleData$sampleTime)
    
    subevents <- kwb.event::hsEvents(
      sampleTimes, 
      evtSepTime = sampleEventSeparationTime, 
      signalWidth = signalWidth)
    
    sampleData$samplerFile <- sprintf(
      "%s#%03d", 
      sampleData$samplerFile, 
      kwb.event::hsEventNumber(sampleTimes, subevents))    
  }
  
  samplingEvents <- NULL
  
  for (eventName in unique(sampleData$samplerFile)) {
    
    #cat("samplerFile:", samplerFile, "\n")
    sampleData.subset <- sampleData[sampleData$samplerFile == eventName, ]
    
    samplingEvents.new <- sampleDataToSamplingEvents(
      sampleData.subset,         
      method = method,
      signalWidth = signalWidth)
    
    samplingEvents <- rbind(samplingEvents, samplingEvents.new)
  }      
  
  bottleEvents <- samplingEventsToBottleEvents(
    samplingEvents, 
    signalWidth = signalWidth)
  
  samplerEvents <- bottleEventsToSamplerEvents(
    bottleEvents, 
    signalWidth = signalWidth)
  
  list(samplingEvents = samplingEvents,
       bottleEvents = bottleEvents,
       samplerEvents = samplerEvents)  
}

# sampleDataToSamplingEvents ---------------------------------------------------

#' Sample Data to Sampling Events
#' 
#' @param sampleData sample data as returned by
#'   \code{\link{readAndJoinSamplerFiles}}
#' @param method one of c("left", "right", "centre")
#' @param default.interval.width.s default interval width in seconds
#' @param signalWidth interval length in seconds that a sampling action is
#'   assumed to represent. It is only used to calculate the duration D of the
#'   time interval between two samplings at t1 and t2: D = t2 - t1 + signalWidth
#'   
#' @return data frame with columns \emph{tBeg}, \emph{tEnd}, ...
#' @importFrom kwb.datetime toUTC
#' @export 
sampleDataToSamplingEvents <- function(
  sampleData, method = "centre", default.interval.width.s = 600, signalWidth = 1
)
{
  samplerFile <- unique(sampleData$samplerFile)
  stopifnot(length(samplerFile) == 1)
  
  sampleTimes <- hsToPosix(sampleData$sampleTime)
  
  n <- nrow(sampleData)
  
  if (n == 1) {
    warning("There is only one sample time (", sampleTimes, ") in ", samplerFile, 
            ". Using the default interval width of ", default.interval.width.s,
            " seconds.")
    widths <- default.interval.width.s
  } 
  else {
    widths <- diff(as.integer(sampleTimes))    
  }
  
  # I get crazy! the c()-function converts timestamps to Local Time (because it
  # removes the attributes)! Therefore I use "toUTC" when concatenating dates!
  
  if (method == "left") {
    tBeg <- sampleTimes
    tEnd <- toUTC(c(tBeg[-1], tBeg[n] + kwb.utils::lastElement(widths)) -1)
  }  
  else if (method == "right") {
    tEnd <- sampleTimes
    tBeg <- toUTC(c(tEnd[1] - kwb.utils::firstElement(widths), tEnd[-n]) +1)
  }
  else if (method == "centre") {
    if (n > 1) {
      tEnd <- toUTC(sampleTimes + c(widths/2, kwb.utils::lastElement(widths)/2) -1)    
      tBeg <- toUTC(sampleTimes - c(kwb.utils::firstElement(widths)/2, widths/2))
    }
    else {
      tEnd <- toUTC(sampleTimes + widths/2 -1)
      tBeg <- toUTC(sampleTimes - widths/2)
    }
  }
  else {
    stop("Unknown method:", method)
  }
  
  #columns <- c("sampleTime", "sample", "volume", "unit", "result")
  columns <- setdiff(names(sampleData), c("samplerFile", "bottle"))
  
  samples <- data.frame(
    samplerFile = sampleData$samplerFile,
    bottle = sampleData$bottle,
    tBeg = tBeg, 
    tEnd = tEnd, 
    sampleData[, columns],
    stringsAsFactors = FALSE
  )
  
  kwb.event::toEvents(samples, signalWidth=signalWidth, timeUnit="min")
}

# samplingEventsToBottleEvents -------------------------------------------------

#' Sampling Events to Bottle Events
#' 
#' @param samplingEvents data frame with columns \emph{samplerFile},
#'   \emph{bottle}, \emph{tBeg}, \emph{tEnd}
#' @param signalWidth passed to \code{\link[kwb.event]{toEvents}}
#' 
#' @return data frame with columns \emph{tBeg}, \emph{tEnd}, \emph{dur}, 
#'   \emph{bottle}, \emph{samplesOk}
#' 
samplingEventsToBottleEvents <- function(samplingEvents, signalWidth = 1)
{
  groupBy <- list(
    samplerFile = samplingEvents$samplerFile,
    bottle = samplingEvents$bottle)
  
  bottleEvents <- merge(
    x = stats::aggregate(samplingEvents$tBeg, by = groupBy, kwb.utils::firstElement), 
    y = stats::aggregate(samplingEvents$tEnd, by = groupBy, kwb.utils::lastElement), 
    by = names(groupBy))
  
  bottleEvents <- kwb.utils::renameColumns(bottleEvents, list(
    x.x = "tBeg", x.y = "tEnd"
  ))
  
  # R always converts my timestamps to Local Time!
  bottleEvents$tBeg <- toUTC(bottleEvents$tBeg)
  bottleEvents$tEnd <- toUTC(bottleEvents$tEnd)
  
  # add a column indicating the number of successful samples within a bottle
  bottleEvents <- appendColumn_samplesOk(samplingEvents, bottleEvents)
  
  # order by sampler file and bottle number
  rowOrder <- order(bottleEvents$samplerFile, bottleEvents$bottle)
  bottleEvents <- bottleEvents[rowOrder, ]
  
  kwb.event::toEvents(bottleEvents, signalWidth = signalWidth, timeUnit = "min")
}

# appendColumn_samplesOk -------------------------------------------------------

#' Append Column "samplesOk"
#' 
#' @param samplingEvents data frame with columns \code{samplerFile}, 
#'   \code{bottle}, \code{result}
#' @param bottleEvents bottle events (data frame) 
#' @param successWord word indicating a successful sampling in column
#'   \code{result} of \code{samplingEvents}
#' @param columnName name of appended column 
#' 
appendColumn_samplesOk <- function(
  samplingEvents, bottleEvents, successWord = "SUCCESS", 
  columnName = "samplesOk"
)
{
  successEvents <- samplingEvents[samplingEvents$result == successWord, ]
  
  if (! isNullOrEmpty(successEvents)) {
    
    successPerBottle <- stats::aggregate(
      successEvents$result, 
      by = list(
        samplerFile = successEvents$samplerFile,                
        bottle = successEvents$bottle), 
      FUN = length)
    
    bottleEvents <- merge(bottleEvents, successPerBottle, all.x = TRUE)
    bottleEvents <- kwb.utils::renameColumns(bottleEvents, list(x = columnName))
  }
  else {
    bottleEvents[[columnName]] <- NA
  }  
  
  bottleEvents[[columnName]][is.na(bottleEvents[[columnName]])] <- 0  
  
  bottleEvents
}
