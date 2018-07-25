# createDummyEventThresholdFiles -----------------------------------------------

#' Create Threshold Dummy Files
#' 
#' Create dummy files defining H and Q thresholds for different time intervals
#' 
#' @param stations names of monitoring stations
#' @param outdir path to output directory
#' @param settings list from which to take non-given arguments
#'  
#' @return returns a list (with the stations as element names) containing the
#'   paths to the created files.
#'
createDummyEventThresholdFiles <- function(
  stations = names(selectElements(settings, "Hthresholds")),
  outdir = file.path(selectElements(settings, "rawdir"), "..", "META"),
  settings = NULL
)
{
  template_file <- system.file(
    "extdata", "hyd_event_thresholds_Template.txt", package = "kwb.monitoring"
  )
  
  template <- readLines(template_file)
  
  # Define helper function
  write_threshold_file <- function(station) {
    
    file <- file.path(outdir, sprintf("_hyd_event_thresholds_%s.txt", station))
    
    kwb.utils::catAndRun(
      sprintf("Creating dummy event threshold file: '%s'", file),
      writeLines(gsub("<station>", station, template), file)
    )
    
    file
  }
  
  stats::setNames(lapply(stations, write_threshold_file), stations)
}

# configure --------------------------------------------------------------------

#' Configure
#' 
#' Generate a configuration for an analysis run
#' 
#' @param rawdir Where is the "root" directory to raw data?
#' @param station which monitoring station? E.g in OGRE: one of c("EFH", "STR",
#'   "ALT", "NEU", "GEW").
#' @param sampleEventIndex which sample event (logged by sampler), according to
#'   the list of files recorded by the autosampler, sorted by name. Give a
#'   negative number to select files from the end of the list of files: -1 means
#'   "last", -2 "one before last", etc.
#' @param sampleEventMethod one of c("centre", "left", "right"). "left": sample
#'   time is begin of time interval, "right": sample time is end of time
#'   interval, "centre": sample time is middle of time interval
#' @param replaceMissingQMethod one of c("interpolate", "predict").
#'   "interpolate": linear interpolation "predict": prediction from water levels
#'   using a saved square regression
#' @param bottlesToDiscard which bottles are to be discarded (because they are
#'   not full)? Default: NA
#' @param Vbottle sample volume (in mL) given to the bottle representing the
#'   time interval with highest flow volume. Default: 1600
#' @param Vmax maximum total volume for mixed sample, in mL. Default: 5000
#' @param Hthresholds What are the level thresholds (in m) that trigger the
#'   start of the sampler? Named vector of numeric with names representing the
#'   site codes. E.g. for project OGRE: c("EFH", "STR", "ALT", "NEU", "GEW").
#'   Default: 0
#' @param Qthresholds flow thresholds for each station. Mark flow tresholds to 
#'   define runoff events, same structure as Hthresholds
#' @param Vthresholds hydraulic event volume thresholds for each station
#' @param tstep.fill.s time step in seconds used to fill up data. Default: 60
#'   seconds = 1 minute.
#' @param evtSepTime separation of events (how long in seconds may Hthreshold be
#'   underrun within an event?). Default: 2*3600 (= 2 hours)
#' @param sampleEventSeparationTime separation of sampled events within one and
#'   the same sampler file. If the difference between two sample times is
#'   greater than this time (in seconds) two sampled events are distinguished.
#'   Set to NA to prevent the splitting of sampled events.
#' @param durationThreshold minimum duration (in minutes) of hydraulic events to
#'   be considered. Default: 5 minutes
#' @param outsep separator to be used in output files (csv). Default: ";"
#' @param outdec decimal character to be used in output files (csv). Default:
#'   ","
#' @param context Vector of two elements giving the "context" before and after
#'   the event to be plotted, as percentages of the event duration. E.g. c(0.1,
#'   0.2) means that the time interval to be plotted starts 10 percent of the
#'   event duration before the event begin and ends 20 percent of the event
#'   duration after the end of the event.
#' @param plotchars plotting characters for Q.raw, Q.signal, ...
#' @param rain.aggregation.interval time interval in seconds for rain data
#'   aggregation, e.g. 600 = 10 minutes. NA = no aggregation of original rain
#'   data
#' @param max.samples.ok maximum number of "successful" samples within one
#'   bottle. Used for scaling the bottle "height" in the sample plot
#' @param bottlesToConsider numeric vector of bottle numbers. Only bottles of
#'   the given numbers are considered (read from the sampler file). Set to NA to
#'   consider all bottles
#' @param dictionaryFile full path to "dictionary" file that defines the folder
#'   structure and file name patterns in which the input files are expected to
#'   reside and to which the output files will be written. See
#'   kwb.ogre::OGRE_DEFAULT_DICTIONARY or kwb.dswt::DSWT_DEFAULT_DICTIONARY
#'   
configure <- function
(
  rawdir,
  station, 
  sampleEventIndex = -1,
  sampleEventMethod = "centre",
  replaceMissingQMethod = "interpolate",
  bottlesToDiscard = NA,
  Vbottle = 1600, 
  Vmax = 5000,
  Hthresholds = 0,
  Qthresholds = NULL,  
  Vthresholds = NULL,
  tstep.fill.s = 60,
  evtSepTime = 2 * 3600,
  sampleEventSeparationTime = NA,
  durationThreshold = 5,   
  outsep = ";",
  outdec = ",",
  context = c(left = 0.1, right = 0.2),
  plotchars = c(1, 3, 4, 4),
  rain.aggregation.interval = 600,
  max.samples.ok = 4,
  bottlesToConsider = NA,
  dictionaryFile = ""
)
{
  list(
    rawdir = rawdir,
    station = station,
    sampleEventIndex = sampleEventIndex,
    sampleEventMethod = sampleEventMethod,
    replaceMissingQMethod  =  replaceMissingQMethod,
    bottlesToDiscard = bottlesToDiscard,
    Vbottle = Vbottle,
    Vmax = Vmax,
    Hthresholds = Hthresholds,
    Qthresholds = Qthresholds,
    Vthresholds = Vthresholds,
    tstep.fill.s = tstep.fill.s,
    evtSepTime = evtSepTime,
    sampleEventSeparationTime = sampleEventSeparationTime,
    durationThreshold = durationThreshold,
    outsep = outsep,
    outdec = outdec,
    context = context,
    plotchars = plotchars,
    rain.aggregation.interval = rain.aggregation.interval,
    max.samples.ok = max.samples.ok,
    bottlesToConsider = bottlesToConsider,
    dictionaryFile = dictionaryFile
  )
}

# rainGaugesNearStation --------------------------------------------------------

#' Rain Gauges Near to Monitoring Sites
#' 
#' @param station name of monitoring station in KWB project OGRE or DSWT
#' 
#' @return list (one list element per monitoring site) of character vectors 
#'   representing rain gauge names
#'
rainGaugesNearStation <- function(station = NULL)
{
  x <- list(
    EFH = c("Wila", "StgI", "ZhlI", "Wil"),
    NEU = c("Wit", "ReiI", "BlnIX"),
    ALT = c("BlnX", "ReiI", "BlnXI", "BlnIX"),
    STR = c("Kar", "Mal", "BlnX", "BlnXI")
  )
  
  x$GEW <- x$NEU
  x$PNK <- x$ALT
  
  # The following are in fact station names in project DSWT
  # TODO: clean this!
  x$T_M1 <- c("Hsch", "MarI", "Mal", "BieI", "Lbg", "BlnXI")
  x$C_M1 <- c("Wila", "Wil", "Stg", "ZhlI")
  
  for (station.tmp in c("C_M2", "C_M3", "C_M4", "C_M5", "C_M6")) {
    
    x[[station.tmp]] <- x$C_M1
  }
  
  if (! is.null(station)) {
    
    x <- x[[station]]
  } 
  
  x
}
