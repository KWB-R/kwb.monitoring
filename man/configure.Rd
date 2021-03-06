% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/config.R
\name{configure}
\alias{configure}
\title{Configure}
\usage{
configure(rawdir, station, sampleEventIndex = -1,
  sampleEventMethod = "centre", replaceMissingQMethod = "interpolate",
  bottlesToDiscard = NA, Vbottle = 1600, Vmax = 5000,
  Hthresholds = 0, Qthresholds = NULL, Vthresholds = NULL,
  tstep.fill.s = 60, evtSepTime = 2 * 3600,
  sampleEventSeparationTime = NA, durationThreshold = 5,
  outsep = ";", outdec = ",", context = c(left = 0.1, right = 0.2),
  plotchars = c(1, 3, 4, 4), rain.aggregation.interval = 600,
  max.samples.ok = 4, bottlesToConsider = NA, dictionaryFile = "")
}
\arguments{
\item{rawdir}{Where is the "root" directory to raw data?}

\item{station}{which monitoring station? E.g in OGRE: one of c("EFH", "STR",
"ALT", "NEU", "GEW").}

\item{sampleEventIndex}{which sample event (logged by sampler), according to
the list of files recorded by the autosampler, sorted by name. Give a
negative number to select files from the end of the list of files: -1 means
"last", -2 "one before last", etc.}

\item{sampleEventMethod}{one of c("centre", "left", "right"). "left": sample
time is begin of time interval, "right": sample time is end of time
interval, "centre": sample time is middle of time interval}

\item{replaceMissingQMethod}{one of c("interpolate", "predict").
"interpolate": linear interpolation "predict": prediction from water levels
using a saved square regression}

\item{bottlesToDiscard}{which bottles are to be discarded (because they are
not full)? Default: NA}

\item{Vbottle}{sample volume (in mL) given to the bottle representing the
time interval with highest flow volume. Default: 1600}

\item{Vmax}{maximum total volume for mixed sample, in mL. Default: 5000}

\item{Hthresholds}{What are the level thresholds (in m) that trigger the
start of the sampler? Named vector of numeric with names representing the
site codes. E.g. for project OGRE: c("EFH", "STR", "ALT", "NEU", "GEW").
Default: 0}

\item{Qthresholds}{flow thresholds for each station. Mark flow tresholds to 
define runoff events, same structure as Hthresholds}

\item{Vthresholds}{hydraulic event volume thresholds for each station}

\item{tstep.fill.s}{time step in seconds used to fill up data. Default: 60
seconds = 1 minute.}

\item{evtSepTime}{separation of events (how long in seconds may Hthreshold be
underrun within an event?). Default: 2*3600 (= 2 hours)}

\item{sampleEventSeparationTime}{separation of sampled events within one and
the same sampler file. If the difference between two sample times is
greater than this time (in seconds) two sampled events are distinguished.
Set to NA to prevent the splitting of sampled events.}

\item{durationThreshold}{minimum duration (in minutes) of hydraulic events to
be considered. Default: 5 minutes}

\item{outsep}{separator to be used in output files (csv). Default: ";"}

\item{outdec}{decimal character to be used in output files (csv). Default:
","}

\item{context}{Vector of two elements giving the "context" before and after
the event to be plotted, as percentages of the event duration. E.g. c(0.1,
0.2) means that the time interval to be plotted starts 10 percent of the
event duration before the event begin and ends 20 percent of the event
duration after the end of the event.}

\item{plotchars}{plotting characters for Q.raw, Q.signal, ...}

\item{rain.aggregation.interval}{time interval in seconds for rain data
aggregation, e.g. 600 = 10 minutes. NA = no aggregation of original rain
data}

\item{max.samples.ok}{maximum number of "successful" samples within one
bottle. Used for scaling the bottle "height" in the sample plot}

\item{bottlesToConsider}{numeric vector of bottle numbers. Only bottles of
the given numbers are considered (read from the sampler file). Set to NA to
consider all bottles}

\item{dictionaryFile}{full path to "dictionary" file that defines the folder
structure and file name patterns in which the input files are expected to
reside and to which the output files will be written. See
kwb.ogre::OGRE_DEFAULT_DICTIONARY or kwb.dswt::DSWT_DEFAULT_DICTIONARY}
}
\description{
Generate a configuration for an analysis run
}
