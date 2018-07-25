# plot_hydraulic_event ---------------------------------------------------------

#' Plot Hydraulic Event
#' 
#' @param hydraulicData data frame with columns \emph{DateTime}, \emph{H},
#'   \emph{Q}, \emph{Q.raw}, \emph{Q.raw.signal}, \emph{Q.interpol}
#' @param settings list of settings containing additional information
#'   and passed to other functions
#' @param eventAndStat passed to \code{kwb.monitoring:::.plotRainData}
#' @param sampleInformation passed to \code{kwb.monitoring:::.partialPlot_H}
#' @param ylim.Q passed to \code{kwb.monitoring:::.partialPlot_Q}
#' @param rainData optional. Data frame containing rain data
#' @param gauges passed to \code{kwb.monitoring:::.plotRainData} if 
#'   \code{rainData} is given
#' @param left fraction of event length by which xlim is extended to the left
#' @param right fraction of event length by which xlim is extended to the right
#' @param innerMargins.HQ "inner margins" of H and Q plots. Default: c(0.2,
#'   left, 0.1, right)
#' @param innerMargins.rain "inner margins" of rain plots. Default: c(0, left,
#'   0.2, right)
#' @param dbg  logical. If \code{TRUE} \code{eventAndStat} is printed.
#' 
plot_hydraulic_event <- function(
  hydraulicData, settings, eventAndStat, sampleInformation = NULL, 
  ylim.Q = NULL, rainData = NULL, gauges = NULL, left = 0.1, right = 0.1,  
  innerMargins.HQ = c(0.2, left, 0.1, right),
  innerMargins.rain = c(0, left, 0.2, right),
  dbg = FALSE
)
{
  # Set defaults
  gauges <- if (is.null(rainData)) {
    NULL
  } else if (is.null(gauges)) {
    rainGaugesNearStation(settings$station)[1:3]
  }
  
  old_pars <- graphics::par(no.readonly = TRUE) 
  
  on.exit(graphics::par(old_pars), add = TRUE)  
  
  set_page_layout(numberOfGauges = length(gauges))
  
  kwb.plot::setMargins(top = 2, bottom = 3)
  
  # Mark: outer margins are extended to see title (Stations name)
  graphics::par(oma = c(1, 1, 1, 1))
  
  kwb.utils::printIf(dbg, eventAndStat)    
  
  xlim_event <- kwb.event::eventToXLim(eventAndStat)
  
  xlim <- kwb.utils::extendLimits(xlim_event, 1, 1)    
  
  if (! is.null(rainData)) {
    
    .plotRainData(
      rainData = rainData, 
      gauges = gauges, 
      xlim = xlim,
      innerMargins = innerMargins.rain,
      eventAndStat = eventAndStat
    )
    
    graphics::title(paste("Station:", settings$station), outer= TRUE)      
  }
  
  # plot into layout area <numberOfGauges + 1>
  .partialPlot_Q(
    hydraulicData, 
    settings, 
    sampleInformation = NULL, 
    xlim = xlim, 
    ylim = ylim.Q, 
    innerMargins = innerMargins.HQ
  ) 
  
  draw_limits(v = xlim_event, col = "red")
  
  if (is.null(rainData)) {
    
    graphics::title(paste("Station:", settings$station))
  }
  
  # plot into layout area <numberOfGauges + 2>
  .partialPlot_H(
    hydraulicData = hydraulicData, 
    settings = settings, 
    xlim = xlim, 
    sampleInformation = sampleInformation,
    innerMargins = innerMargins.HQ # innerMargins
  )  
  
  draw_limits(v = xlim_event, col = "red")  
  
  textplot_event_info(settings = settings, eventAndStat = eventAndStat)
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

# set_page_layout --------------------------------------------------------------
set_page_layout <- function(numberOfGauges, relativeRainPlotHeight = 0.7)
{
  n_rows <- numberOfGauges + 2
  
  matrix_values <- c(rep(seq_len(n_rows), 2), rep(n_rows + 1, n_rows))
  
  graphics::layout(
    matrix(matrix_values, nrow = n_rows, byrow = FALSE), 
    heights = c(rep(relativeRainPlotHeight, numberOfGauges), 1, 1)
  )
}

# textplot_event_info ----------------------------------------------------------
textplot_event_info <- function(settings, eventAndStat)
{
  elements <- c("evtSepTime", "durationThreshold", "replaceMissingQMethod")
  
  infotext <- paste(
    formatEvent(
      event = eventAndStat, 
      eventNumber = eventAndStat$eventNumber, 
      precisionLevel = settings$precisionLevel
    ),
    "",
    formatEventStatistics(
      eventStatistics = eventAndStat, 
      precisionLevel = settings$precisionLevel
    ), 
    "",
    "", 
    formatSettings(settings = settings, settingNames = elements),
    sep = "\n"
  )
  
  gplots::textplot(infotext, valign = "top")
}
