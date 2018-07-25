# .plotRainData ----------------------------------------------------------------

#' Plot Rain Data
#' 
#' @param rainData data frame with columns \emph{DateTime}
#' @param gauges character vector of gauge names
#' @param xlim vector of two POSIXct determining the x limits
#' @param eventAndStat if not NULL (default) this must be a one row data frame
#'   with columns ...
#' 
.plotRainData <- function(
  rainData, gauges, xlim, innerMargins, eventAndStat = NULL
)
{
  gauge_indices <- seq_along(gauges)
  
  timestamps <- rainData$DateTime
  
  if (any(kwb.utils::inRange(timestamps, xlim[1], xlim[2]))) {  
    
    xlim <- get_appropriate_xlim(xlim, timestamps, shift.to.begin = 0)
    
    # get y limits over all gauges to be plotted
    ylim <- get_appropriate_ylim(
      rainData, gauges, gauge_indices, 
      in.limits = kwb.plot::inLimits(timestamps, xlim)
    )
    
    xlims_rain <- if (! isNullOrEmpty(eventAndStat)) {
      
      get_rain_xlim_per_gauge(eventAndStat, gauges)      
    }
    
    # plot rain for all gauges
    for (gauge in gauges) {
      
      kwb.plot::plotRain(
        timestamps = timestamps, 
        values = rainData[[gauge]], 
        gaugeName = gauge,
        xlim = xlim,
        ylim = ylim, 
        pch = 16, 
        cex = 0.5, 
        innerMargins = innerMargins
      )
      
      if (! is.null(xlims_rain[[gauge]]) ) {
        
        draw_limits(v = xlims_rain[[gauge]])
      }
    }
    
  } else {
    
    for (gauge in gauges) {
      
      gplots::textplot("No rain data available.", cex = 1)
    }      
  }
}

# get_appropriate_xlim ---------------------------------------------------------
get_appropriate_xlim <- function(xlim, timestamps, shift.to.begin = 0)
{
  t_beg <- timestamps - shift.to.begin
  t_end <- t_beg + kwb.datetime::getTimestepInSeconds(timestamps, default = 60)
  
  kwb.plot::appropriateLimits(x = c(t_beg, t_end), limits = xlim)  
}

# get_appropriate_ylim ---------------------------------------------------------
get_appropriate_ylim <- function(rainData, gauges, gaugeIndices, in.limits)
{
  ymax <- 0
  
  for (i in gaugeIndices) {
    
    ymax <- max(ymax, get_rain_limits(rainData[[gauges[i]]], in.limits)[2])
  }
  
  c(0, ymax)  
}

# get_rain_limits --------------------------------------------------------------
get_rain_limits <- function(values, in.limits) 
{
  kwb.plot::appropriateLimits(
    x = values[in.limits], limits = c(0, NA), default = c(0, 1)
  )
}

# get_rain_xlim_per_gauge ------------------------------------------------------
get_rain_xlim_per_gauge <- function(hydraulicEvents, gauges)
{
  xlims_rain <- list()
  
  for (gauge in gauges) {
    
    columns <- paste(gauge, c("tBeg", "tEnd"), "merged", sep = ".")
    
    if (all(columns %in% names(hydraulicEvents))) {
      
      xlims_rain[[gauge]] <- toUTC(
        c(hydraulicEvents[, columns[1]], 
          hydraulicEvents[, columns[2]])
      )     
    }
  }
  
  xlims_rain
}
