# plotSampleInformation --------------------------------------------------------

#' Plot Sample Information
#' 
#' plotSampleInformation. TODO: simplify interface, e.g. 
#'   plotSampleInformation(getSampleInformation(getSampleFiles()[1]))
#' 
#' @param sampleInformation list with elements \emph{samplingEvents},
#'   \emph{bottleEvents}
#' @param add passed to \code{\link[kwb.event]{ganttPlotEvents}}
#' @param xlim passed to \code{\link[kwb.event]{ganttPlotEvents}}
#' @param ylim passed to \code{\link[kwb.event]{ganttPlotEvents}}
#' @param main plot title
#' @param cex.legend passed to  \code{\link{addSampleTimesToPlot}}
#' @param density passed to \code{\link[kwb.event]{ganttPlotEvents}}
#' @param plotSampleIntervals logical. If \code{TRUE} 
#'   \code{\link[kwb.event]{ganttPlotEvents}} is called for 
#'     \code{sampleInformation$samplingEvents}
#' @param maxSamplesOk maximum number of valid samples. This value is used to
#'   calculate an y coordinate
#' @param plotSamplingPoints = logical. If \code{TRUE} 
#'   \code{\link{addSampleTimesToPlot}} is called
#' @importFrom kwb.datetime toUTC
#' @export 
plotSampleInformation <- function(
  sampleInformation,
  add = FALSE,
  xlim = kwb.datetime::toUTC(range(c(
    sampleInformation$samplingEvents$tBeg, 
    sampleInformation$samplingEvents$tEnd
  ))),
  ylim = c(-2, 7),
  main = NA,
  cex.legend = 0.6,
  density = 0,
  plotSampleIntervals = TRUE,
  maxSamplesOk = NULL,
  plotSamplingPoints = plotSampleIntervals
)
{ 
  maxSamplesOk <- kwb.utils::defaultIfNULL(maxSamplesOk, 4)
  
  limits <- kwb.plot::userCoordinatesToLimits(
    kwb.plot::getPlotRegionSizeInUserCoords()
  )
  
  top <- limits$ylim[2]
  
  # Plot interval limits of bottles
  ymin <- 0.1 * top
  ymax <- 0.9 * top
  
  y1 <- 0
  
  #y2 <- 0.1*top + 0.2*top*sampleInformation$bottleEvents$samplesOk
  fraction_ok <- sampleInformation$bottleEvents$samplesOk / maxSamplesOk
  
  y2 <- ymin + fraction_ok * (ymax - ymin)
  
  kwb.event::ganttPlotEvents(
    events = sampleInformation$bottleEvents, 
    add = TRUE,
    y1 = y1, 
    y2 = y2, 
    col = "black",  
    density = density, 
    eventLabels = sampleInformation$bottleEvents$bottle,
    yLabel = y2 + 0.02 * top,
    bandheight = 0,
    lwd = 1
  )
  
  if (plotSampleIntervals) {
    
    # Plot interval limits of samples
    kwb.event::ganttPlotEvents(
      events = sampleInformation$samplingEvents, 
      add = add,
      y1 = - 0.05 * top, 
      y2 = + 0.05 * top, 
      xlim = xlim,
      ylim = ylim, 
      col = "red",
      density = 0, 
      showLabels = FALSE, 
      type = "vertical",
      lty = 1, 
      lwd = 0.5
    )
    
    # Plot points indicating sample times
    if (plotSamplingPoints) {
      
      addSampleTimesToPlot(
        sampleInformation$samplingEvents, ymax = top, showTimes = FALSE, 
        legendPosition = "bottomright", cex.text = cex.legend
      )    
    }
  }
  
  if (is.na(main)) {
    
    filenames <- unique(sampleInformation$samplingEvents$samplerFile)
    
    graphics::mtext(paste(filenames, collapse = ", "), 3, 2)    
  }
}

# addSampleTimesToPlot ---------------------------------------------------------

#' Add Sample Times to Plot
#' 
#' @param sampleTimes data frame with columns \code{sampleTime}, \code{bottle},
#'   \code{result}
#' @param ymax maximum y value of plot. Used to determine arrow lengths and
#'   positions
#' @param timeFormat default: "\%d.\%m \%H:\%M"
#' @param cex.text default: 0.7
#' @param showArrows default: FALSE
#' @param showTimes default: FALSE
#' @param showBottleNumbers default: FALSE
#' @param legendPosition character string specifying the legend position,
#'   e.g. "bottom", "[top|bottom]left", "top", "[top|bottom]right"
#' @importFrom kwb.datetime hsToPosix
#' @export 
addSampleTimesToPlot <- function(
  sampleTimes, ymax, timeFormat = default_day_time_format(), cex.text = 0.7, 
  showArrows = FALSE, showTimes = FALSE, showBottleNumbers = FALSE, 
  legendPosition = "bottom"
)
{  
  timestamps <- hsToPosix(sampleTimes$sampleTime)
  
  arrow_length <- ymax/3
  
  result_colours <- .getResultColours(sampleTimes$result)
  
  if (showArrows) {
    
    graphics::arrows(
      x0 = timestamps, 
      y0 = -arrow_length, 
      x1 = timestamps, 
      y1 = 0, 
      col = result_colours, 
      length = 0.1
    )
  }
  
  graphics::points(
    timestamps, rep(0, length(timestamps)), pch = 16, col = result_colours
  )
  
  if (showBottleNumbers) {
    
    graphics::text(
      x = timestamps, 
      y = -arrow_length/2, 
      labels = sampleTimes$bottle, 
      cex = cex.text
    )
  }
  
  if (showTimes) {
    
    graphics::text(
      x = timestamps, 
      y = 0, #- ymax/3, 
      labels = format(timestamps, format = timeFormat),
      cex = cex.text, 
      srt = 90, 
      adj = 1.1
    )
  }
  
  result_colours <- .resultsToColours(sampleTimes$result)
  
  legend_args <- c(default_legend_arguments(legendPosition), list(
    legend = names(result_colours), 
    col = as.character(result_colours),
    lty = 1, 
    pch = 16
  ))
  
  do.call(graphics::legend, args = legend_args)
}

# .getResultColours ------------------------------------------------------------
.getResultColours <- function(resultTypes, colour.success = "darkgreen")
{
  result_colours <- .resultsToColours(resultTypes, colour.success)
  
  unname(sapply(resultTypes, function(x) result_colours[x]))
}

# .resultsToColours ------------------------------------------------------------
.resultsToColours <- function(resultTypes, successColour = "darkgreen")
{
  success_type <- "SUCCESS"
  
  result_types <- unique(as.character(resultTypes))
  
  error_types <- setdiff(result_types, success_type)  
  
  type_colours <- if (success_type %in% result_types) {
    
    stats::setNames(successColour, success_type)
    
  } else {
    
    character()    
  }
  
  if (! isNullOrEmpty(error_types)) {
    
    error_colours <- grDevices::heat.colors(length(error_types))
    
    names(error_colours) <- error_types
    
    type_colours <- c(type_colours, error_colours)
  }
  
  type_colours
}
