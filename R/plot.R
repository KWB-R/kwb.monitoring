# plotTotalDischargeVersusRainProperties ---------------------------------------

#' Plot Total Discharge Versus Rain Properties
#' 
#' @param hydraulicEvents data frame containing information on hydraulic events
#' @param stations vector of monitoring station names
#' @param main plot title
#' @param settings list of settings
#' @param statistics one of \code{c("sum", "mean", "max")}
#' @param to.pdf logial. If \code{TRUE}, the output goes into a PDF file
#' 
plotTotalDischargeVersusRainProperties <- function(
  hydraulicEvents, stations, main, settings, statistics = "sum", to.pdf = FALSE
)
{
  ylab <- c(sum = "Rain height in mm", mean = "mean", max = "max")
  
  rain.columns <- paste(stations, statistics, sep = ".")
  rain.columns <- intersect(rain.columns, names(hydraulicEvents))
    
  if (isNullOrEmpty(rain.columns)) {
    warning("There are no rain properties at all in hydraulicEvents!")
    return(NULL)
  }
  
  stat.columns <- c("V.m3", rain.columns)
  
  xlim = c(0, max(hydraulicEvents[, stat.columns[1]]))
  ylim = c(0, max(hydraulicEvents[, stat.columns[-1]], na.rm = TRUE))
  
  if (to.pdf) {
    
    pdfFile <- file.path(
      kwb.utils::resolve("OUTPUT_DIR", dict = settings$dictionary),
      paste("Rain_Event_Volume_Correlation_", settings$station, ".pdf", sep = "")
    )
    
    kwb.utils::preparePdf(pdfFile)
    on.exit(kwb.utils::finishAndShowPdfIf(TRUE, pdfFile))
  }

  graphics::plot(NA, NA, xlim = xlim, ylim = ylim, xlab = "Runoff volume in L", 
       ylab = ylab[statistics], las = 1, main = main)
  
  plotColours <- grDevices::rainbow(length(stat.columns) - 1)
  names(plotColours) <- stat.columns[-1]
  r.squared <- numeric()
  
  for (sum.column in stat.columns[-1]) {
    x <- hydraulicEvents[, stat.columns[1]]
    y <- hydraulicEvents[, sum.column]
    col <- plotColours[sum.column]
    
    graphics::points(x = x, y = y, col = col)
    
    linearModel <- stats::lm("y~x", data = data.frame(x = x, y = y))
    r.squared <- c(r.squared, summary(linearModel)$r.squared)
    
    graphics::abline(coef = linearModel$coefficients, col = col)
  }
  
  graphics::legend(
    "bottomright", legend = sprintf(
      "%s (R sq. = %0.2f)", sub("\\.sum", "", stat.columns[-1]), r.squared
    ),
    col = plotColours, pch = 1
  )
}

# plot_hydraulic_events --------------------------------------------------------

#' Plot Hydraulic Events
#' 
#' @param hydraulicData data frame with column \code{DateTime}, passed to
#'   the function given in \code{FUN.plot_hydraulic_event}
#' @param settings list of settings (e.g. \code{dictionary}), passed to 
#'   \code{\link{plotEventDistribution}} if \code{plot.event.overview} is 
#'   \code{TRUE}
#' @param eventsAndStat data frame containing event information
#' @param to.pdf if \code{TRUE}, graphical output goes to a temporary pdf file
#' @param rainData passed to the function given in 
#'   \emph{FUN.plot_hydraulic_event}
#' @param gauges passed to the function given in 
#'   \emph{FUN.plot_hydraulic_event}
#' @param landscape orientation of pages in PDF file if \code{to.pdf} is 
#'   \code{TRUE}
#' @param plot.event.overview if \code{TRUE}, 
#'   \code{\link{plotEventDistribution}} is called
#' @param FUN.plot_hydraulic_event function to be called to plot one event
#' @param \dots arguments passed to the function given in 
#'   \emph{FUN.plot_hydraulic_event}
#' 
plot_hydraulic_events <- function 
(
  hydraulicData, settings, eventsAndStat, to.pdf = FALSE, rainData = NULL,
  gauges = NULL, landscape = TRUE, plot.event.overview = TRUE,
  FUN.plot_hydraulic_event = kwb.monitoring::plot_hydraulic_event, ...
)
{
  if (is.null(eventsAndStat)) {
    warning("There are no events to plot.")
    return()
  }
  
  n <- nrow(eventsAndStat)
  
  if (to.pdf) {
    
    timerange <- range(hydraulicData$DateTime)
    
    PDF <- getOrCreatePath(
      "OVERVIEW_EVENTS_PDF", 
      dictionary = settings$dictionary,
      create.dir = TRUE,
      HYDRAULIC_BEGIN = dateToDateStringInPath(timerange[1]),
      HYDRAULIC_END = dateToDateStringInPath(timerange[2])
    )
    
    kwb.utils::preparePdf(PDF, landscape = landscape)
  }
  
  on.exit(kwb.utils::finishAndShowPdfIf(to.pdf, PDF = PDF, dbg = FALSE))
    
  if (plot.event.overview) {
    
    plotEventDistribution(eventsAndStat = eventsAndStat, settings = settings)
  }
  
  for (i in 1:n) {
    
    cat("Event #", i, "\n")
    
    FUN.plot_hydraulic_event(
      hydraulicData,           
      settings = settings, 
      eventAndStat = eventsAndStat[i, ],
      ylim.Q = c(0, NA),
      rainData = rainData, 
      gauges = gauges,
      ...
    )
  }
}

# plotEventDistribution --------------------------------------------------------

#' Plot Event Distribution
#' 
#' @param eventsAndStat data frame containing event information
#' @param settings list of settings, at least with elements 
#'   \code{station, precisionLevel}
#' 
plotEventDistribution <- function(eventsAndStat, settings)
{
  infoGeneral <- formatSettings(
    settings = settings, 
    settingNames = c("evtSepTime", "durationThreshold")
  )

  graphicalParameters <- graphics::par(no.readonly = TRUE)
  
  graphics::layout(matrix(c(1,2), nrow = 2), heights = c(1.1,4)) # was: c(1.0,4)
  #graphics::layout(matrix(c(1,2), nrow = 1), heights = c(1.1,4)) # was: c(1.0,4)
  
  kwb.plot::setMargins(top = 3, bottom = 0)
  
  gplots::textplot(infoGeneral, valign = "top", cex = 1)
  
  main <- paste("Station:", settings$station)          

  graphics::title(main)
  
  statisticsDataFrame <- formatEventStatisticsTable(
    kwb.event::hsEventsToUnit(eventsAndStat, "h"),
    precisionLevel = settings$precisionLevel
  )
  
  gplots::textplot(statisticsDataFrame, show.rownames = FALSE)
  
  # Reset graphical parameters
  graphics::par(graphicalParameters)

  # plot volume versus duration for hydraulic events
  kwb.event::plotEventProperty1VersusEventProperty2(
    kwb.event::hsEventsToUnit(eventsAndStat, tUnit = "h"),
    "dur", "V.m3", 
    xlab = "Duration (hours)",
    ylab = "V (m3)",
    main = main
  )
  
  kwb.event::plotEventProperty1VersusEventProperty2(
    kwb.event::hsEventsToUnit(eventsAndStat, tUnit = "d"),
    "pBefore", "V.m3", 
    xlab = "Pause before (days)", 
    ylab = "V (m3)",
    main = main
  )
}

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
  if (is.null(rainData)) {
    gauges <- NULL
  }
  else if (is.null(gauges)) {
    gauges <- rainGaugesNearStation(settings$station)[1:3]
  }
  
  currentGraphicalParameters <- graphics::par(no.readonly = TRUE) 
  on.exit(graphics::par(currentGraphicalParameters), add = TRUE)  
  
  .setPageLayout(numberOfGauges = length(gauges))
  
  kwb.plot::setMargins(top = 2, bottom = 3)
  
  #Mark: outer margins are extended to see title (Stations name)
  graphics::par(oma = c(1, 1, 1, 1))
  
  kwb.utils::printIf(dbg, eventAndStat)    

  xlim.event <- kwb.event::eventToXLim(eventAndStat)
  xlim <- kwb.utils::extendLimits(xlim.event, 1, 1)    

  if (!is.null(rainData)) {
    
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
  
  .drawLimits(v = xlim.event, col = "red")
  
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
  
  .drawLimits(v = xlim.event, col = "red")  
  
  .textplot_eventInfo(settings = settings, eventAndStat = eventAndStat)
}

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
  gaugeIndices <- seq_len(length(gauges))
  
  timestamps <- rainData$DateTime
  
  if (any(kwb.utils::inRange(timestamps, xlim[1], xlim[2]))) {  
    
    xlim <- .getXLim(xlim = xlim, timestamps = timestamps, shift.to.begin = 0)
    
    # get y limits over all gauges to be plotted
    ylim <- .getYLim(
      rainData, gauges, gaugeIndices, 
      in.limits = kwb.plot::inLimits(timestamps, xlim)
    )
    
    if (isNullOrEmpty(eventAndStat)) {
      xlims.rain <- NULL
    }
    else {
      xlims.rain <- .getRainXLims(eventAndStat, gauges)      
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
      
      if (!is.null(xlims.rain[[gauge]]) ) {
        .drawLimits(v = xlims.rain[[gauge]])
      }
    }  
  }
  else {
    for (gauge in gauges) {
      gplots::textplot("No rain data available.", cex = 1)
    }      
  }
}

# .getRainXLims ----------------------------------------------------------------

.getRainXLims <- function(hydraulicEvents, gauges)
{
  xlims.rain <- list()
  
  for (gauge in gauges) {
    
    columnNames <- paste(gauge, c("tBeg", "tEnd"), "merged", sep = ".")

    if (all(columnNames %in% names(hydraulicEvents))) {
      
      xlims.rain[[gauge]] <- toUTC(
        c(hydraulicEvents[, columnNames[1]], 
          hydraulicEvents[, columnNames[2]])
      )      
    }
  }
  
  xlims.rain
}

# .getXLim ---------------------------------------------------------------------

.getXLim <- function(xlim, timestamps, shift.to.begin = 0)
{
  t.begin <- timestamps - shift.to.begin
  t.end <- t.begin + kwb.datetime::getTimestepInSeconds(timestamps, default = 60)
  
  kwb.plot::appropriateLimits(x = c(t.begin, t.end), limits = xlim)  
}

# .getYLim ---------------------------------------------------------------------

.getYLim <- function(rainData, gauges, gaugeIndices, in.limits)
{
  ymax <- 0
  
  for (i in gaugeIndices) {
    ymax <- max(ymax, .getRainLimits(rainData[[gauges[i]]], in.limits)[2])
  }
  
  c(0, ymax)  
}

# .getRainLimits ---------------------------------------------------------------

.getRainLimits <- function(values, in.limits) 
{
  kwb.plot::appropriateLimits(
    x = values[in.limits], limits = c(0, NA), default = c(0, 1)
  )
}

# .setPageLayout ---------------------------------------------------------------

.setPageLayout <- function(numberOfGauges, relativeRainPlotHeight = 0.7)
{
  numberOfRows <- numberOfGauges + 2
  
  matrixValues <- c(
    rep(seq_len(numberOfRows), 2),
    rep(numberOfRows + 1, numberOfRows)
  )
  
  layoutMatrix <- matrix(matrixValues, nrow = numberOfRows, byrow = FALSE)
  
  graphics::layout(
    layoutMatrix, 
    heights = c(rep(relativeRainPlotHeight, numberOfGauges), 1, 1)
  )
}

# .drawLimits ------------------------------------------------------------------

.drawLimits <- function(v, col = "green", lty = 2, lwd = 2, dbg = FALSE) 
{
  if (dbg) {
    cat("drawing vertical lines at:\n")
    print(v)    
  }
  
  graphics::abline(v = v, col = col, lty = lty, lwd = lwd)
}

# .textplot_eventInfo ----------------------------------------------------------

.textplot_eventInfo <- function(settings, eventAndStat)
{
  infoGeneral <- formatSettings(
    settings = settings, 
    settingNames = c("evtSepTime", "durationThreshold", "replaceMissingQMethod")
  )
  
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
    infoGeneral,
    sep = "\n"
  )
  
  gplots::textplot(infotext, valign = "top")
}

# plot_sampled_event -----------------------------------------------------------

#' Plot Sampled Event
#' 
#' @param hydraulicData data frame with columns \emph{DateTime}, \emph{H},
#'   \emph{Q}, \emph{Q.raw}, \emph{Q.raw.signal}, \emph{Q.interpol}
#' @param settings list with elements \code{dictionary, station, precisionLevel, 
#'   bottlesToDiscard, bottlesToConsider} and being passed to other functions
#' @param sampleInformation passed to \code{kwb.monitoring:::.partialPlot_Q} and
#'   \code{kwb.monitoring:::.partialPlot_H}
#' @param mergedEventAndStat list with event information such as \code{tBeg},
#'   \code{tEnd}, \code{V.m3} and passed to 
#'     \code{kwb.monitoring:::formatEventRelation}
#' @param volumeCompositeSample list with elements \code{bottle}, {V.bottle.mL}, 
#'   \code{V}, passed to \code{kwb.monitoring:::formatVolumeCompositeSample}
#' @param to.pdf logical. If \code{TRUE}, output goes to a PDF file
#' @param interpolate passed to \code{kwb.monitoring:::.partialPlot_Q}
#' @param \dots further arguments given to
#' 
plot_sampled_event <- function(
  hydraulicData, settings, sampleInformation = NULL, mergedEventAndStat,
  volumeCompositeSample = NULL, to.pdf = FALSE, interpolate = TRUE, ...
)
{
  samplerFile <- unique(sampleInformation$samplerEvents$samplerFile)
  stopifnot(length(samplerFile) == 1)
  
  if (to.pdf) {  
    
    eventName <- sampleLogFileToSampleName(samplerFile)
    
    PDF <- getOrCreatePath(
      "SAMPLED_EVENT_PDF_COMPOSITE", 
      dictionary = settings$dictionary, 
      create.dir = TRUE,
      SAMPLED_EVENT_NAME = eventName
    )
    
    PDF <- kwb.utils::preparePdf(PDF, landscape = TRUE)
  }
  
  currentGraphicalParameters <- graphics::par(no.readonly = TRUE) 
  
  on.exit(kwb.utils::finishAndShowPdfIf(to.pdf, PDF = PDF, dbg = FALSE))
  on.exit(graphics::par(currentGraphicalParameters), add = TRUE)
  
  graphics::layout(matrix(nrow = 3, byrow = TRUE, c(
    1, 1 ,3,
    1, 1, 4,
    2, 2, 5
  )))
  
  main <- sprintf(
    "Station: %s, auto-sampler file: %s", settings$station, samplerFile
  )
  
  # plot into layout area 1
  .partialPlot_Q(
    hydraulicData, settings, sampleInformation, main = main,
    innerMargins = c(0.1, 0.2, 0.1, 0.2), interpolate = interpolate
  )
  
  # plot into layout area 2
  kwb.plot::setMargins(top = 0)
  
  .partialPlot_H(
    hydraulicData, settings, xlim = kwb.event::eventToXLim(mergedEventAndStat), 
    sampleInformation, innerMargins = c(0, 0.2, 0.1, 0.2)
  )
  
  # plot into layout area 3
  formattedDataFrame <- formatVolumeCompositeSample(
    volumeCompositeSample, settings$precisionLevel
  )
  
  gplots::textplot(formattedDataFrame, valign = "top", show.rownames = FALSE)
  
  # plot into layout area 4
  graphics::barplot(
    volumeCompositeSample$V.bottle.mL, xlab = "bottle", ylab = "V bottle (mL)",
    names.arg = volumeCompositeSample$bottle, las = 1
  )
  
  graphics::abline(
    h = pretty(volumeCompositeSample$V.bottle.mL, 10), lty = 3, col = "grey"
  )
  
  # plot into layout area 5
  V.event <- mergedEventAndStat$V.m3
  V.sampled <- sum(volumeCompositeSample$V.used, na.rm=TRUE)

  #cat("mergedEventAndStat:\n")
  #print(mergedEventAndStat)
  
  bottlesToDiscard <- settings$bottlesToDiscard
  bottlesToConsider <- settings$bottlesToConsider
  
  if (!all(is.na(bottlesToConsider))) {
    bottlesToDiscard <- intersect(bottlesToDiscard, bottlesToConsider)
  }
  
  infoText <- paste(
    formatBottleEnumeration(
      "Bottles considered", bottlesToConsider, na.text = "all"
    ),
    
    formatBottleEnumeration(
      "Bottles discarded", bottlesToDiscard, na.text = "none"
    ),
    
    "",
    
    formatEventRelation(
      mergedEventAndStat, V.event, V.sampled, settings$precisionLevel
    ),
    
    "",
    
    formatSettings(settings, settingNames = c(
      "evtSepTime", "replaceMissingQMethod", "sampleEventMethod"
    )),
    
    sep = "\n"
  )
  
  gplots::textplot(infoText, valign = "top", halign = "left", mar = c(0, 0, 0, 0))
}

# .partialPlot_Q ---------------------------------------------------------------

.partialPlot_Q <- function(
  hydraulicData, settings, sampleInformation = NULL, xlim = NULL, 
  ylim = c(0, NA), main = "", q.threshold = 0, interpolate = TRUE, ...
)
{
  if (! is.null(sampleInformation)) {
    
    samplerEvent <- sampleInformation$samplerEvents
    
    stopifnot(nrow(samplerEvent) == 1)
    
    if (is.null(xlim)) {
      
      #xlim <- kwb.utils::preparePdf(samplerEvent)
      xlim <- kwb.event::eventToXLim(samplerEvent)
    }    
  }

  eventSettings <- settings$event[[settings$station]]
  
  time.dependent.thresholds <- if (! is.null(eventSettings)) {
    
    kwb.utils::renameColumns(eventSettings, list(
      Qthreshold = "threshold"
    ))
  } # else NULL (implicitly)

  plot_Q_columns(
    hydraulicData, xlim = xlim, ylim = ylim, main = main, 
    q.threshold = get_Q_threshold(settings), 
    time.dependent.thresholds = time.dependent.thresholds, 
    interpolate = interpolate, ...
  )
  
  if (! is.null(sampleInformation)) {
    
    density <- .getShadingLinesDensities(
      sampleInformation$bottleEvents$bottle, settings$bottlesToDiscard
    )
    
    plotSampleInformation(
      sampleInformation, add = TRUE, density = density, main = "", 
      maxSamplesOk = settings$max.samples.ok
    )
  }
}

# .getShadingLinesDensities ----------------------------------------------------

.getShadingLinesDensities <- function(
  bottleNumbers, bottlesToDiscard, density = 15
)
{
  densities <- rep(0, length(bottleNumbers))
  
  if (! all(is.na(bottlesToDiscard))) {
    densities[which(bottleNumbers %in% bottlesToDiscard)] <- density
  }
  
  densities
}

# plot_Q_columns ---------------------------------------------------------------

plot_Q_columns <-function(
  hydraulicData, main = "Station?", xlim = NULL, ylim = NULL, 
  innerMargins = .defaultInnerMargins(), q.threshold = 0, 
  time.dependent.thresholds = NULL, interpolate = TRUE
)
{
  # find y limits over all y-columns
  q.columns <- intersect(
    names(hydraulicData), c("Q.raw", "Q.interpol", "Q.pred")
  )
  
  xlim <- kwb.plot::appropriateLimits(hydraulicData$DateTime, xlim)
  
  in.xlim <- kwb.utils::inRange(hydraulicData$DateTime, xlim[1], xlim[2])
  
  ylim <- kwb.plot::appropriateLimits(hydraulicData[in.xlim, q.columns], ylim)
  
  if(interpolate) {
    
    kwb.plot::plot_variable(
      hydraulicData, "Q.raw", xlim = xlim, ylim = ylim, 
      innerMargins = innerMargins, main = main
    )
    
    kwb.plot::plot_variable(hydraulicData, "Q.signal", add = TRUE)
    
    kwb.plot::plot_variable(
      hydraulicData, "Q.interpol", add = TRUE, col = "green"
    )
  } else {
    
    kwb.plot::plot_variable(
      hydraulicData, "Q", xlim = xlim, ylim = ylim, innerMargins = innerMargins, 
      main = main, type = "l"
    )
  }
  
  threshold.lty <- 2
  graphics::abline(h = q.threshold, lty = threshold.lty)
  
  .drawAdditionalThresholdsIfApplicable(time.dependent.thresholds)
  
  if ("Q.pred" %in% names(hydraulicData)) {
    
    kwb.plot::plot_variable(hydraulicData, "Q.pred", add = TRUE, col = "red")
  }
  
  do.call(graphics::legend, args = c(.defaultLegendArguments(), list(
    legend = c("raw", "H > threshold", "interpolated", "predicted from H"),
    col = c("blue", "black", "green", "red"), 
    pch = c(
      kwb.plot::.defaultPlotParameter("pch", "Q.raw"),
      kwb.plot::.defaultPlotParameter("pch", "Q.signal"),
      kwb.plot::.defaultPlotParameter("pch", "Q.interpol"),
      kwb.plot::.defaultPlotParameter("pch", "Q.pred")
    )
  )))
}

# .drawAdditionalThresholdsIfApplicable ----------------------------------------

.drawAdditionalThresholdsIfApplicable <- function
(
  time.dependent.thresholds,
  col = "red",
  lty = "dashed",
  dbg = FALSE
)
{
  if (!is.null(time.dependent.thresholds)) {
    
    if (dbg) {
      cat("time.dependent.thresholds:\n")
      print(time.dependent.thresholds)      
    }
    
    x0 <- time.dependent.thresholds$tBeg
    x1 <- time.dependent.thresholds$tEnd
    threshold <- time.dependent.thresholds$threshold
    
    if (dbg) {
      cat("Plotting segments at (x0, x1, y):", format(x0), format(x1), threshold, "\n")  
    }
    
    graphics::segments(
      x0 = x0, y0 = threshold, x1 = x1, y1 = threshold, col = col, lty = lty
    )
  } 
  else {
    cat("No special thresholds to plot\n")
  }
}

# plot_H_columns ---------------------------------------------------------------

#' Plot H Columns
#' 
#' @param hydraulicData data frame with columns \emph{DateTime}, \emph{H},
#'   \emph{H.interpol}
#' @param h.threshold H threshold at which a horizontal line is to be drawn
#'   (default: 0)
#' @param time.dependent.thresholds passed to 
#'   \code{kwb.monitoring:::.drawAdditionalThresholdsIfApplicable}
#' @param xlim passed to \code{\link[kwb.plot]{plot_variable}}
#' @param ylim passed to \code{\link[kwb.plot]{plot_variable}}
#' @param innerMargins passed to \code{\link[kwb.plot]{plot_variable}}
#' 
plot_H_columns <- function(
  hydraulicData, h.threshold = 0, time.dependent.thresholds = NULL,  
  xlim = NULL, ylim = NULL, innerMargins = .defaultInnerMargins()
)
{
  kwb.plot::plot_variable(
    hydraulicData, "H", type="p", xlim = xlim, ylim = ylim, 
    innerMargins = innerMargins
  )
  
  kwb.plot::plot_variable(
    hydraulicData, "H.interpol", add = TRUE, col = "green"
  )
  
  threshold.lty <- 2  
  graphics::abline(h = h.threshold, lty = threshold.lty)

  .drawAdditionalThresholdsIfApplicable(time.dependent.thresholds)
  
  legend.args <- c(
    .defaultLegendArguments(), 
    list(
      legend = c(sprintf("H threshold = %0.2f m", h.threshold), "interpolated"),
      lty = c(threshold.lty, NA),
      pch = c(NA, kwb.plot::.defaultPlotParameter("pch", "H.interpol")),
      col = c("black", "green")
    )
  )
  
  do.call(graphics::legend, args = legend.args)    
}

# .defaultInnerMargins ---------------------------------------------------------

.defaultInnerMargins <- function()
{
  c(0.1, 0.1, 0.1, 0.2)
}

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
#' 
plotSampleInformation <- function(
  sampleInformation,
  add = FALSE,
  xlim = toUTC(range(c(
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
  if (is.null(maxSamplesOk)) {
    maxSamplesOk <- 4  
  }
  
  limits <- kwb.plot::userCoordinatesToLimits(
    kwb.plot::getPlotRegionSizeInUserCoords()
  )
  
  top <- limits$ylim[2]
  
  # plot interval limits of bottles
  ymin <- 0.1 * top
  ymax <- 0.9 * top
  
  y1 <- 0
  
  #y2 <- 0.1*top + 0.2*top*sampleInformation$bottleEvents$samplesOk
  fractionOk <- sampleInformation$bottleEvents$samplesOk/maxSamplesOk 
  y2 <- ymin + fractionOk * (ymax - ymin)
  
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
    
    # plot interval limits of samples
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
    
    # plot points indicating sample times
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
#' 
addSampleTimesToPlot <- function(
  sampleTimes, ymax, timeFormat = .defaultDayTimeFormat(), cex.text = 0.7, 
  showArrows = FALSE, showTimes = FALSE, showBottleNumbers = FALSE, 
  legendPosition = "bottom"
)
{  
  timestamps <- hsToPosix(sampleTimes$sampleTime)
  
  arrowLength <- ymax/3
  
  resultColours <- .getResultColours(sampleTimes$result)
  
  if (showArrows) {
    
    graphics::arrows(
      x0 = timestamps, 
      y0 = -arrowLength, 
      x1 = timestamps, 
      y1 = 0, 
      col = resultColours, 
      length = 0.1
    )
  }
  
  graphics::points(
    timestamps, rep(0, length(timestamps)), pch = 16, col = resultColours
  )
  
  if (showBottleNumbers) {
    
    graphics::text(
      x = timestamps, 
      y = -arrowLength/2, 
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
  
  resultColours <- .resultsToColours(sampleTimes$result)
  
  do.call(graphics::legend, args = c(.defaultLegendArguments(legendPosition), list(
    legend = names(resultColours), 
    col = as.character(resultColours),
    lty = 1, 
    pch = 16
  )))
}

# .defaultDayTimeFormat --------------------------------------------------------

.defaultDayTimeFormat <- function()
{
  return ("%d.%m %H:%M")
}

# .partialPlot_H ---------------------------------------------------------------

.partialPlot_H <-function(hydraulicData, settings, xlim, sampleInformation, ...)
{
  eventSettings <- settings$event[[settings$station]]
  
  if (!is.null(eventSettings)) {
    
    time.dependent.thresholds <- kwb.utils::renameColumns(eventSettings, list(
      Hthreshold = "threshold"
    ))
  }
  else {
    time.dependent.thresholds <- NULL
  }
  
  plot_H_columns(
    hydraulicData, 
    h.threshold = get_H_threshold(settings),                  
    time.dependent.thresholds = time.dependent.thresholds,
    xlim = xlim, 
    ylim = c(0, NA),
    ...
  )  
  
  plotRegion <- kwb.plot::getPlotRegionSizeInUserCoords()
  
  if (!is.null(sampleInformation)) {
    
    plotSampleInformation(
      sampleInformation, add = TRUE, main = "", plotSampleIntervals = FALSE,
      maxSamplesOk = settings$max.samples.ok
    )
  }
}

# getFunctionValueOrDefault2 ---------------------------------------------------

#' Get Function Value or Default 2
#' 
#' @param values passed to \code{\link[kwb.utils]{getFunctionValueOrDefault}}
#' @param FUN passed to \code{\link[kwb.utils]{getFunctionValueOrDefault}}
#' @param default passed to \code{\link[kwb.utils]{getFunctionValueOrDefault}}
#' @param timestamps vector of timestamps (used in warning message) 
#' @param columnName column name (used in warning message)
#' 
getFunctionValueOrDefault2 <- function(
  values, FUN, default, timestamps = NULL, columnName = ""
)
{
  prolog <- ""
  
  if (!is.null(timestamps)) {
    prolog <- sprintf(
      "Time interval [%s ... %s]:", timestamps[1], utils::tail(timestamps, 1)
    )
  }
  
  warningMessage <- paste(prolog, sprintf(
    "all %s-values are NA -> taking default: %s", columnName, toString(default)
  ))
  
  kwb.utils::getFunctionValueOrDefault(values, FUN, default, warningMessage)
}

# .defaultLegendArguments ------------------------------------------------------

.defaultLegendArguments <- function(x = "topright", horiz=FALSE)
{
  list(x=x, horiz=horiz, cex = 0.7, bg = "white")
}

# plotEventOverview ------------------------------------------------------------

#' Gantt-Plots for Event Overview
#' 
#' @param events list with elements \emph{hydraulic}, \emph{sample},
#'   \emph{merged} each of which is a data frame with columns \emph{tBeg},
#'   \emph{tEnd}
#' @param settings list with elements \emph{evtSepTime}, \emph{station},
#'   \emph{Hthresholds}
#' @param dbg logical. If \code{TRUE}, the x axis coordinates are printed
#' 
plotEventOverview <- function(events, settings, dbg = FALSE)
{
  currentGraphicalParameters <- graphics::par(no.readonly = TRUE) 
  on.exit(graphics::par(currentGraphicalParameters))
  
  kwb.plot::setMargins(bottom = 7)
  
  myTitle <- sprintf(
    "hydraulic events\nH > %0.2f m\ngap(H <= %0.2f m) > %0.0f h",
    get_H_threshold(settings),
    get_H_threshold(settings),
    settings$evtSepTime/3600)
  
  # TODO: use kwb.event::ganttPlotEventLists instead?
  
  kwb.event::ganttPlotEvents(
    events$hydraulic, 
    ylim = c(0,3.4), 
    y1 = 2.3, 
    title = myTitle, 
    leftMargin = 0.3, 
    xlab = "")
  
  kwb.event::ganttPlotEvents(
    events$sample, 
    add = TRUE, 
    y1 = 1.2, 
    title = "sample-log events")
  
  kwb.event::ganttPlotEvents(
    events$merged, 
    add = TRUE, 
    y1 = 0.1, 
    title="merged events")  
  
  graphics::title(paste("Station:", settings$station))
  
  x <- c(events$hydraulic$tBeg, events$hydraulic$tEnd)
  at <- pretty(x, 20)
  
  kwb.utils::printIf(dbg, at)

  graphics::axis(
    side = 1, at = at, labels = format(at, format = "%d.%m. %H:%M", tz = "UTC"), 
    las = 2
  )
  
  graphics::abline(v=at, col="grey", lty=2)
}

# plot_H_v_Q -------------------------------------------------------------------

plot_H_v_Q <- function(
  datLastDuration, main = "Main Title?", Hmax = NA, vmax = NA, Qmax = NA, 
  in.rows = TRUE, to.pdf = FALSE
) 
{  
  PDF <- kwb.utils::preparePdfIf(to.pdf, landscape = TRUE)
  
  old.par <- if (in.rows) {
    graphics::par(mfrow = c(3, 1), mar = c(3, 5, 3, 2))
  }
  else {
    graphics::par(mfrow = c(1, 3), mar = c(4, 4, 6, 2))
  }
  
  on.exit(graphics::par(old.par))
  
  kwb.plot::plot_variable(
    datLastDuration, "H", main = main, cex.main = 1,
    ylim = kwb.plot::appropriateLimits(datLastDuration$H, c(0, Hmax))
  )
  
  kwb.plot::plot_variable(
    datLastDuration, "v", 
    ylim = kwb.plot::appropriateLimits(datLastDuration$v, c(0, vmax))
  )
  
  kwb.plot::plot_variable(
    datLastDuration, "Q", 
    ylim = kwb.plot::appropriateLimits(datLastDuration$Q, c(0, Qmax))
  )
  
  kwb.utils::finishAndShowPdfIf(to.pdf, PDF)
}

# .getResultColours ------------------------------------------------------------

.getResultColours <- function
(
  resultTypes, 
  colour.success="darkgreen"
)
{
  resultColours <- .resultsToColours(resultTypes, colour.success)
  colourVector <- sapply(resultTypes, function(x) resultColours[x])
  names(colourVector) <- NULL
  
  colourVector
}

# .resultsToColours ------------------------------------------------------------

.resultsToColours <- function
(
  resultTypes, 
  successColour="darkgreen"
)
{
  successType <- "SUCCESS"
  
  resultTypes <- unique(as.character(resultTypes))
  errorTypes <- setdiff(resultTypes, successType)  
  
  if (successType %in% resultTypes) {
    typeColours <- successColour
    names(typeColours) <- successType
  }
  else {
    typeColours <- character()    
  }
  
  if (!isNullOrEmpty(errorTypes)) {
    errorColours <- grDevices::heat.colors(length(errorTypes))
    names(errorColours) <- errorTypes
    typeColours <- c(typeColours, errorColours)
  }
  
  typeColours
}
