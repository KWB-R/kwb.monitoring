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
  
  rain_columns <- paste(stations, statistics, sep = ".")
  
  rain_columns <- intersect(rain_columns, names(hydraulicEvents))
    
  if (isNullOrEmpty(rain_columns)) {
    
    warning("There are no rain properties at all in hydraulicEvents!")
    
    return(NULL)
  }
  
  stat_columns <- c("V.m3", rain_columns)
  
  xlim <- c(0, max(hydraulicEvents[, stat_columns[1]]))
  
  ylim <- c(0, max(hydraulicEvents[, stat_columns[-1]], na.rm = TRUE))
  
  if (to.pdf) {
    
    pdf_file <- file.path(
      kwb.utils::resolve("OUTPUT_DIR", dict = settings$dictionary),
      sprintf("Rain_Event_Volume_Correlation_%s.pdf", settings$station)
    )
    
    kwb.utils::preparePdf(pdf_file)
    
    on.exit(kwb.utils::finishAndShowPdfIf(TRUE, pdf_file))
  }

  graphics::plot(
    NA, NA, xlim = xlim, ylim = ylim, xlab = "Runoff volume in L", 
    ylab = ylab[statistics], las = 1, main = main
  )
  
  plot_colours <- grDevices::rainbow(length(stat_columns) - 1)
  
  names(plot_colours) <- stat_columns[-1]
  
  r_squared <- numeric()
  
  for (sum.column in stat_columns[-1]) {
    
    x <- hydraulicEvents[, stat_columns[1]]
    
    y <- hydraulicEvents[, sum.column]
    
    col <- plot_colours[sum.column]
    
    graphics::points(x = x, y = y, col = col)
    
    model <- stats::lm("y~x", data = data.frame(x = x, y = y))
    
    r_squared <- c(r_squared, summary(model)$r.squared)
    
    graphics::abline(coef = model$coefficients, col = col)
  }
  
  legend <- sprintf(
    "%s (R sq. = %0.2f)", sub("\\.sum", "", stat_columns[-1]), r_squared
  )
  
  graphics::legend("bottomright", legend = legend, col = plot_colours, pch = 1)
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
  
  if (to.pdf) {
    
    timerange <- range(hydraulicData$DateTime)
    
    pdf_file <- getOrCreatePath(
      "OVERVIEW_EVENTS_PDF", 
      dictionary = settings$dictionary,
      create.dir = TRUE,
      HYDRAULIC_BEGIN = dateToDateStringInPath(timerange[1]),
      HYDRAULIC_END = dateToDateStringInPath(timerange[2])
    )
    
    kwb.utils::preparePdf(pdf_file, landscape = landscape)
  }
  
  on.exit(kwb.utils::finishAndShowPdfIf(to.pdf, PDF = pdf_file, dbg = FALSE))
    
  if (plot.event.overview) {
    
    plotEventDistribution(eventsAndStat = eventsAndStat, settings = settings)
  }
  
  for (i in seq_len(nrow(eventsAndStat))) {
    
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
  info_general <- formatSettings(
    settings = settings, 
    settingNames = c("evtSepTime", "durationThreshold")
  )

  old_pars <- graphics::par(no.readonly = TRUE)
  
  graphics::layout(matrix(c(1,2), nrow = 2), heights = c(1.1, 4))
  
  kwb.plot::setMargins(top = 3, bottom = 0)
  
  gplots::textplot(info_general, valign = "top", cex = 1)
  
  main <- paste("Station:", settings$station)          

  graphics::title(main)
  
  statistics <- formatEventStatisticsTable(
    kwb.event::hsEventsToUnit(eventsAndStat, "h"),
    precisionLevel = settings$precisionLevel
  )
  
  gplots::textplot(statistics, show.rownames = FALSE)
  
  # Reset graphical parameters
  graphics::par(old_pars)

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
  gauges <- if (is.null(rainData)) {
    NULL
  } else if (is.null(gauges)) {
    rainGaugesNearStation(settings$station)[1:3]
  }
  
  old_pars <- graphics::par(no.readonly = TRUE) 
  
  on.exit(graphics::par(old_pars), add = TRUE)  
  
  .setPageLayout(numberOfGauges = length(gauges))
  
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
  
  .drawLimits(v = xlim_event, col = "red")
  
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
  
  .drawLimits(v = xlim_event, col = "red")  
  
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
  gauge_indices <- seq_along(gauges)
  
  timestamps <- rainData$DateTime
  
  if (any(kwb.utils::inRange(timestamps, xlim[1], xlim[2]))) {  
    
    xlim <- .getXLim(xlim = xlim, timestamps = timestamps, shift.to.begin = 0)
    
    # get y limits over all gauges to be plotted
    ylim <- .getYLim(
      rainData, gauges, gauge_indices, 
      in.limits = kwb.plot::inLimits(timestamps, xlim)
    )
    
    xlims_rain <- if (! isNullOrEmpty(eventAndStat)) {
      
      .getRainXLims(eventAndStat, gauges)      
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
        
        .drawLimits(v = xlims_rain[[gauge]])
      }
    }
    
  } else {
    
    for (gauge in gauges) {
      
      gplots::textplot("No rain data available.", cex = 1)
    }      
  }
}

# .getRainXLims ----------------------------------------------------------------

.getRainXLims <- function(hydraulicEvents, gauges)
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

# .getXLim ---------------------------------------------------------------------

.getXLim <- function(xlim, timestamps, shift.to.begin = 0)
{
  t_beg <- timestamps - shift.to.begin
  t_end <- t_beg + kwb.datetime::getTimestepInSeconds(timestamps, default = 60)
  
  kwb.plot::appropriateLimits(x = c(t_beg, t_end), limits = xlim)  
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
  n_rows <- numberOfGauges + 2
  
  matrix_values <- c(rep(seq_len(n_rows), 2), rep(n_rows + 1, n_rows))
  
  graphics::layout(
    matrix(matrix_values, nrow = n_rows, byrow = FALSE), 
    heights = c(rep(relativeRainPlotHeight, numberOfGauges), 1, 1)
  )
}

# .drawLimits ------------------------------------------------------------------

.drawLimits <- function(v, col = "green", lty = 2, lwd = 2, dbg = FALSE) 
{
  kwb.utils::printIf(dbg, v, "drawing vertical lines at")

  graphics::abline(v = v, col = col, lty = lty, lwd = lwd)
}

# .textplot_eventInfo ----------------------------------------------------------

.textplot_eventInfo <- function(settings, eventAndStat)
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
  file <- unique(sampleInformation$samplerEvents$samplerFile)
  
  stopifnot(length(file) == 1)
  
  if (to.pdf) {  
    
    event_name <- sampleLogFileToSampleName(file)
    
    pdf_file <- getOrCreatePath(
      "SAMPLED_EVENT_PDF_COMPOSITE", 
      dictionary = settings$dictionary, 
      create.dir = TRUE,
      SAMPLED_EVENT_NAME = event_name
    )
    
    pdf_file <- kwb.utils::preparePdf(pdf_file, landscape = TRUE)
  }
  
  old_pars <- graphics::par(no.readonly = TRUE) 
  
  on.exit(kwb.utils::finishAndShowPdfIf(to.pdf, PDF = pdf_file, dbg = FALSE))
  
  on.exit(graphics::par(old_pars), add = TRUE)
  
  graphics::layout(matrix(nrow = 3, byrow = TRUE, c(
    1, 1 ,3,
    1, 1, 4,
    2, 2, 5
  )))
  
  main <- sprintf(
    "Station: %s, auto-sampler file: %s", settings$station, file
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
  V_event <- mergedEventAndStat$V.m3
  
  V_sampled <- sum(volumeCompositeSample$V.used, na.rm=TRUE)

  #kwb.utils::printIf(TRUE, mergedEventAndStat)

  bottles_to_discard <- settings$bottlesToDiscard
  
  bottles_to_consider <- settings$bottlesToConsider
  
  if (! all(is.na(bottles_to_consider))) {
    
    bottles_to_discard <- intersect(bottles_to_discard, bottles_to_consider)
  }
  
  infoText <- paste(
    formatBottleEnumeration(
      "Bottles considered", bottles_to_consider, na.text = "all"
    ),
    formatBottleEnumeration(
      "Bottles discarded", bottles_to_discard, na.text = "none"
    ),
    "",
    formatEventRelation(
      mergedEventAndStat, V_event, V_sampled, settings$precisionLevel
    ),
    "",
    formatSettings(settings, settingNames = c(
      "evtSepTime", "replaceMissingQMethod", "sampleEventMethod"
    )),
    sep = "\n"
  )
  
  gplots::textplot(
    infoText, valign = "top", halign = "left", mar = c(0, 0, 0, 0)
  )
}

# .partialPlot_Q ---------------------------------------------------------------

.partialPlot_Q <- function(
  hydraulicData, settings, sampleInformation = NULL, xlim = NULL, 
  ylim = c(0, NA), main = "", q.threshold = 0, interpolate = TRUE, ...
)
{
  if (! is.null(sampleInformation)) {
    
    sampler_event <- sampleInformation$samplerEvents
    
    stopifnot(nrow(sampler_event) == 1)
    
    if (is.null(xlim)) {
      
      xlim <- kwb.event::eventToXLim(sampler_event)
    }    
  }

  event_settings <- settings$event[[settings$station]]
  
  thresholds <- if (! is.null(event_settings)) {
    
    kwb.utils::renameColumns(event_settings, list(Qthreshold = "threshold"))
  }

  plot_Q_columns(
    hydraulicData, xlim = xlim, ylim = ylim, main = main, 
    q.threshold = get_Q_threshold(settings), 
    time.dependent.thresholds = thresholds, 
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
  # Find y limits over all y-columns
  q_columns <- c("Q.raw", "Q.interpol", "Q.pred")
  
  q_columns <- intersect(names(hydraulicData), q_columns)
  
  xlim <- kwb.plot::appropriateLimits(hydraulicData$DateTime, xlim)
  
  in_xlim <- kwb.utils::inRange(hydraulicData$DateTime, xlim[1], xlim[2])
  
  ylim <- kwb.plot::appropriateLimits(hydraulicData[in_xlim, q_columns], ylim)
  
  if (interpolate) {
    
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
  
  threshold_line_type <- 2
  
  graphics::abline(h = q.threshold, lty = threshold_line_type)
  
  .drawAdditionalThresholdsIfApplicable(time.dependent.thresholds)
  
  if ("Q.pred" %in% names(hydraulicData)) {
    
    kwb.plot::plot_variable(hydraulicData, "Q.pred", add = TRUE, col = "red")
  }

  legend_args <- c(.defaultLegendArguments(), list(
    legend = c("raw", "H > threshold", "interpolated", "predicted from H"),
    col = c("blue", "black", "green", "red"), 
    pch = c(
      kwb.plot::.defaultPlotParameter("pch", "Q.raw"),
      kwb.plot::.defaultPlotParameter("pch", "Q.signal"),
      kwb.plot::.defaultPlotParameter("pch", "Q.interpol"),
      kwb.plot::.defaultPlotParameter("pch", "Q.pred")
    )
  ))
  
  do.call(graphics::legend, args = legend_args)
}

# .drawAdditionalThresholdsIfApplicable ----------------------------------------

.drawAdditionalThresholdsIfApplicable <- function(
  time.dependent.thresholds, col = "red", lty = "dashed", dbg = FALSE
)
{
  if (! is.null(time.dependent.thresholds)) {
    
    kwb.utils::printIf(dbg, time.dependent.thresholds)

    x0 <- time.dependent.thresholds$tBeg
    
    x1 <- time.dependent.thresholds$tEnd
    
    threshold <- time.dependent.thresholds$threshold
    
    kwb.utils::catIf(
      dbg, "Plotting segments at (x0, x1, y):", format(x0), format(x1), 
      threshold, "\n"
    )

    graphics::segments(
      x0 = x0, y0 = threshold, x1 = x1, y1 = threshold, col = col, lty = lty
    )
    
  } else {
    
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
    hydraulicData, "H", type = "p", xlim = xlim, ylim = ylim, 
    innerMargins = innerMargins
  )
  
  kwb.plot::plot_variable(
    hydraulicData, "H.interpol", add = TRUE, col = "green"
  )
  
  threshold_line_type <- 2
  
  graphics::abline(h = h.threshold, lty = threshold_line_type)

  .drawAdditionalThresholdsIfApplicable(time.dependent.thresholds)
  
  legend.args <- c(
    .defaultLegendArguments(), 
    list(
      legend = c(sprintf("H threshold = %0.2f m", h.threshold), "interpolated"),
      lty = c(threshold_line_type, NA),
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
#' 
addSampleTimesToPlot <- function(
  sampleTimes, ymax, timeFormat = .defaultDayTimeFormat(), cex.text = 0.7, 
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
  
  legend_args <- c(.defaultLegendArguments(legendPosition), list(
    legend = names(result_colours), 
    col = as.character(result_colours),
    lty = 1, 
    pch = 16
  ))
  
  do.call(graphics::legend, args = legend_args)
}

# .defaultDayTimeFormat --------------------------------------------------------

.defaultDayTimeFormat <- function()
{
  "%d.%m %H:%M"
}

# .partialPlot_H ---------------------------------------------------------------

.partialPlot_H <-function(hydraulicData, settings, xlim, sampleInformation, ...)
{
  event_settings <- settings$event[[settings$station]]
  
  thresholds <- if (! is.null(event_settings)) {
    
    kwb.utils::renameColumns(event_settings, list(Hthreshold = "threshold"))
  }
  
  plot_H_columns(
    hydraulicData, 
    h.threshold = get_H_threshold(settings),                  
    time.dependent.thresholds = thresholds,
    xlim = xlim, 
    ylim = c(0, NA),
    ...
  )  
  
  if (! is.null(sampleInformation)) {
    
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
  
  if (! is.null(timestamps)) {
    
    prolog <- sprintf(
      "Time interval [%s ... %s]:", timestamps[1], utils::tail(timestamps, 1)
    )
  }
  
  warning_text <- paste(prolog, sprintf(
    "all %s-values are NA -> taking default: %s", columnName, toString(default)
  ))
  
  kwb.utils::getFunctionValueOrDefault(values, FUN, default, warning_text)
}

# .defaultLegendArguments ------------------------------------------------------

.defaultLegendArguments <- function(x = "topright", horiz = FALSE)
{
  list(x = x, horiz = horiz, cex = 0.7, bg = "white")
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
  old_pars <- graphics::par(no.readonly = TRUE)
  
  on.exit(graphics::par(old_pars))
  
  kwb.plot::setMargins(bottom = 7)
  
  title_text <- sprintf(
    "hydraulic events\nH > %0.2f m\ngap(H <= %0.2f m) > %0.0f h",
    get_H_threshold(settings),
    get_H_threshold(settings),
    settings$evtSepTime/3600)
  
  # TODO: use kwb.event::ganttPlotEventLists instead?
  
  kwb.event::ganttPlotEvents(
    events$hydraulic, 
    ylim = c(0,3.4), 
    y1 = 2.3, 
    title = title_text, 
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
  pdf_file <- kwb.utils::preparePdfIf(to.pdf, landscape = TRUE)
  
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
  
  kwb.utils::finishAndShowPdfIf(to.pdf, pdf_file)
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
