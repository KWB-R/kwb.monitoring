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

