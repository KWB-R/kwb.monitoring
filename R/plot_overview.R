# showOverview -----------------------------------------------------------------

#' Show Overview
#' 
#' @param dat data frame with columns \code{DateTime}
#' @param settings list with elements \code{dictionary, station}
#' @param Qmax passed to \code{\link{plotOverview}}
#' @param Hmax passed to \code{\link{plotOverview}}
#' @param to.pdf if \code{TRUE}, graphical output goes to a temporary pdf file
#' @param save.pdf if \code{TRUE} PDF output is stored to a file stored and 
#'   named according to \code{dictionary$OVERVIEW_HQ_DATA_PDF}
#' @export
showOverview <- function(
  dat, settings, Qmax = NULL, Hmax = NULL, to.pdf = FALSE, save.pdf = FALSE
)
{
  if (save.pdf) {
    
    timerange <- range(dat$DateTime)
    
    PDF <- getOrCreatePath(
      "OVERVIEW_HQ_DATA_PDF", 
      dictionary = settings$dictionary,
      create.dir = TRUE,
      HYDRAULIC_BEGIN = dateToDateStringInPath(timerange[1]),
      HYDRAULIC_END = dateToDateStringInPath(timerange[2])
    )
    
  } else {
    
    PDF <- tempfile(fileext=".pdf")
  }
  
  PDF <- kwb.utils::preparePdfIf(to.pdf, PDF = PDF, landscape = TRUE)
  
  plotOverview(dat, settings$station, Qmax, Hmax)
  
  kwb.utils::finishAndShowPdfIf(to.pdf, PDF = PDF, dbg = FALSE)
}

# plotOverview -----------------------------------------------------------------

#' Plot Overview
#' 
#' @param dat data frame containing the data to be plotted
#' @param station name of monitoring station, used in plot title
#' @param Qmax passed to \code{kwb.monitoring:::plotOverview_byDay}
#' @param Hmax passed to \code{kwb.monitoring:::plotOverview_byDay}
#' 
plotOverview <- function(dat, station, Qmax = NULL, Hmax = NULL)
{
  dat <- prepareDataForOverviewPlot(dat)
  
  commonArguments <- list(
    main = paste("Station:", station),
    type = c("p", "g"), 
    pch = ".", 
    auto.key = list(columns=2),
    as.table = TRUE    
  )
  
  plotOverview_total(dat, commonArguments)
  plotOverview_byDay(dat, commonArguments, Qmax, Hmax)
}

# prepareDataForOverviewPlot --------------------------------------------------

prepareDataForOverviewPlot <- function(dataFrame)
{
  # unit conversion m -> cm
  if (!is.null(dataFrame$H)) {
    dataFrame$H <- dataFrame$H * 100     
  }
  
  # selection of columns
  columns <- intersect(c("DateTime", "H", "Q"), names(dataFrame))
  dataFrame <- dataFrame[, columns]
  
  # matrix to "list"
  dataFrame <- kwb.utils::hsMatrixToListForm(dataFrame, keyFields = "DateTime")
  
  # date and time
  dataFrame$day <- kwb.datetime::hsDateStr(dataFrame$DateTime)
  dataFrame$timeonly <- as.POSIXct(
    as.integer(dataFrame$DateTime) %% 86400, 
    origin = "2000-01-01",
    tz="UTC"
  )
  
  dataFrame
}

# plotOverview_total -----------------------------------------------------------

plotOverview_total <- function(dat, commonArguments)
{
  ylab <- character()
  
  parameterNames <- unique(dat$parName)
  
  if ("Q" %in% parameterNames) {
    ylab <- c(ylab, "Q (L/s)")
  }

  if ("H" %in% parameterNames) {
    ylab <- c(ylab, "H (cm)")
  }
  
  specialArguments <- list(
    parVal ~ DateTime | parName, 
    data = dat, 
    ylab = ylab,
    scales = list(
      x = list(at = pretty(dat$DateTime), format = "%d.%m. %H:%M"),
      y = list(relation = "free", rot = 0)
    ),
    layout = c(1, length(ylab))
  )
  
  trellis.obj <- do.call(lattice::xyplot, args = c(specialArguments, commonArguments))
  
  graphics::plot(trellis.obj)
}

# plotOverview_byDay -----------------------------------------------------------

plotOverview_byDay <- function(dat, commonArguments, Qmax, Hmax)
{
  # ylim can be given as a list, with each element corresponding to the limits
  # of one parameter [according to the order in levels(as.factor(dat$parName))]
  
  parameterNames <- unique(dat$parName)
  
  ylab.H <- "H (cm)"
  ylab.Q <- "Q (L/s)"
  
  if (all(c("H", "Q") %in% parameterNames)) {
    ylim <- list(max_value_to_limit(Hmax), 
                 max_value_to_limit(Qmax))
    ylab <- sprintf("left: %s, right: %s", ylab.H, ylab.Q)
  } 
  else if ("H" %in% parameterNames) {    
    ylim <- max_value_to_limit(Hmax)
    ylab <- ylab.H
  } 
  else if ("Q" %in% parameterNames) {
    ylim <- max_value_to_limit(Qmax)
    ylab <- ylab.Q
  }
  
  # just to guarantee that ylim is interpreted correctly (different limits for
  # the different parameters H and Q) we need to make sure that there is at 
  # least one data point per day. Therefore, we introduce one point at 12:00
  # of value -999 (will not be seen within ylim)
  
  if ("Q" %in% parameterNames) {
    dat <- addFakeEntriesForDaysWithoutData(dat, parameterName = "Q")
  }
  
  if ("H" %in% parameterNames) {
    dat <- addFakeEntriesForDaysWithoutData(dat, parameterName = "H")
  }
  
  # add daily statistics
  dat <- merge(dat, getStatisticsByDay(dat))
  
  specialArguments <- list(
    parVal ~ timeonly | parName + info, 
    data = dat, 
    ylab = ylab,
    scales = list(
      x = list(relation = "same", format = "%H:%M", at = pretty(dat$timeonly)), 
      y = list(relation = "free", rot = 0)
    ),
    layout = c(2, 4), 
    xlab = "hour within day", 
    ylim = ylim
  )
  
  trellis.obj <- do.call(lattice::xyplot, c(specialArguments, commonArguments))
  
  graphics::plot(trellis.obj)
}

# max_value_to_limit -----------------------------------------------------------

max_value_to_limit <- function(maxValue)
{
  if (is.null(maxValue)) {
    NULL
  }
  else {
    c(0, maxValue)
  }
}

# addFakeEntriesForDaysWithoutData ---------------------------------------------

#' Add Fake Entries for Days Without Data
#' 
#' @param dataFrame data frame with columns \emph{day}, \emph{parName},
#'   \emph{parVal}
#' @param parameterName name of parameter, default: \code{"Q"}
#' @export 
#' @importFrom kwb.datetime hsToPosix
addFakeEntriesForDaysWithoutData <- function(dataFrame, parameterName = "Q")
{
  allDays <- unique(dataFrame$day)
  
  indices <- which(dataFrame$parName == parameterName & !is.na(dataFrame$parVal))
  daysWithQData <- unique(dataFrame$day[indices])
  
  missingDays <- setdiff(allDays, daysWithQData)
  
  fake_entries <- lapply(missingDays, function(missingDay) {
    
    # cat("No Q-value at all at", missingDay, "-> introducing \"fake\" value: -999.\n")
    kwb.utils::noFactorDataFrame(
      DateTime = hsToPosix(paste(missingDay, "12:00:00")),
      parName = parameterName,
      parVal = -999,
      day = missingDay,
      timeonly = hsToPosix(paste("2000-01-01", "12:00:00"))
    )
  })
  
  rbind(dataFrame, do.call(rbind, fake_entries))
}

# getStatisticsByDay -----------------------------------------------------------

#' Get Statistics by Day
#' 
#' @param dataFrame data frame with columns \emph{parName}, \emph{parVal},
#'   \emph{day}
#'   
#' @return data frame with additional columns \emph{info} ("day: H max = ... cm,
#'   Q max = ... L/s")
#' 
getStatisticsByDay <- function(dataFrame)
{
  parameterNames <- unique(dataFrame$parName)
  
  qmax <- NULL
  hmax <- NULL
  
  if ("Q" %in% parameterNames) {
    
    qdat <- dataFrame[dataFrame$parName == "Q", ]
    qmax <- stats::aggregate(qdat$parVal, by = list(day = qdat$day), max, na.rm = TRUE)
    qmax <- kwb.utils::renameColumns(qmax, list(x = "qmax"))    
  } 
  
  
  if ("H" %in% parameterNames) {
    
    hdat <- dataFrame[dataFrame$parName == "H", ]
    hmax <- stats::aggregate(hdat$parVal, by = list(day = hdat$day), max, na.rm = TRUE)
    hmax <- kwb.utils::renameColumns(hmax, list(x = "hmax"))
  } 
  
  dailyStatistics <- data.frame(
    day = unique(c(qmax$day, hmax$day)), 
    stringsAsFactors = FALSE
  )
  
  if (!is.null(hmax)) {
    dailyStatistics <- merge(dailyStatistics, hmax)
  }
  
  if (!is.null(qmax)) {
    dailyStatistics <- merge(dailyStatistics, qmax)
  }
  
  dailyStatistics$info <- sprintf("%s:", dailyStatistics$day)
  
  if (!is.null(hmax)) {
    dailyStatistics$info <- paste(
      dailyStatistics$info, 
      sprintf("H max = %0.1f cm", dailyStatistics$hmax),
      sep = " "
    )
  }
  
  if (!is.null(qmax)) {
    dailyStatistics$info <- paste(
      dailyStatistics$info, 
      sprintf("Q max = %0.2f L/s", dailyStatistics$qmax),
      sep = ifelse(is.null(hmax), " ", ", ")
    )
  }
  
  dailyStatistics
}
