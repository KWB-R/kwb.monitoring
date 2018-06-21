# findUniqueTimeStepOrStop -----------------------------------------------------

findUniqueTimeStepOrStop <- function(timeStamps)   
{
  timeStep.s <- unique(diff(as.integer(timeStamps)))
  
  if(length(timeStep.s) != 1) {
    print(timeStep.s)
    stop("time step is not unique")
  }
  
  timeStep.s
}

# validateAndFillHydraulicData -------------------------------------------------

#' Validate and Fill Hydraulic Data
#' 
#' 1. remove rows with duplicate timestamps 2. fill gaps within hydraulic events
#' given by time intervals exceeding H threshold
#' 
#' @param hydraulicData data frame with column \emph{DateTime}, ...
#' @param tstep.fill.s target time step in seconds. Time gaps in
#'   \emph{hydraulicData} will be filled with interpolated values
#' @param replaceMissingQMethod one of c("interpolate", "predict").
#'   "interpolate": linear interpolation "predict": prediction from water levels
#'   using a saved square regression
#' @param regressionModels data frame with character columns \emph{from},
#'   \emph{to} (or POSIXct columns \emph{tBeg} and \emph{tEnd}) and
#'   \emph{modelFile} determining the time intervals to which the different
#'   correlation models are assigned.
#' @param regressionUsage data frame with character columns \emph{from},
#'   \emph{to} (or POSIXct columns \emph{tBeg} and \emph{tEnd}) defining the
#'   first and last timestamp of the time intervals in which the correlation is
#'   to be used
#' @param hydraulicEvents hydraulic events
#' @param additionalColumns columns additional to "DateTime", "H" and "Q" to be
#'   selected from \emph{hydraulicData}
#' @param modelDir full path to the directory where the model files (of the
#'   current station) are stored)
#' @param settings settings as returned by \code{\link{configure}}. Will be used
#'   to lookup function parameters for which no values have been given (see
#'   defaults)
#'   
validateAndFillHydraulicData <- function(
  hydraulicData, 
  tstep.fill.s = selectElements(settings, "tstep.fill.s"),
  replaceMissingQMethod = selectElements(settings, "replaceMissingQMethod"),
  regressionModels = selectElements(selectElements(
    settings, "regression"), "models")[[selectElements(settings, "station")]],
  regressionUsage = selectElements(selectElements(selectElements(
    settings, "regression"), "usage"), selectElements(settings, "station")),
  hydraulicEvents = NULL,
  additionalColumns = NULL,
  modelDir = getOrCreatePath("REGRESSION_DIR", dict = selectElements(
    settings, "dictionary"
  )),
  settings = NULL
)
{
  #kwb.utils::assignArgumentDefaults(kwb.monitoring::validateAndFillHydraulicData)
  #kwb.utils::assignPackageObjects("kwb.monitoring")
  
  # if there is no column "H", introduce one with values 0
  if (is.null(hydraulicData$H)) {
    
    message("There is no column \"H\". I introduce it with values 0!")
    
    hydraulicData$H <- rep(0, nrow(hydraulicData))
  }
  
  # Check for duplicate timestamps
  hydraulicData <- removeDuplicates(hydraulicData)
  
  # Continue only with selected columns
  # Check for missing columns
  columnNames <- c("DateTime", "H", "Q", additionalColumns)
  hydraulicData <- selectColumns(hydraulicData, columnNames)
  
  # inform on gaps...
  
  # fill gaps and interpolate within the time intervals given by begin and end 
  # times of the hydraulic events
  limits <- if (is.null(hydraulicEvents)) {
    
    NULL
    
  } else {
    
    selectColumns(hydraulicEvents, c("tBeg", "tEnd"))
  }
  
  hydraulicData <- kwb.base::hsFillUp(
    tseries = hydraulicData, 
    tsField = "DateTime", 
    step_s = tstep.fill.s, 
    limits = limits
  )
  
  # rename "_orig" ".raw"
  names(hydraulicData) <- gsub("\\_orig$", "\\.raw", names(hydraulicData))
  
  # if there is a model, append a column "Q.pred" with Q values predicted from H
  # values
  hydraulicData$Q.pred <- getPredictionOfQ(
    hydraulicData = hydraulicData, 
    regressionModels = regressionModels,
    modelDir = modelDir
  )

  # for plotting purposes, introduce a column Q.interpol containing
  # only the interpolated Q values (when Q.raw was NA)
  hydraulicData <- appendInterpolColumns(hydraulicData)
  
  # if replaceMissingQMethod is "predict" use the predicted values in column Q
  # when Q.raw is missing
  
  cat("replaceMissingQMethod =", replaceMissingQMethod, "-> ")
  
  if (is.null(regressionUsage) && replaceMissingQMethod == "predict") {
    timeRange <- as.character(range(hydraulicData$DateTime))
    regressionUsage <- data.frame(from = timeRange[1], to = timeRange[2])
  }
  
  if (replaceMissingQMethod == "predict") {
    
    cat("The predicted values are used if Q.raw is NA.\n")
    rows <- which(is.na(hydraulicData$Q.raw))
    
    rowsToPredict <- indicesInIntervals(
      timestamps = hydraulicData$DateTime,
      intervals = .toIntervalList(
        from = selectColumns(regressionUsage, "from"),
        to = selectColumns(regressionUsage, "to")
      )
    )
    
    rows <- intersect(rows, rowsToPredict)
    
    if ("Q.pred" %in% names(hydraulicData)) {
      
      predictedValues <- hydraulicData$Q.pred[rows]
      numberOfUnpredicted <- sum(is.na(predictedValues))
      
      if (numberOfUnpredicted > 0) {
        warning("There are ", numberOfUnpredicted, " values for which no ",
                "prediction is available. Check the regression settings!")
      }
      
      valid <- ! is.na(predictedValues)
      hydraulicData$Q[rows][valid] <- predictedValues[valid]
      
    } else {
      warning(paste("I cannot take predicted values as there is no regression",
                    "model defined yet. Linearly interpolated values are taken",
                    "instead."))
    }
  } 
  else if (replaceMissingQMethod == "interpolate") {
    cat("The interpolated values are used when Q.raw is NA.\n")
  } 
  else if (replaceMissingQMethod == "setToZero") {
    cat("Q is set to zero (0.0) if Q.raw is NA.\n")
    hydraulicData$Q[is.na(hydraulicData$Q.raw)] <- 0.0
  }
  else {
    stop("Unknown method (use 'interpolate' or 'predict') ",
         "for replacing missing Q values: ", replaceMissingQMethod)
  }
  
  # for plotting purposes, append a column Q.signal containing NA for times
  # when the H threshold is not exceeded and the Q value of column \emph{Q}
  # for times when the threshold is exceeded.
  hydraulicData$Q.signal <- getSignalColumn(hydraulicData, settings = settings)
  
  #                DateTime     H      Q H.raw  Q.raw eventNumber   Q.pred Q.signal
  #   1 2014-03-31 09:22:00 0.139 12.728 0.139 12.728           1 14.69543   12.728
  #   2 2014-03-31 09:24:00 0.140 13.526 0.140 13.526           1 15.07291   13.526
  #   3 2014-03-31 09:26:00 0.142 13.567 0.142 13.567           1 15.83229   13.567
  
  hydraulicData
}

# removeDuplicates -------------------------------------------------------------

#' Remove Duplicates
#' 
#' @param hydraulicData data frame with date and time column as named in 
#'   \code{timeColumnName}
#' @param timeColumnName name of date and time column in \code{hydraulicData}
#' @param keep.first logical. If \code{TRUE} (\code{FALSE} is not implemented!)
#'   only the first rows of sets of rows with duplicated time stamps are kept.
#' 
removeDuplicates <- function(
  hydraulicData, timeColumnName = "DateTime", keep.first = TRUE
)
{  
  times <- kwb.utils::selectColumns(hydraulicData, timeColumnName)
  
  indices <- which(duplicated(times))
  
  # Return hydraulicData unchanged if there are no duplicates
  if (length(indices) == 0) {
    return (hydraulicData)
  }
  
  messageText <- sprintf(
    "There are duplicated timestamps. The rows coming %s are kept:\n%s",
    ifelse(keep.first, "first", "last"),
    paste(
      utils::capture.output(hydraulicData[times %in% times[indices], ]),
      collapse = "\n"
    )
  )
  
  if (! keep.first) {
    
    stop("keep.first = FALSE is not yet implemented!")
    
    # Determine the indices of the rows to be removed...
    # indices <- ...
  }
  
  warning(messageText)
  
  # Return hydraulicData without the selected rows  
  hydraulicData[-indices, ]
}

# getPredictionOfQ -------------------------------------------------------------

#' Get Prediction of Q
#' 
#' @param hydraulicData data frame with columns as named in \code{columns}
#' @param regressionModels data frame with character columns \emph{from},
#'   \emph{to} (or POSIXct columns \emph{tBeg} and \emph{tEnd}) and character
#'   columns \emph{modelFile} determining the time intervals to which the
#'   different correlation models are assigned.
#' @param modelDir full path to directory where the model files (of the current
#'   station) are stored
#' @param columns names of columns containing Date and Time, water levels and 
#'   water flows
#' @param only.if.na logical. Not used!
#' 
getPredictionOfQ <- function(
  hydraulicData, regressionModels = NULL, modelDir, 
  columns = c(DateTime = "DateTime", H = "H", Q.raw = "Q.raw"), 
  only.if.na = TRUE
)
{
  # Prepare a vector of Q values predicted from H/Q correlation
  Qpred <- rep(NaN, nrow(hydraulicData))
  
  if (! is.null(regressionModels)) {
    
    # There may be either columns tBeg and tEnd (already POSIXct timestamps)
    # or columns from and to (factor or character). Conversion to POSIXct will
    # be done in .toIntervalList() if required
    
    if ("tBeg" %in% names(regressionModels)) {
      intervalColumns <- c("tBeg", "tEnd")
      tzone <- attr(regressionModels$tBeg, "tzone")
    } 
    else {
      intervalColumns <- c("from", "to")
      tzone <- "UTC"
    }
    
    timeIntervals <- .toIntervalList(
      from = selectColumns(regressionModels, intervalColumns[1]), 
      to = selectColumns(regressionModels, intervalColumns[2]), 
      tzone = tzone
    )
    
    times <- selectColumns(hydraulicData, columns["DateTime"])
    Qraw  <- selectColumns(hydraulicData, columns["Q.raw"])
    H     <- selectColumns(hydraulicData, columns["H"])
      
    for (i in seq_len(length.out = nrow(regressionModels))) {
      
      modelFile <- file.path(modelDir, regressionModels$modelFile[i])
      
      model <- getModelFromFile(modelFile, warn = FALSE)
      
      if (is.null(model)) {
        stop("There is no such regression model: ", modelFile, ".\n",
             "You may run section \"Find H-Q regression to create such a model.")
      } 
      
      # indices of rows belonging to the time interval and not being NA
      indices <- which((times %within% timeIntervals[[i]]) & is.na(Qraw))
      
      # There must not be NA in H-values!
      h <- .replace_NA_with_zero_and_warn(H[indices])
      
      # Replace NA values in Qpred with predicted values. 
      Qpred[indices] <- stats::predict(model, newdata = data.frame(H = h))
    }
  }
  
  Qpred
}

# .replace_NA_with_zero_and_warn -----------------------------------------------

.replace_NA_with_zero_and_warn <- function(H)
{
  H.is.na <- is.na(H)
  
  if (any(H.is.na)) {
    warning("There are ", sum(H.is.na), " NA's in H-values. They are set ",
            "to 0.0 in order to let prediction not fail")
    H[H.is.na] <- 0.0
  }
  
  H
}

# appendInterpolColumns --------------------------------------------------------

#' Append Interpol Columns
#' 
#' @param hydraulicData data frame containing hydraulic data
#' @param settings list of settings (not used!)
#' @param columnQraw name of column containing raw discharge
#' @param columnQ name of column containing (valid) discharge
#' @param columnQInterpol name of column to be added containing interpolated
#'   discharges
#' @param columnHraw name of column containing raw water levels
#' @param columnH name of column containing (valid) water levels
#' @param columnHInterpol name of column to be added containing interpolated
#'   water levels
#' 
#' @return data frame with columns \emph{H.interpol}, \emph{Q.interpol}
#'   appended, containing only the interpolated values and NA for times when H
#'   (or Q, respectively) was already available in column \emph{H.raw} (or 
#'   \emph{Q.raw}, respectively)
#' 
appendInterpolColumns <- function(
  hydraulicData, settings, columnQraw = "Q.raw", columnQ = "Q", 
  columnQInterpol = "Q.interpol", columnHraw = "H.raw", columnH = "H",
  columnHInterpol = "H.interpol"
)
{
  hydraulicData[[columnHInterpol]] <- NA
  hydraulicData[[columnQInterpol]] <- NA
  
  indices <- which(is.na(hydraulicData[[columnHraw]]))  
  
  if (! isNullOrEmpty(indices)) {
    hydraulicData[indices, columnHInterpol] <- hydraulicData[indices, columnH]    
  }
  
  indices <- which(is.na(hydraulicData[[columnQraw]]))  
  
  if (! isNullOrEmpty(indices)) {
    hydraulicData[indices, columnQInterpol] <- hydraulicData[indices, columnQ]    
  }
  
  hydraulicData
}

# .toIntervalList --------------------------------------------------------------

#' Character Vectors to List of "lubridate" Intervals
#' 
#' Converts two character vectors representing the beginning and end timestamps 
#' of time intervals into a list of lubridate interval objects created by 
#' \code{interval}
#' 
#' @param from vector of timestamps (vector of character will be converted to
#'   vector of POSIXct using \code{kwb.datetime::stringToPosix}) representing
#'   the starts of the intervals
#' @param to vector of timestamps (vector of character will be converted to
#'   vector of POSIXct using \code{kwb.datetime::stringToPosix}) representing
#'   the ends of the intervals
#' @param tzone passed to \code{kwb.datetime::stringToPosix} and 
#'   \code{lubridate::interval}
#' 
.toIntervalList <- function(from = NULL, to = NULL, tzone = "UTC")
{
  length.from <- length(from)
  
  if (length.from != length(to)) {
    stop("from and to must be of same length!")
  }
  
  needToConvert <- ! sapply(list(from = from, to = to), inherits, "POSIXt")
  
  lapply(seq_len(length.from), function(i) {
    
    start <- from[i]
    end <- to[i]
    
    if (needToConvert["from"]) {
      start <- kwb.datetime::stringToPosix(start, tzone = tzone)
    }
    
    if (needToConvert["to"]) {
      end <- kwb.datetime::stringToPosix(end, tzone = tzone)
    }
    
    lubridate::interval(start = start, end = end, tzone = tzone)
  })
}

# .test_toIntervalList ---------------------------------------------------------

.test_toIntervalList <- function()
{
  intervals1 <- data.frame(
    from = c("2015-01-01", "2016-01-01"),
    to   = c("2016-01-01", "2017-01-01")
  )
  
  intervals2 <- data.frame(
    from = c("2015-01-01 12:00", "2015-07-01 12:00"),
    to   = c("2015-01-01 15:00", "2015-07-01 20:00")
  )
  
  intervals3 <- data.frame(
    from =           c("2015-01-01 12:00:00", "2015-07-01 12:00:30"),
    to   = hsToPosix(c("2015-01-01 15:00:20", "2015-07-01 20:00:00"))
  )
  
  .toIntervalList(intervals1$from, intervals1$to)
  .toIntervalList(intervals1$from, intervals1$to, tzone = "Etc/GMT-1")
  
  .toIntervalList(intervals2$from, intervals2$to)
  .toIntervalList(intervals2$from, intervals2$to, tzone = "Etc/GMT-1")
  
  .toIntervalList(from = intervals3$from, to = intervals3$to)
  .toIntervalList(intervals3$from, intervals3$to, tzone = "Etc/GMT-1")
}

# indicesInIntervals -----------------------------------------------------------

#' Indices of Rows Belonging to Time Intervals
#' 
#' @param timestamps vector of POSIXct timestamps
#' @param intervals vector of Interval objects as returned by
#'   lubridate::interval
#'   
#' @return indices of elements in \emph{timestamps} that belong to the given
#'   time \emph{intervals}
#' 
indicesInIntervals <- function(timestamps, intervals) 
{
  indices <- integer()
  
  for (interval in intervals) {
    
    indices <- unique(c(indices, which(timestamps %within% interval)))
  }

  indices
}

# getSignalColumn --------------------------------------------------------------

getSignalColumn <- function(
  hydraulicData, 
  eventSettings = selectElements(
    selectElements(settings, "event"), 
    selectElements(settings, "station")
  ),
  thresholds = c(
    H = get_H_threshold(settings, do.stop = FALSE), 
    Q = get_Q_threshold(settings, do.stop = FALSE)
  ),
  columnQ = "Q", 
  settings = NULL
)
{
  # Initialise the result vector
  signals <- rep(NaN, nrow(hydraulicData))
  
  indices <- getIndicesWithinEvents(
    hydraulicData = hydraulicData, 
    eventSettings = eventSettings,
    thresholds = thresholds
  )
  
  if (isNullOrEmpty(indices)) {
    warning(sprintf(
      "The water level is never above the threshold of %0.2f m.",
      thresholds["H"]
    ))
  } else {
    
    # Copy the Q values at the indices belonging to events
    Q <- selectColumns(hydraulicData, columnQ)
    signals[indices] <- Q[indices]
  }  
  
  signals  
}

# removeIntervals --------------------------------------------------------------

#' Remove Intervals
#' 
#' @param dataFrame data frame with a column as named in \code{dateTimeColumn}
#' @param intervals vector of Interval objects as returned by
#'   lubridate::interval, passed to \code{\link{indicesInIntervals}}
#' @param dateTimeColumn name of date and time column in \code{dataFrame}
#' 
removeIntervals <- function(
  dataFrame, intervals,
  dateTimeColumn = names(kwb.utils::posixColumnAtPosition(dataFrame))[1]
) 
{
  times <- selectColumns(dataFrame, dateTimeColumn)
  
  indices <- indicesInIntervals(times, intervals)
  
  if (length(indices) > 0) {
    dataFrame[-indices, ]
  } else {
    dataFrame
  }
}
