# getModelFromFile -------------------------------------------------------------

#' Read Regression Model from Text File
#' 
#' @param modelFile path to .RData file containing the model
#' @param warn if \code{TRUE} (the default), a warning is given if the model
#'   file does not exist
#' @export
getModelFromFile <- function(modelFile, warn = TRUE)
{
  if (!file.exists(modelFile)) {
    
    regressionModel <- NULL
    
    if (warn) {
      .message_no_regression_model()
    }
  }
  else {
    tempEnvironment <- new.env()
    load(modelFile, envir = tempEnvironment)
    regressionModel <- get("regressionModel", envir = tempEnvironment)
    rm(tempEnvironment)    
  }
  
  return(regressionModel)
}

# .message_no_regression_model -------------------------------------------------

.message_no_regression_model <- function()
{
  message(
    "\n*** There is no regression model available yet.\n",
    "Please create a model interactively by calling\n",
    "  \"selectIntervalsForCorrelation(dat.all, settings)\",\n",
    "press \"Done\" and run\n", 
    "  \"saveRegressionModel(regressionState$model, settings)\"\n",
    "to save the model."
  )  
}

# saveRegressionModel ----------------------------------------------------------

#' Save Regression Model
#' 
#' @param regressionModel model object to be stored
#' @param settings optional. List from which to take the arguments if not given
#' @param dictionary dictionary (list) containing entries "REGRESSION_MODEL_TXT"
#'   and "REGRESSION_COEFF_TXT"
#' @param sep column separator in created file
#' @param dec decimal character in created file
#' @importFrom kwb.utils selectElements
#' @export
saveRegressionModel <- function(
  regressionModel, 
  settings = NULL, 
  dictionary = kwb.utils::selectElements(settings, "dictionary"), 
  sep = kwb.utils::selectElements(settings, "outsep"), 
  dec = kwb.utils::selectElements(settings, "outdec")
)
{
  modelFile <- getOrCreatePath(
    "REGRESSION_MODEL_TXT", dictionary = dictionary, create.dir = TRUE)
  
  coeffFile <- getOrCreatePath(
    "REGRESSION_COEFF_TXT", dictionary = dictionary, create.dir = TRUE)

  cat("Saving model to", modelFile, "... ")
  save(regressionModel, file = modelFile, ascii = TRUE)
  cat("ok.\n")
  
  cat("Saving model coefficients to", coeffFile, "... ")
  utils::write.table(
    as.data.frame(t(stats::coefficients(regressionModel))), 
    file = coeffFile, 
    sep = sep, 
    dec = dec,
    row.names = FALSE
  )
  cat("ok.\n")  
}

# updateSelection --------------------------------------------------------------

updateSelection <- function(i, j, h.threshold, action, go, done)
{
  regressionState <- manipulate::manipulatorGetState("regressionState")

  if (is.null(regressionState)) {
    
    regressionState <- get("regressionState", envir = .GlobalEnv)
  }
  
  #graphics::layout(matrix(c(1,1,2,2,3,4), ncol=2, byrow=TRUE), heights=c(1.5,1.5,1))
  graphics::layout(
    matrix(c(1, 1, 2, 2, 3, 3), ncol = 2, byrow = TRUE), heights = c(1, 1, 2)
  )
  
  if (go && action %in% c("add", "remove")) {
    
    cat(action, "rows between index", i, "and", j, "\n")  
    regressionState$used[i:j] <- (action == "add")
  }
    
  if (go && action %in% c("zoom.in", "zoom.out")) {
    
    regressionState$zoomHistory <- updateZoomHistory(
      action, regressionState$zoomHistory, i, j)
    cat("zoomHistory after update:\n")
    print(regressionState$zoomHistory)
  }

  if (go && action == "threshold") {
    
    regressionState$used <- (regressionState$data$H > h.threshold)
  }
  
  if (done) {
    
    kwb.utils::assignGlobally("regressionState", regressionState)
    grDevices::dev.off()
    return()
  }
  
  limits <- getLimitsFromHistoryOrDefault(regressionState)
  
  xlim <- regressionState$data$DateTime[limits]    
  
  removedIntervals <- getRemovedIntervals(regressionState$used)
  
  kwb.plot::setMargins(bottom = 0.5)
  
  graphics::plot(
    regressionState$data$DateTime, regressionState$data$Q, 
    xaxt = "n", ylab = "Q (L/s)", xlim = xlim, 
    main = paste("Station:", regressionState$settings$station)
  )
  
  plotRemovedIntervals(regressionState$data, removedIntervals)
  graphics::abline(v=regressionState$data$DateTime[c(i, j)], col = "red")

  kwb.plot::setMargins(bottom = 4, top = 0)
  
  kwb.plot::plot_variable(regressionState$data, "H", type = "p", xlim = xlim)
  plotRemovedIntervals(regressionState$data, removedIntervals)
  graphics::abline(v = regressionState$data$DateTime[c(i, j)], col = "red")
  
  graphics::abline(h = h.threshold, lty = 2)
  
  regressionState$model <- plot_H_vs_Q(regressionState$data[regressionState$used, ])
  
  manipulate::manipulatorSetState("regressionState", regressionState)
}

# updateZoomHistory ------------------------------------------------------------

#' Update Zoom History
#'
#' @param action one of \code{c("zoom.in", "zoom.out")}
#' @param zoomHistory list of (i, j) pairs, storing the history of zoom settings
#' @param i index in range of slider "left"
#' @param j index in range of slider "right"
#' 
updateZoomHistory <- function(action, zoomHistory, i, j) 
{
  if (action == "zoom.in") {
    zoomHistory <- addZoomToHistory(zoomHistory, i, j)
  }
  else if (action == "zoom.out"){
    zoomHistory <- removeZoomFromHistory(zoomHistory)
  }  
  else {
    stop("unexpected action: ", action)
  }
  
  return (zoomHistory)
}

# addZoomToHistory -------------------------------------------------------------

#' Add Zoom to History
#' 
#' @param zoomHistory list of (i, j) pairs, storing the history of zoom settings
#' @param i index in range of slider "left"
#' @param j index in range of slider "right"
#' 
addZoomToHistory <- function(zoomHistory, i, j)
{
  c(zoomHistory, list(c(i, j)))
}

# removeZoomFromHistory --------------------------------------------------------

#' Remove Zoom from History
#' 
#' @param zoomHistory list of (i, j) pairs, storing the history of zoom settings
#' 
removeZoomFromHistory <- function(zoomHistory)
{
  n <- length(zoomHistory)

  if (n > 0) {
    zoomHistory[-n]
  }
  else {
    zoomHistory    
  }  
}

# getLimitsFromHistoryOrDefault ------------------------------------------------

getLimitsFromHistoryOrDefault <- function(regressionState)
{
  n <- length(regressionState$zoomHistory)
  
  if (n > 0) {
    return (regressionState$zoomHistory[[n]])
  }
  else {
    return (c(1, nrow(regressionState$data)))
  }  
}

# getRemovedIntervals ----------------------------------------------------------

getRemovedIntervals <- function(used)
{
  changes <- kwb.event::hsEventsOnChange(used)
  firstStatus <- used[changes$iBeg[1]]  
  indices <- which(1:nrow(changes) %% 2 == as.integer(!firstStatus))  
  changes[indices, ]  
}

# plotRemovedIntervals ---------------------------------------------------------

plotRemovedIntervals <- function(data, removedIntervals)
{
  if (isNullOrEmpty(removedIntervals)) {
    return()
  }
    
  xleft <- data$DateTime[removedIntervals$iBeg]
  xright <- data$DateTime[removedIntervals$iEnd]

  plotAreaRectangle <- graphics::par()$usr
  
  ybottom <- plotAreaRectangle[3]
  ytop <- plotAreaRectangle[4]
  
  yrange <- ytop - ybottom
  
  ybottom <- ybottom + 0.01*yrange
  ytop <- ytop - 0.01*yrange
  
  graphics::rect(
    xleft = xleft, ybottom = ybottom, xright = xright, ytop = ytop, density = 5, 
    col = "grey"
  )
}

# plot_H_vs_Q ------------------------------------------------------------------

plot_H_vs_Q <- function(selection) 
{
  legendtext <- getLegendTextWithNaInfo(selection)
  
  selection <- selection[!is.na(selection$H) & !is.na(selection$Q), ]
  
  graphics::plot(selection$Q, selection$H, xlab="Q", ylab="H")

  graphics::legend("topleft", legend = legendtext) 
  
  selection <- selection[order(selection$H), ]
  
  model <- stats::lm(Q ~ poly(H, degree = 2, raw = TRUE), data = selection)
  
  graphics::lines(stats::predict(model), selection$H, col = "red")
  
  return (model)
}

# getLegendTextWithNaInfo ------------------------------------------------------

getLegendTextWithNaInfo <- function(selection)
{
  is.na.q <- is.na(selection$Q)
  is.na.h <- is.na(selection$H)
  is.na.both <- is.na.q && is.na.h
  
  c(sprintf("number of points: %d", nrow(selection)),
    sprintf("is.na(Q): %d", sum(is.na.q)),
    sprintf("is.na(H): %d", sum(is.na.h)),
    sprintf("is.na(both): %d", sum(is.na.both)))  
}

# selectIntervalsForCorrelation ------------------------------------------------

#' Select Intervals for Correlation
#' 
#' @param dat.all data frame containing all relevant data passed to 
#'   \code{kwb.monitoring:::H_above_threshold}
#' @param settings list of settings with elements \code{Hthresholds, "station"}
#'   and being passed to \code{kwb.monitoring:::H_above_threshold}
#' @param h.threshold.max maximum value for the slider
#' 
selectIntervalsForCorrelation <- function(
  dat.all, 
  settings, 
  h.threshold.max = 0.4
)
{
  #kwb.utils::assignGlobally("graphicalParameters", par(no.readonly = TRUE))
  
  regressionState <- list(
    data = dat.all,
    settings = settings,
    used = H_above_threshold(dat.all, settings),
    zoomHistory = list()
  )
  
  kwb.utils::assignGlobally("regressionState", regressionState)
  
  cat("*** The variable \"regressionState\" was assigned in the global environment.\n")

  h.threshold.ini <- kwb.utils::selectElements(settings, "Hthresholds")[kwb.utils::selectElements(settings, "station")]
  
  n <- nrow(dat.all)
  
  #graphicalParameters <- par(no.readonly = TRUE) # save default, for resetting...
  #on.exit(par(graphicalParameters)) # reset to default
    
  manipulate.args <- list(
    kwb.utils::stringToExpression(
      "updateSelection(i, j, h.threshold, action, go, done)"
    ),
    i = manipulate::slider(1, n, step = 1, label = "left"),
    j = manipulate::slider(1, n, n, step = 1, label = "right"),
    h.threshold = manipulate::slider(
      min = 0, 
      max = h.threshold.max, 
      step = 0.01,
      initial = h.threshold.ini, 
      label = "H threshold"
    ),
    action = manipulate::picker(
      "Remove Interval" = "remove", 
      "Add Interval" = "add", 
      "Zoom in" = "zoom.in",
      "Zoom out" = "zoom.out",
      "Apply H threshold" = "threshold",
      label = "Action"
    ),
    go = manipulate::button(label="Go"),
    done = manipulate::button(label="Done")
  )

  do.call(manipulate::manipulate, args = manipulate.args)
}
