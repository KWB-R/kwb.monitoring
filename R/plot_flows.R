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
    
    density <- get_shade_densities(
      sampleInformation$bottleEvents$bottle, settings$bottlesToDiscard
    )
    
    plotSampleInformation(
      sampleInformation, add = TRUE, density = density, main = "", 
      maxSamplesOk = settings$max.samples.ok
    )
  }
}

# plot_Q_columns ---------------------------------------------------------------
plot_Q_columns <-function(
  hydraulicData, main = "Station?", xlim = NULL, ylim = NULL, 
  innerMargins = default_inner_margins(), q.threshold = 0, 
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
  
  draw_thresholds_if_applicable(time.dependent.thresholds)
  
  if ("Q.pred" %in% names(hydraulicData)) {
    
    kwb.plot::plot_variable(hydraulicData, "Q.pred", add = TRUE, col = "red")
  }
  
  legend_args <- c(default_legend_arguments(), list(
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

# get_shade_densities ----------------------------------------------------------
get_shade_densities <- function(
  bottleNumbers, bottlesToDiscard, density = 15
)
{
  densities <- rep(0, length(bottleNumbers))
  
  if (! all(is.na(bottlesToDiscard))) {
    
    densities[which(bottleNumbers %in% bottlesToDiscard)] <- density
  }
  
  densities
}
