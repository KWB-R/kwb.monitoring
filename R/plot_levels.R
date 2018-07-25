# .partialPlot_H ---------------------------------------------------------------
.partialPlot_H <- function(
  hydraulicData, settings, xlim, sampleInformation, ...
)
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

# plot_H_columns ---------------------------------------------------------------

#' Plot H Columns
#' 
#' @param hydraulicData data frame with columns \emph{DateTime}, \emph{H},
#'   \emph{H.interpol}
#' @param h.threshold H threshold at which a horizontal line is to be drawn
#'   (default: 0)
#' @param time.dependent.thresholds passed to 
#'   \code{kwb.monitoring:::draw_thresholds_if_applicable}
#' @param xlim passed to \code{\link[kwb.plot]{plot_variable}}
#' @param ylim passed to \code{\link[kwb.plot]{plot_variable}}
#' @param innerMargins passed to \code{\link[kwb.plot]{plot_variable}}
#' 
plot_H_columns <- function(
  hydraulicData, h.threshold = 0, time.dependent.thresholds = NULL,  
  xlim = NULL, ylim = NULL, innerMargins = default_inner_margins()
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
  
  draw_thresholds_if_applicable(time.dependent.thresholds)
  
  legend.args <- c(
    default_legend_arguments(), 
    list(
      legend = c(sprintf("H threshold = %0.2f m", h.threshold), "interpolated"),
      lty = c(threshold_line_type, NA),
      pch = c(NA, kwb.plot::.defaultPlotParameter("pch", "H.interpol")),
      col = c("black", "green")
    )
  )
  
  do.call(graphics::legend, args = legend.args)    
}
