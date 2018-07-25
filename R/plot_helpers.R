# default_day_time_format ------------------------------------------------------
default_day_time_format <- function()
{
  "%d.%m %H:%M"
}

# default_inner_margins --------------------------------------------------------
default_inner_margins <- function()
{
  c(0.1, 0.1, 0.1, 0.2)
}

# default_legend_arguments -----------------------------------------------------
default_legend_arguments <- function(x = "topright", horiz = FALSE)
{
  list(x = x, horiz = horiz, cex = 0.7, bg = "white")
}

# draw_thresholds_if_applicable ------------------------------------------------
draw_thresholds_if_applicable <- function(
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

# draw_limits ------------------------------------------------------------------
draw_limits <- function(v, col = "green", lty = 2, lwd = 2, dbg = FALSE) 
{
  kwb.utils::printIf(dbg, v, "drawing vertical lines at")
  
  graphics::abline(v = v, col = col, lty = lty, lwd = lwd)
}
