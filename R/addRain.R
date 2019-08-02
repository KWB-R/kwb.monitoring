# add rain to existing plot as upside-down bars
addRain <- function(raindat, ymax, scale, color, rainGauge)
{
  for (i in seq_len(nrow(raindat) - 1)) {
    
    x0 <- raindat[[1]][i]
    x1 <- raindat[[1]][i + 1]
    
    rain_heights <- kwb.utils::selectColumns(raindat, rainGauge)
    
    y1 <- rain_heights[i]
    
    graphics::polygon(
      x = c(x0, x0, x1, x1),
      y = c(ymax, -y1 * scale + ymax, -y1 * scale + ymax, ymax),
      col = color, 
      border = color
    )
  }
  
  iNmax <- max(rain_heights, na.rm = TRUE)
  
  iNby <- if (iNmax < 0.5) {
    0.1
  } else if (iNmax < 1) {
    0.2
  } else if (iNmax < 2) {
    0.5
  } else if (iNmax < 5) {
    1
  } else {
    2
  }

  rAx <- -seq(0, iNmax, by = iNby) * scale + ymax
  rAxLab <- round(seq(0, iNmax, by = iNby), digits = 1)
  
  graphics::axis(4, las = 2, at = rAx, labels = rAxLab, hadj = 0.3)
  
  text <- expression(paste(i[N], " [mm/5min.]"))
  graphics::mtext(text, side = 4, line = 1.8, at = ymax, adj = 1, cex = 0.8)
}
