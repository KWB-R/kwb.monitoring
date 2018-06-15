# whichAboveThresholds ---------------------------------------------------------

#' In Which Rows Are Thresholds Exceeded?
#' 
#' Get indices of rows in \emph{hydraulicData} in which H or Q thresholds are
#'   exceeded
#' 
#' @param hydraulicData data frame with columns as named in \code{columns}
#' @param indices vector of indices of preselected rows from which to exclude
#'   those in which the thresholds are exceeded
#' @param thresholds vector of thresholds for \code{H} and \code{Q} values, 
#'   respectively 
#' @param columns vector of names containing \code{H} and \code{Q} values, 
#'   respectively
#' 
whichAboveThresholds <- function(
  hydraulicData, indices = seq_len(nrow(hydraulicData)), 
  thresholds = c(H = NA, Q = NA), columns = c(H = "H", Q = "Q")
)
{
  # If a H threshold is defined intersect with indices that are above the H
  # threshold. If a Q threshold is defined intersect with indices that are above
  # the threshold
  
  for (variable in c("H", "Q")) {
    
    threshold <- thresholds[variable]

    if (! (is.null(threshold) || is.na(threshold))) {
      
      values <- hydraulicData[[columns[variable]]]
      
      indices <- intersect(indices, which(values > threshold))
    }
  }

  indices
}

# apply_H_threshold ------------------------------------------------------------

#' Apply H Threshold
#' 
#' Apply H threshold given in settings to H in \emph{dat.raw}
#' 
#' @param dat.raw data frame with column \emph{H}
#' @param settings list as returned by \code{\link{configure}} with list element
#'   \emph{Hthresholds}
#' 
apply_H_threshold <- function(dat.raw, settings)
{
  dat.raw[which(H_above_threshold(dat.raw, settings)), ]
}

# H_above_threshold ------------------------------------------------------------

#' H Above Threshold
#' 
#' Vector of TRUE/FALSE with TRUE at positions where H is above the threshold
#' 
#' @param dat.raw data frame with column \emph{H}
#' @param settings list as returned by \code{\link{configure}} with list element
#'   \emph{Hthresholds}
#' 
H_above_threshold <- function(dat.raw, settings)
{
  dat.raw$H > get_H_threshold(settings)
}

# get_H_threshold --------------------------------------------------------------

get_H_threshold <- function(settings, do.stop = FALSE)
{
  .getThresholdOrStop(settings$Hthresholds, settings$station, "H", do.stop)
}

# get_Q_threshold --------------------------------------------------------------

get_Q_threshold <- function(settings, do.stop = FALSE)
{
  .getThresholdOrStop(settings$Qthresholds, settings$station, "Q", do.stop)
}

# get_V_threshold --------------------------------------------------------------

get_V_threshold <- function(settings, do.stop = FALSE)
{
  .getThresholdOrStop(settings$Vthresholds, settings$station, "V", do.stop)  
}

# .getThresholdOrStop ----------------------------------------------------------

.getThresholdOrStop <- function(thresholds, station, thresholdType, do.stop = TRUE)
{
  threshold <- thresholds[station]
  
  if (do.stop && (is.null(threshold) || is.na(threshold))) {
    stop(
      sprintf("There is no %s-threshold defined for station \"%s\"!",
              thresholdType, station)
    )
  }
  threshold  
}
