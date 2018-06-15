# calculateVolumeCompositeSample -----------------------------------------------

#' Calculate Volume Composite Sample
#' 
#' @param hydraulicSubset data frame with columns \emph{DateTime}, \emph{Q}, 
#'   \emph{bottle}
#' @param settings settings as returned by \code{\link{configure}}
#' @param show.all.bottles if \code{TRUE}, the volume table is shown not only
#'   for the selected bottles but also for all bottles (inclusive discarded
#'   ones). Default: \code{FALSE}
#'   
calculateVolumeCompositeSample <- function(
  hydraulicSubset, settings, show.all.bottles = FALSE
) 
{
  # calculate discharge volume (= timestep * sum_of_flows) per bottle  
  V.per.bottle <- stats::aggregate(
    hydraulicSubset$Q, 
    by = list(bottle = hydraulicSubset$bottle), 
    FUN = sum
  )
  
  V.per.bottle <- kwb.utils::renameColumns(V.per.bottle, list(x = "Q.sum"))
  
  # there must be a unique time step in hydraulicData
  timeStep.s <- unique(diff(as.integer(hydraulicSubset$DateTime)))
  
  if (length(timeStep.s) > 1) {
    print(hydraulicSubset)
    stop("There are different time steps (printed above): ", timeStep.s)
  }
  
  V.per.bottle$V.m3 <- (V.per.bottle$Q.sum * timeStep.s) / 1000
  
  if (show.all.bottles) {
    cat("\n\n*** Cumulated flow per interval represented by bottle (all bottles):\n")
    print(V.per.bottle)    
  }  
  
  V.per.bottle$useBottle <- TRUE
  
  # do not consider bottles that were not fully filled...
  if (!all(is.na(settings$bottlesToDiscard))) {
    
    cat(sprintf(
      "\n\nBottles to discard: %s\n", 
      kwb.utils::commaCollapsed(settings$bottlesToDiscard)
    ))
    
    rows.to.discard <- which(V.per.bottle$bottle %in% settings$bottlesToDiscard)
    V.per.bottle$useBottle[rows.to.discard] <- FALSE
  }
  
  if (! any(V.per.bottle$useBottle)) {
    stop("not at least one bottle to be used!")    
  }
  
  V.per.bottle$V.used <- V.per.bottle$V.m3
  V.per.bottle$V.used[! V.per.bottle$useBottle] <- NA
  
  V.total <- sum(V.per.bottle$V.used, na.rm=TRUE)
  
  if (V.total != 0) {
    V.per.bottle$ratio <- V.per.bottle$V.used / V.total  
  }
  else {
    V.per.bottle$ratio <- rep(0, nrow(V.per.bottle))
  }
  
  maxRatio <- max(V.per.bottle$ratio, na.rm=TRUE)
  
  if (maxRatio != 0) {
    V.per.bottle$ratioNormed <- V.per.bottle$ratio / maxRatio    
  }
  else {
    V.per.bottle$ratioNormed <- rep(0, nrow(V.per.bottle))
  }
  
  V.per.bottle$V.bottle.mL <- V.per.bottle$ratioNormed * settings$Vbottle
  
  cat("\n\n*** cumulated flow per interval represented by non-discarded bottles:\n\n")  
  print(V.per.bottle[V.per.bottle$useBottle, ])
  
  V.bottle.total <- sum(V.per.bottle$V.bottle.mL, na.rm=TRUE)
  cat(sprintf("\nTotal volume: %.0f mL\n", V.bottle.total))
  
  if (V.bottle.total > settings$Vmax) {
    
    reductionFactor <- settings$Vmax / V.bottle.total
    
    cat(sprintf("Total volume > Vmax (= %0.2f mL) -> Volumes are reduced by factor %0.2f to fit the maximum...\n",
                settings$Vmax, reductionFactor))
    
    V.per.bottle$V.bottle.mL <- V.per.bottle$V.bottle.mL * reductionFactor
  } else {
    cat(sprintf("to increase total volume, increase Vbottle (currently %0.2f mL)...\n",
                settings$Vbottle))
  }
  
  kwb.utils::removeColumns(V.per.bottle, "Q.sum")
}
