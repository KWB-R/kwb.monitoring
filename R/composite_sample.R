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
#' @importFrom kwb.utils commaCollapsed
#' @importFrom kwb.utils quotient
#' @importFrom kwb.utils removeColumns 
#' @importFrom kwb.utils renameColumns 
#' 
calculateVolumeCompositeSample <- function(
  hydraulicSubset, settings, show.all.bottles = FALSE
) 
{
  # Calculate discharge volume (= timestep * sum_of_flows) per bottle  
  V_per_bottle <- stats::aggregate(
    hydraulicSubset$Q, 
    by = list(bottle = hydraulicSubset$bottle), 
    FUN = sum
  )
  
  V_per_bottle <- renameColumns(V_per_bottle, list(x = "Q.sum"))
  
  # There must be a unique time step in hydraulicData
  timestep_s <- unique(diff(as.integer(hydraulicSubset$DateTime)))
  
  if (length(timestep_s) > 1) {
    
    print(hydraulicSubset)
    
    stop("There are different time steps (printed above): ", timestep_s)
  }
  
  V_per_bottle$V.m3 <- (V_per_bottle$Q.sum * timestep_s) / 1000
  
  if (show.all.bottles) {
    
    cat("\n\n*** Cumulated flow per interval represented by bottle (all bottles):\n")
    
    print(V_per_bottle)    
  }  
  
  V_per_bottle$useBottle <- TRUE
  
  # Do not consider bottles that were not fully filled...
  if (! all(is.na(settings$bottlesToDiscard))) {
    
    catFormatted(
      "\n\nBottles to discard: %s\n", commaCollapsed(settings$bottlesToDiscard)
    )
    
    rows_to_discard <- which(V_per_bottle$bottle %in% settings$bottlesToDiscard)
    
    V_per_bottle$useBottle[rows_to_discard] <- FALSE
  }
  
  if (! any(V_per_bottle$useBottle)) {
    
    stop("Not at least one bottle to be used!")    
  }
  
  V_per_bottle$V.used <- V_per_bottle$V.m3
  
  V_per_bottle$V.used[! V_per_bottle$useBottle] <- NA
  
  V_total <- sum(V_per_bottle$V.used, na.rm = TRUE)
  
  V_per_bottle$ratio <- quotient(V_per_bottle$V.used, V_total, 0)
  
  maxRatio <- max(V_per_bottle$ratio, na.rm = TRUE)
  
  V_per_bottle$ratioNormed <- quotient(V_per_bottle$ratio, maxRatio, 0)

  V_per_bottle$V.bottle.mL <- V_per_bottle$ratioNormed * settings$Vbottle
  
  cat("\n\n*** cumulated flow per interval represented by non-discarded bottles:\n\n")
  
  print(V_per_bottle[V_per_bottle$useBottle, ])
  
  V_bottle_total <- sum(V_per_bottle$V.bottle.mL, na.rm = TRUE)
  
  catFormatted("\nTotal volume: %.0f mL\n", V_bottle_total)
  
  if (V_bottle_total > settings$Vmax) {
    
    reduction <- settings$Vmax / V_bottle_total
    
    catFormatted(
      "Total volume > Vmax (= %0.2f mL) -> Volumes are reduced by factor %0.2f to fit the maximum...\n",
      settings$Vmax, reduction
    )
    
    V_per_bottle$V.bottle.mL <- V_per_bottle$V.bottle.mL * reduction
    
  } else {
    
    catFormatted(
      "to increase total volume, increase Vbottle (currently %0.2f mL)...\n",
      settings$Vbottle
    )
  }
  
  removeColumns(V_per_bottle, "Q.sum")
}

# catFormatted -----------------------------------------------------------------
catFormatted <- function(format, ...)
{
  cat(sprintf(format, ...))
}
