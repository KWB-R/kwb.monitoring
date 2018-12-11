# printSampleInformation -------------------------------------------------------

#' Print Sample Information
#' 
#' @param sampleInformation list with elements \code{sampleData, samplingEvents, 
#'   bottleEvents}
#' 
printSampleInformation <- function(sampleInformation)
{
  cat(sprintf("\n\n*** Selected sample event: \"%s\":\n\n",
              unique(sampleInformation$sampleData$file)))
  #print(sampleInformation$sampleData[, -which(names(sampleData) == "file")])    
  print(sampleInformation$sampleData)
  
  cat("\n\n*** Sampling events:\n\n")
  print(sampleInformation$samplingEvents)  
  
  cat("\n\n*** Bottle events:\n\n")
  print(sampleInformation$bottleEvents)  
  cat("\n\n")  
}

# saveSampleInformation --------------------------------------------------------

#' Save Sample Information
#' 
#' @param sampleInformation list with elements \code{samplingEvents, 
#'   bottleEvents}
#' @param settings list with elements \code{dictionary, outsep, outdec}
#' @param sampleFile name of auto sampler file, passed to 
#'   \code{\link{sampleLogFileToSampleName}}
#' @export
#' 
saveSampleInformation <- function(sampleInformation, settings, sampleFile)
{
  # get sample event name in order to resolve folder names
  sampledEventName <- sampleLogFileToSampleName(sampleFile)
  
  filePath <- getOrCreatePath(
    "SAMPLED_EVENT_CSV_SAMPLES", 
    dictionary = settings$dictionary, 
    create.dir = TRUE,
    SAMPLED_EVENT_NAME = sampledEventName
  )
  
  cat("Writing", filePath, "... ")
  utils::write.table(sampleInformation$samplingEvents, 
              file = filePath, 
              sep = settings$outsep, 
              dec = settings$outdec, 
              row.names = FALSE)
  cat("ok.\n")
  
  filePath <- getOrCreatePath(
    "SAMPLED_EVENT_CSV_BOTTLES", 
    dictionary = settings$dictionary, 
    create.dir = TRUE,
    SAMPLED_EVENT_NAME = sampledEventName
  )
  
  cat("Writing", filePath, "... ")
  utils::write.table(sampleInformation$bottleEvents, 
              file = filePath, 
              sep = settings$outsep, 
              dec = settings$outdec, 
              row.names = FALSE)
  cat("ok.\n")
}

# sampleLogFileToSampleName ----------------------------------------------------

#' Sample Log File to Sample Name
#' 
#' @param sampleFile full path to auto sampler file
#' 
sampleLogFileToSampleName <- function(sampleFile)
{
  sampleFile <- basename(sampleFile)
  sampleFile <- gsub("\\.csv", "", sampleFile)
  sampleFile <- gsub("#\\d+", "", sampleFile)
  sampleFile <- gsub("_log_", "_", sampleFile)
  sampleFile
}

# writeCsvToPathFromDictionary -------------------------------------------------

#' Write CSV to Path From Dictionary
#' 
#' @param dataFrame data frame containing data to save
#' @param key key in \emph{settings$dictionary} to be resolved to file path
#' @param settings list of settings with elements \emph{dictionary},
#'   \emph{outsep}, \emph{outdec}
#' @param open.directory if TRUE (default), the directory in which the file is
#'   created is opened in the Windows Explorer after the file has been written.
#' @param \dots arguments passed to \code{\link{getOrCreatePath}}
#' @export 
writeCsvToPathFromDictionary <- function(
  dataFrame, 
  key,
  settings,
  open.directory = TRUE,
  ...
)
{ 
  filePath <- getOrCreatePath(
    variableName = key, settings = settings, create.dir = TRUE, ...)
  
  .writeToCsv(dataFrame = dataFrame, filePath = filePath, settings = settings)
  
  if (open.directory) {
    
    kwb.utils::hsOpenWindowsExplorer(dirname(filePath))  
  }
}

# .writeToCsv ------------------------------------------------------------------

.writeToCsv <- function(dataFrame, filePath, settings, dbg = TRUE)
{
  kwb.utils::catIf(dbg, sprintf("Writing to '%s'... ", filePath))
  
  utils::write.table(
    dataFrame, 
    file = filePath, 
    sep = settings$outsep, 
    dec = settings$outdec, 
    row.names = FALSE, 
    na = ""
  )
  
  kwb.utils::catIf(dbg, "ok.\n")
}

# writeDataAndOpenDirectory ----------------------------------------------------

#' Write Data and Open Directory
#' 
#' @param dataFrame data frame containing data to save
#' @param filePath base name of file ("_<moniPoint>_<sampleName>.csv" will be
#'   appended)
#' @param settings list of settings, as returned by \code{\link{configure}}
#' 
writeDataAndOpenDirectory <- function(dataFrame, filePath, settings)
{  
  .writeToCsv(dataFrame = dataFrame, filePath = filePath, settings = settings)
  
  kwb.utils::hsOpenWindowsExplorer(dirname(filePath))
}
