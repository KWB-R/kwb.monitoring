# pathDictionary ---------------------------------------------------------------

#' Path Dictionary
#' 
#' Read dictionary from file and set RAW_DIR and STATION
#' 
#' @param dictionaryFile full path to file defining a dictionary and being 
#'   read with \code{\link[kwb.utils]{readDictionary}}
#' @param RAW_DIR value for placeholder of the same name in the dictionary
#' @param STATION value for placeholder of the same name in the dictionary
#' @param settings optional. List with elements \code{rawdir, station} from 
#'   which to take values to be used for \code{RAW_DIR, STATION}
#' @export

pathDictionary <- function(
  dictionaryFile, RAW_DIR = settings$rawdir, STATION = settings$station,
  settings = NULL  
)
{
  dictionary <- kwb.utils::readDictionary(dictionaryFile)
  
  c(dictionary, list(RAW_DIR = RAW_DIR, STATION = STATION))
}

# getOrCreatePath --------------------------------------------------------------

#' Get or Create Path
#' 
#' @param variableName key to be looked up in \emph{dictionary}, resolving to a
#'   file path
#' @param dictionary dictionary (list of key/value pairs) in which
#'   \emph{variableName} is looked up
#' @param settings default: NULL
#' @param create.dir if TRUE, the directory is created
#' @param stop.on.no.resolving if TRUE and \emph{variableName} could not be
#'   resolved the program stops
#' @param dbg if TRUE, debug messages are shown
#' @param \dots arguments passed to \code{\link[kwb.utils]{resolve}}
#' @export
getOrCreatePath <- function(
  variableName, dictionary = settings$dictionary, settings = NULL, 
  create.dir = FALSE, stop.on.no.resolving = TRUE, dbg = FALSE, ...
)
{
  filePath <- kwb.utils::resolve(variableName, dictionary, ...)
  
  if (filePath == variableName && stop.on.no.resolving) {
    
    stop(
      "Could not resolve the placeholder '", variableName, "'.\n", 
      "Please define a key of this name in the path dictionary.", call. = FALSE
    )
  }
  
  if (create.dir) {
    
    stopifnot(! is.null(kwb.utils::createDirectory(dirname(filePath), dbg = dbg)))
  }
  
  filePath
}

# dateToDateStringInPath -------------------------------------------------------

#' Date to Date String in Path
#' 
#' @param x date or time object passed to \code{\link[kwb.datetime]{hsDateStr}}
#' 
dateToDateStringInPath <- function(x)
{
  gsub("\\-", "_", kwb.datetime::hsDateStr(x))
}
