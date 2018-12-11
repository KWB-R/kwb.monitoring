# do_resolve -------------------------------------------------------------------
do_resolve <- function(key = NULL, dictionary = NULL)
{
  if (is.null(key)) {
    
    clean_stop("No key given to be resolved.")  
  }
  
  if (is.null(dictionary)) {
    
    clean_stop(sprintf("No dictionary given to resolve the key '%s'.", key))
  }
  
  if (! key %in% names(dictionary)) {
    
    clean_stop(sprintf(
      "No key '%s' contained in the dictionary.\nAvailable keys:\n%s",
      key, kwb.utils::stringList(names(dictionary), collapse = "\n")
    ))
  }
  
  kwb.utils::resolve(key, dictionary)
}

# clean_stop -------------------------------------------------------------------
clean_stop <- function(...)
{
  stop(..., call. = FALSE)
}

# clean_warning ----------------------------------------------------------------
clean_warning <- function(...)
{
  warning(..., call. = FALSE)
}

# getFunctionValueOrDefault2 ---------------------------------------------------

#' Get Function Value or Default 2
#' 
#' @param values passed to \code{\link[kwb.utils]{getFunctionValueOrDefault}}
#' @param FUN passed to \code{\link[kwb.utils]{getFunctionValueOrDefault}}
#' @param default passed to \code{\link[kwb.utils]{getFunctionValueOrDefault}}
#' @param timestamps vector of timestamps (used in warning message) 
#' @param columnName column name (used in warning message)
#' @export
getFunctionValueOrDefault2 <- function(
  values, FUN, default, timestamps = NULL, columnName = ""
)
{
  prolog <- ""
  
  if (! is.null(timestamps)) {
    
    prolog <- sprintf(
      "Time interval [%s ... %s]:", timestamps[1], utils::tail(timestamps, 1)
    )
  }
  
  warning_text <- paste(prolog, sprintf(
    "all %s-values are NA -> taking default: %s", columnName, toString(default)
  ))
  
  kwb.utils::getFunctionValueOrDefault(values, FUN, default, warning_text)
}
