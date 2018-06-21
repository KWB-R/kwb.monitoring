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
