#' Utility to read properties
#'
#' @param x A vector of properties file paths
#'
#' @return A character vector of named property elements
#'
#' @description A property file contains key/value pair entries delimited by an
#' equal sign or colon (`:`).
#'
#' The first existing properties file in the vector of paths `x` is read.
#'
#' The property key or name is the string before the first equal sign or colon.
#' The value is the remainder of the line after the first equal sign or colon.
#' Any equal signs after the first occurrence is considered part of the value.
#'
#' A property name includes the characters A-Z case insensitive, digits 0-9 and
#' punctuation period and underscore.
#'
#' The last occurrence of a property definition is used. Prior definitions
#' are ignored.
#'
#' A line that starts with a hash (`#`) or exclamation (`!`) is assumed a
#' comment and is ignored.
#'
#' Trailing and leading white space is ignored.
#'
#' @examples
#'
#' # not run
#' result <- cxlib_propertiesread( "/my/path/to/my.properties" )
#'
#' @export


cxlib_propertiesread <- function( x ) {
  
  # - initialize return
  rtrn <- character(0)
  
  
  # - futility ... nothing here to process
  if ( missing(x) || is.null(x) || all(is.na(x)) || ! any(file.exists(x)) )
    return(rtrn)
  
  
  # - identify the properties file to process, i.e. the first one that exists
  xpath <- utils::head( x[ file.exists(x) ], n = 1 )
  
  
  # - read property file
  xraw <- try( base::readLines( con = xpath ) )
  
  if ( inherits( xraw, "try-error") )
    return( rtrn )
  
  
  # - identify property lines to read
  # - this would ignore # and ! comments, whitespace and any non-printable
  xlines <- xraw[ grepl( "^[a-z0-9\\._].*", trimws(xraw), ignore.case = TRUE, perl = TRUE ) ]
  
  
  for ( xl in xlines ) {
    
    idx <- utils::head( unlist(gregexpr( "[=:]", xl, perl = TRUE)), n  = 1 )
    
    # - futility ... need at least one character before the key/value delimiter
    if ( idx < 2 )
      next()
    
    # - property name
    key <- tolower(trimws(base::substring(xl, 1, last = idx - 1 )))
    
    if ( ! grepl( "^[a-z0-9\\._]+$", key, perl = TRUE, ignore.case = TRUE ) )
      next()
    
    
    # - property value
    value <- trimws(base::substring( xl, idx + 1))
    
    rtrn[ key ] <- value
    
  }
  
  
  return(rtrn)
}