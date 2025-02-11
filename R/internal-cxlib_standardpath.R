#' Simple utility to standard path reference 
#' 
#' @param x A vector of paths
#' @param normalize Normalize path
#' 
#' @return A vector of paths in standard reference format
#' 
#' @description
#' A standard reference uses single forward slashes as directory and file 
#' separator.
#' 
#' @keywords internal

.cxlib_standardpath <- function(x, normalize = FALSE ) {


  xpath <- x
  
  if ( normalize )
    xpath <- base::normalizePath( xpath )
  
  rtrn <- character(0)
  
  for ( xitem in xpath ) 
    rtrn <- append( rtrn, gsub( "/{2,}", "/", gsub( "\\\\", "/", xitem ) ) )
  
  
  return(rtrn)

}
