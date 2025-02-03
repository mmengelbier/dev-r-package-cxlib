#' Internal utility function to generate a unique reference iD of different types
#' 
#' @param type ID type
#' 
#' @return A character vector with the reference ID
#' 
#' @description
#' 
#' Type equal to `uuid` returns a secure random UUID
#' 
#' Type equal to `sha` or `raw` returns the SHA-1 of the UUID
#' 
#' Type equal to `NULL` returns formatted SHA-1 of the UUID
#' 
#' 
#' @keywords internal

.cxlib_referenceid <- function( type = NULL ) {
  
  # -- generate ID
  refid <- uuid::UUIDgenerate()
  
  
  if ( "uuid" %in% type )
    return(refid)

  
    
  # -- generate a SHA-1 of reference id
  sha <- digest::digest( refid, algo = "sha1", file = FALSE )

  if ( any( c( "sha", "raw" ) %in% type ) )
    return(sha)
  

  # -- formatted string  
  if ( base::nchar(sha) != 40 )
    stop( "SHA-1 has unexpected length" )

  str <- character(0)
  
  for ( i in 1:(nchar(sha)/8) ) 
    str <- append( str, base::substr( sha, 8*i - 7, 8*i ) )  
  
  return(paste( str, collapse = "-") )
  
}




