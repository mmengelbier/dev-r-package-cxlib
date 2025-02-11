#' Retrieve annotations from a given program
#' 
#' @param x A program path
#' 
#' @return A named list of annotations
#' 
#' @description
#' An annotation is a special comment that follows the annotation convention.
#' 
#' One annotation per line. Wrapping an annotation across several lines is not
#' supported.
#' 
#' Annotation follows the convention of first non-space character on a line is 
#' a `#` followed by an `@` and the annotation keyword, e.g. `# @cx.input ...`. 
#' 
#' Note: Only annotations with the keyword prefix `cx.<word>` are processed and
#' the return drops the `cx.` prefix in annotation names. 
#' 
#' There is at least one white space character between the annotation keyword 
#' and value. The value is terminated by end of line character. Leading and 
#' trailing white space is removed.
#' 
#' @keywords internal


.cxlib_programannotations <- function( x ) {
  
  annotations <- list()
  
  if ( missing(x) || is.null(x) || any(is.na(x)) || (length(x) != 1) || ! inherits( x, "character") )
    stop( "Missing or invalid program file reference" )
  
  if ( ! file.exists(x) )
    stop( "Program file ", x, " does not exist")
  
  
  # -- read program
  pgm <- try( readLines( x, warn = FALSE ) )
  
  if ( inherits( pgm, "try-error" ) )
    stop( "Could not read program file", x )
  
  
  # -- identify lines with annotations
  #    note: annotations start with prefix cx.* to differentiate from "other" annotations
  pgm_annolines <- pgm[ grepl( "^#\\s*@cx\\.[a-z0-9]+\\s+.*", trimws(pgm), perl = TRUE, ignore.case = TRUE ) ]
  
  
  for ( xline in pgm_annolines ) {
    
    xanno <- base::tolower( gsub( "^#\\s*@cx\\.([a-z0-9]+)\\s.*", "\\1", base::trimws(xline), perl = TRUE, ignore.case = TRUE ) )
    xvalue <- gsub( paste0( "^#\\s*@cx\\.[a-z0-9]+\\s(.*)"), "\\1", trimws(xline), perl = TRUE, ignore.case = TRUE ) 
    
    if ( ! xanno %in% names(annotations) )
      annotations[[ xanno ]] <- character(0)
    
    annotations[[ xanno ]] <- append( annotations[[ xanno ]], as.character(xvalue) )
    
  }


  return(invisible(annotations))
}