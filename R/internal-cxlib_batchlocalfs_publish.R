#' Internal utility to publish results of a job to local file system
#' 
#' @param x Job results
#' 
#' @return Invisible NULL
#' 
#' @keywords internal


.cxlib_batch_localfs_publish <- function( x ) {

  
  if ( missing(x) || is.null(x) || ( class(x) != "list" ) )
    stop( "Invalid job results" )
  
  if ( length(x) == 0 ) 
    stop( "Empty job results unexpected" )
  
  
  if ( ! "mode" %in% names(x) || x[["mode"]] != "localfs" )
    stop( "Invalid mode" )
  
  
  # -- initiate return
  jresults <- list( "mode" = "localfs", 
                    "working.directory" = character(0),
                    "work.area" = character(0),
                    "actions" = list(), 
                    "inputs" = list(),
                    "outputs" = list() )
  
  
  
  # -- working directory
  
  if ( ! "working.directory" %in% names(x) || 
       is.null(x[["working.directory"]]) ||
       any(is.na(x[["working.directory"]])) ||
       ( class(x[["working.directory"]]) != "character" ) ||
       ( length(x[["working.directory"]]) != 1 ) || 
       ! dir.exists( x[["working.directory"]] ) )
    stop( "Working directory in job results is missing or invalid" )
  
  jresults[["working.directory"]] <- cxlib:::.cxlib_standardpath( x[["working.directory"]] )
  
  
  
  # -- work area
  
  if ( ! "work.area" %in% names(x) || 
       is.null(x[["work.area"]]) ||
       any(is.na(x[["work.area"]])) || 
       ( class(x[["work.area"]]) != "character" ) ||
       ( length(x[["work.area"]]) != 1 ) || 
       ! dir.exists( x[["work.area"]] ) )
    stop( "Work area in job results is missing or invalid" )
  
  jresults[["work.area"]] <- cxlib:::.cxlib_standardpath( x[["work.area"]] )
  
  
  
  # -- futility check for actions ... nothing to process 
  
  if ( ! "actions" %in% names(x) ||
       is.null(x[["actions"]]) ||
       any(is.na(x[["actions"]])) || 
       ( class(x[["actions"]]) != "list" ) ||
       (length(x[["actions"]]) == 0) )
    return(invisible(jresults))
  
  
  # -- inputs are passed through
  if ( "inputs" %in% names(x) )
    jresults[["inputs"]] <- x[["inputs"]]
    
    
  # -- action integrity checks
  
  for ( xact in x[["actions"]] ) {
    # note: using if-blocks for multiple integrity checks on the action record

    # - program
    
    if ( all( c( "type", "path", "sha1") %in% names(xact) ) && ( "program" %in% xact[["type"]]) ) {
      
      # note: integrity is all equal
      sha1_hashes <- c( digest::digest( file.path( jresults[["working.directory"]], xact[["path"]]), algo = "sha1", file = TRUE ), 
                        digest::digest( file.path( jresults[["work.area"]], xact[["path"]]), algo = "sha1", file = TRUE ),
                        xact[["sha1"]] )
      
      
      if ( length( base::unique( sha1_hashes )) != 1 )
        stop( "Inegrity check fail for program ", xact[["path"]], ". The program has changed during job execution." )
      
    }  # end of program integrity check
    
    
    # - log
    
    # note: only valid for program actions
    # note: only valid if the log exists in the working directory (our source for programs and inputs)
    # note: we expect reference.sa1 not equal to NA if log file exists (reference.sha1 is SHA-1 for file when program is staged)
    # note: if reference.sha1 is not NA then compare refrerence.sha1 to log SHA-1 in working directory
    # note: if log file exists in working directory, refernece.sha1 is NA or do not match SHA-1 then we assume the log has been created/updated
    # note: log created/updated surrogate for program being executed
    
    if ( "type" %in% names(xact) && "program" %in% xact[["type"]] &&
         "log" %in% names(xact) && all( c( "path", "reference.sha1" ) %in% names(xact[["log"]]) ) &&
         file.exists( file.path( jresults[["working.directory"]], xact[["log"]]["path"], fsep = "/" ) ) &&
         ( any(is.na( xact[["log"]]["reference.sha1"] )) || 
           ( digest::digest( file.path( jresults[["working.directory"]], xact[["log"]]["path"], fsep = "/" ), algo = "sha1", file = TRUE ) != xact[["log"]]["reference.sha1"] ) )
       ) 
      stop( "Inegrity check fail for program ", xact[["path"]], ". The program log has changed during job execution." )


  } # end of for-statement on actions
  
  
  
  
  # -- identify files to publish 
  
  lst_files <- character(0)
  
  if ( "outputs" %in% names(x) )
    for ( xout in x[["outputs"]] ) {
      
      xpath <- ifelse( ("path" %in% names(xout)), unlist(xout["path"], use.names = FALSE), xout )
      
      if ( ! dir.exists( file.path( jresults[["work.area"]], xpath, sep = "/" ) ) )
        next()

      # - futility ... make sure we have a valid destination
      if ( ! dir.exists( file.path( jresults[["working.directory"]], xpath, sep = "/" ) ) )
        stop( "The output directory ", xpath, " does not exist in the working directory" )
        
            
      xpath_files <- list.files( file.path( jresults[["work.area"]], xpath, sep = "/" ), full.names = FALSE, recursive = FALSE, include.dirs = FALSE )

      lst_files <- base::unique( append( lst_files, file.path( xpath, xpath_files, fsep = "/" ) ) )

    }
  
  
  output_files <- lapply( sort(lst_files), function(x) {
    c( "path" = x,
       "sha1" = digest::digest( file.path( jresults[["work.area"]], x, sep = "/" ), algo = "sha1", file = TRUE ) )
  })
  
  

  
  
  # -- time to finally publish 
  
  for ( xout in output_files ) {
    
    xsrc_path <-  file.path( jresults[["work.area"]],         xout["path"], sep = "/" )
    xtrgt_path <- file.path( jresults[["working.directory"]], xout["path"], sep = "/" ) 
    
    if ( file.exists( xsrc_path ) && 
         ! file.copy( xsrc_path, base::dirname( xtrgt_path ), overwrite = TRUE, recursive = FALSE, copy.mode = FALSE, copy.date = TRUE ) ) 
      stop( "Could not publish file ", xout["path"], " in the working directory" )
    
    
    if ( digest::digest( xtrgt_path, algo = "sha1", file = TRUE ) != unname(xout["sha1"]) )
      stop( "Unequal SHA-1 digests for published file ", xout["path"], " in the working directory" )
    
    
    # add to jresults
    jresults[["outputs"]][[ length(jresults[["outputs"]]) + 1 ]] <- xout

  }
  

  
  return(invisible(jresults))
}