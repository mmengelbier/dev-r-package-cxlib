#' Internal utility to publish results of a job to local file system
#' 
#' @param x Job results
#' 
#' @return Invisible Job results
#' 
#' @keywords internal


.cxlib_batch_localfs_publish <- function( x ) {

  
  if ( missing(x) || is.null(x) || ! inherits(x, "list") )
    stop( "Invalid job results" )
  
  if ( length(x) == 0 ) 
    stop( "Empty job results unexpected" )
  
  
  if ( ! "mode" %in% names(x) || x[["mode"]] != "localfs" )
    stop( "Invalid mode" )
  
  
  # -- initiate return
  jresults <- list( "mode" = "localfs", 
                    "working.directory" = character(0),
                    "work.area" = character(0),
                    "output.locations" = character(0),
                    "actions" = list(), 
                    "inputs" = list(),
                    "outputs" = list(),
                    "deleted" = list())
  
  
  
  # -- working directory
  
  if ( ! "working.directory" %in% names(x) || 
       is.null(x[["working.directory"]]) ||
       any(is.na(x[["working.directory"]])) ||
       ! inherits( x[["working.directory"]], "character" ) ||
       ( length(x[["working.directory"]]) != 1 ) || 
       ! dir.exists( x[["working.directory"]] ) )
    stop( "Working directory in job results is missing or invalid" )
  
  jresults[["working.directory"]] <- cxlib:::.cxlib_standardpath( x[["working.directory"]] )
  
  
  
  # -- work area
  
  if ( ! "work.area" %in% names(x) || 
       is.null(x[["work.area"]]) ||
       any(is.na(x[["work.area"]])) || 
       ! inherits( x[["work.area"]], "character" ) ||
       ( length(x[["work.area"]]) != 1 ) || 
       ! dir.exists( x[["work.area"]] ) )
    stop( "Work area in job results is missing or invalid" )
  
  jresults[["work.area"]] <- cxlib:::.cxlib_standardpath( x[["work.area"]] )
  
  

  # -- pass-through  
  
  for ( xitem in c( "actions", "inputs" ) )
    if ( xitem %in% names(x) && ! is.null(x[[xitem]]) && ! any(is.na(x[[xitem]])) && inherits( x[[xitem]], "list" ) )
      jresults[xitem] <- x[xitem]
  
  if ( "output.locations" %in% names(x) && ! is.null(x[["output.locations"]]) && ! any(is.na(x[["output.locations"]])) && inherits( x[["output.locations"]], "character" ) )
    jresults["output.locations"] <- x["output.locations"]
  

  # -- initiate list of files to publish and delete
  #    note: publish after delete means publish
  lst_files_logs <- character(0)
  lst_files_publish <- character(0)
  lst_files_delete <- character(0)
  

  # -- actions and action integrity checks 
  #    note: using jresults as a clean list of actions

  if ( "actions" %in% names(jresults) )
    for ( xact in jresults[["actions"]] ) {
      # note: using if-blocks for multiple integrity checks on the action record
      
      # - program
      
      if ( all( c( "type", "path", "sha1") %in% names(xact) ) && ( "program" %in% xact[["type"]]) ) {
        
        # note: integrity is all equal
        sha1_hashes <- c( digest::digest( file.path( jresults[["working.directory"]], xact[["path"]], fsep = "/"), algo = "sha1", file = TRUE ), 
                          digest::digest( file.path( jresults[["work.area"]], xact[["path"]], fsep = "/"), algo = "sha1", file = TRUE ),
                          xact[["sha1"]] )
        
        
        if ( length( base::unique( sha1_hashes )) != 1 )
          stop( "Inegrity check fail for program ", xact[["path"]], ". The program has changed during job execution." )
        
      }  # end of program integrity check
      
      
      # - log
      
      # note: only valid for program actions
      # note: only valid if the log exists in the working directory (our source for programs and inputs)
      
      if ( "type" %in% names(xact) && "program" %in% xact[["type"]] &&
           "log" %in% names(xact) && "path" %in% names(xact[["log"]]) &&
           file.exists( file.path( jresults[["work.area"]], xact[["log"]]["path"], fsep = "/" ) ) ) {
        
        # integrity check
        # note: we expect reference.sa1 not equal to NA if log file exists (reference.sha1 is SHA-1 for file when program is staged)
        # note: if reference.sha1 is not NA then compare refrerence.sha1 to log SHA-1 in working directory
        # note: if log file exists in working directory, refernece.sha1 is NA or do not match SHA-1 then we assume the log has been created/updated
        # note: log created/updated surrogate for program being executed
        
        if ( file.exists( file.path( jresults[["working.directory"]], xact[["log"]]["path"], fsep = "/" ) ) &&
             "reference.sha1" %in% names( xact[["log"]] ) &&
             ( any(is.na( xact[["log"]]["reference.sha1"] )) || 
               ( digest::digest( file.path( jresults[["working.directory"]], xact[["log"]]["path"], fsep = "/" ), algo = "sha1", file = TRUE ) != xact[["log"]]["reference.sha1"] ) )
        ) 
          stop( "Inegrity check fail for program ", xact[["path"]], ". The program log has changed during job execution." )
        
        # register log 
        lst_files_logs <- unique( append( lst_files_logs, unname(xact[["log"]]["path"]) ) )

        # include log even if not within outputs
        lst_files_publish[ unname(xact[["log"]]["path"]) ] <- NA
        
        if ( "sha1" %in% names(xact[["log"]]) ) 
          lst_files_publish[ unname(xact[["log"]]["path"]) ] <- unname(xact[["log"]]["sha1"])
        
                
      }  # end of if-statement on log in action
      
      

      # - files from action

      #  note: incrementally build list of files to publish and files to delete
      #  note: time to keep track of file changes across actions
      
      
      if ( "files.deleted"  %in% names(xact) ) 
        for ( xitem in xact[["files.deleted"]] ) {
         
          lst_files_delete[ unname(xitem["path"]) ] <- unname(xitem["sha1"])
          
          lst_files_publish <- lst_files_publish[ ! names(lst_files_publish) %in% names(lst_files_delete) ]
          
        } # end of for-statement for files deleted
        

      for ( xfile_action in c( "files.created", "files.updated" ) )  
        if ( xfile_action %in% names(xact) )
          for ( xitem in xact[[ xfile_action ]] ) {

            # add file to publish list
            lst_files_publish[ unname(xitem["path"]) ] <- unname(xitem["sha1"])
            
            # remove file from delete list if it is there
            # note: rather use full filter
            lst_files_delete <- lst_files_delete[ ! names(lst_files_delete) %in% names(lst_files_publish) ]
  
          } # end of for-statement for files created and updated 
      
  
    } # end of for-statement on actions




  # -- integrity check on output files
  for ( xout in names(lst_files_publish) )
    if ( digest::digest( file.path( jresults[["work.area"]], xout, fsep = "/" ), algo = "sha1", file = TRUE ) != unname(lst_files_publish[xout]) )
      stop( "Integrity failure on output ", xout, ". No record of file update." )
  

  
  # -- time to finally delete files 
  for ( xfile in sort(names(lst_files_delete)) ) {
    
    # disregard file if not in a known output location
    if ( ! base::dirname( xfile ) %in% jresults[["output.locations"]] )
      next()

        
    xpath <- file.path( jresults[["working.directory"]], xfile, fsep = "/" ) 
    
    # delete file
    if ( ! file.exists( xpath ) ) 
      next()

          
    # delete
    base::unlink( xpath, recursive = FALSE )
    
    # verify delete
    if ( file.exists( xpath ) ) {
      message( "Unable to delete file ", xpath)
      next()
    }

        
    # add record of deleted file 
    jresults[["deleted"]][[ length(jresults[["deleted"]]) + 1 ]] <- c( "path" = xfile,
                                                                       "sha1" = unname(lst_files_delete[xfile]) )
    
  }
    

  
  # -- time to finally publish files
  for ( xfile in sort(names(lst_files_publish)) ) {

    # disregard file if not in a known output location
    if ( ! base::dirname( xfile ) %in% jresults[["output.locations"]] && 
         ! xfile %in% lst_files_logs  )
      next()
    
    
    xpath_src <- file.path( jresults[["work.area"]], xfile, fsep = "/" )
    xpath_target <- file.path( jresults[["working.directory"]], xfile, fsep = "/" ) 

    # copy file
    if ( ! file.copy( xpath_src, base::dirname(xpath_target), recursive = FALSE, overwrite = TRUE, copy.mode = FALSE, copy.date = TRUE ) ||
         ( digest::digest( xpath_target, algo = "sha1", file = TRUE ) != lst_files_publish[xfile] ) ) {
      
      message( "Unable to save file as ", xpath_target )
      
      next()
    }


    # add record of deleted file
    jresults[["outputs"]][[ length(jresults[["outputs"]]) + 1 ]] <- c( "path" = xfile,
                                                                       "sha1" = unname(lst_files_publish[xfile]) )

  }

  
  
  return(invisible(jresults))
}