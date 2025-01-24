#' Internal utility function to execute a R program in batch
#' 
#' 
#' @param x Program file
#' @param work.area Working area for program execution
#' 
#' @return A list of the execution results
#' 
#' 
#' @keywords internal


.cxlib_programexec <- function( x, work.area = NULL ) {
  
  
  
  # -- check for work area
  
  # default work area is current working directory
  work_area <- base::getwd()
  
  if ( ! is.null(work.area) ) 
    work_area <- work.area
  
   
  if ( any(is.na(work_area)) || ! dir.exists(work_area) )
    stop( "The work area ", work_area, " does not exist" )
  
  
  
  
  
  # -- check for program
  
  if ( missing(x) || is.null(x) || any(is.na(x)) || (length(x) != 1) || (class(x) != "character") )
    stop( "The program is not specified or as specified invalid" )
  
  if ( ! file.exists( file.path( work_area, x ) ) )
    stop( "The program ", x , " does not exist in the work area" )
  
  
  
  
  # -- standard result 
  
  exec_result <- list( "program" = character(0),
                       "log" = character(0),
                       "audit" = list( "inputs" = list(),
                                       "created" = list(),
                                       "updated" = list(),
                                       "deleted" = list() )
                    )
 
  

  # -- spec program 
  
  exec_result[["program"]] <- c( "path" = cxlib:::.cxlib_standardpath( x ), 
                                 "sha1" = digest::digest( cxlib:::.cxlib_standardpath( file.path( work_area, x) ), algo = "sha1", file = TRUE ) )
  

  # -- spec log
  
  exec_result[["log"]] <- c( "path" = paste0( tools::file_path_sans_ext( x ), ".Rout" ),
                             "sha1" = NA )
  
  

  # -- pre-inventory
  
  pre_inv <- sapply( list.files( work_area, recursive = TRUE, include.dirs = FALSE, full.names = FALSE ), function( z ) {
    digest::digest( file.path( work_area, z), algo = "sha1", file = TRUE )
  }, USE.NAMES = TRUE )
  
  
    
  
  # -- batch execute program
  
  #    - initiate batch cmd sequence
  batch_cmd <- character(0)
  
  
  #    - add redirect to internal working directory
  batch_cmd <- paste( "cd", work_area )
  
  
  #    - build R command  
  batch_cmd_r <- character(0)
  
  if ( ( .Platform$OS.type == "unix" ) && file.exists( file.path( R.home("bin"), "R") ) )
    batch_cmd_r <- file.path( R.home("bin"), "R")
  
  if ( ( .Platform$OS.type == "windows" ) && file.exists( file.path( R.home("bin"), "R.exe") ) )
    batch_cmd_r <- file.path( R.home("bin"), "R.exe")
  
  if ( ( length(batch_cmd_r) != 1 ) || ! file.exists( batch_cmd_r) )
    stop( "Cannot identify R executable" )
  
  
  #    - add batch instruction
  batch_cmd_r <- append( batch_cmd_r, "CMD BATCH")
  
  #    - add program 
  batch_cmd_r <- append( batch_cmd_r, exec_result[["program"]]["path"] )
  
  #    - add log 
  batch_cmd_r <- append( batch_cmd_r, exec_result[["log"]]["path"] )
  
  #    - add std error redirect
  if ( .Platform$OS.type == "unix" ) 
    batch_cmd_r <- append( batch_cmd_r, "2>&1" )
  
  #    append R cmd to command sequence
  batch_cmd <- append( batch_cmd, paste( batch_cmd_r, collapse = " ") )
  
  
  #    - run program 
  cmd <- paste( batch_cmd, collapse = " ; ")
  
  rc <- base::suppressWarnings( try( system( cmd, intern = TRUE, wait = TRUE ), silent = TRUE ) )  

  if ( inherits( rc, "try-error" ) || ! file.exists( file.path( work_area, exec_result[["log"]]["path"] ) ) ) {
    
    cat( c( "-----------------", "Command", cmd, "-----------------"), sep = "\n" )
    
    stop( "Executing program ", x, " failed" )
  }
  
  
  # -- add log reference to results  
  exec_result[["log"]]["sha1"] <- digest::digest( file.path( work_area, exec_result[["log"]]["path"] ), algo = "sha1", file = TRUE )

  

  # -- post-inventory
  
  post_inv <- sapply( list.files( work_area, recursive = TRUE, include.dirs = FALSE, full.names = FALSE ), function( z ) {
    digest::digest( file.path( work_area, z), algo = "sha1", file = TRUE )
  }, USE.NAMES = TRUE )
  
  
  
  # -- audit details
  
  # - inputs
  exec_result[["audit"]][["inputs"]] <- lapply( sort(names(pre_inv)), function(z) {
    c( "path" = z, "sha1" = unname(pre_inv[z]) )
  } )
  
  
  # - created
  created <- post_inv[ ! names(post_inv) %in% names(pre_inv) ]
  
  exec_result[["audit"]][["created"]] <- lapply( sort(names(created)), function(z) {
    c( "path" = z, "sha1" = unname(created[z]) )
  } )
  
  
  # - updated
  updated_files <- unlist( lapply( base::intersect( names(pre_inv), names(post_inv) ), function(z) {
    
    if ( post_inv[ z ] != pre_inv[ z ] ) 
      return(z)
    else
      return(NULL)
    
  }), use.names = FALSE )  
  
  
  exec_result[["audit"]][["updated"]] <- lapply( sort(updated_files), function(z) {
    c( "path" = z, "sha1" = unname(post_inv[z]) )
  })
  
  
  # - deleted
  deleted <- pre_inv[ ! names(pre_inv) %in% names(post_inv) ]
  
  exec_result[["audit"]][["deleted"]] <- lapply( sort(names(deleted)), function(z) {
    c( "path" = z, "sha1" = unname(deleted[z]) )
  } )
  
  
  
  
  return(invisible( exec_result ))
}