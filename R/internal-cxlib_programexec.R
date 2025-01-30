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
  
  if ( ! file.exists( file.path( work_area, x, fsep = "/" ) ) )
    stop( "The program ", x , " does not exist in the work area" )
  
  
  
  
  # -- standard result 
  
  exec_result <- list( "program" = character(0),
                       "log" = character(0),
                       "files.input" = list(),
                       "files.created" = list(),
                       "files.updated" = list(),
                       "files.deleted" = list() 
                    )
 
  

  # -- spec program 
  
  exec_result[["program"]] <- c( "path" = cxlib:::.cxlib_standardpath( x ), 
                                 "sha1" = digest::digest( cxlib:::.cxlib_standardpath( file.path( work_area, x, fsep = "/") ), algo = "sha1", file = TRUE ) )
  

  # -- spec log
  
  exec_result[["log"]] <- c( "path" = paste0( tools::file_path_sans_ext( x ), ".Rout" ),
                             "sha1" = NA )
  
  

  # -- pre-inventory
  
  pre_inv <- sapply( list.files( work_area, recursive = TRUE, include.dirs = FALSE, full.names = FALSE ), function( z ) {
    digest::digest( file.path( work_area, z, fsep = "/"), algo = "sha1", file = TRUE )
  }, USE.NAMES = TRUE )
  
  
    
  
  # -- batch execute program
  
  batch_args <- character(0)
  
  # - add R workspace flags
  batch_args <- append( batch_args, c( "--no-restore --no-test" ) )
  
  # - add program 
  batch_args <- append( batch_args, exec_result[["program"]]["path"] )

  # - add log 
  batch_args <- append( batch_args, exec_result[["log"]]["path"] )
  
  # - run program
  rc <- try( callr::rcmd( "BATCH", cmdargs = batch_args, wd = work_area, echo = FALSE, show = FALSE, spinner = FALSE ), silent = TRUE )


  if ( inherits( rc, "try-error") || ( rc[["status"]] != 0 ) || ! file.exists( file.path( work_area, exec_result[["log"]]["path"], fsep = "/" ) ) )
    stop( "Executing program ", x, " failed" )

  
  # -- add log reference to results  
  exec_result[["log"]]["sha1"] <- digest::digest( file.path( work_area, exec_result[["log"]]["path"], fsep = "/" ), algo = "sha1", file = TRUE )

  

  # -- post-inventory
  
  post_inv <- sapply( list.files( work_area, recursive = TRUE, include.dirs = FALSE, full.names = FALSE ), function( z ) {
    digest::digest( file.path( work_area, z, fsep = "/"), algo = "sha1", file = TRUE )
  }, USE.NAMES = TRUE )
  
  
  
  # -- audit details
  
  # - inputs
  exec_result[["files.input"]] <- lapply( sort(names(pre_inv)), function(z) {
    c( "path" = z, "sha1" = unname(pre_inv[z]) )
  } )
  
  
  # - created
  created <- post_inv[ ! names(post_inv) %in% names(pre_inv) ]
  
  exec_result[["files.created"]] <- lapply( sort(names(created)), function(z) {
    c( "path" = z, "sha1" = unname(created[z]) )
  } )
  
  
  # - updated
  updated_files <- unlist( lapply( base::intersect( names(pre_inv), names(post_inv) ), function(z) {
    
    if ( post_inv[ z ] != pre_inv[ z ] ) 
      return(z)
    else
      return(NULL)
    
  }), use.names = FALSE )  
  
  
  exec_result[["files.updated"]] <- lapply( sort(updated_files), function(z) {
    c( "path" = z, "sha1" = unname(post_inv[z]) )
  })
  
  
  # - deleted
  deleted <- pre_inv[ ! names(pre_inv) %in% names(post_inv) ]
  
  exec_result[["files.deleted"]] <- lapply( sort(names(deleted)), function(z) {
    c( "path" = z, "sha1" = unname(deleted[z]) )
  } )
  
  
  
  
  return(invisible( exec_result ))
}