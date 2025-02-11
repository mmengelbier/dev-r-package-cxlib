#' Internal utility function to execute a R program in batch
#' 
#' 
#' @param x Program file
#' @param job.id Jon ID reference
#' @param options Execution options
#' 
#' @return A list of the execution results
#' 
#' @description
#' 
#' `options` is a named list of options when executing the program.
#' 
#' Option `work.area` specifies the work area to use when executing the program.
#' If not specified, the current working directory is used.
#' 
#' Option `log` specifies an alternative log file. If the `log` option is not 
#' specified, the log file name is derived from the program file name with 
#' suffix `.Rout`and is saved to the program parent directory. 
#' 
#' 
#' @keywords internal


.cxlib_programexec <- function( x, job.id = NULL, options = NULL ) {
  
  
  
  # -- check for work area
  
  # default work area is current working directory
  work_area <- base::getwd()
  
  if ( ! is.null(options) && "work.area" %in% names(options) && ! is.null(options[["work.area"]]) ) 
    work_area <- options[["work.area"]]

  if ( any(is.na(work_area)) || ! dir.exists(work_area) )
    stop( "The work area ", work_area, " does not exist" )
  
  
  
  
  
  # -- check for program
  
  if ( missing(x) || is.null(x) || any(is.na(x)) || (length(x) != 1) || ! inherits( x, "character") )
    stop( "The program is not specified or as specified invalid" )
  
  if ( ! file.exists( file.path( work_area, x, fsep = "/" ) ) )
    stop( "The program ", x , " does not exist in the work area" )
  
  
  
  
  # -- standard result 
  
  exec_result <- list( "id" = cxlib:::.cxlib_referenceid( type = NULL ),
                       "job.id" = cxlib:::.cxlib_referenceid( type = "uuid" ),
                       "program" = character(0),
                       "log" = character(0),
                       "files.input" = list(),
                       "files.created" = list(),
                       "files.updated" = list(),
                       "files.deleted" = list(), 
                       "start.time" = NA,
                       "end.time" = NA
                    )
 
  
  # -- spec job reference
  if ( ! is.null( job.id ) && ! any(is.na(job.id)) && inherits( job.id, "character") )
    exec_result[["job.id"]] <- job.id
  
  

  # -- spec program 
  
  exec_result[["program"]] <- c( "path" = cxlib:::.cxlib_standardpath( x ), 
                                 "sha1" = digest::digest( cxlib:::.cxlib_standardpath( file.path( work_area, x, fsep = "/") ), algo = "sha1", file = TRUE ) )
  

  # -- spec log
  
  exec_result[["log"]] <- c( "path" = paste0( tools::file_path_sans_ext( exec_result[["program"]]["path"] ), ".Rout" ),
                             "sha1" = NA )

  
  if ( ! is.null(options) && "log" %in% names(options) && ! is.null(options[["log"]]) && ! any(is.na(options[["log"]])) ) 
    exec_result[["log"]]["path"] <- options[["log"]]
  
  if ( ! dir.exists( base::dirname(file.path( work_area, exec_result[["log"]]["path"], fsep = "/" )) ) )
    stop( "Parent directory to log does not exist")
  
  

  # -- pre-inventory
  
  pre_inv <- sapply( list.files( work_area, recursive = TRUE, include.dirs = FALSE, full.names = FALSE ), function( z ) {
    digest::digest( file.path( work_area, z, fsep = "/"), algo = "sha1", file = TRUE )
  }, USE.NAMES = TRUE )
  
  
    
  
  # -- batch execute program
  
  batch_args <- character(0)
  
  # - add R workspace flags
  batch_args <- append( batch_args, c( "--no-restore --no-save" ) )
  
  # - add program 
  batch_args <- append( batch_args, exec_result[["program"]]["path"] )

  # - add log 
  batch_args <- append( batch_args, exec_result[["log"]]["path"] )
  

  # - run program
  
  exec_result[["start.time"]] <- format( Sys.time(), "%Y-%d-%m %H:%M:%S %Z" )
  
  rc <- try( callr::rcmd( "BATCH", cmdargs = batch_args, wd = work_area, echo = FALSE, show = FALSE, spinner = FALSE ), silent = TRUE )

  exec_result[["end.time"]] <- format( Sys.time(), "%Y-%d-%m %H:%M:%S %Z" )

  
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
  
  
  
  # -- log preamble
  
  log_preamble <- paste(rep_len("-", 65), collapse = "")
  
  # identify execution
  log_preamble <- append( log_preamble, 
                          c( exec_result[["id"]],
                             " ",
                             paste( "Job ID    :", exec_result[["job.id"]] ),
                             paste( "Started   :", exec_result[["start.time"]] ),
                             paste( "Completed :", exec_result[["end.time"]] ),
                             base::rep_len( " ", 2 ) ) )
  
  # identify input files
  log_preamble <- append( log_preamble,
                          c( "Input files", 
                             paste(rep_len("-", 35), collapse = "") ) )
  

  for ( xfile in sort(names(pre_inv)) )
    log_preamble <- append( log_preamble, 
                            c( xfile, 
                               paste0( "(SHA-1: ", pre_inv[xfile], ")" ), 
                               " ") )
  

  log_preamble <- append( log_preamble, paste(rep_len("-", 65), collapse = "") )
  
  # -- end of log preamble
  

  
  # -- log results
  
  log_result <- c( paste(rep_len("-", 65), collapse = ""), 
                   "Program execution results" ) 
  
  # identify created files
  log_result <- append( log_result,
                        c( base::rep_len( " ", 2 ), 
                           "Created files", 
                           paste(rep_len("-", 35), collapse = "") ) )
  
  if ( length(created) == 0 )
    log_result <- append( log_result, "None" ) 

  for ( xfile in sort(names(created)) ) {
    
    # add file path reference
    log_result <- append( log_result, xfile )
    
    
    # add SHA-1 reference
    
    xfile_sha1 <- paste0( "(SHA-1: ", created[xfile], ")" )
    
    if ( xfile == exec_result[["log"]]["path"] )
      xfile_sha1 <- "(This log file)"
      
    log_result <- append( log_result, c( xfile_sha1, " " ) )

  }
  
  
  # identify updated files
  log_result <- append( log_result,
                        c( base::rep_len( " ", 2 ), 
                           "Updated files", 
                           paste(rep_len("-", 35), collapse = "") ) )
  
  if ( length(updated_files) == 0 )
    log_result <- append( log_result, "None" )
  
  
  for ( xfile in sort(names(updated_files)) ) {
    
    # add file path reference
    log_result <- append( log_result, xfile )
    
    
    # add SHA-1 reference
    
    xfile_sha1 <- paste0( "(SHA-1: ", updated_files[xfile], ")" )
    
    if ( xfile == exec_result[["log"]]["path"] )
      xfile_sha1 <- "(This log file)"
    
    log_result <- append( log_result, c( xfile_sha1, " " ) )

  }


  
  # identify deleted files
  log_result <- append( log_result,
                        c( base::rep_len( " ", 2 ), 
                           "Deleted files", 
                           paste(rep_len("-", 35), collapse = "") ) )
  
  if ( length(deleted) == 0 )
    log_result <- append( log_result, "None" )
  
  for ( xfile in sort(names(deleted)) )
    log_result <- append( log_result, 
                          c( xfile, 
                             paste0( "(SHA-1: ", deleted[xfile], ")" ), 
                             " ") )

  log_result <- append( log_result, c( " ", paste(rep_len("-", 65), collapse = "") ) )
  
  # -- end of log results
  
  
  # -- update log

  # read in the current log
  log_lines <- base::readLines( con = file.path( work_area, exec_result[["log"]]["path"], fsep = "/") , warn = FALSE )

  
  # add preamble and results
  updated_log_lines <- c( paste( "#>", log_preamble, sep = "  "),
                          base::rep_len( " ", 3 ),
                          log_lines, 
                          base::rep_len( " ", 3 ),
                          paste( "#>", log_result, sep = "  " ) )
  
  
  # re-write log and update SHA-1 for log in associated entries
  
  base::writeLines( updated_log_lines, con = file.path( work_area, exec_result[["log"]]["path"], fsep = "/") )  
  log_sha1 <- digest::digest( file.path( work_area, exec_result[["log"]]["path"], fsep = "/" ), algo = "sha1", file = TRUE )
  
  exec_result[["log"]]["sha1"] <- log_sha1
  

  for ( xentry in c( "files.created", "files.updated" ) )
    if ( length(exec_result[[ xentry ]] ) > 0 )
      for ( xi in 1:length(exec_result[[ xentry ]] ) ) 
        if ( exec_result[[ xentry ]][[ xi ]]["path"] == exec_result[["log"]]["path"] ) {
          exec_result[[ xentry ]][[ xi ]]["sha1"] <- log_sha1
          break()
        }

  
  return(invisible( exec_result ))
}