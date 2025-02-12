#' Simple utility to execute an R program script
#' 
#' @param x A job
#' @param options List of options for executing R programs
#' @param silent Disable messaging
#' 
#' @return Named list of execution results
#' 
#' @description
#' 
#' A job is a list of tasks to be executed. In its simplest form, it one or more 
#' programs that executes in batch mode. 
#' 
#' A job is executed in an isolated, temporary and transient working area.
#' 
#' Note: All paths are relative to the current R session working directory, i.e.
#' `getwd()`.
#'  
#' Program required inputs and output locations are annotated in the program file
#' using standard annotation syntax anywhere in the program file. Each annotation 
#' is defined on a separate line starting with the comment character `#`. One or 
#' more spaces separate the annotation keyword and its value.
#' 
#' Program input is specified using `@cx.input` followed by the input directory or
#' file path. If the input is a directory, all files in that directory is 
#' included. 
#' 
#' Program output locations is specified using the `@cx.output` annotation. It is 
#' assumed that the output location is a directory.
#' 
#' Only files created, updated or deleted in the output directory are returned 
#' in addition to the program log.
#' 
#' The function supports different execution options.
#' 
#' The `logs` option is the directory where logs are stored. If the option is 
#' `NULL`, the program directory is used. 
#' 
#' The `log.fileext` specifies te log file extension or suffix. Default is the
#' standard `Rout`. 
#' 
#' 
#' @export


cxlib_batch <- function( x, options = list( "logs" = NULL, "log.fileext" = "Rout" ), silent = FALSE ) {

  # -- not yet implemented
  trace <- FALSE
  
  
  # -- generate job ID
  #    note: job ID is in the UUID format
  exec_record <- list( "job.id" = cxlib:::.cxlib_referenceid( type = "uuid" ) )
  


  # -- identify execution mode
  
  # currently only supported
  exec_mode <- "locafs"
  
  
  
  
  # -- stage 
  #    note: initializes execution results with execution definition
  
  exec_job <- cxlib:::.cxlib_batch_localfs_stage( x, options = options )


  if ( ! "work.area" %in% names(exec_job) )
    stop( "Work area is not defined in job execution definition" )
    
  
  on.exit({
    base::unlink( exec_job[["work.area"]], recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  
  # -- execute
  
  for ( xi in 1:length(exec_job[["actions"]]) ) {
    
    if ( trace ) 
      cat ( c( " ", 
               paste( rep_len("-", 60), collapse = "" ),
               paste0( "Action #", as.character(xi)),
               " "),
            sep = "\n" )
      


    if ( ! "type" %in% names(exec_job[["actions"]][[xi]]) )
      next()
    
    if ( exec_job[["actions"]][[xi]][["type"]] == "program" ) {
    
      # ensure program file exists
      if ( ! "path" %in% names(exec_job[["actions"]][[xi]]) || ! file.exists( file.path( exec_job[["work.area"]], exec_job[["actions"]][[xi]][["path"]], fsep = "/" ) ) )
        next()
      
      if ( trace ) 
        cat( paste( "Program", exec_job[["actions"]][[xi]][["path"]] ), sep = "\n" )
      
      
      # program integrity check
      if ( "sha1" %in% names(exec_job[["actions"]][[xi]]) && 
           ( digest::digest( file.path( exec_job[["work.area"]], exec_job[["actions"]][[xi]][["path"]], fsep = "/" ), algo = "sha1", file = TRUE ) != exec_job[["actions"]][[xi]][["sha1"]] ) )
        stop( "Program integrity check failed" )
      

      # - program options

      # init with work area      
      pgm_opt <- list( "work.area" = exec_job[["work.area"]] )
      
      # add log
      if ( "log" %in% names(exec_job[["actions"]][[xi]]) && "path" %in% names(exec_job[["actions"]][[xi]][["log"]]) )
        pgm_opt[["log"]] <- exec_job[["actions"]][[xi]][["log"]]["path"]
      
      
      # - execute the program 
      exec_jresult <- cxlib:::.cxlib_programexec( exec_job[["actions"]][[xi]][["path"]], job.id = unname(exec_record[["job.id"]]), options = pgm_opt )
      
      
      # - process results
      
      # log
      if ( file.exists( file.path( exec_job[["work.area"]], exec_job[["actions"]][[xi]][["log"]]["path"], fsep = "/" ) ) )
        exec_job[["actions"]][[xi]][["log"]]["sha1"] <- exec_jresult[["log"]]["sha1"]
      
      # copy results to the action 
      
      for ( xentry in c( "id", "files.input", "files.created", "files.updated", "files.deleted", "start.time",  "end.time" ) )
        exec_job[["actions"]][[xi]][ xentry ] <- exec_jresult[ xentry ]
      

    } # end of if-statement for type program
    

  } # end of for-statement across actions

  
  # -- publish
  exec_publish <- cxlib:::.cxlib_batch_localfs_publish( exec_job )

  # add to execution record
  exec_record <- append( exec_record, exec_publish )  
  

  # -- non-silent report
  
  if ( ! silent ) {
    
    # - add header
    notes <- c( " ", 
                paste( rep_len("-", 60), collapse = "" ) )
    
    # - add job
    notes <- append( notes, 
                     paste( "Job", exec_record[["job.id"]], sep = "  " ) )
    
    
    # - add inputs
    notes <- append( notes, 
                     c( " ", 
                        "Input files",
                        paste( rep_len("-", 40), collapse = "" )))
    
    if ( length( exec_record[["inputs"]]) == 0 )
      notes <- append(notes, "None")
    
    
    if ( length( exec_record[["inputs"]]) > 0 )
      for ( xinput in exec_record[["inputs"]] )
        notes <- append( notes, 
                         c( xinput["path"], 
                            paste0( "(SHA-1: ", xinput["sha1"], ")" ), 
                            " ") )
    
    
    # - add actions
    notes <- append( notes, 
                     c( " ", 
                        "Actions",
                        paste( rep_len("-", 40), collapse = "" )))
    
    
    if ( length( exec_record[["actions"]]) == 0 )
      notes <- append(notes, "No actions on record")
    
    
    if ( length( exec_record[["actions"]]) > 0 )
      for ( xi in 1:length(exec_record[["actions"]]) ) {

        if ( "program" %in% exec_record[["actions"]][[xi]][["type"]] ) {
          
          str_label <- paste0( "#", as.character(xi), " Executed program")
          
          notes <- append( notes, 
                           c( " ",
                              str_label,
                              paste( rep_len("-", base::nchar(str_label) + 2 ), collapse = "" ),
                              exec_record[["actions"]][[xi]][["path"]],
                              paste0( "(SHA-1: ", exec_record[["actions"]][[xi]]["sha1"], ")" ),
                              " ",
                              paste( "Log", exec_record[["actions"]][[xi]][["log"]]["path"] ),
                              paste0( "(SHA-1: ", exec_record[["actions"]][[xi]][["log"]]["sha1"], ")" ), 
                              " ",
                              paste( "Started   ", exec_record[["actions"]][[xi]]["start.time"] ),
                              paste( "Completed ", exec_record[["actions"]][[xi]]["end.time"] ),
                              " "
                             ))
          

        }  # end of if-statement for type program
        
      } # end of for-statement on exec_record actions element
    
    
    
    
    # - add outputs
    notes <- append( notes, 
                     c( " ", 
                        "Output files",
                        paste( rep_len("-", 40), collapse = "" )))
    
    if ( length( exec_record[["outputs"]]) == 0 )
      notes <- append(notes, "None")
    
    
    if ( length( exec_record[["outputs"]]) > 0 )
      for ( xoutput in exec_record[["outputs"]] )
        notes <- append( notes, 
                         c( xoutput["path"], 
                            paste0( "(SHA-1: ", xoutput["sha1"], ")" ), 
                            " ") )
    
    
    
    # - add deleted
    notes <- append( notes, 
                     c( " ", 
                        "Deleted files",
                        paste( rep_len("-", 40), collapse = "" )))
    
    if ( length( exec_record[["deleted"]]) == 0 )
      notes <- append(notes, "None")
    
    
    if ( length( exec_record[["deleted"]]) > 0 )
      for ( xdeleted in exec_record[["deleted"]] )
        notes <- append( notes, 
                         c( xdeleted["path"], 
                            paste0( "(SHA-1: ", xdeleted["sha1"], ")" ), 
                            " ") )
    
    
    
    # - add footer
    notes <- append( notes,
                     c( " ", 
                        paste( rep_len("-", 60), collapse = "" ),
                        " ") )
    
    cat( notes, sep = "\n" )
    
  }

  
  
    
  # -- return results
  # return(invisible(exec_publish))
  return(invisible(exec_record))
  
}
  
