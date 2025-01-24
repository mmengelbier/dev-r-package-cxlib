#' Simple utility to execute an R program script
#' 
#' @param x A job
#' @param Rout Execution log file
#' @param user.libary Enable user library
#' @param overwrite Enable overwriting existing output
#' @param overwrite.input Enable overwriting inputs
#' @param libs (Experimental) A vector of library paths
#' @param slient (Not implemented) Disable messaging
#' 
#' @return Named list of execution references
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
#' #' 
#' Program required inputs and output locations are annotated in the program file
#' using standard annotation syntax anywhere in the program file. Each annotation 
#' is defined on a separate line starting with the comment character `#`. One or 
#' more spaces separate the annotation keyword and its value.
#' 
#' Program input is specified using `@input` followed by the input directory or
#' file path. If the input is a directory, all files in that directory is 
#' included. 
#' 
#' Program output locations is specified using the `@output` directive. It is 
#' assumed that the output location is a directory.
#' 
#' If the `log.file` is specified as a directory, a log file created will use 
#' the convention <program name>.Rout. To use a different file name convention, 
#' specify the `log.file` as a full file name. The parent directory to the log
#' file must exist. 
#' 
#' The working directory is either set to the parent directory of the program 
#' `use.wd = "program"` or the root of the batch execution area `use.wd = "root`.
#' The default is the root of the batch execution area
#' 
#' The user R package library can be disabled with `user.library = FALSE`. If 
#' `internal = TRUE`, user library is disabled by default.
#' 
#' The `internal` mode is used when `cxlib_batch()` is used as an internal 
#' utility. 
#' 
#' If `silent=FALSE` the program execution log is printed to the console. 
#' 
#' @export


cxlib_batch <- function( x, options = list( "logs" = NULL, "log.fileext" = "Rout" ), silent = FALSE ) {


  # -- experimental extensions
  use.wd <- "root"
  internal <- FALSE
  libs <- .libPaths()
  user.library <- TRUE
  overwrite <- TRUE
  overwrite.input <- FALSE


  # -- initialize baseline default configuration 
  
  #    baseline configuration elements
  batch_cfg <- list( ".internal" = list(),
                     "program" = character(0),
                     "log" = character(0),
                     "inputs" = list(),
                     "outputs" = list() 
                    )
  

  
  
  # -- current working directory
  #    note: all paths are relative to the current R session working directory
  batch_cfg[[".internal"]][["session.wd"]] <- getwd()


  # -- default library paths
  def_libpaths <- c( .Library.site, .Library )
  
  if ( ! internal && user.library && ! is.na(Sys.getenv("R_LIBS_USER", unset = NA)) )
    def_libpaths <- append( unlist(strsplit( Sys.getenv("R_LIBS_USER"), .Platform$path.sep, fixed = TRUE), use.names = FALSE) , def_libpaths )
  
  batch_cfg[[".internal"]][["library.paths"]] <- def_libpaths
  
  
    
  # -- set up temporary execution area
  batch_tmp <- base::tempfile( pattern = "cxbatch-", tmpdir = base::tempdir(), fileext = "" )
  
  on.exit({
    base::unlink( batch_tmp, recursive = TRUE, force = TRUE )
  }, add = TRUE)
  
  
  if ( dir.exists( batch_tmp ) )
    stop( "Unexpected isolated execution area already exists" )
  
  if ( ! dir.create( batch_tmp, recursive = TRUE ) )
    stop( "Could not create an isolated execution area" )
  
  
  #    add wrk to internal references
  batch_cfg[[".internal"]][["temp.area"]] <- batch_tmp
  

  
  # -- check on program 
  
  if ( missing(x) || is.null(x) || any(is.na(x)) || (length(x) != 1) || (class(x) != "character") || (base::nchar(gsub("\\s", "", x)) == 0)  )
    stop( "Program x invalid" )
  
  if ( ! file.exists( gsub( "//", "/", file.path(getwd(), x) ) ) || dir.exists( gsub( "//", "/", file.path(getwd(), x) ) ) )
    stop( "Program file ", gsub( "//", "/", file.path(getwd(), x) ), " does not exist or is a directory" )  
  
  
  
  
  # -- stage 
  
  batch_cfg[[".internal"]] <- append( batch_cfg[[".internal"]], 
                                      cxlib:::.cxlib_batch_localfs( x, tmpdir = batch_tmp ) )

  
  # -- program log
  #    note: there is an internal log and an external one ... this is for the external one
  
  if ( is.null(log.file) ) {
    
    batch_cfg[[".internal"]][["log"]]["ref.path"] <- file.path( dirname(batch_cfg[[".internal"]][["program"]]["ref.path"]), 
                                                                paste0( tools::file_path_sans_ext( base::basename(batch_cfg[[".internal"]][["program"]]["ref.path"])) , ".Rout" ) )
    
  } else {

    if ( any(is.na(log.file)) || (length(log.file) != 1) || (class(log.file) != "character") || ( base::nchar( base::trimws(log.file)) < 1 ) )
      stop( "Log file is invalid" )
   
    
    if ( dir.exists( cxlib:::.cxlib_standardpath( file.path( base::getwd(), log.file ) ) ) ) {
      #    log.file is a directory
      #    note: log file name is based on program name
    
      xlog <- cxlib:::.cxlib_standardpath( file.path( base::getwd(),
                                                      log.file,
                                                      paste0( tools::file_path_sans_ext( base::basename(batch_cfg[[".internal"]][["program"]]["ref.path"])) , ".Rout" ) ) )

      batch_cfg[[".internal"]][["log"]]["ref.path"] <- xlog

    } else {
      #    log.file is assumed a file
      
      batch_cfg[[".internal"]][["log"]]["ref.path"] <- cxlib:::.cxlib_standardpath( file.path( base::getwd(), log.file ) )  
      
    }    
    
    
    if ( ! dir.exists(dirname( batch_cfg[[".internal"]][["log"]]["ref.path"] )) )
      stop("Parent directory ", dirname( batch_cfg[[".internal"]][["log"]]["ref.path"] ), " for the log does not exists" )

  }  # end of if-else-statement log.file is null
    

  
  # -- define library environment

  #    environment 
  batch_cfg[[".internal"]][[".environ.file"]] <- file.path( batch_cfg[[".internal"]][["work.area"]], ".Renviron" )
  
    
  #    init with default environment libraries 
  env_libs <- batch_cfg[[".internal"]][["library.paths"]]
  
  #    add specified libs that exist
  if ( ! is.null(libs) )
    env_libs <- libs[ dir.exists( libs ) ]

  
  #    if user library should be disabled  
  if ( ( internal || ! user.library ) && ! is.na(Sys.getenv("R_LIBS_USER", unset = NA ) ) ) {

    #    get user libraries    
    env_usrlibs <- unlist( strsplit( Sys.getenv("R_LIBS_USER", unset = NA ), .Platform$path.sep, fixed = TRUE ), use.names = FALSE )
    
    #    remove user libraries
    env_libs <- env_libs[ ! env_libs %in% env_usrlibs ]
  } 
  
  batch_cfg[[".internal"]][["library.paths"]] <- env_libs


  # 
  # # -- process annotations
  # 
  # #    supported annotations
  # anno <- c( "input", "output" )
  # 
  # 
  # #    read in program
  # pgm <- try( base::suppressWarnings( base::readLines( batch_cfg[["program"]]["path"] ) ), silent = silent )
  # 
  # if ( inherits( pgm, "try-error" ) )
  #   stop( "Could not read program ", batch_cfg[[".internal"]][["program"]] )
  # 
  # #    process annotations
  # anno_rgxp <- paste0( "^#\\s*@(", paste0( anno, collapse = "|"), ")\\s+.*" )
  # pgm_annolines <- pgm[ grepl( anno_rgxp, trimws(pgm), ignore.case = TRUE, perl = TRUE ) ]
  # 
  # 
  # for ( xstr in pgm_annolines ) {
  # 
  #   #    get annotation keyword
  #   xanno <- tolower(base::trimws(gsub( anno_rgxp, "\\1", base::trimws(xstr) )))
  # 
  #   #    get the value  
  #   xvalue <- base::trimws(gsub( "^#\\s*@[a-z]+\\s+(.*)", "\\1", base::trimws(xstr), ignore.case = TRUE, perl = TRUE ))
  #   
  #   if ( tolower(xvalue) == tolower(xstr) )
  #     stop( "Failed in parsing annotation '", xstr, "'")
  # 
  #   #    check if annotation path is specified as absolute
  #   if ( grepl("^/", xvalue, perl = TRUE) )
  #     stop( "Paths in annotations are relative paths and cannot start with a leading slash ('/')" )
  #   
  #   
  #   #    resolve annotation path
  #   xpath <- cxlib:::.cxlib_standardpath( file.path( base::getwd(), xvalue) )
  #       
  #   #    input annotation
  #   if ( xanno == "input" ) {
  # 
  #     anno_files <- character(0)
  #     
  #     #    input is a file
  #     if ( ! dir.exists( xpath ) && file.exists( xpath ) )
  #       anno_files <- xpath 
  #     
  #     #    input is a directory ... grab all files non-recursive
  #     if ( dir.exists( xpath ) )
  #       anno_files <- list.files( xpath, full.names = TRUE, recursive = FALSE, include.dirs = FALSE ) 
  # 
  #     
  #     #    add file to config with hash/digest
  #     for ( xfile in anno_files ) {
  #       
  #       # note: +2 removes leading slash
  #       xref <- base::substring( xfile, base::nchar(base::getwd()) + 2 )  
  #       
  #       batch_cfg[[ "inputs" ]][[ xref ]] <- c( "path" = xfile,
  #                                               "sha1" = digest::digest( xfile, algo = "sha1", file = TRUE ))
  #       
  #       batch_cfg[[".internal"]][[ "inputs" ]][ xref ] <- cxlib:::.cxlib_standardpath( file.path( batch_cfg[[".internal"]][["work.area"]], xref))
  #     }
  #       
  #     next()
  #   }  # end of stage input
  # 
  #   
  #   
  #   if ( xanno == "output" ) {
  #   
  #     xref <- cxlib:::.cxlib_standardpath( xvalue )
  # 
  #     batch_cfg[[".internal"]][[ "outputs" ]][[ xref ]] <- list( "path" = cxlib:::.cxlib_standardpath( file.path( batch_cfg[[".internal"]][["work.area"]], xref)),
  #                                                                "files" = character(0) )
  #     
  #     next()
  #   }  # end of stage output
  #   
  #   
  #   #    if we get here we do not know how to deal with the annotation
  #   stop( "The annotation ", xanno, " not known" )
  # 
  # }
  # 
  # 
  # 
  # # -- stage execution work area
  # 
  # #    add structure to work area
  # wrk_dirs <- base::dirname( batch_cfg[[".internal"]][["program"]] )
  # 
  # for ( xpath in base::sort( wrk_dirs, decreasing = TRUE ) )
  #   if ( ! dir.exists( xpath ) && ! dir.create( xpath, recursive = TRUE ) )
  #     stop( "Failed to stage directory ", xpath, " in work area" )
  # 
  # 
  # #    add program to work area
  # if ( ! file.copy( batch_cfg[["program"]]["path"], batch_cfg[[".internal"]][["program"]], copy.mode = FALSE, copy.date = FALSE ) )
  #   stop( "Failed to stage program in the work area" )
  #   
  # if ( digest::digest( batch_cfg[[".internal"]][["program"]], algo = "sha1", file = TRUE ) != batch_cfg[["program"]]["sha1"] )
  #   stop( "Digest failure when staging program in the work area" )
  # 
  # 
  # #    create environment file
  # base::writeLines( paste( "R_LIBS_SITE", paste( batch_cfg[["library.paths"]], collapse = .Platform$path.sep ), sep = "=" ), con = batch_cfg[[".internal"]][[".environ.file"]] )
  # 
  # 
  # #    stage inputs
  # for ( xitem in names(batch_cfg[["inputs"]]) ) 
  #   if ( file.exists( batch_cfg[["inputs"]][[xitem]]["path"]) ) {
  #    
  #     if ( ! dir.exists(dirname(batch_cfg[[".internal"]][[ "inputs" ]][ xref ])) && ! dir.create( dirname(batch_cfg[[".internal"]][[ "inputs" ]][ xref ]), recursive = TRUE ) )
  #       stop( "Could not stage parent directory for input ", xitem )
  #     
  #     if ( ! file.copy( batch_cfg[["inputs"]][[xitem]]["path"], batch_cfg[[".internal"]][[ "inputs" ]][ xref ], copy.mode = FALSE, copy.date = FALSE ) )
  #       stop( "Failed to stage input ", xitem )
  #     
  #   }
  # 
  # 
  # #    stage output areas
  # for ( xitem in names( batch_cfg[[".internal"]][[ "outputs" ]] ) ) {
  #   
  #   xpath <- batch_cfg[[".internal"]][[ "outputs" ]][[ xitem ]][["path"]]
  #   
  #   if ( ! dir.exists( xpath ) && ! dir.create( xpath, recursive = TRUE) )
  #     stop( "Failed to stage output destination ", xpath )
  # }
  # 
  # 
  
  
  # -- perform pre-execution inventory    
  pre_inv <- sapply( list.files( batch_cfg[[".internal"]][["work.area"]], full.names = TRUE, recursive = TRUE ), function(x) {
    digest::digest( x, algo = "sha1", file = TRUE )  
  }, USE.NAMES = TRUE)
  


  # -- batch execute program
  
  #    initiate batch cmd sequence
  batch_cmd <- character(0)
  
  
  #    add redirect to internal working directory
  batch_cmd <- paste( "cd", batch_cfg[[".internal"]][["work.area"]] )
  
  
  #    build R command  
  batch_cmd_r <- character(0)
  
  if ( ( .Platform$OS.type == "unix" ) && file.exists( file.path( R.home("bin"), "R") ) )
    batch_cmd_r <- file.path( R.home("bin"), "R")
  
  if ( ( .Platform$OS.type == "windows" ) && file.exists( file.path( R.home("bin"), "R.exe") ) )
    batch_cmd_r <- file.path( R.home("bin"), "R.exe")
  
  if ( length(batch_cmd_r) != 1 )
    stop( "Cannot identify R executable" )
    
  #    - add batch instruction
  batch_cmd_r <- append( batch_cmd_r, "CMD BATCH")
  
  #    - add program 
  batch_cmd_r <- append( batch_cmd_r, batch_cfg[[".internal"]][["program"]]["path"] )

  #    - add log 
  batch_cmd_r <- append( batch_cmd_r, batch_cfg[[".internal"]][["log"]]["path"] )
  
  #    - add std error redirect
  if ( .Platform$OS.type == "unix" ) 
    batch_cmd_r <- append( batch_cmd_r, "2>&1" )
  
  #    append R cmd to command sequence
  batch_cmd <- append( batch_cmd, paste( batch_cmd_r, collapse = " ") )
  
  
  #    run program 
  cmd <- paste( batch_cmd, collapse = " ; ")

  rc <- base::suppressWarnings( try( system( cmd, intern = TRUE, wait = TRUE ), silent = TRUE ) )
  
  
  #    initial assessment
  exec_reports_fail <- FALSE
  
  if ( inherits( rc, "try-error") || ( "status" %in% names(attributes(rc)) && attributes(rc)$status != 0 ) )
    exec_reports_fail <- TRUE


  #    verify we have a log
  if ( ! file.exists( batch_cfg[[".internal"]][["log"]]["path"] ) )
    stop( "Execution log not generated" )

  batch_cfg[[".internal"]][["log"]]["sha1"] <- digest::digest( batch_cfg[[".internal"]][["log"]]["path"], algo = "sha1", file = TRUE )
  

  # -- perform post-execution inventory    
  
  #    inventor file system   
  post_inv <- sapply( list.files( batch_cfg[[".internal"]][["work.area"]], full.names = TRUE, recursive = TRUE ), function(x) {
    digest::digest( x, algo = "sha1", file = TRUE )  
  }, USE.NAMES = TRUE)
  
  
  #    program integrity check 
  if ( post_inv[[ batch_cfg[[".internal"]][["program"]]["path"] ]] != batch_cfg[[".internal"]][["program"]]["sha1"] ) 
    stop( "Integrity check failure. Program ", base::basename(batch_cfg[[".internal"]][["program"]]["path"]), " changed during processing." )
  
  
  
  # -- output log in console    
  if ( ! silent )
    cat( c( "-----------------------------------------------------------------------",
            "-- Begin program log  -------------------------------------------------",
            " ",
            readLines( batch_cfg[[".internal"]][["log"]]["path"] ),            
            
            " ",
            "-- End program log  ---------------------------------------------------",
            "-----------------------------------------------------------------------"
    ),
    sep = "\n")
  
  
      

  
print(post_inv)  
return()


  #    identify files in output locations
  post_inv_outputs <- character(0)
  
  for ( xitem in names(batch_cfg[[".internal"]][[ "outputs" ]]) ) {
    
    xpath <- batch_cfg[[".internal"]][[ "outputs" ]][[xitem]][["path"]] 
    
    if ( ! dir.exists(xpath) )
      next()
    
    post_inv_outputs <- append( post_inv_outputs, list.files( xpath, full.names = TRUE, recursive = FALSE, include.dirs = FALSE ) )
  }
  
  
    
  
  


  
  # -- determine file system changes 
  
  #    note: list is in the form of named vectors of sha1's
  fs_delta <- list( "new" = character(0), "updates" = character(0), "deleted" = character(0))
  
  
  
  
  
  # -- identify outputs
  
  for ( xitem in names(batch_cfg[[".internal"]][[ "outputs" ]]) ) {
    
    xpath <- batch_cfg[[".internal"]][[ "outputs" ]][[xitem]][["path"]] 
    
    if ( ! dir.exists(xpath) )
      next()
    
    batch_cfg[[".internal"]][[ "outputs" ]][[xitem]][["files"]] <- list.files( xpath, full.names = FALSE, recursive = FALSE ) 
  }
  
  
    
  
  # -- write back 

  #    log
  if ( ! file.rename( batch_cfg[[".internal"]][["log"]], file.path( base::dirname(batch_cfg[[".internal"]][["log"]]), base::basename(batch_cfg[["log"]]["path"])) ) )
    stop( "Could not set name for log" )

    
  if ( ! file.copy( file.path( base::dirname(batch_cfg[[".internal"]][["log"]]), base::basename(batch_cfg[["log"]]["path"])), 
                    dirname(batch_cfg[["log"]]["path"]), overwrite = TRUE, copy.date = TRUE, copy.mode = FALSE ) )
    stop( "Could not save log" )
  
  
  #    outputs
  for ( xitem in names( batch_cfg[[".internal"]][[ "outputs" ]] ) ) {
    
    xpath <- batch_cfg[[".internal"]][[ "outputs" ]][[xitem]][["path"]]
    
    for ( xfile in batch_cfg[[".internal"]][[ "outputs" ]][[xitem]][["files"]] ) {
      
      xtarget <- cxlib:::.cxlib_standardpath( file.path( getwd(), xitem, base::basename(xfile) )  )
      
      if ( ! file.copy( file.path(xpath, xfile), xtarget, copy.mode = FALSE, copy.date = TRUE, overwrite = TRUE ) )
        stop( "Could not write file ", xtarget )
      
      batch_cfg[["outputs"]][[ file.path(xitem, xfile) ]] <- c( "path" = xtarget, "sha1" = digest::digest( xtarget, algo = "sha1", file = TRUE) )
    }
    
  }
  
        
  
  # -- standard return details
  return(invisible(batch_cfg))    
  
}


