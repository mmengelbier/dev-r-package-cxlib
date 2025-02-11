#' Internal utility function to stage a local program with inputs 
#' 
#' @param x A character vector with the program paths
#' @param options A list of options 
#' 
#' @return A list of named entries detailing the staged content
#' 
#' @description
#' The vector of programs `x` is assumed to define the order of execution
#' 
#' 
#' @keywords internal


.cxlib_batch_localfs_stage <- function( x, options = NULL ) {

  
  # -- initialize job definition
  
  jdef <- list( "mode" = "localfs",
                "working.directory" = cxlib:::.cxlib_standardpath( base::getwd() ),
                "work.area" = character(0),
                "actions" = list(),
                "inputs" = list(),
                "output.locations" = character(0)
              )
  

  # -- define work area
  #    note: three semi-obfuscated levels
  
  wrk <- character(0)
  
  wrk <- unique( replicate( 100, 
                             paste( sample( c( letters, LETTERS, as.character(0:9) ), 15, replace = FALSE ), collapse = "" ), 
                             simplify = TRUE ) )
  
  if ( length(wrk) < 3 )
    stop( "Internal error generating obfuscated levels" )
  
  
  jdef[["work.area"]] <- cxlib:::.cxlib_standardpath( file.path( base::tempdir(), paste( utils::head( wrk, n = 3), collapse = "/" ), fsep = "/" ) )
  

  if ( ! dir.exists( jdef[["work.area"]] ) && ! dir.create( jdef[["work.area"]], recursive = TRUE ) )
    stop( "Could not stage work area for job" )
    

  
  # -- if no program files .. return empty job
  if ( missing(x) || is.null(x) || ( length(x) == 0 ) || any(is.na(x)) || ! inherits(x, "character" ) )
    return(jdef)
  
  
  # -- ensure all programs exist
  if ( ! all( file.exists( x ) ) )
    stop( "One or more programs do not exist" )
  
  
  
  # -- normalize paths to relative
  
  for ( xitem in x ) {
    # note: assuming check for existing files above still applies

    # note: xpath is relative    
    xpath <- character(0)

    if ( file.exists( xitem ) && file.exists( file.path( jdef[["working.directory"]], xitem, fsep = "/" ) ) ) {
      # relative path as both item and item in working directory directory
      
      xpath <- cxlib:::.cxlib_standardpath( xitem )

    } else {
      # absolute path

      if ( ! base::startsWith( cxlib:::.cxlib_standardpath( xitem ), paste0( jdef[["working.directory"]], "/" ) ) )
        stop( "The program file ", xitem, " is not in a subdirectory of the working directory" )
      
      xpath <- base::substring( cxlib:::.cxlib_standardpath( xitem ), base::nchar(jdef[["working.directory"]]) + 2 )

    }
    
    
    # - identify the program log 
    
    # defaults to program directory with file extension .Rout
    log_suffix <- "Rout"
    log_dir <- base::dirname(xpath)


    if ( ! is.null(options) ) {
      
      # log directory
      if ( "logs" %in% names(options) && ! is.null(options[["logs"]]) ) {
        
        if ( ! dir.exists( file.path( jdef[["working.directory"]], options[["logs"]], fsep = "/" ) ) )
          stop( "The specified directory for logs does not exist" )
        
        log_dir <- cxlib:::.cxlib_standardpath( options[["logs"]] )
      }
      
      
      # log file suffix
      if ( "log.fileext" %in% names(options) && ! is.null(options[["log.fileext"]]) && ! any(is.na(options[["log.fileext"]])) ) 
        log_suffix <-as.character( utils::head( options[["log.fileext"]], n = 1 ) )     

    }
    
    
    log_path <- file.path( log_dir, paste0( tools::file_path_sans_ext( base::basename(xpath)), ".", log_suffix ), fsep = "/" )
 

    # note: sha1 for current log in working directory     
    log_ref_sha1 <- NA

    if ( file.exists( file.path( jdef[["working.directory"]], log_path, fsep = "/" ) ) )
      log_ref_sha1 <- digest::digest( file.path( jdef[["working.directory"]], log_path, fsep = "/" ), algo = "sha1", file = TRUE )

    
        
    jdef[["actions"]][[ length(jdef[["actions"]]) + 1 ]] <- list( "type" = "program", 
                                                                  "path" = xpath,
                                                                  "sha1" = digest::digest( xpath, algo = "sha1", file = TRUE ),
                                                                  "log" = c( "path" = log_path,
                                                                             "sha1" = NA,
                                                                             "reference.sha1" = log_ref_sha1 ) )  
    
  }
  
  
  
  
  # -- process actions
  
  
  # -- stage programs
  
  for ( xaction in jdef[["actions"]] ) {
    
    
    
    if ( xaction["type"] != "program" ) 
      next()
    
    
    # - program actions
    
    #   note: not all elements ... nothing to do
    if ( ! all( c( "type", "path", "sha1") %in% names(xaction) ) )
      next()
      
    
    #   note: working with program is absolute
    xsrc <- file.path( jdef[["working.directory"]], xaction["path"], fsep = "/" )
    
    
    # - process annotations
    
    xanno <- cxlib:::.cxlib_programannotations( xsrc )
    
    
    # - process input annotation
    if ( "input" %in% names(xanno) )
      for ( xinput in xanno[["input"]] ) {
        
        xinput_path <- file.path( jdef[["working.directory"]], xinput, fsep = "/" )

        # input does not currently exist in working directory
        # note: letting the program fail
        if ( ! file.exists( xinput_path ) && ! dir.exists( xinput_path ) )
          next()

                
        # identify files as input
        # note: path relative to working directory
        xinput_files <- character(0)
        
        
        if ( file.exists( xinput_path ) )
          xinput_files <- xinput
        
        if ( dir.exists( xinput_path ) ) {

          lst_files <-  cxlib:::.cxlib_standardpath( list.files( xinput_path, full.names = TRUE, recursive = FALSE, include.dirs = FALSE ) )
          
          xinput_files <- base::substring( lst_files, base::nchar( jdef[["working.directory"]] ) + 2 )
        }
        

        for ( xfile in xinput_files ) {
          
          # stage parent directories for inputs in work area
          
          xfile_parent <- base::dirname( file.path( jdef[["work.area"]], xfile, fsep = "/" ) )
          
          if ( ! dir.exists( xfile_parent  ) && 
               ! dir.create( xfile_parent, recursive = TRUE ) )
            stop( "Could not stage parent directory for ", xfile )


          # futility ... no need to stage file twice
          
          if ( file.exists( file.path( jdef[["work.area"]], xfile, fsep = "/" ) ) ) 
            next()

          
          # stage file          

          src_digest <- digest::digest( file.path( jdef[["working.directory"]], xfile, fsep = "/" ), algo = "sha1", file = TRUE )
          
          if ( ! file.copy( file.path( jdef[["working.directory"]], xfile, fsep = "/" ), 
                            xfile_parent, 
                            copy.mode = FALSE, copy.date = FALSE ) )
            stop( "Could not stage input file ", xfile )
          
        
          if ( digest::digest( file.path( jdef[["work.area"]], xfile, fsep = "/" ), algo = "sha1", file = TRUE ) != src_digest )
            stop( "File incosistency when staging input file ", xfile )
          
          # register file as an input
          jdef[["inputs"]][[ xfile ]] <- c( "path" = xfile , "sha1" = src_digest )
          
        }
        
      }  # end of for-statement for annotated inputs
    
    
    
    # - process output annotation
    if ( "output" %in% names(xanno) )
      for ( xoutput in xanno[["output"]] ) {

        if ( ! dir.exists( file.path( jdef[["working.directory"]], xoutput, fsep = "/" ) ) )
          stop( "Output directory ", xoutput, " does not exist or is not a relative path to the working directory" )
        
        # generate full path in work area
        xpath <- cxlib:::.cxlib_standardpath( file.path( jdef[["work.area"]], xoutput, fsep = "/" ) )
        
        if ( ! dir.exists( xpath ) && ! dir.create( xpath, recursive = TRUE ) )
          stop( "Could not stage output path ", xoutput )
        
        jdef[["output.locations"]] <- base::unique( append( jdef[["output.locations"]], cxlib:::.cxlib_standardpath( xoutput ) ) )
      }
    
    
    
    
    # - stage program
          
    xtrgt <- file.path( jdef[["work.area"]], xaction[["path"]], fsep = "/" ) 
    
    if ( ! dir.exists( base::dirname(xtrgt) ) && ! dir.create( base::dirname(xtrgt), recursive = TRUE ) )
      stop( "Could not create parent directory for program ", xaction[["path"]] )
    
    if ( ! file.exists( xsrc ) )
      stop( "Program ", xaction[["path"]], " does not exist" )
    
    if ( ! file.copy( xsrc, base::dirname(xtrgt), overwrite = FALSE, copy.date = FALSE, copy.mode = FALSE ) ||
         ( digest::digest( xtrgt, algo = "sha1", file = TRUE ) != xaction[["sha1"]] ) )
      stop( "Program ", xaction[["path"]], " could not be staged in the transient work area" )
      

    
    # - stage log directory
    
    xlog <- file.path( jdef[["work.area"]], base::dirname(xaction[["log"]]["path"]), fsep = "/" )

    if ( ! dir.exists( xlog ) && ! dir.create( xlog, recursive = TRUE ) )
      stop( "Log directory for ", xaction[["path"]], " could not be staged in the transient work area" ) 
    
    
  } # <-- end of for-loop for all actions
  

  # -- update list of inputs
  jdef[["inputs"]] <- lapply( sort(cxlib:::.cxlib_standardpath( list.files( jdef[["work.area"]], full.names = FALSE, recursive = TRUE, include.dirs = FALSE ) )), function(x) {
    c( "path" = x, 
       "sha1" = digest::digest( file.path( jdef[["work.area"]], x, fsep = "/" ), algo = "sha1", file = TRUE ) )
  })
  
    
  
  # -- return job definition

  jdef[["output.locations"]] <- base::unique( base::sort( jdef[["output.locations"]] ) )
  
  
  return(invisible( jdef ))

}
