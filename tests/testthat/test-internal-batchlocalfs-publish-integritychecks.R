#
#   Tests for cxlib:::.cxlib_batch_localfs_publish()
#
#   Integrity checks
#


testthat::test_that( "batchlocalfs.publishJobActionsProgramIntegrityFailWorkArea", {
  
  # note: the program in the work area has "changed"
  # note: program file in the work area has been re-written with new content
  # note: SHA-1 in action record is different from the SHA-1 on the file on the file system
  
  
  # -- stage
  
  current_wd <- base::getwd()
  
  test_root <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit({
    base::setwd( current_wd )
    unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE)
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE) )
    testthat::fail( "Could not create test area" )
  
  
  # working directory  
  
  wrkdir <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "working-directory-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( wrkdir ) || ! dir.create( wrkdir, recursive = TRUE) )
    testthat::fail( "Could not create test working directory" )
  
  base::setwd( wrkdir )
  
  
  # work area
  
  wrkarea <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "work-area-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( wrkarea ) || ! dir.create( wrkarea, recursive = TRUE) )
    testthat::fail( "Could not create test work area" )
  

  # program actions
  
  test_program_parent <- file.path( "some", "path", "to", "programs", fsep = "/" )
  
  if ( ! dir.exists( file.path( wrkdir, test_program_parent, fsep = "/" ) ) && ! dir.create( file.path( wrkdir, test_program_parent, fsep = "/" ), recursive = TRUE) )
    testthat::fail( "Could not stage test program directory under working directory" )
  
  if ( ! dir.exists( file.path( wrkarea, test_program_parent, fsep = "/" ) ) && ! dir.create( file.path( wrkarea, test_program_parent, fsep = "/" ), recursive = TRUE) )
    testthat::fail( "Could not stage test program directory in work area" )
  
  
  test_programs <- replicate( 3, 
                              cxlib:::.cxlib_standardpath( base::tempfile( pattern = "test-program-", tmpdir = file.path( wrkdir, test_program_parent, fsep = "/" ), fileext = ".R" ) ),
                              simplify = TRUE )
    
  
  # note: program path references are relative to the working directory and work area root
  test_program_refs <- base::substring( test_programs, base::nchar( wrkdir ) + 2 )


  
  for ( xprg in test_programs ) {
    
    base::writeLines( c( "# test program", 
                         paste( sample( c( LETTERS, letters, as.character(0:9) ), 40 ), collapse = "" ) ), con = xprg )
  
    if ( ! file.exists( xprg ) )
      testthat::fail("Could not stage test program in working directory")
    
    if ( ! file.copy( xprg, file.path( wrkarea, test_program_parent, fsep = "/"), recursive = FALSE, copy.date = FALSE, copy.mode = FALSE ) )
      testthat::fail("Could not stage test program in work area")
    
  }
  

  test_actions <- lapply( test_program_refs, function(x) {
   list( "type" = "program",
         "path" = x,
         "sha1" = digest::digest( file.path( wrkdir, x, fsep = "/" ), algo = "sha1", file = TRUE ) ) 
  })  
  
    
  # manipulate last action to fail sha1 on work area
  base::writeLines( c( "# updated test program", 
                       paste( sample( c( LETTERS, letters, as.character(0:9) ), 40 ), collapse = "" ) ),
                    con = file.path( wrkarea, test_actions[[ length(test_actions) ]][["path"]], fsep = "/" ) )
  
  if ( digest::digest( file.path( wrkarea, test_actions[[ length(test_actions) ]][["path"]], fsep = "/" ), algo = "sha1", file = TRUE ) == test_actions[[ length(test_actions) ]][["sha1"]] )
    testthat::fail( "Failed to stage test program SHA-1 failure in work area" )

  
  test_execresult <- list( "mode" = "localfs",
                           "working.directory" = wrkdir,
                           "work.area" =  wrkarea,
                           "actions" = test_actions )


  # -- test
  testthat::expect_error( cxlib:::.cxlib_batch_localfs_publish( test_execresult ), 
                          regexp = paste0("^Inegrity check fail for program ", test_actions[[ length(test_actions) ]][["path"]], ". The program has changed during job execution.$") )

})




testthat::test_that( "batchlocalfs.publishJobActionsProgramIntegrityFailWorkingDirectory", {
  
  # note: the program in the working directory has "changed"
  # note: program file in the working directory has been re-written with new content
  # note: SHA-1 in action record is different from the SHA-1 on the file on the file system
  
  
  
  # -- stage
  
  current_wd <- base::getwd()
  
  test_root <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit({
    base::setwd( current_wd )
    unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE)
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE) )
    testthat::fail( "Could not create test area" )
  
  
  # working directory  
  
  wrkdir <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "working-directory-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( wrkdir ) || ! dir.create( wrkdir, recursive = TRUE) )
    testthat::fail( "Could not create test working directory" )
  
  base::setwd( wrkdir )
  
  
  # work area
  
  wrkarea <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "work-area-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( wrkarea ) || ! dir.create( wrkarea, recursive = TRUE) )
    testthat::fail( "Could not create test work area" )
  
  
  # program actions
  
  test_program_parent <- file.path( "some", "path", "to", "programs", fsep = "/" )
  
  if ( ! dir.exists( file.path( wrkdir, test_program_parent, fsep = "/" ) ) && ! dir.create( file.path( wrkdir, test_program_parent, fsep = "/" ), recursive = TRUE) )
    testthat::fail( "Could not stage test program directory under working directory" )
  
  if ( ! dir.exists( file.path( wrkarea, test_program_parent, fsep = "/" ) ) && ! dir.create( file.path( wrkarea, test_program_parent, fsep = "/" ), recursive = TRUE) )
    testthat::fail( "Could not stage test program directory in work area" )
  
  
  test_programs <- replicate( 3, 
                              cxlib:::.cxlib_standardpath( base::tempfile( pattern = "test-program-", tmpdir = file.path( wrkdir, test_program_parent, fsep = "/" ), fileext = ".R" )),
                              simplify = TRUE )
  
  
  # note: program path references are relative to the working directory and work area root
  test_program_refs <- base::substring( test_programs, base::nchar( wrkdir ) + 2 )
  
  
  
  for ( xprg in test_programs ) {
    
    base::writeLines( c( "# test program", 
                         paste( sample( c( LETTERS, letters, as.character(0:9) ), 40 ), collapse = "" ) ), con = xprg )
    
    if ( ! file.exists( xprg ) )
      testthat::fail("Could not stage test program in working directory")
    
    if ( ! file.copy( xprg, file.path( wrkarea, test_program_parent, fsep = "/"), recursive = FALSE, copy.date = FALSE, copy.mode = FALSE ) )
      testthat::fail("Could not stage test program in work area")
    
  }
  
  
  test_actions <- lapply( test_program_refs, function(x) {
    list( "type" = "program",
          "path" = x,
          "sha1" = digest::digest( file.path( wrkdir, x, fsep = "/"), algo = "sha1", file = TRUE ) ) 
  })  
  
  
  # manipulate last action to fail sha1 on working directory
  base::writeLines( c( "# updated test program", 
                       paste( sample( c( LETTERS, letters, as.character(0:9) ), 40 ), collapse = "" ) ),
                    con = file.path( wrkdir, test_actions[[ length(test_actions) ]][["path"]], fsep = "/" ) )
  
  if ( digest::digest( file.path( wrkdir, test_actions[[ length(test_actions) ]][["path"]], fsep = "/" ), algo = "sha1", file = TRUE ) == test_actions[[ length(test_actions) ]][["sha1"]] )
    testthat::fail( "Failed to stage test program SHA-1 failure in working directory" )
  
  
  test_execresult <- list( "mode" = "localfs",
                           "working.directory" = wrkdir,
                           "work.area" =  wrkarea,
                           "actions" = test_actions )
  
  
  # -- test
  testthat::expect_error( cxlib:::.cxlib_batch_localfs_publish( test_execresult ), 
                          regexp = paste0("^Inegrity check fail for program ", test_actions[[ length(test_actions) ]][["path"]], ". The program has changed during job execution.$") )
  
})




testthat::test_that( "batchlocalfs.publishJobActionsProgramIntegrityFailSHARecord", {
  
  # note: the action record does not match program in the work area or working directory
  # note: SHA-1 in action record is different from the SHA-1 on the files on the file system

  
  # -- stage
  
  current_wd <- base::getwd()
  
  test_root <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit({
    base::setwd( current_wd )
    unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE)
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE) )
    testthat::fail( "Could not create test area" )
  
  
  # working directory  
  
  wrkdir <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "working-directory-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( wrkdir ) || ! dir.create( wrkdir, recursive = TRUE) )
    testthat::fail( "Could not create test working directory" )
  
  base::setwd( wrkdir )
  
  
  # work area
  
  wrkarea <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "work-area-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( wrkarea ) || ! dir.create( wrkarea, recursive = TRUE) )
    testthat::fail( "Could not create test work area" )
  
  
  # program actions
  
  test_program_parent <- file.path( "some", "path", "to", "programs", fsep = "/" )
  
  if ( ! dir.exists( file.path( wrkdir, test_program_parent, fsep = "/" ) ) && ! dir.create( file.path( wrkdir, test_program_parent, fsep = "/" ), recursive = TRUE) )
    testthat::fail( "Could not stage test program directory under working directory" )
  
  if ( ! dir.exists( file.path( wrkarea, test_program_parent, fsep = "/" ) ) && ! dir.create( file.path( wrkarea, test_program_parent, fsep = "/" ), recursive = TRUE) )
    testthat::fail( "Could not stage test program directory in work area" )
  
  
  test_programs <- replicate( 3, 
                              cxlib:::.cxlib_standardpath( base::tempfile( pattern = "test-program-", tmpdir = file.path( wrkdir, test_program_parent, fsep = "/" ), fileext = ".R" )),
                              simplify = TRUE )
  
  
  # note: program path references are relative to the working directory and work area root
  test_program_refs <- base::substring( test_programs, base::nchar( wrkdir ) + 2 )
  
  
  
  for ( xprg in test_programs ) {
    
    base::writeLines( c( "# test program", 
                         paste( sample( c( LETTERS, letters, as.character(0:9) ), 40 ), collapse = "" ) ), con = xprg )
    
    if ( ! file.exists( xprg ) )
      testthat::fail("Could not stage test program in working directory")
    
    if ( ! file.copy( xprg, file.path( wrkarea, test_program_parent, fsep = "/"), recursive = FALSE, copy.date = FALSE, copy.mode = FALSE ) )
      testthat::fail("Could not stage test program in work area")
    
  }
  
  
  test_actions <- lapply( test_program_refs, function(x) {
    list( "type" = "program",
          "path" = x,
          "sha1" = digest::digest( file.path( wrkdir, x, fsep = "/" ), algo = "sha1", file = TRUE ) ) 
  })  
  
  
  # manipulate last action to fail sha1 on record
  
  test_actions[[ length(test_actions) ]][["sha1"]] <- digest::digest( paste( sample( c( LETTERS, letters, as.character(0:9) ), 40 ), collapse = "" ),
                                                                      algo = "sha1", 
                                                                      file = FALSE )
  
  if ( digest::digest( file.path( wrkdir, test_actions[[ length(test_actions) ]][["path"]], fsep = "/" ), algo = "sha1", file = TRUE ) == test_actions[[ length(test_actions) ]][["sha1"]] )
    testthat::fail( "Failed to stage test program SHA-1 failure in input record" )
  
  
  test_execresult <- list( "mode" = "localfs",
                           "working.directory" = wrkdir,
                           "work.area" =  wrkarea,
                           "actions" = test_actions )
  
  
  # -- test
  testthat::expect_error( cxlib:::.cxlib_batch_localfs_publish( test_execresult ), 
                          regexp = paste0("^Inegrity check fail for program ", test_actions[[ length(test_actions) ]][["path"]], ". The program has changed during job execution.$") )
  
})





testthat::test_that( "batchlocalfs.publishJobActionsLogIntegrityFailPriorLogNotExist", {
  
  # note: log does not exist in working directory when program started executing (reference.sha1 = NA)
  # note: log file in the working directory exists 

  
  # -- stage
  
  current_wd <- base::getwd()
  
  test_root <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit({
    base::setwd( current_wd )
    unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE)
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE) )
    testthat::fail( "Could not create test area" )
  
  
  # working directory  
  
  wrkdir <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "working-directory-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( wrkdir ) || ! dir.create( wrkdir, recursive = TRUE) )
    testthat::fail( "Could not create test working directory" )
  
  base::setwd( wrkdir )
  
  
  # work area
  
  wrkarea <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "work-area-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( wrkarea ) || ! dir.create( wrkarea, recursive = TRUE) )
    testthat::fail( "Could not create test work area" )
  
  
  # program actions
  
  test_program_parent <- file.path( "some", "path", "to", "programs", fsep = "/" )
  
  if ( ! dir.exists( file.path( wrkdir, test_program_parent, fsep = "/" ) ) && ! dir.create( file.path( wrkdir, test_program_parent, fsep = "/" ), recursive = TRUE) )
    testthat::fail( "Could not stage test program directory under working directory" )
  
  if ( ! dir.exists( file.path( wrkarea, test_program_parent, fsep = "/" ) ) && ! dir.create( file.path( wrkarea, test_program_parent, fsep = "/" ), recursive = TRUE) )
    testthat::fail( "Could not stage test program directory in work area" )
  
  
  test_programs <- replicate( 3, 
                              cxlib:::.cxlib_standardpath( base::tempfile( pattern = "test-program-", tmpdir = file.path( wrkdir, test_program_parent, fsep = "/" ), fileext = ".R" )),
                              simplify = TRUE )
  
  
  # note: program path references are relative to the working directory and work area root
  test_program_refs <- base::substring( test_programs, base::nchar( wrkdir ) + 2 )
  
  
  
  for ( xprg in test_program_refs ) {
    
    # program
    base::writeLines( c( "# test program", 
                         paste( sample( c( LETTERS, letters, as.character(0:9) ), 40 ), collapse = "" ) ), 
                      con = file.path( wrkdir, xprg, fsep = "/" ) )
    
    if ( ! file.exists( file.path( wrkdir, xprg, fsep = "/" ) ) )
      testthat::fail("Could not stage test program in working directory")
    
    if ( ! file.copy( file.path( wrkdir, xprg, fsep = "/" ), file.path( wrkarea, test_program_parent, fsep = "/"), recursive = FALSE, copy.date = FALSE, copy.mode = FALSE ) )
      testthat::fail("Could not stage test program in work area")
    
    
    # create log associated with program
    base::writeLines( c( "# test log", 
                         paste( sample( c( LETTERS, letters, as.character(0:9) ), 40 ), collapse = "" ) ), 
                      con =  file.path( wrkarea, paste0( tools::file_path_sans_ext(xprg), ".Rout" ), fsep = "/" ) )

  }
  
  
  test_actions <- lapply( test_program_refs, function(x) {
    list( "type" = "program",
          "path" = x,
          "sha1" = digest::digest( file.path( wrkdir, x, fsep = "/" ), algo = "sha1", file = TRUE ), 
          "log" = c( "path" = paste0( tools::file_path_sans_ext( x ), ".Rout"), 
                     "sha1" = digest::digest( file.path( wrkarea, paste0( tools::file_path_sans_ext( x ), ".Rout"), fsep = "/"), algo = "sha1", file = TRUE ),
                     "reference.sha1" = NA )
        ) 
  })  
  
  
  # manipulate last action to fail sha1 on log
  base::writeLines( c( "# test log", 
                       paste( sample( c( LETTERS, letters, as.character(0:9) ), 40 ), collapse = "" ) ),
                    con = file.path( wrkdir, test_actions[[ length(test_actions) ]][["log"]]["path"], fsep = "/" ) )
  
  if ( ! file.exists( file.path( wrkdir, test_actions[[ length(test_actions) ]][["log"]]["path"], fsep = "/" ) ) )
    testthat::fail( "Failed to stage test log in working directory" )
  
  
  test_execresult <- list( "mode" = "localfs",
                           "working.directory" = wrkdir,
                           "work.area" =  wrkarea,
                           "actions" = test_actions )
  

  # -- test
  testthat::expect_error( cxlib:::.cxlib_batch_localfs_publish( test_execresult ), 
                          regexp = paste0("^Inegrity check fail for program ", test_actions[[ length(test_actions) ]][["path"]], ". The program log has changed during job execution.$") )
  
})




testthat::test_that( "batchlocalfs.publishJobActionsLogIntegrityPriorLogExists", {
  
  # note: log does not exist in working directory when program started executing (reference.sha1 = NA)
  # note: log file in the working directory exists 
  
  
  # -- stage
  
  current_wd <- base::getwd()
  
  test_root <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit({
    base::setwd( current_wd )
    unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE)
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE) )
    testthat::fail( "Could not create test area" )
  
  
  # working directory  
  
  wrkdir <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "working-directory-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( wrkdir ) || ! dir.create( wrkdir, recursive = TRUE) )
    testthat::fail( "Could not create test working directory" )
  
  base::setwd( wrkdir )
  
  
  # work area
  
  wrkarea <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "work-area-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( wrkarea ) || ! dir.create( wrkarea, recursive = TRUE) )
    testthat::fail( "Could not create test work area" )
  
  
  # program actions
  
  test_program_parent <- file.path( "some", "path", "to", "programs", fsep = "/" )
  
  if ( ! dir.exists( file.path( wrkdir, test_program_parent, fsep = "/" ) ) && ! dir.create( file.path( wrkdir, test_program_parent, fsep = "/" ), recursive = TRUE) )
    testthat::fail( "Could not stage test program directory under working directory" )
  
  if ( ! dir.exists( file.path( wrkarea, test_program_parent, fsep = "/" ) ) && ! dir.create( file.path( wrkarea, test_program_parent, fsep = "/" ), recursive = TRUE) )
    testthat::fail( "Could not stage test program directory in work area" )
  
  
  test_programs <- replicate( 3, 
                              cxlib:::.cxlib_standardpath( base::tempfile( pattern = "test-program-", tmpdir = file.path( wrkdir, test_program_parent, fsep = "/" ), fileext = ".R" )),
                              simplify = TRUE )
  
  
  # note: program path references are relative to the working directory and work area root
  test_program_refs <- base::substring( test_programs, base::nchar( wrkdir ) + 2 )
  
  
  
  for ( xprg in test_program_refs ) {
    
    # program
    base::writeLines( c( "# test program", 
                         paste( sample( c( LETTERS, letters, as.character(0:9) ), 40 ), collapse = "" ) ), 
                      con = file.path( wrkdir, xprg, fsep = "/" ) )
    
    if ( ! file.exists( file.path( wrkdir, xprg, fsep = "/" ) ) )
      testthat::fail("Could not stage test program in working directory")
    
    if ( ! file.copy( file.path( wrkdir, xprg, fsep = "/" ), file.path( wrkarea, test_program_parent, fsep = "/"), recursive = FALSE, copy.date = FALSE, copy.mode = FALSE ) )
      testthat::fail("Could not stage test program in work area")
    
    
    # create work area log associated with program
    base::writeLines( c( "# test log", 
                         paste( sample( c( LETTERS, letters, as.character(0:9) ), 40 ), collapse = "" ) ), 
                      con =  file.path( wrkarea, paste0( tools::file_path_sans_ext(xprg), ".Rout" ), fsep = "/" ) )

    # create prior log associated with program
    base::writeLines( c( "# test log", 
                         paste( sample( c( LETTERS, letters, as.character(0:9) ), 40 ), collapse = "" ) ), 
                      con =  file.path( wrkdir, paste0( tools::file_path_sans_ext(xprg), ".Rout" ), fsep = "/" ) )
    
        
  }  
  

  
  test_actions <- lapply( test_program_refs, function(x) {
    list( "type" = "program",
          "path" = x,
          "sha1" = digest::digest( file.path( wrkdir, x, fsep = "/" ), algo = "sha1", file = TRUE ), 
          "log" = c( "path" = paste0( tools::file_path_sans_ext( x ), ".Rout"), 
                     "sha1" = digest::digest( file.path( wrkarea, paste0( tools::file_path_sans_ext( x ), ".Rout"), fsep = "/"), algo = "sha1", file = TRUE ),
                     "reference.sha1" = digest::digest( file.path( wrkdir, paste0( tools::file_path_sans_ext( x ), ".Rout"), fsep = "/"), algo = "sha1", file = TRUE ) )
    ) 
  })  
  
  
  # manipulate log of last action simulate a program run
  base::writeLines( c( "# test log", 
                       paste( sample( c( LETTERS, letters, as.character(0:9) ), 40 ), collapse = "" ) ),
                    con = file.path( wrkdir, test_actions[[ length(test_actions) ]][["log"]]["path"], fsep = "/" ) )
  
  if ( digest::digest( file.path( wrkdir, test_actions[[ length(test_actions) ]][["log"]]["path"], fsep = "/" ), algo = "sha1", file = TRUE ) == test_actions[[ length(test_actions) ]][["log"]]["reference.sha1"] )
    testthat::fail( "Failed to stage updated test log in working directory" )
  
  
  test_execresult <- list( "mode" = "localfs",
                           "working.directory" = wrkdir,
                           "work.area" =  wrkarea,
                           "actions" = test_actions )
  

  # -- test
  testthat::expect_error( cxlib:::.cxlib_batch_localfs_publish( test_execresult ), 
                          regexp = paste0("^Inegrity check fail for program ", test_actions[[ length(test_actions) ]][["path"]], ". The program log has changed during job execution.$") )
  
})

