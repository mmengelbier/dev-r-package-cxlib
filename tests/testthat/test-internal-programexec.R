#
#   Tests for cxlib:::.cxlib_programexec()
#
#
#

testthat::test_that( "programexec.noParams", {
  
  # -- test
  testthat::expect_error( cxlib:::.cxlib_programexec(), regexp = "^The program is not specified or as specified invalid$" )
  
})


testthat::test_that( "programexec.programNull", {
  
  # -- test
  testthat::expect_error( cxlib:::.cxlib_programexec( NULL ), regexp = "^The program is not specified or as specified invalid$" )
  
})



testthat::test_that( "programexec.programNA", {
  
  # -- test
  testthat::expect_error( cxlib:::.cxlib_programexec( NA ), regexp = "^The program is not specified or as specified invalid$" )
  
})



testthat::test_that( "programexec.programCharacterEmpty", {
  
  # -- test
  testthat::expect_error( cxlib:::.cxlib_programexec( character(0) ), regexp = "^The program is not specified or as specified invalid$" )
  
})


testthat::test_that( "programexec.programNotCharacter", {
  
  # -- test
  testthat::expect_error( cxlib:::.cxlib_programexec( 1 ), regexp = "^The program is not specified or as specified invalid$" )
  
})



testthat::test_that( "programexec.multiplePrograms", {
  
  
  # -- stage
  
  current_wd <- base::getwd()
  
  test_root <- base::gsub( "\\\\", "/", base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit({
    base::setwd( current_wd )
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )  
  
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Could not stage test root" )
  
  base::setwd( test_root )

  
  
  # test programs
  test_programs <- replicate( 5, 
                              base::gsub( "\\\\", "/", base::tempfile( pattern = "test-program-", tmpdir = test_root, fileext = ".R" ) ), 
                              simplify = TRUE)
  
  for ( xpgm in test_programs ) 
    base::writeLines( "# test program", con = xpgm ) 
    
  
  # -- test
  testthat::expect_error( cxlib:::.cxlib_programexec( test_programs ), regexp = "^The program is not specified or as specified invalid$" )

})





testthat::test_that( "programexec.programNotExist", {
  
  
  # -- stage
  
  current_wd <- base::getwd()
  
  test_root <- base::gsub( "\\\\", "/", base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit({
    base::setwd( current_wd )
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )  
  
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Could not stage test root" )
  
  base::setwd( test_root )
  
  
  
  # test programs
  test_program <- base::gsub( "\\\\", "/", base::tempfile( pattern = "test-program-", tmpdir = test_root, fileext = ".R" ) )
  
  if ( file.exists( test_program ) )
    testthat::fail( "Unexpected test program exists" )
  
  
  # -- test
  testthat::expect_error( cxlib:::.cxlib_programexec( base::basename( test_program ) ), 
                          regexp = paste( "^The program", base::basename( test_program ), "does not exist in the work area$" ) )
  
})




testthat::test_that( "programexec.programLog", {
  
  
  # -- stage
  
  current_wd <- base::getwd()
  
  test_root <- base::gsub( "\\\\", "/", base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit({
    base::setwd( current_wd )
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Could not stage test root" )
  
  base::setwd( test_root )
  

  # test reference
  test_reference <- paste( base::sample( c( letters, LETTERS, as.character(0:9) ) , 40 ), collapse = "" )
    
    
  
  # test programs
  test_program <- base::gsub( "\\\\", "/", base::tempfile( pattern = "test-program-", tmpdir = test_root, fileext = ".R" ) )
  
  pgm <- c( "# test program",
            paste0( "cat( \"--->", test_reference, "<---\", sep = \"\")") )
  
  base::writeLines( pgm, con = test_program )
  
  
  if ( ! file.exists( test_program ) )
    testthat::fail( "Unexpected test program does not exist" )
  
  test_program_ref <- base::substring( test_program, base::nchar(test_root) + 2 )
  
  
  
  # -- test
  result <- cxlib:::.cxlib_programexec( base::basename( test_program_ref ) )
  

  # -- expected
  expected_program <- c( "path" = base::basename( test_program_ref ), 
                         "sha1" = digest::digest( test_program, algo = "sha1", file = TRUE) )

  expected_log <- c( "path" = paste0( tools::file_path_sans_ext( test_program_ref ), ".Rout"), 
                     "sha1" = digest::digest( paste0( tools::file_path_sans_ext( test_program ), ".Rout"), algo = "sha1", file = TRUE) )
  
  expected_audit <- list( "inputs" = list( expected_program ),  
                          "created" = list( expected_log ),
                          "updated" = list(), "deleted" = list() )
  
  
  expected_result <- list( "program" = expected_program,
                           "log" = expected_log,
                           "files.input" = list( expected_program ),
                           "files.created" = list( expected_log ),
                           "files.updated" = list(),
                           "files.deleted" = list() )  
  
  
  # -- assertions

  # result  
  testthat::expect_equal( result[ names(expected_result) ], expected_result )
  
  # log
  testthat::expect_true( file.exists( file.path(test_root, result[["log"]][["path"]], fsep = "/") ) ) 

  # verify test reference is in log
  log_lines <- base::readLines( con = result[["log"]][["path"]] )
  testthat::expect_true( any( base::startsWith( log_lines, paste0( "--->", test_reference, "<---" ) )) )


})




testthat::test_that( "programexec.workareaNull", {
  
  
  # -- stage
  
  current_wd <- base::getwd()
  
  test_root <- base::gsub( "\\\\", "/", base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit({
    base::setwd( current_wd )
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )  
  
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Could not stage test root" )
  
  base::setwd( test_root )
  
  
  # test reference
  test_reference <- paste( base::sample( c( letters, LETTERS, as.character(0:9) ) , 40 ), collapse = "" )
  
  
  
  # test programs
  test_program_parent <- file.path( test_root, "some", "path", "to", "programs", fsep = "/" )
  
  if ( ! dir.exists( test_program_parent ) && ! dir.create( test_program_parent, recursive = TRUE) )
    testthat::fail( "Could not create test program parent directory" )
  
  test_program <- base::gsub( "\\\\", "/", base::tempfile( pattern = "test-program-", tmpdir = test_program_parent, fileext = ".R" ) )
  
  pgm <- c( "# test program",
            paste0( "cat( c( \"--->\", base::getwd(), \"<---\"), sep = \"\")") )
  
  base::writeLines( pgm, con = test_program )
  
  
  if ( ! file.exists( test_program ) )
    testthat::fail( "Unexpected test program does not exist" )
  
  test_program_ref <- base::substring( test_program, base::nchar(test_root) + 2 )
  
  
  # -- test
  result <- cxlib:::.cxlib_programexec( test_program_ref )
  

  # -- expected
  expected_program <- c( "path" = test_program_ref, 
                         "sha1" = digest::digest( test_program, algo = "sha1", file = TRUE) )
  
  expected_log <- c( "path" = paste0( tools::file_path_sans_ext( test_program_ref ), ".Rout"), 
                     "sha1" = digest::digest( paste0( tools::file_path_sans_ext( test_program ), ".Rout"), algo = "sha1", file = TRUE) )

  expected_audit <- list( "inputs" = list( expected_program ),  
                          "created" = list( expected_log ),
                          "updated" = list(), "deleted" = list() )
  
  
  
  expected_result <- list( "program" = expected_program,
                           "log" = expected_log,
                           "files.input" = list( expected_program ),
                           "files.created" = list( expected_log ),
                           "files.updated" = list(),
                           "files.deleted" = list() )  
  
  
  
  # -- assertions
  
  # result  
  testthat::expect_equal( result[names(expected_result)], expected_result )

  
  # # log
  testthat::expect_true( file.exists( file.path(test_root, result[["log"]][["path"]], fsep = "/")) )

  # verify test reference is in log
  log_lines <- base::readLines( con = result[["log"]][["path"]] )
  testthat::expect_true( any( base::startsWith( log_lines, paste0( "--->", test_root, "<---" ) )) )


})





testthat::test_that( "programexec.workareaNotWorkdirWorkareaNull", {
  
  
  # -- stage
  
  test_root <- base::gsub( "\\\\", "/", base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )  
  
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Could not stage test root" )
  

  if ( base::normalizePath( test_root ) == base::normalizePath( base::getwd() ) )
    testthat::fail( "Working directory same as test root" )
  
  
  # test reference
  test_reference <- paste( base::sample( c( letters, LETTERS, as.character(0:9) ) , 40 ), collapse = "" )
  
  
  
  # test programs
  test_program_parent <- file.path( test_root, "some", "path", "to", "programs", fsep = "/" )
  
  if ( ! dir.exists( test_program_parent ) && ! dir.create( test_program_parent, recursive = TRUE) )
    testthat::fail( "Could not create test program parent directory" )
  
  test_program <- base::gsub( "\\\\", "/", base::tempfile( pattern = "test-program-", tmpdir = test_program_parent, fileext = ".R" ) )
  
  pgm <- c( "# test program",
            paste0( "cat( c( \"--->\", base::getwd(), \"<---\"), sep = \"\")") )
  
  base::writeLines( pgm, con = test_program )
  
  
  if ( ! file.exists( test_program ) )
    testthat::fail( "Unexpected test program does not exist" )
  
  test_program_ref <- base::substring( test_program, base::nchar(test_root) + 2 )
  
  
  # -- test
  testthat::expect_error( cxlib:::.cxlib_programexec( test_program_ref, options = list( "work.area" = NULL ) ), 
                          regexp = paste( "^The program", test_program_ref, "does not exist in the work area$" ) )
  
  
})



testthat::test_that( "programexec.workareaNotWorkdirWorkareaNA", {
  
  
  # -- stage
  
  test_root <- base::gsub( "\\\\", "/", base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )  
  
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Could not stage test root" )
  

  if ( base::normalizePath( test_root ) == base::normalizePath( base::getwd() ) )
    testthat::fail( "Working directory same as test root" )
  
  
  # test reference
  test_reference <- paste( base::sample( c( letters, LETTERS, as.character(0:9) ) , 40 ), collapse = "" )
  
  
  
  # test programs
  test_program_parent <- file.path( test_root, "some", "path", "to", "programs", fsep = "/" )
  
  if ( ! dir.exists( test_program_parent ) && ! dir.create( test_program_parent, recursive = TRUE) )
    testthat::fail( "Could not create test program parent directory" )
  
  test_program <- base::gsub( "\\\\", "/", base::tempfile( pattern = "test-program-", tmpdir = test_program_parent, fileext = ".R" ) )
  
  pgm <- c( "# test program",
            paste0( "cat( c( \"--->\", base::getwd(), \"<---\"), sep = \"\")") )
  
  base::writeLines( pgm, con = test_program )
  
  
  if ( ! file.exists( test_program ) )
    testthat::fail( "Unexpected test program does not exist" )
  
  test_program_ref <- base::substring( test_program, base::nchar(test_root) + 2 )
  
  
  # -- test
  testthat::expect_error( cxlib:::.cxlib_programexec( test_program_ref, options = list( "work.area" = NA ) ), 
                          regexp = "^The work area NA does not exist$" )
  
  
})




testthat::test_that( "programexec.workareaNotWorkdirWorkareaNotExist", {
  
  
  # -- stage
  
  test_root <- base::gsub( "\\\\", "/", base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )  
  
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Could not stage test root" )
  
  
  if ( base::normalizePath( test_root ) == base::normalizePath( base::getwd() ) )
    testthat::fail( "Working directory same as test root" )
  
  
  # test reference
  test_reference <- paste( base::sample( c( letters, LETTERS, as.character(0:9) ) , 40 ), collapse = "" )
  
  
  
  # test programs
  test_program_parent <- file.path( test_root, "some", "path", "to", "programs", fsep = "/" )
  
  if ( ! dir.exists( test_program_parent ) && ! dir.create( test_program_parent, recursive = TRUE) )
    testthat::fail( "Could not create test program parent directory" )
  
  test_program <- base::gsub( "\\\\", "/", base::tempfile( pattern = "test-program-", tmpdir = test_program_parent, fileext = ".R" ) )
  
  pgm <- c( "# test program",
            paste0( "cat( c( \"--->\", base::getwd(), \"<---\"), sep = \"\")") )
  
  base::writeLines( pgm, con = test_program )
  
  
  if ( ! file.exists( test_program ) )
    testthat::fail( "Unexpected test program does not exist" )
  
  test_program_ref <- base::substring( test_program, base::nchar(test_root) + 2 )
  

  # work area 
  wrk <- base::gsub( "\\\\", "/", base::tempfile( pattern = "work-area-does-not-exist-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( wrk ) )
    testthat::fail( "Unexpected work area exists" )
  
  
    
  # -- test
  testthat::expect_error( cxlib:::.cxlib_programexec( test_program_ref, options = list( "work.area" = wrk ) ), 
                          regexp = paste( "^The work area", wrk, "does not exist$" ) )
  
  
})






testthat::test_that( "programexec.workareaNotWorkdirWorkarea", {
  
  
  # -- stage
  
  test_root <- base::gsub( "\\\\", "/", base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )  
  
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Could not stage test root" )
  
  
  if ( base::normalizePath( test_root ) == base::normalizePath( base::getwd() ) )
    testthat::fail( "Working directory same as test root" )
  
  
  # test reference
  test_reference <- paste( base::sample( c( letters, LETTERS, as.character(0:9) ) , 40 ), collapse = "" )
  
  
  
  # test programs
  test_program_parent <- file.path( test_root, "some", "path", "to", "programs", fsep = "/" )
  
  if ( ! dir.exists( test_program_parent ) && ! dir.create( test_program_parent, recursive = TRUE) )
    testthat::fail( "Could not create test program parent directory" )
  
  test_program <- base::gsub( "\\\\", "/", base::tempfile( pattern = "test-program-", tmpdir = test_program_parent, fileext = ".R" ) )
  
  pgm <- c( "# test program",
            paste0( "cat( c( \"--->\", base::getwd(), \"<---\"), sep = \"\")") )
  
  base::writeLines( pgm, con = test_program )
  
  
  if ( ! file.exists( test_program ) )
    testthat::fail( "Unexpected test program does not exist" )
  
  test_program_ref <- base::substring( test_program, base::nchar(test_root) + 2 )
  
  
  # -- test
  result <- cxlib:::.cxlib_programexec( test_program_ref, options = list( "work.area" = test_root ) )


  # -- expected
  expected_program <- c( "path" = test_program_ref, 
                         "sha1" = digest::digest( test_program, algo = "sha1", file = TRUE) )
  
  expected_log <- c( "path" = paste0( tools::file_path_sans_ext( test_program_ref ), ".Rout"), 
                     "sha1" = digest::digest( paste0( tools::file_path_sans_ext( test_program ), ".Rout"), algo = "sha1", file = TRUE) )
  

  
  expected_result <- list( "program" = expected_program,
                           "log" = expected_log,
                           "files.input" = list( expected_program ),
                           "files.created" = list( expected_log ),
                           "files.updated" = list(),
                           "files.deleted" = list() )  
  
  
  
  # -- assertions
  
  # result  
  testthat::expect_equal( result[ names(expected_result) ], expected_result )
  

  # log
  testthat::expect_true( file.exists( file.path(test_root, result[["log"]][["path"]], fsep = "/")) ) 

  # verify test reference is in log
  log_lines <- base::readLines( con = file.path(test_root, result[["log"]][["path"]], fsep = "/") )
  testthat::expect_true( any( base::startsWith( log_lines, paste0( "--->", test_root, "<---" ) )) )


})






testthat::test_that( "programexec.programJobId", {
  
  
  # -- stage
  
  current_wd <- base::getwd()
  
  test_root <- base::gsub( "\\\\", "/", base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit({
    base::setwd( current_wd )
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Could not stage test root" )
  
  base::setwd( test_root )
  
  
  # test reference
  test_reference <- paste( base::sample( c( letters, LETTERS, as.character(0:9) ) , 40 ), collapse = "" )
  
  
  
  # test programs
  test_program <- base::gsub( "\\\\", "/", base::tempfile( pattern = "test-program-", tmpdir = test_root, fileext = ".R" ) )
  
  pgm <- c( "# test program",
            paste0( "cat( \"--->", test_reference, "<---\", sep = \"\")") )
  
  base::writeLines( pgm, con = test_program )
  
  
  if ( ! file.exists( test_program ) )
    testthat::fail( "Unexpected test program does not exist" )
  
  test_program_ref <- base::substring( test_program, base::nchar(test_root) + 2 )
  
  
  # test job id
  test_job <- cxlib:::.cxlib_referenceid( type = "uuid" )

  
  # -- test
  result <- cxlib:::.cxlib_programexec( base::basename( test_program_ref ), job.id = test_job )
  
  
  # -- expected
  expected_program <- c( "path" = base::basename( test_program_ref ), 
                         "sha1" = digest::digest( test_program, algo = "sha1", file = TRUE) )
  
  expected_log <- c( "path" = paste0( tools::file_path_sans_ext( test_program_ref ), ".Rout"), 
                     "sha1" = digest::digest( paste0( tools::file_path_sans_ext( test_program ), ".Rout"), algo = "sha1", file = TRUE) )
  
  expected_audit <- list( "inputs" = list( expected_program ),  
                          "created" = list( expected_log ),
                          "updated" = list(), "deleted" = list() )
  
  
  expected_result <- list( "program" = expected_program,
                           "log" = expected_log,
                           "files.input" = list( expected_program ),
                           "files.created" = list( expected_log ),
                           "files.updated" = list(),
                           "files.deleted" = list() )  
  
  
  # -- assertions
  
  # result  
  testthat::expect_equal( result[ names(expected_result) ], expected_result )
  
  # log
  testthat::expect_true( file.exists( file.path(test_root, result[["log"]][["path"]], fsep = "/") ) ) 
  
  # log content
  log_lines <- base::readLines( con = result[["log"]][["path"]] )
  
  # verify test reference is in log
  testthat::expect_true( any( base::startsWith( log_lines, paste0( "--->", test_reference, "<---" ) )) )
  
  # verify Job id is in log
  # note: expecting format '#> Job ID : <job ID>
  testthat::expect_true( any( grepl( paste0( "^#>\\s+job\\sid\\s*:\\s+", test_job, "$"), log_lines, perl = TRUE, ignore.case = TRUE ) ) )
  
})
