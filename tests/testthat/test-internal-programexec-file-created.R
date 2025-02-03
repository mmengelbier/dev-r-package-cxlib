#
#   Tests for cxlib:::.cxlib_programexec()
#
#   created filesed event
#


testthat::test_that( "programexec.filesCreated", {
  
  
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
  
  
  # test output area
  test_output_parent_ref <- file.path( "some", "path", "to", "outputs", fsep = "/" )
  
  if ( ! dir.exists( file.path( test_root, test_output_parent_ref, fsep = "/" ) ) && ! dir.create( file.path( test_root, test_output_parent_ref, fsep = "/" ), recursive = TRUE) )
    testthat::fail( "Could not create test output parent directory" )
  
  
  
  # test programs
  test_program_parent <- file.path( test_root, "some", "path", "to", "programs", fsep = "/" )
  
  if ( ! dir.exists( test_program_parent ) && ! dir.create( test_program_parent, recursive = TRUE) )
    testthat::fail( "Could not create test program parent directory" )
  
  test_program <- base::gsub( "\\\\", "/", base::tempfile( pattern = "test-program-", tmpdir = test_program_parent, fileext = ".R" ) )
  
  pgm <- c( "# test program",
            paste0( "base::writeLines( \"", test_reference, "\", con = \"", file.path( test_output_parent_ref, "output.txt", fsep = "/") , "\" )") )
  
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
  
  expected_created_files <- c( paste0( tools::file_path_sans_ext( test_program_ref ), ".Rout"), 
                               file.path( test_output_parent_ref, "output.txt", fsep = "/") )
  
  expected_created <- lapply( sort(expected_created_files), function(x) {
    c( "path" = x, 
       "sha1" = digest::digest( file.path(test_root, x, fsep = "/" ), algo = "sha1", file = TRUE ) )
  }) 
  
  
  expected_result <- list( "program" = expected_program,
                           "log" = expected_log,
                           "files.input" = list( expected_program ),
                           "files.created" = expected_created,
                           "files.updated" = list(),
                           "files.deleted" = list() )
  

  # -- assertions

  # result  
  testthat::expect_equal( result[ names(expected_result) ], expected_result )

  # log
  testthat::expect_true( file.exists( file.path(test_root, result[["log"]][["path"]], fsep = "/" ) ) )

  # verify test reference is in output
  output_lines <- base::readLines( con = file.path(test_root, test_output_parent_ref, "output.txt", fsep = "/" ) )
  testthat::expect_true( any( output_lines == test_reference ) )
  

})




testthat::test_that( "programexec.filesMultiCreated", {
  
  
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
  test_references <- replicate( 6, 
                                paste( base::sample( c( letters, LETTERS, as.character(0:9) ) , 40 ), collapse = "" ), 
                                simplify = TRUE )
  
  
  # test output area
  
  # will set up alternating output-1 and output-2 directories 
  test_output_parent_refs <- paste( file.path( "some", "path", "to", fsep = "/" ), rep( paste0( "output-", as.character(1:2)), 3), sep = "/" )
  
  for ( xpath in unique(test_output_parent_refs) )
    if ( ! dir.exists( file.path( test_root, xpath, fsep = "/" ) ) && ! dir.create( file.path( test_root, xpath, fsep = "/" ), recursive = TRUE) )
      testthat::fail( "Could not create test output parent directory" )

  
  # test programs
  test_program_parent <- file.path( test_root, "some", "path", "to", "programs", fsep = "/" )
  
  if ( ! dir.exists( test_program_parent ) && ! dir.create( test_program_parent, recursive = TRUE) )
    testthat::fail( "Could not create test program parent directory" )
  
  test_program <- base::gsub( "\\\\", "/", base::tempfile( pattern = "test-program-", tmpdir = test_program_parent, fileext = ".R" ) )
  
  pgm <- c( "# test program")
  
  for ( xref in test_references ) {
  
    xoutfile <- file.path( test_output_parent_refs[match( xref, test_references)], 
                           paste0( "output-", as.character(match( xref, test_references)), ".txt"), fsep = "/")
      
    pgm <- append( pgm, 
                   paste0( "base::writeLines( \"", xref, "\", con = \"", xoutfile , "\" )") ) 
  }


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
  
  expected_created_outfiles <- sapply( test_references, function(x) {
    file.path( test_output_parent_refs[match( x, test_references)], 
               paste0( "output-", as.character(match( x, test_references)), ".txt"), fsep = "/")
  }, USE.NAMES = FALSE )
  
  expected_created_files <- c( paste0( tools::file_path_sans_ext( test_program_ref ), ".Rout"), 
                               expected_created_outfiles )
  
  expected_created <- lapply( sort(expected_created_files), function(x) {
    c( "path" = x, 
       "sha1" = digest::digest( file.path(test_root, x, fsep = "/" ), algo = "sha1", file = TRUE ) )
  }) 
  

  
  expected_result <- list( "program" = expected_program,
                           "log" = expected_log,
                           "files.input" = list( expected_program ),
                           "files.created" = expected_created,
                           "files.updated" = list(),
                           "files.deleted" = list() )  
  
  # -- assertions

  # result  
  testthat::expect_equal( result[ names(expected_result) ], expected_result )
  

})



testthat::test_that( "programexec.filesMultiKeepCreated", {
  
  
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
  test_references <- replicate( 6, 
                                paste( base::sample( c( letters, LETTERS, as.character(0:9) ) , 40 ), collapse = "" ), 
                                simplify = TRUE )
  
  
  # test output area
  
  # will set up alternating output-1 and output-2 directories 
  test_output_parent_refs <- paste( file.path( "some", "path", "to", fsep = "/" ), rep( paste0( "output-", as.character(1:2)), 3), sep = "/" )
  
  for ( xpath in unique(test_output_parent_refs) )
    if ( ! dir.exists( file.path( test_root, xpath, fsep = "/" ) ) && ! dir.create( file.path( test_root, xpath, fsep = "/" ), recursive = TRUE) )
      testthat::fail( "Could not create test output parent directory" )
  
  
  # test programs
  test_program_parent <- file.path( test_root, "some", "path", "to", "programs", fsep = "/" )
  
  if ( ! dir.exists( test_program_parent ) && ! dir.create( test_program_parent, recursive = TRUE) )
    testthat::fail( "Could not create test program parent directory" )
  
  test_program <- base::gsub( "\\\\", "/", base::tempfile( pattern = "test-program-", tmpdir = test_program_parent, fileext = ".R" ) )
  
  pgm <- c( "# test program")
  
  for ( xref in test_references ) {
    
    xoutfile <- file.path( test_output_parent_refs[match( xref, test_references)], 
                           paste0( "output-", as.character(match( xref, test_references)), ".txt"), fsep = "/")
    
    pgm <- append( pgm, 
                   paste0( "base::writeLines( \"", xref, "\", con = \"", xoutfile , "\" )") ) 
  }
  
  # note: add delete action to first file in the list
  test_out_delfile_ref <- file.path( test_output_parent_refs[ 1 ], "output-1.txt", fsep = "/")
  
  pgm <- append( pgm, 
                 paste0( "base::unlink( \"", test_out_delfile_ref, "\" )") ) 
  
  
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
  
  expected_created_outfiles <- sapply( test_references, function(x) {
    file.path( test_output_parent_refs[match( x, test_references)], 
               paste0( "output-", as.character(match( x, test_references)), ".txt"), fsep = "/")
  }, USE.NAMES = FALSE )

  
  # note: dropping the file deleted in the program   
  expected_created_files <- c( paste0( tools::file_path_sans_ext( test_program_ref ), ".Rout"), 
                               expected_created_outfiles[ ! expected_created_outfiles %in% test_out_delfile_ref ] )

  expected_created <- lapply( sort(expected_created_files), function(x) {
    c( "path" = x, 
       "sha1" = digest::digest( file.path(test_root, x, fsep = "/" ), algo = "sha1", file = TRUE ) )
  }) 
  
  
  
  expected_result <- list( "program" = expected_program,
                           "log" = expected_log,
                           "files.input" = list( expected_program ),
                           "files.created" = expected_created,
                           "files.updated" = list(),
                           "files.deleted" = list() )  
  
  # -- assertions
  
  # result  
  testthat::expect_equal( result[ names(expected_result) ], expected_result )
  
  
})

