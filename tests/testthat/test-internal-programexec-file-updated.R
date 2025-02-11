#
#   Tests for cxlib:::.cxlib_programexec()
#
#   updated audited event
#




testthat::test_that( "programexec.auditUpdated", {
  
  
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
  
  # stage outputs
  base::writeLines( paste( base::sample( c( letters, LETTERS, as.character(0:9) ) , 40 ), collapse = "" ), 
                    con = file.path( test_root, test_output_parent_ref, "output.txt", fsep = "/" ) )
  
  if ( ! file.exists( file.path( test_root, test_output_parent_ref, "output.txt", fsep = "/") ) )
    testthat::fail( "Could not stage output file" )
  

  
  # test programs
  test_program_parent <- file.path( test_root, "some", "path", "to", "programs", fsep = "/" )
  
  if ( ! dir.exists( test_program_parent ) && ! dir.create( test_program_parent, recursive = TRUE) )
    testthat::fail( "Could not create test program parent directory" )
  
  test_program <- base::gsub( "\\\\", "/", base::tempfile( pattern = "test-program-", tmpdir = test_program_parent, fileext = ".R" ) )
  
  pgm <- c( "# test program",
            paste0( "base::writeLines( \"", test_reference, "\", con = \"", file.path( test_output_parent_ref, "output.txt", fsep = "/"), "\" )") )
  
  base::writeLines( pgm, con = test_program )
  
  
  if ( ! file.exists( test_program ) )
    testthat::fail( "Unexpected test program does not exist" )
  
  test_program_ref <- base::substring( test_program, base::nchar(test_root) + 2 )
  
  
  # reference sha1
  ref_sha1 <- sapply( list.files( test_root, recursive = TRUE, full.names = FALSE, include.dirs = FALSE ), function(x) {
    digest::digest( file.path( test_root, x, fsep = "/"), algo = "sha1", file = TRUE ) 
  }, USE.NAMES = TRUE )
  
  

  
  # -- test
  result <- cxlib:::.cxlib_programexec( test_program_ref, options = list( "work.area" = test_root ) )
  

  # -- expected
  
  expected_program <- c( "path" = test_program_ref, 
                         "sha1" = unname(ref_sha1[ test_program_ref ]) )  
  
  expected_log <- c( "path" = paste0( tools::file_path_sans_ext( test_program_ref ), ".Rout"), 
                     "sha1" = digest::digest( paste0( tools::file_path_sans_ext( test_program ), ".Rout"), algo = "sha1", file = TRUE) )

    
  # input files
  expected_inputs <- lapply( sort( names(ref_sha1) ), function(x) {
    c( "path" = x, 
       "sha1" = unname(ref_sha1[x]) )
  })
  

  # created    
  expected_created <- list( expected_log )
  
  
  # updated
  expected_updated <- list( c( "path" = file.path( test_output_parent_ref, "output.txt", fsep = "/"), 
                               "sha1" = digest::digest( file.path( test_root, test_output_parent_ref, "output.txt", fsep = "/"), algo = "sha1", file = TRUE ) ) )
  


  
  expected_result <- list( "program" = expected_program,
                           "log" = expected_log,
                           "files.input" = expected_inputs,
                           "files.created" = expected_created,
                           "files.updated" = expected_updated,
                           "files.deleted" = list() )  
  
  # -- assertions
  
  # result  
  testthat::expect_equal( result[ names(expected_result) ], expected_result )

  # verify test reference is in output
  output_lines <- base::readLines( con = file.path(test_root, test_output_parent_ref, "output.txt", fsep = "/" ) )
  testthat::expect_true( any( output_lines == test_reference ) )
  

})




testthat::test_that( "programexec.auditMultiUpdated", {
  
  
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

  
  
  # stage outputs
  for ( xref in test_references ) {
    
    out_file <- file.path( test_root, 
                           test_output_parent_refs[ base::match( xref, test_references ) ], 
                           paste0( "output-", as.character(base::match( xref, test_references )), ".txt"),
                           fsep = "/" )
    
    base::writeLines( xref, con = out_file )
  
    if ( ! file.exists( out_file ) )
      testthat::fail( "Could not stage output file" )
    
  }
  

  
  # test programs
  test_program_parent <- file.path( test_root, "some", "path", "to", "programs", fsep = "/" )
  
  if ( ! dir.exists( test_program_parent ) && ! dir.create( test_program_parent, recursive = TRUE) )
    testthat::fail( "Could not create test program parent directory" )
  
  test_program <- base::gsub( "\\\\", "/", base::tempfile( pattern = "test-program-", tmpdir = test_program_parent, fileext = ".R" ) )
  
  
  test_files_to_be_updated <- sapply( utils::head(test_references, n = 4 ), function(x) {
    file.path( test_output_parent_refs[ base::match( x, test_references ) ], 
               paste0( "output-", as.character(base::match( x, test_references )), ".txt"),
               fsep = "/" )  
  }, USE.NAMES = FALSE )

  
  pgm <- c( "# test program")
  
  for ( xpath in test_files_to_be_updated )
    pgm <- append( pgm, 
                   paste0( "base::writeLines( paste( base::sample( c( letters, LETTERS, as.character(0:9) ) , 40 ), collapse = \"\" ), con = \"", xpath , "\" )") ) 
  
  base::writeLines( pgm, con = test_program )
  


  if ( ! file.exists( test_program ) )
    testthat::fail( "Unexpected test program does not exist" )

  
  test_program_ref <- base::substring( test_program, base::nchar(test_root) + 2 )
  
  
  # reference sha1
  ref_sha1 <- sapply( list.files( test_root, recursive = TRUE, full.names = FALSE, include.dirs = FALSE ), function(x) {
    digest::digest( file.path( test_root, x, fsep = "/"), algo = "sha1", file = TRUE ) 
  }, USE.NAMES = TRUE )
  
  
  
  
  # -- test
  result <- cxlib:::.cxlib_programexec( test_program_ref, options = list( "work.area" = test_root ) )

  
  # -- expected

  expected_program <- c( "path" = test_program_ref,
                         "sha1" = unname(ref_sha1[ test_program_ref ]) )

  expected_log <- c( "path" = paste0( tools::file_path_sans_ext( test_program_ref ), ".Rout"),
                     "sha1" = digest::digest( paste0( tools::file_path_sans_ext( test_program ), ".Rout"), algo = "sha1", file = TRUE) )


  # input files
  expected_inputs <- lapply( sort( names(ref_sha1) ), function(x) {
    c( "path" = x,
       "sha1" = unname(ref_sha1[x]) )
  })
  
  # # created    
  expected_created <- list( expected_log )


  # updated
  expected_updated <- lapply( sort( test_files_to_be_updated ), function(x) {
    c( "path" = x,
       "sha1" = digest::digest( file.path( test_root, x, fsep = "/"), algo = "sha1", file = TRUE) )
  })
  

  expected_result <- list( "program" = expected_program,
                           "log" = expected_log,
                           "files.input" = expected_inputs,
                           "files.created" = expected_created,
                           "files.updated" = expected_updated,
                           "files.deleted" = list() )  

  # -- assertions

  # result  
  testthat::expect_equal( result[ names(expected_result) ], expected_result )
  
  
  # program
  testthat::expect_equal( unname(result[["program"]]["sha1"]),
                          digest::digest( file.path( test_root, result[["program"]]["path"], fsep = "/" ), algo = "sha1", file = TRUE ) )


  # expected output files have been updated
  for ( xpath in test_files_to_be_updated )
    testthat::expect_false( ( digest::digest( file.path( test_root, xpath, fsep = "/"), algo = "sha1", file = TRUE ) == ref_sha1[ xpath ] ) )
  
  
})
