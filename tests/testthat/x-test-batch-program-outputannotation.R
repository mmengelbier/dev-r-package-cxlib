#
#
#  Tests for cxlib::cxlib_batch()
#
#  Output annotations
#
#

testthat::test_that( "batch.programAnnotationSimpleOutput", {
  
  
  # -- stage
  
  #    test area
  test_root <- base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" )
  
  on.exit( {
    unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE)
  
  if ( ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Cannot stage test area" )
  
  #    set working directory
  cur_wd <- base::getwd()
  
  on.exit({
    base::setwd( cur_wd )
  }, add = TRUE)
  
  base::setwd( test_root )
  
  
  #    stage an output location
  test_out_value <- paste( sample(letters, 20, replace = TRUE), collapse = "" )
  
  test_outfile <- base::tempfile( pattern = "test-data-", tmpdir = file.path( test_root, "some", "output") , fileext = ".txt" )
  
  if ( ! dir.exists(dirname(test_outfile)) && ! dir.create( dirname(test_outfile), recursive = TRUE ) )
    testthat::fail( "Could not stage parent directory for test output file" )
  

  test_out <- base::substring( test_outfile, base::nchar(test_root) + 2 )
  
  
  #    add program file
  test_pgmfile <- base::tempfile( pattern = "test-program-", tmpdir = file.path( test_root, "some", "path") , fileext = ".R" )
  
  if ( ! dir.exists(dirname(test_pgmfile)) && ! dir.create( dirname(test_pgmfile), recursive = TRUE ) )
    testthat::fail( "Could not stage parent directory for test program" )
  
  # test program is 
  #
  # # test file
  # # @output <test data file path relative to root>
  # base::writeLines( "<test output data value>", con = "<output file>" )
  #
  
  test_out_dir <- dirname(test_out)
  
  pgm <- c( "# test file", 
            paste( "# @output", test_out_dir, sep = " "),
            paste0( "base::writeLines( \"", test_out_value, "\", con = \"", test_out, "\" )" )
  )
  
  base::writeLines( pgm , con = test_pgmfile )
  
  
  #    test program reference
  test_program <- base::substring( test_pgmfile, base::nchar(test_root) + 1 )
  
  
  # -- test
  result <- cxlib::cxlib_batch( test_program, silent = TRUE )


  
  # -- expectations
  expect_log <- file.path( dirname( test_pgmfile), paste0( tools::file_path_sans_ext( basename(test_pgmfile) ), ".Rout" ) )
  
  #    library paths
  expect_libs <- c( .Library.site, .Library )
  
  if ( ! is.na(Sys.getenv( "R_LIBS_USER", unset = NA) ) ) 
    expect_libs <- append( unlist(strsplit(Sys.getenv( "R_LIBS_USER"), .Platform$path.sep )), expect_libs )
  
  
  #    expected result
  expected_result <- list( "program" = c( "path" = test_pgmfile, "sha1" = digest::digest( test_pgmfile, algo = "sha1", file = TRUE) ),
                           "log" = c( "path" = expect_log, "sha1" = digest::digest( expect_log, algo = "sha1", file = TRUE) ),
                           "library.paths" = expect_libs, 
                           "inputs" = list(),
                           "outputs" = list()
  )
  
  expected_result[["outputs"]][[test_out]] <- c( "path" = test_outfile, "sha1" = digest::digest( test_outfile, algo = "sha1", file = TRUE) )
  

  #    expected files
  expected_files <- c( test_outfile, test_pgmfile, expect_log )
  
  
  #    expected data line
  expected_data <- test_out_value
  
  
  # -- assertions
  testthat::expect_true( file.exists( expect_log ) )
  testthat::expect_equal( sort(list.files(test_root, full.names = TRUE, recursive = TRUE)), sort(expected_files) )
  testthat::expect_equal( result[ names(expected_result)], expected_result )  
  
  
  #    check data value in output file
  testthat::expect_equal( base::readLines(test_outfile), test_out_value )
  
})





testthat::test_that( "batch.programAnnotationOutputLeadingSlash", {
  
  
  # -- stage
  
  #    test area
  test_root <- base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" )
  
  on.exit( {
    unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE)
  
  if ( ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Cannot stage test area" )
  
  #    set working directory
  cur_wd <- base::getwd()
  
  on.exit({
    base::setwd( cur_wd )
  }, add = TRUE)
  
  base::setwd( test_root )
  
  
  #    stage an output location
  test_out_value <- paste( sample(letters, 20, replace = TRUE), collapse = "" )
  
  test_outfile <- base::tempfile( pattern = "test-data-", tmpdir = file.path( test_root, "some", "output") , fileext = ".txt" )
  
  if ( ! dir.exists(dirname(test_outfile)) && ! dir.create( dirname(test_outfile), recursive = TRUE ) )
    testthat::fail( "Could not stage parent directory for test output file" )
  
  
  test_out <- base::substring( test_outfile, base::nchar(test_root) + 1 )
  
  
  #    add program file
  test_pgmfile <- base::tempfile( pattern = "test-program-", tmpdir = file.path( test_root, "some", "path") , fileext = ".R" )
  
  if ( ! dir.exists(dirname(test_pgmfile)) && ! dir.create( dirname(test_pgmfile), recursive = TRUE ) )
    testthat::fail( "Could not stage parent directory for test program" )
  
  # test program is 
  #
  # # test file
  # # @output <test data file path relative to root>
  # base::writeLines( "<test output data value>", con = "<output file>" )
  #
  
  test_out_dir <- dirname(test_out)
  
  pgm <- c( "# test file", 
            paste( "# @output", test_out_dir, sep = " "),
            paste0( "base::writeLines( \"", test_out_value, "\", con = \"", test_out, "\" )" )
  )
  
  base::writeLines( pgm , con = test_pgmfile )
  
  
  #    test program reference
  test_program <- base::substring( test_pgmfile, base::nchar(test_root) + 1 )
  
  
  # -- test
  testthat::expect_error( cxlib::cxlib_batch( test_program, silent = TRUE ), 
                          regexp = "^Paths in annotations are relative paths and cannot start with a leading slash \\('/'\\)$" )
  

})
