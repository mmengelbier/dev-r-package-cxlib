#
#
#  Tests for cxlib::cxlib_batch()
#
#  Input annotations
#
#


testthat::test_that( "batch.programAnnotationSingleInputFile", {
  
  
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
  
  
  #    stage an input file
  test_data_value <- paste( sample(letters, 20, replace = TRUE), collapse = "" )
  
  test_datafile <- base::tempfile( pattern = "test-data-", tmpdir = file.path( test_root, "some", "data") , fileext = ".txt" )
  
  if ( ! dir.exists(dirname(test_datafile)) && ! dir.create( dirname(test_datafile), recursive = TRUE ) )
    testthat::fail( "Could not stage parent directory for test data file" )
  
  base::writeLines( test_data_value, con = test_datafile )
  
  test_data <- base::substring( test_datafile, base::nchar(test_root) + 2 )
  
  
  #    add program file
  test_pgmfile <- base::tempfile( pattern = "test-program-", tmpdir = file.path( test_root, "some", "path") , fileext = ".R" )
  
  if ( ! dir.exists(dirname(test_pgmfile)) && ! dir.create( dirname(test_pgmfile), recursive = TRUE ) )
    testthat::fail( "Could not stage parent directory for test program" )
  
  # test program is 
  #
  # # test file
  # # @input <test data file path relative to root>
  # cat( c( "--->", readLines( "<data file>" ), "<---" ), sep = "" )
  #
  
  pgm <- c( "# test file", 
            paste( "# @input", test_data, sep = " "),
            paste0( "cat( paste0( c( \"--->\", readLines( \"", test_data, "\"), \"<---\"), collapse = \"\"), sep = \"\\n\" )" )
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
  
  expected_result[["inputs"]][[ test_data ]] <- c( "path" = test_datafile, "sha1" = digest::digest( test_datafile, algo = "sha1", file = TRUE) )  
  
  
  #    expected files
  expected_files <- c( test_datafile, test_pgmfile, expect_log )
  
  
  #    expected data line
  expected_data <- paste0( "--->", test_data_value, "<---" )
  
  
  # -- assertions
  testthat::expect_true( file.exists( expect_log ) )
  testthat::expect_equal( sort(list.files(test_root, full.names = TRUE, recursive = TRUE)), sort(expected_files) )
  testthat::expect_equal( result[ names(expected_result)], expected_result )  
  
  
  #    check data value in log
  testthat::expect_true( any(grepl( paste0( "^", expected_data, "$"), base::readLines(expect_log), ignore.case = FALSE, perl = TRUE )) )
  
})




testthat::test_that( "batch.programAnnotationSingleInputAsDirectory", {
  
  
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
  
  
  #    stage an input file
  test_data_value <- paste( sample(letters, 20, replace = TRUE), collapse = "" )
  
  test_datafile <- base::tempfile( pattern = "test-data-", tmpdir = file.path( test_root, "some", "data") , fileext = ".txt" )
  
  if ( ! dir.exists(dirname(test_datafile)) && ! dir.create( dirname(test_datafile), recursive = TRUE ) )
    testthat::fail( "Could not stage parent directory for test data file" )
  
  base::writeLines( test_data_value, con = test_datafile )
  
  test_data <- base::substring( test_datafile, base::nchar(test_root) + 2 )
  
  
  #    add program file
  test_pgmfile <- base::tempfile( pattern = "test-program-", tmpdir = file.path( test_root, "some", "path") , fileext = ".R" )
  
  if ( ! dir.exists(dirname(test_pgmfile)) && ! dir.create( dirname(test_pgmfile), recursive = TRUE ) )
    testthat::fail( "Could not stage parent directory for test program" )
  
  # test program is 
  #
  # # test file
  # # @input <test data file path relative to root>
  # cat( c( "--->", readLines( "<data file>" ), "<---" ), sep = "" )
  #
  
  pgm <- c( "# test file", 
            paste( "# @input", dirname(test_data), sep = " "),
            paste0( "cat( paste0( c( \"--->\", readLines( \"", test_data, "\"), \"<---\"), collapse = \"\"), sep = \"\\n\" )" )
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
  
  expected_result[["inputs"]][[ test_data ]] <- c( "path" = test_datafile, "sha1" = digest::digest( test_datafile, algo = "sha1", file = TRUE) )  
  
  
  #    expected files
  expected_files <- c( test_datafile, test_pgmfile, expect_log )
  
  
  #    expected data line
  expected_data <- paste0( "--->", test_data_value, "<---" )
  
  
  # -- assertions
  testthat::expect_true( file.exists( expect_log ) )
  testthat::expect_equal( sort(list.files(test_root, full.names = TRUE, recursive = TRUE)), sort(expected_files) )
  testthat::expect_equal( result[ names(expected_result)], expected_result )  
  
  
  #    check data value in log
  testthat::expect_true( any(grepl( paste0( "^", expected_data, "$"), base::readLines(expect_log), ignore.case = FALSE, perl = TRUE )) )
  
})




testthat::test_that( "batch.programAnnotationInputAbsolutePath", {
  
  
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
  
  
  #    stage an input file
  test_data_value <- paste( sample(letters, 20, replace = TRUE), collapse = "" )
  
  test_datafile <- base::tempfile( pattern = "test-data-", tmpdir = file.path( test_root, "some", "data") , fileext = ".txt" )
  
  if ( ! dir.exists(dirname(test_datafile)) && ! dir.create( dirname(test_datafile), recursive = TRUE ) )
    testthat::fail( "Could not stage parent directory for test data file" )
  
  base::writeLines( test_data_value, con = test_datafile )
  
  test_data <- base::substring( test_datafile, base::nchar(test_root) + 1 )
    
  
  #    add program file
  test_pgmfile <- base::tempfile( pattern = "test-program-", tmpdir = file.path( test_root, "some", "path") , fileext = ".R" )
  
  if ( ! dir.exists(dirname(test_pgmfile)) && ! dir.create( dirname(test_pgmfile), recursive = TRUE ) )
    testthat::fail( "Could not stage parent directory for test program" )
  
  # test program is 
  #
  # # test file
  # # @input <test data file path relative to root>
  # cat( c( "--->", readLines( "<data file>" ), "<---" ), sep = "" )
  #
  
  pgm <- c( "# test file", 
            paste( "# @input", test_data, sep = " "),
            paste0( "cat( paste0( c( \"--->\", readLines( \"", base::substring( test_data, 2 ), "\"), \"<---\"), collapse = \"\"), sep = \"\\n\" )" )
          )
  
  base::writeLines( pgm , con = test_pgmfile )
  
  
  #    test program reference
  test_program <- base::substring( test_pgmfile, base::nchar(test_root) + 1 )
  
  
  # -- test
  
  testthat::expect_error( cxlib::cxlib_batch( test_program, silent = TRUE ), 
                          regexp = "^Paths in annotations are relative paths and cannot start with a leading slash \\('/'\\)$" )
  

})
