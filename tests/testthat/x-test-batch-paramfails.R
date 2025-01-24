#
#
#  Tests for cxlib::cxlib_batch()
#
#  Parameter fails
#
#


testthat::test_that( "batch.noParms", {
  
  testthat::expect_error( cxlib::cxlib_batch(), regexp = "^Program x invalid$" )
  
})


testthat::test_that( "batch.programNULL", {
  
  testthat::expect_error( cxlib::cxlib_batch( NULL ), regexp = "^Program x invalid$" )
  
})


testthat::test_that( "batch.programNA", {
  
  testthat::expect_error( cxlib::cxlib_batch( NA ), regexp = "^Program x invalid$" )
  
})


testthat::test_that( "batch.programEmptyString", {
  
  testthat::expect_error( cxlib::cxlib_batch( "  " ), regexp = "^Program x invalid$" )
  
})



testthat::test_that( "batch.programNotCharacter", {
  
  testthat::expect_error( cxlib::cxlib_batch( 1 ), regexp = "^Program x invalid$" )
  
})


testthat::test_that( "batch.programVector", {
  
  testthat::expect_error( cxlib::cxlib_batch( c("a", "b") ), regexp = "^Program x invalid$" )
  
})


testthat::test_that( "batch.programAnyNA", {
  
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
  
  
  #    add file
  test_pgmfile <- base::tempfile( pattern = "test-program-", tmpdir = test_root, fileext = ".R" )
  base::writeLines("# test file", con = test_pgmfile )
  
  
  #    test program reference
  test_program <- base::substring( test_pgmfile, base::nchar(test_root) + 1 )

    
  
  # -- test & assertion
  testthat::expect_error( cxlib::cxlib_batch( c("NA", test_program) ), regexp = "^Program x invalid$" )
  
})




testthat::test_that( "batch.programNotExist", {
  
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
  
  
  #    file reference
  test_pgmfile <- base::tempfile( pattern = "test-program-", tmpdir = test_root, fileext = ".R" )
  
  if ( file.exists( test_pgmfile ) )
    testthat::fail( "Test file unexpectedly exists" )
  
  
  test_program <- base::substring( test_pgmfile, base::nchar(test_root) + 1 )

  
  # -- test & assertion
  testthat::expect_error( cxlib::cxlib_batch( test_program ), regexp = paste( "^Program file", test_pgmfile, "does not exist or is a directory$" ) )
  
})



testthat::test_that( "batch.programReferenceIsDirectory", {
  
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
  
  
  #    add file
  test_pgmfile <- base::tempfile( pattern = "test-program-", tmpdir = test_root, fileext = ".R" )
  
  if ( ! dir.create( test_pgmfile, recursive = TRUE ) )
    testthat::fail( "Could not stage test file as a directory" )
  
  
  #    test program reference
  test_program <- base::substring( test_pgmfile, base::nchar(test_root) + 1 )
  
  
  
  # -- test & assertion
  testthat::expect_error( cxlib::cxlib_batch( test_program ), regexp = paste( "^Program file", test_pgmfile, "does not exist or is a directory$" ) )
  
})







