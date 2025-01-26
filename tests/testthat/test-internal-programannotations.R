#
# Tests for internal function cxlib:::.programannotations()
#
#
#
#


testthat::test_that( "programanno.noParam", {
  
  testthat::expect_error( cxlib:::.cxlib_programannotations(), regexp = "^Missing or invalid program file reference$" )
  
})



testthat::test_that( "programanno.paramNull", {
  
  testthat::expect_error( cxlib:::.cxlib_programannotations( NULL ), regexp = "^Missing or invalid program file reference$" )
  
})


testthat::test_that( "programanno.paramNA", {
  
  testthat::expect_error( cxlib:::.cxlib_programannotations( NA ), regexp = "^Missing or invalid program file reference$" )
  
})


testthat::test_that( "programanno.paramNonCharacter", {
  
  testthat::expect_error( cxlib:::.cxlib_programannotations( 1 ), regexp = "^Missing or invalid program file reference$" )
  
})


testthat::test_that( "programanno.paramEmpty", {
  
  testthat::expect_error( cxlib:::.cxlib_programannotations( character(0) ), regexp = "^Missing or invalid program file reference$" )
  
})


testthat::test_that( "programanno.paramLengthNotOne", {
  
  testthat::expect_error( cxlib:::.cxlib_programannotations( character(10) ), regexp = "^Missing or invalid program file reference$" )
  
})



testthat::test_that( "programanno.paramEmpty", {
  
  # -- stage
  test_root <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" )) 
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )  
  
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Could not stage test root" )
  

  # test program that does not exist
  test_program <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "test-program-", tmpdir = test_root, fileext = ".R" ) )
  
  if ( file.exists( test_program ) )
    testthat::fail("Unexpected test program exists")
  
    
  # -- test
  testthat::expect_error( cxlib:::.cxlib_programannotations( test_program ), regexp = paste( "^Program file", test_program, "does not exist$", sep = " ") )
  
})




testthat::test_that( "programanno.programEmpty", {
  
  # -- stage
  test_root <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) ) 
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )  
  
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Could not stage test root" )
  
  
  # test program that does not exist
  test_program <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "test-program-", tmpdir = test_root, fileext = ".R" ) )
  
  writeLines( character(0), con = test_program )
  
  if ( ! file.exists( test_program ) )
    testthat::fail("Unexpected test program exists")
  
  
  # -- test
  result <- cxlib:::.cxlib_programannotations( test_program )
  
  
  # -- assertions
  testthat::expect_length( result, 0 )
  testthat::expect_equal( class(result), "list" )
  
})




testthat::test_that( "programanno.programNoAnnotations", {
  
  # -- stage
  test_root <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )  
  
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Could not stage test root" )
  
  
  # test program that does not exist
  test_program <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "test-program-", tmpdir = test_root, fileext = ".R" ) )
  
  writeLines( "# test program with no annotations", con = test_program )
  
  if ( ! file.exists( test_program ) )
    testthat::fail("Unexpected test program exists")
  
  
  # -- test
  result <- cxlib:::.cxlib_programannotations( test_program )
  
  
  # -- assertions
  testthat::expect_length( result, 0 )
  testthat::expect_equal( class(result), "list" )
  
})




testthat::test_that( "programanno.programSingleAnnotation", {
  
  # -- stage
  test_root <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )  
  
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Could not stage test root" )
  
  
  # test program that does not exist
  test_program <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "test-program-", tmpdir = test_root, fileext = ".R" ) )
  
  pgm_lines <- c( "# test program with no annotations",
                  "@abc 123",
                  "# @cx.def 456" )
  
  writeLines( pgm_lines, con = test_program )
  
  if ( ! file.exists( test_program ) )
    testthat::fail("Unexpected test program exists")
  
  
  # -- test
  result <- cxlib:::.cxlib_programannotations( test_program )
  
  
  # -- assertions
  testthat::expect_equal( class(result), "list" )
  testthat::expect_length( result, 1 )
  
  testthat::expect_equal( names(result), "def" )
  testthat::expect_length( result[["def"]], 1 )
  testthat::expect_equal( result[["def"]], "456" )
  
})




testthat::test_that( "programanno.programSingleAnnotationIgnoreNonPrefix", {
  
  # -- stage
  test_root <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )  
  
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Could not stage test root" )
  
  
  # test program that does not exist
  test_program <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "test-program-", tmpdir = test_root, fileext = ".R" ) )
  
  pgm_lines <- c( "# test program with no annotations",
                  "# @abc 123",
                  "# @cx.def 456" )
  
  writeLines( pgm_lines, con = test_program )
  
  if ( ! file.exists( test_program ) )
    testthat::fail("Unexpected test program exists")
  
  
  # -- test
  result <- cxlib:::.cxlib_programannotations( test_program )
  
  
  # -- assertions
  testthat::expect_equal( class(result), "list" )
  testthat::expect_length( result, 1 )
  
  testthat::expect_equal( names(result), "def" )
  testthat::expect_length( result[["def"]], 1 )
  testthat::expect_equal( result[["def"]], "456" )
  
})



testthat::test_that( "programanno.programSingleAnnotationKewordMultipleValues", {
  
  # -- stage
  test_root <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )  
  
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Could not stage test root" )
  
  
  # test program that does not exist
  test_program <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "test-program-", tmpdir = test_root, fileext = ".R" ) )
  
  pgm_lines <- c( "# test program with no annotations",
                  "  # @cx.def 123",
                  "# @cx.def 456" )
  
  writeLines( pgm_lines, con = test_program )
  
  if ( ! file.exists( test_program ) )
    testthat::fail("Unexpected test program exists")
  
  
  # -- test
  result <- cxlib:::.cxlib_programannotations( test_program )
  
  
  # -- assertions
  testthat::expect_equal( class(result), "list" )
  testthat::expect_length( result, 1 )
  
  testthat::expect_equal( names(result), "def" )
  testthat::expect_length( result[["def"]], 2 )
  testthat::expect_equal( result[["def"]], c( "123", "456") )
  
})


testthat::test_that( "programanno.programMultiAnnotationKewordMultipleValues", {
  
  # -- stage
  test_root <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )  
  
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Could not stage test root" )
  
  
  # test program that does not exist
  test_program <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "test-program-", tmpdir = test_root, fileext = ".R" ))
  
  pgm_lines <- c( "# test program with no annotations",
                  "  # @cx.def 123",
                  "# @cx.def 456", 
                  "# @cx.ghi lorem ipsum",
                  "# @cx.ghi ipsum")
  
  writeLines( pgm_lines, con = test_program )
  
  if ( ! file.exists( test_program ) )
    testthat::fail("Unexpected test program exists")
  
  
  # -- test
  result <- cxlib:::.cxlib_programannotations( test_program )


  # -- expected
  expected_annotations <- list( "def" = c( "123", "456"),
                                "ghi" = c( "lorem ipsum", "ipsum") )


  # -- assertions
  testthat::expect_equal( class(result), "list" )
  testthat::expect_length( result, 2 )
  
  testthat::expect_equal( sort(names(result)), sort(c( "def", "ghi")) )

  # note: tests any order of the annotations keywords but specific order within each annotation
  testthat::expect_equal( result[ sort(names(expected_annotations)) ], expected_annotations[ sort(names(expected_annotations)) ] )

})

