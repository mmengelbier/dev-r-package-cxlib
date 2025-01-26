#
#   tests for reading properties file
#

testthat::test_that( "propread.nothingDefaults", {
  
  # - test
  result <- cxlib::cxlib_propertiesread()
  
  # - assertions
  testthat::expect_equal( class(result), "character")
  testthat::expect_length( result, 0)
  
})



testthat::test_that( "propread.nullDefaults", {
  
  # - test
  result <- cxlib::cxlib_propertiesread( NULL )
  
  # - assertions
  testthat::expect_equal( class(result), "character")
  testthat::expect_length( result, 0)
  
})




testthat::test_that( "propread.NADefaults", {
  
  # - test
  result <- cxlib::cxlib_propertiesread( NA )
  
  # - assertions
  testthat::expect_equal( class(result), "character")
  testthat::expect_length( result, 0)
  
})




testthat::test_that( "propread.fileNotExist", {
  
  # - stage
  test_root <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  if ( ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Could not stage test root" )
  
  on.exit( {
    unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  
  # - test
  result <- cxlib::cxlib_propertiesread( file.path( test_root, "idonotexist.properties", fsep = "/") )
  
  
  # - assertions
  testthat::expect_equal( class(result), "character")
  testthat::expect_length( result, 0)
  
})





testthat::test_that( "propread.simpleCase", {
  
  # - stage
  test_root <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  if ( ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Could not stage test root" )
  
  on.exit( {
    unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  
  test_file <- file.path( test_root, "my.properties", fsep = "/" )
  
  base::writeLines( c( "A=123", "B=345"), con = test_file, sep = "\n" )
  
  
  # - test
  result <- cxlib::cxlib_propertiesread( test_file )
  
  
  # - expected results
  expected_props <- c( "a" = "123", "b" = "345" )
  
  # - assertions
  testthat::expect_equal( class(result), "character")
  
  testthat::expect_equal( sort(names(result)), sort(names(expected_props)) )
  testthat::expect_equal( result[ sort(names(result)) ], expected_props[ sort(names(expected_props)) ] )
  
})




testthat::test_that( "propread.simpleCaseComment", {
  
  # - stage
  test_root <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  if ( ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Could not stage test root" )
  
  on.exit( {
    unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  
  test_file <- file.path( test_root, "my.properties", fsep = "/" )
  
  base::writeLines( c( "A=123", " # B=345", "! B=09309809483", "C=0987654321"), con = test_file, sep = "\n" )
  
  
  # - test
  result <- cxlib::cxlib_propertiesread( test_file )
  
  
  # - expected results
  expected_props <- c( "a" = "123", "c" = "0987654321" )
  
  # - assertions
  testthat::expect_equal( class(result), "character")
  
  testthat::expect_equal( sort(names(result)), sort(names(expected_props)) )
  testthat::expect_equal( result[ sort(names(result)) ], expected_props[ sort(names(expected_props)) ] )
  
})


