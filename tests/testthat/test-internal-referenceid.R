#
#  Tests for cxlib:::.cxlib_referenceid()
#
#
#


testthat::test_that( "referenceid.typeNull", {
  
  # -- stage
  
  # look at uniqueness across 10 000 IDs
  test_length <- 10000

  
  # -- test
  result <- replicate( test_length, 
                       cxlib:::.cxlib_referenceid( type = NULL ), 
                       simplify = TRUE)
  
  

  # -- expected
  
  expected_length <- test_length
  
  # note: format is 5 repeating blocks of 8 characters a-z and digits 0-9 delimited by a dash
  expected_format <- "^([a-f0-9]{8}-){4}[a-f0-9]{8}$"
  
  # -- assertions
  
  # uniqueness
  testthat::expect_equal( length(base::unique(result)), expected_length )
  
  # format
  testthat::expect_true(all( grepl( expected_format, result) ))
  
  
})



testthat::test_that( "referenceid.typeSHA", {
  
  # -- stage
  
  # look at uniqueness across 10 000 IDs
  test_length <- 10000
  
  
  # -- test
  result <- replicate( test_length, 
                       cxlib:::.cxlib_referenceid( type = "sha" ), 
                       simplify = TRUE)
  
  
  
  # -- expected
  
  expected_length <- test_length
  
  # note: format is a squence of 40 characters a-z and digits 0-9 
  expected_format <- "^[a-f0-9]{40}$"
  
  # -- assertions
  
  # uniqueness
  testthat::expect_equal( length(base::unique(result)), expected_length )
  
  # format
  testthat::expect_true(all( grepl( expected_format, result) ))
  
  
})




testthat::test_that( "referenceid.typeRaw", {
  
  # -- stage
  
  # look at uniqueness across 10 000 IDs
  test_length <- 10000
  
  
  # -- test
  result <- replicate( test_length, 
                       cxlib:::.cxlib_referenceid( type = "raw" ), 
                       simplify = TRUE)
  
  
  
  # -- expected
  
  expected_length <- test_length
  
  # note: format is a squence of 40 characters a-z and digits 0-9 
  expected_format <- "^[a-f0-9]{40}$"
  
  # -- assertions
  
  # uniqueness
  testthat::expect_equal( length(base::unique(result)), expected_length )
  
  # format
  testthat::expect_true(all( grepl( expected_format, result) ))
  
  
})





testthat::test_that( "referenceid.typeUUID", {
  
  # -- stage
  
  # look at uniqueness across 10 000 IDs
  test_length <- 10000
  
  
  # -- test
  result <- replicate( test_length, 
                       cxlib:::.cxlib_referenceid( type = "uuid" ), 
                       simplify = TRUE)
  
  
  
  # -- expected
  
  expected_length <- test_length
  
  # note: format is a squence of 40 characters a-z and digits 0-9 
  # example: bcf1321b-7560-4124-8f29-8f6f3c35296f
  expected_format <- "^[a-f0-9]{8}-([a-f0-9]{4}-){3}[a-f0-9]{12}$"
  
  # -- assertions
  
  # uniqueness
  testthat::expect_equal( length(base::unique(result)), expected_length )
  
  # format
  testthat::expect_true(all( grepl( expected_format, result) ))
  
  
})
