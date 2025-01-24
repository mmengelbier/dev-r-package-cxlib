#
#
#  Tests for cxlib::cxlib_batch()
#
#  Program execution environment
#
#

testthat::test_that( "batch.programEmptyDefaults", {
  
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
  test_pgmfile <- base::tempfile( pattern = "test-program-", tmpdir = file.path( test_root, "some", "path") , fileext = ".R" )
  
  if ( ! dir.exists(dirname(test_pgmfile)) && ! dir.create( dirname(test_pgmfile), recursive = TRUE ) )
    testthat::fail( "Could not stage parent directory for test program" )
  
  base::writeLines("# test file", con = test_pgmfile )
  
  
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

  #    expected files
  expected_files <- c( test_pgmfile, expect_log )
  

  # -- assertions
  testthat::expect_true( file.exists( expect_log ) )
  testthat::expect_equal( sort(list.files(test_root, full.names = TRUE, recursive = TRUE)), sort(expected_files) )
  testthat::expect_equal( result[ names(expected_result)], expected_result )  
  
      
})

  

testthat::test_that( "batch.programEmptyAltLogfileName", {
  
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
  test_pgmfile <- base::tempfile( pattern = "test-program-", tmpdir = file.path( test_root, "some", "path") , fileext = ".R" )
  
  if ( ! dir.exists(dirname(test_pgmfile)) && ! dir.create( dirname(test_pgmfile), recursive = TRUE ) )
    testthat::fail( "Could not stage parent directory for test program" )
  
  base::writeLines("# test file", con = test_pgmfile )
  
  
  #    test program reference
  test_program <- base::substring( test_pgmfile, base::nchar(test_root) + 1 )

  #    log file reference
  test_log <- base::tempfile( pattern = "alt-log-", tmpdir = base::dirname(test_program), fileext = ".log"  )
  
  # -- test
  result <- cxlib::cxlib_batch( test_program, log.file = test_log, silent = TRUE )
  
  # -- expectations
  expect_log <- gsub( "//", "/", file.path( test_root, test_log ) )
  
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
  
  #    expected files
  expected_files <- c( test_pgmfile, expect_log )
  
  
  # -- assertions
  testthat::expect_true( file.exists( expect_log ) )
  testthat::expect_equal( sort(list.files(test_root, full.names = TRUE, recursive = TRUE)), sort(expected_files) )
  testthat::expect_equal( result[ names(expected_result)], expected_result )  
  
  
})





testthat::test_that( "batch.programEmptyAltLogDirectory", {
  
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
  test_pgmfile <- base::tempfile( pattern = "test-program-", tmpdir = file.path( test_root, "some", "path") , fileext = ".R" )
  
  if ( ! dir.exists(dirname(test_pgmfile)) && ! dir.create( dirname(test_pgmfile), recursive = TRUE ) )
    testthat::fail( "Could not stage parent directory for test program" )
  
  base::writeLines("# test file", con = test_pgmfile )
  
  
  #    test program reference
  test_program <- base::substring( test_pgmfile, base::nchar(test_root) + 1 )
  
  
  #    add log directory
  test_logdir <- base::tempfile( pattern = "logs-", tmpdir = base::dirname(base::dirname( test_pgmfile )), fileext = "" )
  
  if ( ! dir.exists(test_logdir) && ! dir.create( test_logdir, recursive = TRUE) )
    testthat::fail("Could not stage log directory")
  
  test_log <- base::substring( test_logdir, base::nchar(test_root) + 1 )
  
  
  # -- test
  result <- cxlib::cxlib_batch( test_program, log.file = test_log, silent = TRUE )
  
  
  # -- expectations
  expect_log <- file.path( test_logdir, paste0( tools::file_path_sans_ext( basename(test_pgmfile) ), ".Rout" ) )
  
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
  
  #    expected files
  expected_files <- c( test_pgmfile, expect_log )
  
  
  # -- assertions
  testthat::expect_true( file.exists( expect_log ) )
  testthat::expect_equal( sort(list.files(test_root, full.names = TRUE, recursive = TRUE)), sort(expected_files) )
  testthat::expect_equal( result[ names(expected_result)], expected_result )  
  
  
})




testthat::test_that( "batch.programEmptyDisableUserLibrary", {
  
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
  test_pgmfile <- base::tempfile( pattern = "test-program-", tmpdir = file.path( test_root, "some", "path") , fileext = ".R" )
  
  if ( ! dir.exists(dirname(test_pgmfile)) && ! dir.create( dirname(test_pgmfile), recursive = TRUE ) )
    testthat::fail( "Could not stage parent directory for test program" )
  
  base::writeLines("# test file", con = test_pgmfile )
  
  
  #    test program reference
  test_program <- base::substring( test_pgmfile, base::nchar(test_root) + 1 )
  
  
  # -- test
  result <- cxlib::cxlib_batch( test_program, user.library = FALSE, silent = TRUE )
  
  
  # -- expectations
  expect_log <- file.path( dirname( test_pgmfile), paste0( tools::file_path_sans_ext( basename(test_pgmfile) ), ".Rout" ) )
  
  #    library paths
  expect_libs <- c( .Library.site, .Library )
  

  #    expected result
  expected_result <- list( "program" = c( "path" = test_pgmfile, "sha1" = digest::digest( test_pgmfile, algo = "sha1", file = TRUE) ),
                           "log" = c( "path" = expect_log, "sha1" = digest::digest( expect_log, algo = "sha1", file = TRUE) ),
                           "library.paths" = expect_libs, 
                           "inputs" = list(),
                           "outputs" = list()
  )
  
  #    expected files
  expected_files <- c( test_pgmfile, expect_log )
  
  
  # -- assertions
  testthat::expect_true( file.exists( expect_log ) )
  testthat::expect_equal( sort(list.files(test_root, full.names = TRUE, recursive = TRUE)), sort(expected_files) )
  testthat::expect_equal( result[ names(expected_result)], expected_result )  
  
  
})


