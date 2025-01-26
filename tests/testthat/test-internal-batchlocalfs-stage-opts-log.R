#
#   Tests for cxlib:::.cxlib_batch_localfs_stage()
#
#   Log options 
#


testthat::test_that( "batchlocalfs.programSingleLogOptSuffix", {
  
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
  
  
  # test program
  test_program <- base::gsub( "\\\\", "/", base::tempfile( pattern = "test-program-", tmpdir = file.path( test_root, "some", "path", "to", "programs", fsep = "/" ), fileext = ".R" ) )
  
  if ( dir.exists( base::dirname( test_program ) ) || ! dir.create( base::dirname( test_program ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  writeLines( "# test program", con = test_program )
  
  if ( ! file.exists( test_program ) )
    testthat::fail( "Could not stage test program" )
  
  
  test_program_relpath <- base::substring( test_program, base::nchar(test_root) + 2 )
  
  # -- test 
  result <- cxlib:::.cxlib_batch_localfs_stage( test_program_relpath, options = list( "log.fileext" = "log") )
  
  
  # -- expected
  
  expected_action <- list( "type" = "program", 
                           "path" = test_program_relpath, 
                           "sha1" = digest::digest( test_program, algo = "sha1", file = TRUE ),
                           "log" = c( "path" = paste0( tools::file_path_sans_ext(test_program_relpath), ".log"),
                                      "sha1" = NA,
                                      "reference.sha1" = NA )
  )
  
  # -- assertions
  
  # work area
  testthat::expect_true( "work.area" %in% names(result) )
  testthat::expect_false( base::startsWith( result[["work.area"]] , test_root) )
  
  # program action
  testthat::expect_true( "actions" %in% names(result) )
  testthat::expect_length( result[["actions"]], 1 )
  
  testthat::expect_true( all( c("type", "path", "sha1" ) %in% names(result[["actions"]][[1]]) ) )
  testthat::expect_equal( result[["actions"]][[1]], expected_action )
  
  
  # staged program
  testthat::expect_true( file.exists( file.path( result[["work.area"]], expected_action[["path"]], fsep = "/" ) ))
  testthat::expect_equal( digest::digest( file.path( result[["work.area"]], expected_action[["path"]], fsep = "/" ), algo = "sha1", file = TRUE ), unname(expected_action[["sha1"]]) )
  
})



testthat::test_that( "batchlocalfs.programMulitpleLogOptSuffix", {
  
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
  
  
  # test program
  
  test_program_parent <- file.path( test_root, "some", "path", "to", "programs", fsep = "/" )
  
  test_programs <- replicate( 5, 
                              base::gsub( "\\\\", "/", base::tempfile( pattern = "test-program-", tmpdir = test_program_parent , fileext = ".R" ) ),
                              simplify = TRUE  )
  
  
  
  if ( dir.exists( test_program_parent ) || ! dir.create( test_program_parent, recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  for ( xpgm in test_programs )
    writeLines( c( "# test program", 
                   paste( "#", base::sample( letters, size = 15) ) ), con = xpgm ) 
  
  if ( ! any(file.exists( test_programs)) )
    testthat::fail( "Could not stage test program" )
  
  
  test_programs_relpath <- base::substring( test_programs, base::nchar(test_root) + 2 )
  
  # -- test 
  result <- cxlib:::.cxlib_batch_localfs_stage( test_programs_relpath, options = list( "log.fileext" = "log") )
  
  # -- expected
  
  expected_actions <- lapply( test_programs_relpath, function(x) {
    list( "type" = "program",
          "path" = x,
          "sha1" = digest::digest( file.path( test_root, x, fsep = "/"), algo = "sha1", file = TRUE ),
          "log" = c( "path" = paste0( tools::file_path_sans_ext(x), ".log"),
                     "sha1" = NA,
                     "reference.sha1" = NA ) ) 
  })
  
  
  expected_files <- file.path( result[["work.area"]], test_programs_relpath, fsep = "/" )
  
  # -- assertions
  
  # work area
  testthat::expect_true( "work.area" %in% names(result) )
  testthat::expect_false( base::startsWith( result[["work.area"]] , test_root) )
  
  # program action
  testthat::expect_true( "actions" %in% names(result) )
  testthat::expect_length( result[["actions"]], length(expected_actions) )
  
  testthat::expect_equal( result[["actions"]], expected_actions )
  
  
  # staged program
  testthat::expect_true( all( file.exists(expected_files) ) )
  testthat::expect_true( all( sapply( expected_actions, function(x) {
    ( x[["sha1"]] == digest::digest( file.path( result[["work.area"]], x[["path"]], fsep = "/"), algo = "sha1", file = TRUE ) )
  }) ) ) 
  
  
})




testthat::test_that( "batchlocalfs.programSingleLogOptPathNotExist", {
  
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
  
  
  # test program
  test_program <- base::gsub( "\\\\", "/", base::tempfile( pattern = "test-program-", tmpdir = file.path( test_root, "some", "path", "to", "programs", fsep = "/" ), fileext = ".R" ) )
  
  if ( dir.exists( base::dirname( test_program ) ) || ! dir.create( base::dirname( test_program ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  writeLines( "# test program", con = test_program )
  
  if ( ! file.exists( test_program ) )
    testthat::fail( "Could not stage test program" )
  
  
  test_program_relpath <- base::substring( test_program, base::nchar(test_root) + 2 )
  
  
  # logs directory
  test_log_relpath <- "some/path/to/logs"
  
  if ( dir.exists( file.path( test_root, test_log_relpath, fsep = "/")) )
    testthat::fail( "Unexpected logs directory exists" )
  
  
  # -- test 
  testthat::expect_error( cxlib:::.cxlib_batch_localfs_stage( test_program_relpath, options = list( "logs" = "some/path/to/logs") ), 
                          regexp = "^The specified directory for logs does not exist$" )
  
  
})






testthat::test_that( "batchlocalfs.programSingleLogOptPath", {
  
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
  
  
  # test program
  test_program <- base::gsub( "\\\\", "/", base::tempfile( pattern = "test-program-", tmpdir = file.path( test_root, "some", "path", "to", "programs", fsep = "/" ), fileext = ".R" ) )
  
  if ( dir.exists( base::dirname( test_program ) ) || ! dir.create( base::dirname( test_program ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  writeLines( "# test program", con = test_program )
  
  if ( ! file.exists( test_program ) )
    testthat::fail( "Could not stage test program" )
  
  
  test_program_relpath <- base::substring( test_program, base::nchar(test_root) + 2 )
  
  
  
  # logs directory
  test_log_relpath <- "some/path/to/logs"
  
  if ( ! dir.exists( file.path( test_root, test_log_relpath, fsep = "/" ) ) && ! dir.create( file.path( test_root, test_log_relpath, fsep = "/"), recursive = TRUE ) )
    testthat::fail( "Could not stage logs directory" )
  
  
  
  
  # -- test 
  result <- cxlib:::.cxlib_batch_localfs_stage( test_program_relpath, options = list( "logs" = test_log_relpath ) )
  
  
  # -- expected
  
  expected_action <- list( "type" = "program", 
                           "path" = test_program_relpath, 
                           "sha1" = digest::digest( test_program, algo = "sha1", file = TRUE ),
                           "log" = c( "path" = paste0( test_log_relpath, "/", tools::file_path_sans_ext( base::basename(test_program_relpath) ), ".Rout"),
                                      "sha1" = NA,
                                      "reference.sha1" = NA )
  )
  
  
  expected_logs_path <- test_log_relpath

  
  # -- assertions
  
  # work area
  testthat::expect_true( "work.area" %in% names(result) )
  testthat::expect_false( base::startsWith( result[["work.area"]] , test_root) )
  
  # program action
  testthat::expect_true( "actions" %in% names(result) )
  testthat::expect_length( result[["actions"]], 1 )
  
  testthat::expect_true( all( c("type", "path", "sha1" ) %in% names(result[["actions"]][[1]]) ) )
  testthat::expect_equal( result[["actions"]][[1]], expected_action )
  
  
  # staged program
  testthat::expect_true( file.exists( file.path( result[["work.area"]], expected_action[["path"]], fsep = "/" ) ))
  testthat::expect_equal( digest::digest( file.path( result[["work.area"]], expected_action[["path"]], fsep = "/" ), algo = "sha1", file = TRUE ), unname(expected_action[["sha1"]]) )

  
  # staged logs directory 
  testthat::expect_true( dir.exists( file.path( result[["work.area"]], expected_logs_path, fsep = "/" ) ) )
  
})





testthat::test_that( "batchlocalfs.programSingleLogOptPathFileExt", {
  
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
  
  
  # test program
  test_program <- base::gsub( "\\\\", "/", base::tempfile( pattern = "test-program-", tmpdir = file.path( test_root, "some", "path", "to", "programs", fsep = "/" ), fileext = ".R" ) )
  
  if ( dir.exists( base::dirname( test_program ) ) || ! dir.create( base::dirname( test_program ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  writeLines( "# test program", con = test_program )
  
  if ( ! file.exists( test_program ) )
    testthat::fail( "Could not stage test program" )
  
  
  test_program_relpath <- base::substring( test_program, base::nchar(test_root) + 2 )
  
  
  
  # logs directory
  test_log_relpath <- "some/path/to/logs"
  
  if ( ! dir.exists( file.path( test_root, test_log_relpath, fsep = "/" ) ) && ! dir.create( file.path( test_root, test_log_relpath, fsep = "/"), recursive = TRUE ) )
    testthat::fail( "Could not stage logs directory" )
  
  
  # logs file extension
  test_log_fileext <- paste( sample( c( LETTERS, letters, as.character(0:9) ), 5 ), collapse = "" )
  
  
  # -- test 
  result <- cxlib:::.cxlib_batch_localfs_stage( test_program_relpath, options = list( "logs" = test_log_relpath, "log.fileext" = test_log_fileext ) )
  
  
  # -- expected
  
  expected_action <- list( "type" = "program", 
                           "path" = test_program_relpath, 
                           "sha1" = digest::digest( test_program, algo = "sha1", file = TRUE ),
                           "log" = c( "path" = paste0( test_log_relpath, "/", tools::file_path_sans_ext( base::basename(test_program_relpath) ), ".", test_log_fileext ),
                                      "sha1" = NA,
                                      "reference.sha1" = NA )
  )
  
  
  expected_logs_path <- test_log_relpath
  
  
  # -- assertions
  
  # work area
  testthat::expect_true( "work.area" %in% names(result) )
  testthat::expect_false( base::startsWith( result[["work.area"]] , test_root) )
  
  # program action
  testthat::expect_true( "actions" %in% names(result) )
  testthat::expect_length( result[["actions"]], 1 )
  
  testthat::expect_true( all( c("type", "path", "sha1" ) %in% names(result[["actions"]][[1]]) ) )
  testthat::expect_equal( result[["actions"]][[1]], expected_action )
  
  
  # staged program
  testthat::expect_true( file.exists( file.path( result[["work.area"]], expected_action[["path"]], fsep = "/" ) ))
  testthat::expect_equal( digest::digest( file.path( result[["work.area"]], expected_action[["path"]], fsep = "/" ), algo = "sha1", file = TRUE ), unname(expected_action[["sha1"]]) )
  
  
  # staged logs directory 
  testthat::expect_true( dir.exists( file.path( result[["work.area"]], expected_logs_path, fsep = "/" ) ) )
  
})

