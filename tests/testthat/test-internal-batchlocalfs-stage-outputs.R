#
#   Tests for cxlib:::.cxlib_batch_localfs_stage()
#
#   Output annotations
#
#


testthat::test_that( "batchlocalfs-output.programNoOutput", {
  
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
  
  
  
  # -- test 
  result <- cxlib:::.cxlib_batch_localfs_stage( test_program )
  
  
  # -- expected
  
  expected_action <- list( "type" = "program", 
                           "path" = base::substring( test_program, base::nchar( test_root ) + 2 ), 
                           "sha1" = digest::digest( test_program, algo = "sha1", file = TRUE ),
                           "log" = c( "path" = paste0( tools::file_path_sans_ext(base::substring( test_program, base::nchar( test_root ) + 2 )), ".Rout"),
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
  
  
  # no input annotation
  testthat::expect_true( "output.locations" %in% names(result) )
  testthat::expect_length( result[["output.locations"]], 0 )  
  
})




testthat::test_that( "batchlocalfs-output.programOutputNotExist", {
  
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
  
  # test output
  test_output <- base::gsub( "\\\\", "/", base::tempfile( pattern = "test-output-", tmpdir = file.path( test_root, "some", "path", "to", "output", fsep = "/" ), fileext = "" ) )
  
  if ( dir.exists( test_output ) )
    testthat::fail( "Test output exists" )
  
  if ( ! dir.exists( base::dirname(test_output) ) && ! dir.create( base::dirname(test_output), recursive = TRUE) )
    testthat::fail( "Could not create parent directory for non-existant output" )
  
  test_output_ref <- base::substring( test_output, base::nchar( test_root ) + 2 )
  
  
  # test program
  test_program <- base::gsub( "\\\\", "/", base::tempfile( pattern = "test-program-", tmpdir = file.path( test_root, "some", "path", "to", "programs", fsep = "/" ), fileext = ".R" ) )
  
  if ( dir.exists( base::dirname( test_program ) ) || ! dir.create( base::dirname( test_program ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  pgm <- c( "# test program",
            paste( "# @cx.output", test_output_ref) )
  
  writeLines( pgm, con = test_program )
  
  if ( ! file.exists( test_program ) )
    testthat::fail( "Could not stage test program" )
  
  
  # -- test 
  testthat::expect_error( cxlib:::.cxlib_batch_localfs_stage( test_program ), regexp = paste( "^Output directory", test_output_ref, "does not exist or is not a relative path to the working directory$") )

})





testthat::test_that( "batchlocalfs-output.programOutput", {
  
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
  
  
  # test output
  test_output <- base::gsub( "\\\\", "/", base::tempfile( pattern = "test-output-", tmpdir = file.path( test_root, "some", "path", "to", "outputs", fsep = "/" ), fileext = "" ) )
  
  if ( dir.exists( test_output ) )
    testthat::fail( "Test output exists" )
  
  if ( ! dir.exists( test_output ) && ! dir.create( test_output, recursive = TRUE) )
    testthat::fail( "Could not create directory for output" )
  
  test_output_ref <- base::substring( test_output, base::nchar( test_root ) + 2 )
  
  
  
  # test program
  test_program <- base::gsub( "\\\\", "/", base::tempfile( pattern = "test-program-", tmpdir = file.path( test_root, "some", "path", "to", "programs", fsep = "/" ), fileext = ".R" ) )
  
  if ( dir.exists( base::dirname( test_program ) ) || ! dir.create( base::dirname( test_program ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  pgm <- c( "# test program",
            paste( "# @cx.output", test_output_ref) )  
  
  writeLines( pgm, con = test_program )
  
  if ( ! file.exists( test_program ) )
    testthat::fail( "Could not stage test program" )
  
  
  
  # -- test 
  result <- cxlib:::.cxlib_batch_localfs_stage( test_program )
  
  
  # -- expected
  
  expected_action <- list( "type" = "program", 
                           "path" = base::substring( test_program, base::nchar( test_root ) + 2 ), 
                           "sha1" = digest::digest( test_program, algo = "sha1", file = TRUE ),
                           "log" = c( "path" = paste0( tools::file_path_sans_ext(base::substring( test_program, base::nchar( test_root ) + 2 )), ".Rout"),
                                      "sha1" = NA,
                                      "reference.sha1" = NA )   
  )
  
  expected_outputs <- test_output_ref
  
  
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
  
  
  # output annotations
  testthat::expect_true( "output.locations" %in% names(result) )
  testthat::expect_equal( result[["output.locations"]], expected_outputs )
  
})





testthat::test_that( "batchlocalfs-output.programMultipleOutputs", {
  
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
  
  
  # test output
  test_outputs <- replicate( 5,
                             base::gsub( "\\\\", "/", base::tempfile( pattern = "test-output-", tmpdir = file.path( test_root, "some", "path", "to", "outputs", fsep = "/" ), fileext = "" ) ),
                             simplify = TRUE )
                         
  
  if ( any( dir.exists( test_outputs ) ) )
    testthat::fail( "Test output exists" )

  for ( xout in test_outputs )  
    if ( ! dir.exists( xout ) && ! dir.create( xout, recursive = TRUE) )
      testthat::fail( "Could not create directory for output" )
  
  test_output_ref <- base::substring( test_outputs, base::nchar( test_root ) + 2 )
  
  
  
  # test program
  test_program <- base::gsub( "\\\\", "/", base::tempfile( pattern = "test-program-", tmpdir = file.path( test_root, "some", "path", "to", "programs", fsep = "/" ), fileext = ".R" ) )
  
  if ( dir.exists( base::dirname( test_program ) ) || ! dir.create( base::dirname( test_program ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  pgm <- c( "# test program",
            paste( "# @cx.output", test_output_ref) )  
  
  writeLines( pgm, con = test_program )
  
  if ( ! file.exists( test_program ) )
    testthat::fail( "Could not stage test program" )
  
  
  
  # -- test 
  result <- cxlib:::.cxlib_batch_localfs_stage( test_program )
  
  
  # -- expected
  
  expected_action <- list( "type" = "program", 
                           "path" = base::substring( test_program, base::nchar( test_root ) + 2 ), 
                           "sha1" = digest::digest( test_program, algo = "sha1", file = TRUE ),
                           "log" = c( "path" = paste0( tools::file_path_sans_ext(base::substring( test_program, base::nchar( test_root ) + 2 )), ".Rout"),
                                      "sha1" = NA,
                                      "reference.sha1" = NA )   
  )
  
  expected_outputs <- sort(test_output_ref)
  
  
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
  
  
  # output annotations
  testthat::expect_true( "output.locations" %in% names(result) )
  testthat::expect_equal( result[["output.locations"]], expected_outputs )
  
})





testthat::test_that( "batchlocalfs-output.multiProgramMultipleOutputs", {
  
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
  
  
  # test output
  test_outputs <- replicate( 5,
                             base::gsub( "\\\\", "/", base::tempfile( pattern = "test-output-", tmpdir = file.path( test_root, "some", "path", "to", "outputs", fsep = "/" ), fileext = "" ) ),
                             simplify = TRUE )
  
  
  if ( any( dir.exists( test_outputs ) ) )
    testthat::fail( "Test output exists" )
  
  for ( xout in test_outputs )  
    if ( ! dir.exists( xout ) && ! dir.create( xout, recursive = TRUE) )
      testthat::fail( "Could not create directory for output" )
  
  test_output_ref <- base::substring( test_outputs, base::nchar( test_root ) + 2 )
  
  
  
  # test program
  test_program_parent <- file.path( test_root, "some", "path", "to", "programs", fsep = "/" )
  
  test_programs <- replicate( 5, 
                              base::gsub( "\\\\", "/", base::tempfile( pattern = "test-program-", tmpdir = test_program_parent, fileext = ".R" ) ), 
                              simplify = TRUE )
  
  if ( ! dir.exists( test_program_parent ) && ! dir.create( test_program_parent, recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  
  for ( xpgm in utils::head( test_programs, n = 3) ) {
      
    pgm <- c( "# test program",
              paste( "# @cx.output", utils::tail( test_output_ref, n = 3 ) ) )  
    
    writeLines( pgm, con = xpgm )
    
    if ( ! file.exists( xpgm ) )
      testthat::fail( "Could not stage test program" )
  
  }
  
  
  for ( xpgm in utils::tail( test_programs, n = 2) ) {
    
    pgm <- c( "# test program",
              paste( "# @cx.output", utils::head( test_output_ref, n = 2 ) ) )  
    
    writeLines( pgm, con = xpgm )
    
    if ( ! file.exists( xpgm ) )
      testthat::fail( "Could not stage test program" )
    
  }
  
  
  test_programs_relpath <- base::substring( test_programs, base::nchar(test_root) + 2 )
  
  
  
  # -- test 
  result <- cxlib:::.cxlib_batch_localfs_stage( test_programs )
  
  
  # -- expected
  
  expected_actions <- lapply( test_programs_relpath, function(x) {
    list( "type" = "program",
          "path" = x,
          "sha1" = digest::digest( file.path( test_root, x, fsep = "/"), algo = "sha1", file = TRUE ),
          "log" = c( "path" = paste0( tools::file_path_sans_ext(x), ".Rout"),
                     "sha1" = NA,
                     "reference.sha1" = NA ) ) 
  })

  expected_outputs <- sort(test_output_ref)

  expected_program_files <- file.path( result[["work.area"]], test_programs_relpath, fsep = "/" )

  
  # -- assertions
  
  # work area
  testthat::expect_true( "work.area" %in% names(result) )
  testthat::expect_false( base::startsWith( result[["work.area"]] , test_root) )
  
  # program action
  testthat::expect_true( "actions" %in% names(result) )
  testthat::expect_length( result[["actions"]], length(expected_actions) )
  
  testthat::expect_true( all( c("type", "path", "sha1" ) %in% names(result[["actions"]][[1]]) ) )
  testthat::expect_equal( result[["actions"]], expected_actions )
  
  
  # staged program
  testthat::expect_true( all(file.exists( expected_program_files )) ) 
  testthat::expect_true( all( sapply( expected_actions, function(x) {
    ( x[["sha1"]] == digest::digest( file.path( result[["work.area"]], x[["path"]], fsep = "/"), algo = "sha1", file = TRUE ) )
  }) ) ) 
  
  
  # output annotations
  testthat::expect_true( "output.locations" %in% names(result) )
  testthat::expect_equal( result[["output.locations"]], expected_outputs )
  
})


