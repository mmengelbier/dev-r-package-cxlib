#
#   Tests for cxlib:::.cxlib_batch_localfs_stage()
#
#
#




testthat::test_that( "batchlocalfs.programMissing", {
  
  # -- test 
  result <- cxlib:::.cxlib_batch_localfs_stage()
  

  # -- assertions
  #    note: epecting actions to be empty
  testthat::expect_true( "actions" %in% names(result) )
  testthat::expect_length( result[["actions"]], 0 )  

})





testthat::test_that( "batchlocalfs.programNull", {
  
  # -- test 
  result <- cxlib:::.cxlib_batch_localfs_stage( NULL )
  
  
  # -- assertions
  #    note: epecting actions to be empty
  testthat::expect_true( "actions" %in% names(result) )
  testthat::expect_length( result[["actions"]], 0 )  
  
})



testthat::test_that( "batchlocalfs.programNA", {
  
  # -- test 
  result <- cxlib:::.cxlib_batch_localfs_stage( NA )
  
  
  # -- assertions
  #    note: epecting actions to be empty
  testthat::expect_true( "actions" %in% names(result) )
  testthat::expect_length( result[["actions"]], 0 )  
  
})




testthat::test_that( "batchlocalfs.programEmpty", {
  
  # -- test 
  result <- cxlib:::.cxlib_batch_localfs_stage( character(0) )
  
  
  # -- assertions
  #    note: epecting actions to be empty
  testthat::expect_true( "actions" %in% names(result) )
  testthat::expect_length( result[["actions"]], 0 )  
  
})




testthat::test_that( "batchlocalfs.programNotCharacter", {
  
  # -- test 
  result <- cxlib:::.cxlib_batch_localfs_stage( 1 )
  
  
  # -- assertions
  #    note: epecting actions to be empty
  testthat::expect_true( "actions" %in% names(result) )
  testthat::expect_length( result[["actions"]], 0 )  
  
})





testthat::test_that( "batchlocalfs.programSingleNotExist", {
  
  # -- stage
  
  test_root <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )

  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )  

  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Could not stage test root" )
  
  
  test_program <- base::gsub( "\\\\", "/", base::tempfile( pattern = "i-do-not-exist-", tmpdir = test_root, fileext = ".R" ) )
  
  if ( file.exists( test_program ) )
    testthat::fail( "Dummy test program exists" )
  
  
  # -- test 
  testthat::expect_error( cxlib:::.cxlib_batch_localfs_stage( test_program ), regexp = "^One or more programs do not exist$" )
  
  
})




testthat::test_that( "batchlocalfs.programSingleNotExist", {
  
  # -- stage
  
  test_root <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )  
  
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Could not stage test root" )
  
  
  # list of test programs
  test_programs <- character(0)
  
  for ( x in 1:3 )
    test_programs[x] <- base::gsub( "\\\\", "/", base::tempfile( pattern = "test-program-", tmpdir = test_root, fileext = ".R" ) )
  
  # create all but last program 
  for ( xpath in utils::head( test_programs, n = length(test_programs) -1  ) )  {
    
    writeLines( "# test program", con = xpath)
    
    if ( ! file.exists( xpath ) )
      testthat::fail( "Could not stage test program" )
    
  }


  # -- test 
  testthat::expect_error( cxlib:::.cxlib_batch_localfs_stage( test_programs ), regexp = "^One or more programs do not exist$" )
  
  
})





testthat::test_that( "batchlocalfs.programSingleNotInWorkingDirectory", {
  
  # -- stage
  
  test_root <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )  
  
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Could not stage test root" )
  
  
  # test program
  test_program <- base::gsub( "\\\\", "/", base::tempfile( pattern = "test-program-", tmpdir = test_root, fileext = ".R" ) )
  
  writeLines( "# test program", con = test_program )
    
  if ( ! file.exists( test_program ) )
    testthat::fail( "Could not stage test program" )
    
  # test work directory
  
  test_wd <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )

  current_wd <- base::getwd()
    
  on.exit({
    base::setwd( current_wd )
    base::unlink( test_wd, recursive = TRUE, force = TRUE )
  }, add = TRUE )  
  
  if ( dir.exists( test_wd ) || ! dir.create( test_wd, recursive = TRUE ) )
    testthat::fail( "Could not stage test working directory" )
  
  base::setwd( test_wd )
  
  
  # -- test 
  testthat::expect_error( cxlib:::.cxlib_batch_localfs_stage( test_program ), 
                          regexp = paste( "^The program file", test_program, "is not in a subdirectory of the working directory$" ) )
  
  
})



testthat::test_that( "batchlocalfs.programSingleSimpleAbsPath", {
  
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

})



testthat::test_that( "batchlocalfs.programSingleSimpleRelPath", {
  
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
  result <- cxlib:::.cxlib_batch_localfs_stage( test_program_relpath )
  
  
  # -- expected
  
  expected_action <- list( "type" = "program", 
                           "path" = test_program_relpath, 
                           "sha1" = digest::digest( test_program, algo = "sha1", file = TRUE ),
                           "log" = c( "path" = paste0( tools::file_path_sans_ext(test_program_relpath), ".Rout"),
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





testthat::test_that( "batchlocalfs.programMultipleSimpleRelPath", {
  
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
  result <- cxlib:::.cxlib_batch_localfs_stage( test_programs_relpath )
  
  # -- expected

  expected_actions <- lapply( test_programs_relpath, function(x) {
    list( "type" = "program",
          "path" = x,
          "sha1" = digest::digest( file.path( test_root, x, fsep = "/"), algo = "sha1", file = TRUE ),
          "log" = c( "path" = paste0( tools::file_path_sans_ext(x), ".Rout"),
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





testthat::test_that( "batchlocalfs.programSingleLogExists", {
  
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
  
  
  # test log
  
  test_log_relpath <- paste0( tools::file_path_sans_ext(test_program_relpath), ".Rout")
  
  test_log_reference <- paste( sample( c( LETTERS, letters, as.character(0:9) ), 30 ), collapse = "" )
  
  writeLines( c( "# test log", test_log_reference ), con = file.path( test_root, test_log_relpath, fsep = "/" ) )
  
  if ( ! file.exists( file.path( test_root, test_log_relpath, fsep = "/" ) ) )
    testthat::fail( "Could not stage test log" )
  
  
  
  
  # -- test 
  result <- cxlib:::.cxlib_batch_localfs_stage( test_program_relpath )
  
 
  # -- expected
  
  expected_action <- list( "type" = "program", 
                           "path" = test_program_relpath, 
                           "sha1" = digest::digest( test_program, algo = "sha1", file = TRUE ),
                           "log" = c( "path" = paste0( tools::file_path_sans_ext(test_program_relpath), ".Rout"),
                                      "sha1" = NA,
                                      "reference.sha1" = digest::digest( file.path( test_root, test_log_relpath, fsep = "/" ), algo = "sha1", file = TRUE ) )
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

