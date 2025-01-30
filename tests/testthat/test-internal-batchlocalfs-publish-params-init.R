#
#   Tests for cxlib:::.cxlib_batch_localfs_publish()
#
#   Parameter and init checks
#



testthat::test_that( "batchlocalfs.publishJobMissing", {
  
  testthat::expect_error( cxlib:::.cxlib_batch_localfs_publish(), regexp = "^Invalid job results$" )
  
})



testthat::test_that( "batchlocalfs.publishJobNull", {
  
  testthat::expect_error( cxlib:::.cxlib_batch_localfs_publish(NULL), regexp = "^Invalid job results$" )
  
})



testthat::test_that( "batchlocalfs.publishJobNA", {
  
  testthat::expect_error( cxlib:::.cxlib_batch_localfs_publish(NA), regexp = "^Invalid job results$" )
  
})



testthat::test_that( "batchlocalfs.publishJobNotList", {
  
  testthat::expect_error( cxlib:::.cxlib_batch_localfs_publish( character(0) ), regexp = "^Invalid job results$" )
  
})



testthat::test_that( "batchlocalfs.publishJobEmptyList", {
  
  testthat::expect_error( cxlib:::.cxlib_batch_localfs_publish( list() ), regexp = "^Empty job results unexpected$" )
  
})



testthat::test_that( "batchlocalfs.publishJobModeMissing", {
  
  testthat::expect_error( cxlib:::.cxlib_batch_localfs_publish( list( "abc" = "123") ), regexp = "^Invalid mode$" )
  
})


testthat::test_that( "batchlocalfs.publishJobWorkingDirectoryMissing", {
  
  # -- stage 
  
  # note: should not include named element working.directory
  test_execresult <- list( "mode" = "localfs" )
  

  # -- test
  testthat::expect_error( cxlib:::.cxlib_batch_localfs_publish( test_execresult ), regexp = "^Working directory in job results is missing or invalid$" )
  
})



testthat::test_that( "batchlocalfs.publishJobWorkingDirectoryNull", {
  
  # -- stage 
  test_execresult <- list( "mode" = "localfs",
                           "working.directory" = NULL )
  
  
  # -- test
  testthat::expect_error( cxlib:::.cxlib_batch_localfs_publish( test_execresult ), regexp = "^Working directory in job results is missing or invalid$" )
  
})


testthat::test_that( "batchlocalfs.publishJobWorkingDirectoryNA", {
  
  # -- stage 
  test_execresult <- list( "mode" = "localfs",
                           "working.directory" = NULL )
  
  
  # -- test
  testthat::expect_error( cxlib:::.cxlib_batch_localfs_publish( test_execresult ), regexp = "^Working directory in job results is missing or invalid$" )
  
})


testthat::test_that( "batchlocalfs.publishJobWorkingDirectoryNotCharacter", {
  
  # -- stage 
  test_execresult <- list( "mode" = "localfs",
                           "working.directory" = TRUE )
  
  
  # -- test
  testthat::expect_error( cxlib:::.cxlib_batch_localfs_publish( test_execresult ), regexp = "^Working directory in job results is missing or invalid$" )
  
})



testthat::test_that( "batchlocalfs.publishJobWorkingDirectoryCharacterEmpty", {
  
  # -- stage 
  test_execresult <- list( "mode" = "localfs",
                           "working.directory" = character(0) )
  
  
  # -- test
  testthat::expect_error( cxlib:::.cxlib_batch_localfs_publish( test_execresult ), regexp = "^Working directory in job results is missing or invalid$" )
  
})




testthat::test_that( "batchlocalfs.publishJobWorkingDirectoryNotExist", {
  
  # -- stage 
  test_execresult <- list( "mode" = "localfs",
                           "working.directory" = cxlib:::.cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) ) )
  
  if ( dir.exists(test_execresult[["working.directory"]]) )
    testthat::fail( "Unexpected test working directory exists" )
  
  
  # -- test
  testthat::expect_error( cxlib:::.cxlib_batch_localfs_publish( test_execresult ), regexp = "^Working directory in job results is missing or invalid$" )
  
})





testthat::test_that( "batchlocalfs.publishJobWorkAreaMissing", {
    
  # -- stage
  
  test_root <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit({
    unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE)

  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE) )
    testthat::fail( "Could not create test area" )
  
  
  wrkdir <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "working-directory-", tmpdir = test_root, fileext = "" ) )

  if ( dir.exists( wrkdir ) || ! dir.create( wrkdir, recursive = TRUE) )
    testthat::fail( "Could not create test area" )
  

  # note: should not include named element work.area
  test_execresult <- list( "mode" = "localfs",
                           "working.directory" = wrkdir )
  
  
  
  # -- test
  testthat::expect_error( cxlib:::.cxlib_batch_localfs_publish( test_execresult ), regexp = "^Work area in job results is missing or invalid$" )
  
})




testthat::test_that( "batchlocalfs.publishJobWorkAreaNull", {
  
  # -- stage
  
  test_root <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit({
    unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE)
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE) )
    testthat::fail( "Could not create test area" )
  
  
  wrkdir <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "working-directory-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( wrkdir ) || ! dir.create( wrkdir, recursive = TRUE) )
    testthat::fail( "Could not create test area" )
  
  
  test_execresult <- list( "mode" = "localfs",
                           "working.directory" = wrkdir,
                           "work.area" = NULL )
  
  
  
  # -- test
  testthat::expect_error( cxlib:::.cxlib_batch_localfs_publish( test_execresult ), regexp = "^Work area in job results is missing or invalid$" )
  
})




testthat::test_that( "batchlocalfs.publishJobWorkAreaNA", {
  
  # -- stage
  
  test_root <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit({
    unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE)
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE) )
    testthat::fail( "Could not create test area" )
  
  
  wrkdir <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "working-directory-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( wrkdir ) || ! dir.create( wrkdir, recursive = TRUE) )
    testthat::fail( "Could not create test area" )
  
  
  test_execresult <- list( "mode" = "localfs",
                           "working.directory" = wrkdir,
                           "work.area" = NA )
  
  
  
  # -- test
  testthat::expect_error( cxlib:::.cxlib_batch_localfs_publish( test_execresult ), regexp = "^Work area in job results is missing or invalid$" )
  
})


testthat::test_that( "batchlocalfs.publishJobWorkAreaNotCharacter", {
  
  # -- stage
  
  test_root <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit({
    unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE)
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE) )
    testthat::fail( "Could not create test area" )
  
  
  wrkdir <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "working-directory-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( wrkdir ) || ! dir.create( wrkdir, recursive = TRUE) )
    testthat::fail( "Could not create test area" )
  
  
  test_execresult <- list( "mode" = "localfs",
                           "working.directory" = wrkdir,
                           "work.area" = 1 )
  
  
  
  # -- test
  testthat::expect_error( cxlib:::.cxlib_batch_localfs_publish( test_execresult ), regexp = "^Work area in job results is missing or invalid$" )
  
})



testthat::test_that( "batchlocalfs.publishJobWorkAreaEmpty", {
  
  # -- stage
  
  test_root <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit({
    unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE)
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE) )
    testthat::fail( "Could not create test area" )
  
  
  wrkdir <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "working-directory-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( wrkdir ) || ! dir.create( wrkdir, recursive = TRUE) )
    testthat::fail( "Could not create test area" )
  
  
  test_execresult <- list( "mode" = "localfs",
                           "working.directory" = wrkdir,
                           "work.area" = character(0) )
  
  
  
  # -- test
  testthat::expect_error( cxlib:::.cxlib_batch_localfs_publish( test_execresult ), regexp = "^Work area in job results is missing or invalid$" )
  
})




testthat::test_that( "batchlocalfs.publishJobWorkAreaNotExist", {
  
  # -- stage
  
  test_root <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit({
    unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE)
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE) )
    testthat::fail( "Could not create test area" )
  
  
  wrkdir <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "working-directory-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( wrkdir ) || ! dir.create( wrkdir, recursive = TRUE) )
    testthat::fail( "Could not create test working directory" )
  
  
  test_execresult <- list( "mode" = "localfs",
                           "working.directory" = wrkdir,
                           "work.area" =  cxlib:::.cxlib_standardpath( base::tempfile( pattern = "work-area-not-exist-", tmpdir = test_root, fileext = "" ) ) )
  
  
  
  # -- test
  testthat::expect_error( cxlib:::.cxlib_batch_localfs_publish( test_execresult ), regexp = "^Work area in job results is missing or invalid$" )
  
})





testthat::test_that( "batchlocalfs.publishJobActionsMissing", {
  
  # -- stage
  
  test_root <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit({
    unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE)
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE) )
    testthat::fail( "Could not create test area" )
  
  
  # working directory  
  
  wrkdir <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "working-directory-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( wrkdir ) || ! dir.create( wrkdir, recursive = TRUE) )
    testthat::fail( "Could not create test working directory" )
  
  
  # work area
  
  wrkarea <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "work-area-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( wrkarea ) || ! dir.create( wrkarea, recursive = TRUE) )
    testthat::fail( "Could not create test work area" )
  
  
  
  
  test_execresult <- list( "mode" = "localfs",
                           "working.directory" = wrkdir,
                           "work.area" =  wrkarea )
  
  
  
  # -- test
  result <- cxlib:::.cxlib_batch_localfs_publish( test_execresult )
  

  # -- expected
  expected_jresult <- list( "mode" = "localfs",
                            "working.directory" = wrkdir, 
                            "work.area" = wrkarea,
                            "output.locations" = character(0),
                            "actions" = list(), 
                            "inputs" = list(),
                            "outputs" = list(), 
                            "deleted" = list() )
  
  # -- assertions
  testthat::expect_equal( result, expected_jresult )
  
})



testthat::test_that( "batchlocalfs.publishJobActionsNull", {
  
  # -- stage
  
  test_root <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit({
    unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE)
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE) )
    testthat::fail( "Could not create test area" )
  
  
  # working directory  
  
  wrkdir <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "working-directory-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( wrkdir ) || ! dir.create( wrkdir, recursive = TRUE) )
    testthat::fail( "Could not create test working directory" )
  
  
  # work area
  
  wrkarea <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "work-area-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( wrkarea ) || ! dir.create( wrkarea, recursive = TRUE) )
    testthat::fail( "Could not create test work area" )
  
  
  
  
  test_execresult <- list( "mode" = "localfs",
                           "working.directory" = wrkdir,
                           "work.area" =  wrkarea, 
                           "actions" = NULL )
  
  
  
  # -- test
  result <- cxlib:::.cxlib_batch_localfs_publish( test_execresult )
  
  
  # -- expected
  expected_jresult <- list( "mode" = "localfs",
                            "working.directory" = wrkdir, 
                            "work.area" = wrkarea,
                            "output.locations" = character(0),
                            "actions" = list(), 
                            "inputs" = list(),
                            "outputs" = list(), 
                            "deleted" = list() )
  
  # -- assertions
  testthat::expect_equal( result, expected_jresult )
  
})




testthat::test_that( "batchlocalfs.publishJobActionsNA", {
  
  # -- stage
  
  test_root <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit({
    unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE)
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE) )
    testthat::fail( "Could not create test area" )
  
  
  # working directory  
  
  wrkdir <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "working-directory-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( wrkdir ) || ! dir.create( wrkdir, recursive = TRUE) )
    testthat::fail( "Could not create test working directory" )
  
  
  # work area
  
  wrkarea <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "work-area-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( wrkarea ) || ! dir.create( wrkarea, recursive = TRUE) )
    testthat::fail( "Could not create test work area" )
  
  
  
  
  test_execresult <- list( "mode" = "localfs",
                           "working.directory" = wrkdir,
                           "work.area" =  wrkarea, 
                           "actions" = NA )
  
  
  
  # -- test
  result <- cxlib:::.cxlib_batch_localfs_publish( test_execresult )
  
  
  # -- expected
  expected_jresult <- list( "mode" = "localfs",
                            "working.directory" = wrkdir, 
                            "work.area" = wrkarea,
                            "output.locations" = character(0),
                            "actions" = list(), 
                            "inputs" = list(),
                            "outputs" = list(), 
                            "deleted" = list() )
  
  # -- assertions
  testthat::expect_equal( result, expected_jresult )
  
})



testthat::test_that( "batchlocalfs.publishJobActionsNotList", {
  
  # -- stage
  
  test_root <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit({
    unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE)
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE) )
    testthat::fail( "Could not create test area" )
  
  
  # working directory  
  
  wrkdir <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "working-directory-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( wrkdir ) || ! dir.create( wrkdir, recursive = TRUE) )
    testthat::fail( "Could not create test working directory" )
  
  
  # work area
  
  wrkarea <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "work-area-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( wrkarea ) || ! dir.create( wrkarea, recursive = TRUE) )
    testthat::fail( "Could not create test work area" )
  
  
  
  
  test_execresult <- list( "mode" = "localfs",
                           "working.directory" = wrkdir,
                           "work.area" =  wrkarea, 
                           "actions" = character(1) )
  
  
  
  # -- test
  result <- cxlib:::.cxlib_batch_localfs_publish( test_execresult )
  
  
  # -- expected
  expected_jresult <- list( "mode" = "localfs",
                            "working.directory" = wrkdir, 
                            "work.area" = wrkarea,
                            "output.locations" = character(0),
                            "actions" = list(), 
                            "inputs" = list(),
                            "outputs" = list(), 
                            "deleted" = list() )
  
  # -- assertions
  testthat::expect_equal( result, expected_jresult )
  
})





testthat::test_that( "batchlocalfs.publishJobActionsEmptytList", {
  
  # -- stage
  
  test_root <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit({
    unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE)
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE) )
    testthat::fail( "Could not create test area" )
  
  
  # working directory  
  
  wrkdir <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "working-directory-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( wrkdir ) || ! dir.create( wrkdir, recursive = TRUE) )
    testthat::fail( "Could not create test working directory" )
  
  
  # work area
  
  wrkarea <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "work-area-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( wrkarea ) || ! dir.create( wrkarea, recursive = TRUE) )
    testthat::fail( "Could not create test work area" )
  
  
  
  
  test_execresult <- list( "mode" = "localfs",
                           "working.directory" = wrkdir,
                           "work.area" =  wrkarea, 
                           "actions" = list() )
  
  
  
  # -- test
  result <- cxlib:::.cxlib_batch_localfs_publish( test_execresult )
  
  
  # -- expected
  expected_jresult <- list( "mode" = "localfs",
                            "working.directory" = wrkdir, 
                            "work.area" = wrkarea,
                            "output.locations" = character(0),
                            "actions" = list(), 
                            "inputs" = list(),
                            "outputs" = list(), 
                            "deleted" = list() )
  
  # -- assertions
  testthat::expect_equal( result, expected_jresult )
  
})


