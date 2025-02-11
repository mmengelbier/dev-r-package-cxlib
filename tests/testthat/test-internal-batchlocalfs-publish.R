#
#   Tests for cxlib:::.cxlib_batch_localfs_publish()
#
#   
#


testthat::test_that( "batchlocalfs.publishJobNoAction", {
  
  
  # -- stage
  
  current_wd <- base::getwd()
  
  test_root <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit({
    base::setwd( current_wd )
    unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE)
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE) )
    testthat::fail( "Could not create test area" )
  
  
  # working directory
  
  wrkdir <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "working-directory-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( wrkdir ) || ! dir.create( wrkdir, recursive = TRUE) )
    testthat::fail( "Could not create test working directory" )
  
  base::setwd( wrkdir )
  
  
  # work area
  
  wrkarea <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "work-area-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( wrkarea ) || ! dir.create( wrkarea, recursive = TRUE) )
    testthat::fail( "Could not create test work area" )
  
  
  
  # stage output directory in work area and working directory
  
  test_output_parent <- file.path( "some", "path", "to", "outputs", fsep = "/" )
  
  for ( xpath in c( wrkarea, wrkdir ) )    
    if ( dir.exists( file.path( xpath, test_output_parent, fsep = "/" ) ) || ! dir.create( file.path( xpath, test_output_parent, fsep = "/" ), recursive = TRUE) )
      testthat::fail( "Could not create test output directory in work area" )
  
  
  # stage outputs in work area
  
  test_files <- replicate( 3,
                           cxlib:::.cxlib_standardpath( base::tempfile( pattern = "test-output-", tmpdir = file.path( wrkarea, test_output_parent, fsep = "/" ), fileext = ".txt" ) ),
                           simplify = TRUE )
  
  for ( xfile in test_files )
    base::writeLines( paste( sample( c( letters, LETTERS, as.character(0:9) ), 40 ), collapse = "" ), con = xfile )
  
  if ( ! all( file.exists( test_files ) ) )
    testthat::fail( "Could not stage test files in work area output directory" )
  
  
  test_file_refs <- base::substring( test_files, base::nchar(wrkarea) + 2)
  
  
  
  # set up job
  
  test_action <- list( "type" = "program",
                       "files.created" = lapply( utils::head( sort(test_file_refs), n = length(test_file_refs) - 1), function(x) {
                         c( "path" = x,
                            "sha1" = digest::digest( file.path( wrkarea, x, fsep = "/"), algo = "sha1", file = TRUE ) )
                       }), 
                       "files.updated" = lapply( utils::tail( sort(test_file_refs), n = 1), function(x) {
                         c( "path" = x,
                            "sha1" = digest::digest( file.path( wrkarea, x, fsep = "/"), algo = "sha1", file = TRUE ) )
                       }) )
  
  
  test_job <- list( "mode" = "localfs",
                    "working.directory" = wrkdir,
                    "work.area" = wrkarea,
                    "output.locations" = test_output_parent,
                    "actions" = list(),
                    "inputs" = list() )
  
  
  # -- test 
  
  result <- cxlib:::.cxlib_batch_localfs_publish( test_job )
  
  
  # -- expectations
  
  expected_outputs <- lapply( sort(test_file_refs), function(x) {
    c( "path" = x,
       "sha1" = digest::digest( file.path( wrkarea, x, fsep = "/"), algo = "sha1", file = TRUE ) )
  })
  
  expected_result <- list( "mode" = "localfs",
                           "working.directory" = wrkdir,
                           "work.area" = wrkarea,
                           "output.locations" = test_output_parent,
                           "actions" = list(),
                           "inputs" = list(), 
                           "outputs" = list(),
                           "deleted" = list() )
  
  
  # -- assertions 
  
  # result
  testthat::expect_equal( result, expected_result )
  
  
  # files in work area exists
  testthat::expect_true( all( file.exists( file.path( wrkarea, base::substring( test_files, base::nchar(wrkarea) + 2 ), fsep = "/" )) ) )
  
  
  # files in work area are copied to working directory
  testthat::expect_false( all( file.exists(file.path( wrkdir, base::substring( test_files, base::nchar(wrkarea) + 2 ), fsep = "/" )) ) )
  

    
})



testthat::test_that( "batchlocalfs.publishJobProgramActionCreateUpdate", {
  
  
  # -- stage
  
  current_wd <- base::getwd()
  
  test_root <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit({
    base::setwd( current_wd )
    unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE)
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE) )
    testthat::fail( "Could not create test area" )
  
  
  # working directory
  
  wrkdir <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "working-directory-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( wrkdir ) || ! dir.create( wrkdir, recursive = TRUE) )
    testthat::fail( "Could not create test working directory" )
  
  base::setwd( wrkdir )
  
  
  # work area
  
  wrkarea <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "work-area-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( wrkarea ) || ! dir.create( wrkarea, recursive = TRUE) )
    testthat::fail( "Could not create test work area" )
  
  
  
  # stage output directory in work area and working directory
  
  test_output_parent <- file.path( "some", "path", "to", "outputs", fsep = "/" )
  
  for ( xpath in c( wrkarea, wrkdir ) )    
    if ( dir.exists( file.path( xpath, test_output_parent, fsep = "/" ) ) || ! dir.create( file.path( xpath, test_output_parent, fsep = "/" ), recursive = TRUE) )
      testthat::fail( "Could not create test output directory in work area" )
  
  
  # stage outputs in work area
  
  test_files <- replicate( 3,
                           cxlib:::.cxlib_standardpath( base::tempfile( pattern = "test-output-", tmpdir = file.path( wrkarea, test_output_parent, fsep = "/" ), fileext = ".txt" ) ),
                           simplify = TRUE )
  
  for ( xfile in test_files )
    base::writeLines( paste( sample( c( letters, LETTERS, as.character(0:9) ), 40 ), collapse = "" ), con = xfile )
  
  if ( ! all( file.exists( test_files ) ) )
    testthat::fail( "Could not stage test files in work area output directory" )
  
  
  test_file_refs <- base::substring( test_files, base::nchar(wrkarea) + 2)
  
  
  
  # set up job
  
  test_action <- list( "type" = "program",
                       "files.created" = lapply( utils::head( sort(test_file_refs), n = length(test_file_refs) - 1), function(x) {
                         c( "path" = x,
                            "sha1" = digest::digest( file.path( wrkarea, x, fsep = "/"), algo = "sha1", file = TRUE ) )
                        }), 
                       "files.updated" = lapply( utils::tail( sort(test_file_refs), n = 1), function(x) {
                         c( "path" = x,
                            "sha1" = digest::digest( file.path( wrkarea, x, fsep = "/"), algo = "sha1", file = TRUE ) )
                       }) )
  
  
  test_job <- list( "mode" = "localfs",
                    "working.directory" = wrkdir,
                    "work.area" = wrkarea,
                    "output.locations" = test_output_parent,
                    "actions" = list( test_action ),
                    "inputs" = list() )
  
  
  # -- test 
  
  result <- cxlib:::.cxlib_batch_localfs_publish( test_job )
  

  # -- expectations
  
  expected_outputs <- lapply( sort(test_file_refs), function(x) {
    c( "path" = x,
       "sha1" = digest::digest( file.path( wrkarea, x, fsep = "/"), algo = "sha1", file = TRUE ) )
  })

  expected_result <- list( "mode" = "localfs",
                           "working.directory" = wrkdir,
                           "work.area" = wrkarea,
                           "output.locations" = test_output_parent,
                           "actions" = list(test_action),
                           "inputs" = list(), 
                           "outputs" = expected_outputs,
                           "deleted" = list() )
  
  
  # -- assertions 
  
  # result
  testthat::expect_equal( result, expected_result )
  
  
  # files in work area exists
  testthat::expect_true( all( file.exists( file.path( wrkarea, base::substring( test_files, base::nchar(wrkarea) + 2 ), fsep = "/" )) ) )
  
  
  # files in work area are copied to working directory
  testthat::expect_true( all( file.exists(file.path( wrkdir, base::substring( test_files, base::nchar(wrkarea) + 2 ), fsep = "/" )) ) )
  
  
})





testthat::test_that( "batchlocalfs.publishJobProgramActionCreateUpdateOverwrite", {
  
  
  # -- stage
  
  current_wd <- base::getwd()
  
  test_root <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit({
    base::setwd( current_wd )
    unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE)
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE) )
    testthat::fail( "Could not create test area" )
  
  
  # working directory
  
  wrkdir <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "working-directory-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( wrkdir ) || ! dir.create( wrkdir, recursive = TRUE) )
    testthat::fail( "Could not create test working directory" )
  
  base::setwd( wrkdir )
  
  
  # work area
  
  wrkarea <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "work-area-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( wrkarea ) || ! dir.create( wrkarea, recursive = TRUE) )
    testthat::fail( "Could not create test work area" )
  
  
  
  # stage output directory in work area and working directory
  
  test_output_parent <- file.path( "some", "path", "to", "outputs", fsep = "/" )
  
  for ( xpath in c( wrkarea, wrkdir ) )    
    if ( dir.exists( file.path( xpath, test_output_parent, fsep = "/" ) ) || ! dir.create( file.path( xpath, test_output_parent, fsep = "/" ), recursive = TRUE) )
      testthat::fail( "Could not create test output directory in work area" )
  
  
  # stage outputs in work area
  
  test_files <- replicate( 3,
                           cxlib:::.cxlib_standardpath( base::tempfile( pattern = "test-output-", tmpdir = file.path( wrkarea, test_output_parent, fsep = "/" ), fileext = ".txt" ) ),
                           simplify = TRUE )
  
  for ( xfile in test_files )
    base::writeLines( paste( sample( c( letters, LETTERS, as.character(0:9) ), 40 ), collapse = "" ), con = xfile )
  
  if ( ! all( file.exists( test_files ) ) )
    testthat::fail( "Could not stage test files in work area output directory" )
  
  
  test_file_refs <- base::substring( test_files, base::nchar(wrkarea) + 2)
  
  
  # stage outputs in working directory for overwrite
  # note: using first and last
  # note: look in test_job .. first is created and last is updated
  
  for ( xfile in c( test_file_refs[ c( 1, length(test_file_refs) ) ] ) ) {
    
    base::writeLines( paste( sample( c( letters, LETTERS, as.character(0:9) ), 40 ), collapse = "" ), 
                      con = file.path( wrkdir, xfile, fsep = "/" ) )
   
    if ( digest::digest( file.path( wrkdir, xfile, fsep = "/" ), algo = "sha1", file = TRUE ) ==
         digest::digest( file.path( wrkarea, xfile, fsep = "/" ), algo = "sha1", file = TRUE ) )
      testthat::fail( "Could not stage test files in working directory" )
     
  }
  
  

  # set up job
  
  test_action <- list( "type" = "program",
                       "files.created" = lapply( utils::head( sort(test_file_refs), n = length(test_file_refs) - 1), function(x) {
                         c( "path" = x,
                            "sha1" = digest::digest( file.path( wrkarea, x, fsep = "/"), algo = "sha1", file = TRUE ) )
                       }), 
                       "files.updated" = lapply( utils::tail( sort(test_file_refs), n = 1), function(x) {
                         c( "path" = x,
                            "sha1" = digest::digest( file.path( wrkarea, x, fsep = "/"), algo = "sha1", file = TRUE ) )
                       }) )
  
  
  test_job <- list( "mode" = "localfs",
                    "working.directory" = wrkdir,
                    "work.area" = wrkarea,
                    "output.locations" = test_output_parent,
                    "actions" = list( test_action ),
                    "inputs" = list() )
  
  
  # -- test 
  
  result <- cxlib:::.cxlib_batch_localfs_publish( test_job )
  
  
  # -- expectations
  
  expected_outputs <- lapply( sort(test_file_refs), function(x) {
    c( "path" = x,
       "sha1" = digest::digest( file.path( wrkarea, x, fsep = "/"), algo = "sha1", file = TRUE ) )
  })
  
  expected_result <- list( "mode" = "localfs",
                           "working.directory" = wrkdir,
                           "work.area" = wrkarea,
                           "output.locations" = test_output_parent,
                           "actions" = list(test_action),
                           "inputs" = list(), 
                           "outputs" = expected_outputs,
                           "deleted" = list() )
  
  
  # -- assertions 
  
  # result
  testthat::expect_equal( result, expected_result )
  
  
  # files in work area exists
  testthat::expect_true( all( file.exists( file.path( wrkarea, base::substring( test_files, base::nchar(wrkarea) + 2 ), fsep = "/" )) ) )
  
  
  # files in work area are copied to working directory
  testthat::expect_true( all( file.exists(file.path( wrkdir, base::substring( test_files, base::nchar(wrkarea) + 2 ), fsep = "/" )) ) )
  
  
  # verify SHA-1 from work area
  for ( xout in expected_result[["outputs"]] ) 
    testthat::expect_equal( digest::digest( file.path( wrkdir, xout["path"], fsep = "/" ), algo = "sha1", file = TRUE ),
                            unname(xout["sha1"]) )

})




testthat::test_that( "batchlocalfs.publishJobProgramActionCreateDelete", {
  
  
  # -- stage
  
  current_wd <- base::getwd()
  
  test_root <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit({
    base::setwd( current_wd )
    unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE)
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE) )
    testthat::fail( "Could not create test area" )
  
  
  # working directory
  
  wrkdir <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "working-directory-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( wrkdir ) || ! dir.create( wrkdir, recursive = TRUE) )
    testthat::fail( "Could not create test working directory" )
  
  base::setwd( wrkdir )
  
  
  # work area
  
  wrkarea <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "work-area-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( wrkarea ) || ! dir.create( wrkarea, recursive = TRUE) )
    testthat::fail( "Could not create test work area" )
  
  
  
  # stage output directory in work area and working directory
  
  test_output_parent <- file.path( "some", "path", "to", "outputs", fsep = "/" )
  
  for ( xpath in c( wrkarea, wrkdir ) )    
    if ( dir.exists( file.path( xpath, test_output_parent, fsep = "/" ) ) || ! dir.create( file.path( xpath, test_output_parent, fsep = "/" ), recursive = TRUE) )
      testthat::fail( "Could not create test output directory in work area" )
  
  
  # stage outputs in work area
  
  test_files <- sort( replicate( 3,
                           cxlib:::.cxlib_standardpath( base::tempfile( pattern = "test-output-", tmpdir = file.path( wrkarea, test_output_parent, fsep = "/" ), fileext = ".txt" ) ),
                           simplify = TRUE ) )
  
  for ( xfile in test_files )
    base::writeLines( paste( sample( c( letters, LETTERS, as.character(0:9) ), 40 ), collapse = "" ), con = xfile )
  
  if ( ! all( file.exists( test_files ) ) )
    testthat::fail( "Could not stage test files in work area output directory" )
  
  
  test_file_refs <- base::substring( test_files, base::nchar(wrkarea) + 2)
  
  
  # stage outputs in working directory for overwrite
  # note: using first and last
  # note: look in test_job .. first is created and last is updated
  
  for ( xfile in c( test_file_refs[ c( 1, length(test_file_refs) ) ] ) ) {
    
    base::writeLines( paste( sample( c( letters, LETTERS, as.character(0:9) ), 40 ), collapse = "" ), 
                      con = file.path( wrkdir, xfile, fsep = "/" ) )
    
    if ( digest::digest( file.path( wrkdir, xfile, fsep = "/" ), algo = "sha1", file = TRUE ) ==
         digest::digest( file.path( wrkarea, xfile, fsep = "/" ), algo = "sha1", file = TRUE ) )
      testthat::fail( "Could not stage test files in working directory" )
    
  }
  
  
  
  # set up job
  
  test_action <- list( "type" = "program",
                       "files.created" = lapply( utils::head( sort(test_file_refs), n = length(test_file_refs) - 1), function(x) {
                         c( "path" = x,
                            "sha1" = digest::digest( file.path( wrkarea, x, fsep = "/"), algo = "sha1", file = TRUE ) )
                       }), 
                       "files.deleted" = lapply( utils::tail( sort(test_file_refs), n = 1), function(x) {
                         c( "path" = x,
                            "sha1" = digest::digest( file.path( wrkarea, x, fsep = "/"), algo = "sha1", file = TRUE ) )
                       }) )
  
  
  test_job <- list( "mode" = "localfs",
                    "working.directory" = wrkdir,
                    "work.area" = wrkarea,
                    "output.locations" = test_output_parent,
                    "actions" = list( test_action ),
                    "inputs" = list() )
  
  
  # -- test 
  
  result <- cxlib:::.cxlib_batch_localfs_publish( test_job )

  
  # -- expectations
  
  expected_outputs <- lapply( sort(test_file_refs), function(x) {
    c( "path" = x,
       "sha1" = digest::digest( file.path( wrkarea, x, fsep = "/"), algo = "sha1", file = TRUE ) )
  })
  
  expected_result <- list( "mode" = "localfs",
                           "working.directory" = wrkdir,
                           "work.area" = wrkarea,
                           "output.locations" = test_output_parent,
                           "actions" = list(test_action),
                           "inputs" = list(), 
                           "outputs" = utils::head( expected_outputs, n = 2),
                           "deleted" = utils::tail( expected_outputs, n = 1) )
  
  # -- assertions 
  
  # result
  testthat::expect_equal( result, expected_result )
  
  
  # files in work area exists
  # note: I know the action deletes the file .. just to make sure that it is deleted in the right place by publish
  testthat::expect_true( all( file.exists( file.path( wrkarea, base::substring( test_files, base::nchar(wrkarea) + 2 ), fsep = "/" )) ) )


  # files in work area are copied to working directory
  testthat::expect_true( all( file.exists( file.path( wrkdir, base::substring( utils::head( sort(test_files), n = 2), base::nchar(wrkarea) + 2 ), fsep = "/" )) ) )

  # file to be deleted is
  testthat::expect_false( all( file.exists(file.path( wrkdir, base::substring( utils::tail( sort(test_files), n = 1), base::nchar(wrkarea) + 2 ), fsep = "/" )) ) )


  # verify SHA-1 from work area
  for ( xout in expected_result[["outputs"]] ) 
    testthat::expect_equal( digest::digest( file.path( wrkdir, xout["path"], fsep = "/" ), algo = "sha1", file = TRUE ),
                            unname(xout["sha1"]) )

})




testthat::test_that( "batchlocalfs.publishJobProgramActionCreateDeleteOnlyExist", {
  
  
  # -- stage
  
  current_wd <- base::getwd()
  
  test_root <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit({
    base::setwd( current_wd )
    unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE)
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE) )
    testthat::fail( "Could not create test area" )
  
  
  # working directory
  
  wrkdir <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "working-directory-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( wrkdir ) || ! dir.create( wrkdir, recursive = TRUE) )
    testthat::fail( "Could not create test working directory" )
  
  base::setwd( wrkdir )
  
  
  # work area
  
  wrkarea <- cxlib:::.cxlib_standardpath( base::tempfile( pattern = "work-area-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( wrkarea ) || ! dir.create( wrkarea, recursive = TRUE) )
    testthat::fail( "Could not create test work area" )
  
  
  
  # stage output directory in work area and working directory
  
  test_output_parent <- file.path( "some", "path", "to", "outputs", fsep = "/" )
  
  for ( xpath in c( wrkarea, wrkdir ) )    
    if ( dir.exists( file.path( xpath, test_output_parent, fsep = "/" ) ) || ! dir.create( file.path( xpath, test_output_parent, fsep = "/" ), recursive = TRUE) )
      testthat::fail( "Could not create test output directory in work area" )
  
  
  # stage outputs in work area
  
  test_files <- sort( replicate( 3,
                                 cxlib:::.cxlib_standardpath( base::tempfile( pattern = "test-output-", tmpdir = file.path( wrkarea, test_output_parent, fsep = "/" ), fileext = ".txt" ) ),
                                 simplify = TRUE ) )
  
  for ( xfile in test_files )
    base::writeLines( paste( sample( c( letters, LETTERS, as.character(0:9) ), 40 ), collapse = "" ), con = xfile )
  
  if ( ! all( file.exists( test_files ) ) )
    testthat::fail( "Could not stage test files in work area output directory" )
  
  
  test_file_refs <- base::substring( test_files, base::nchar(wrkarea) + 2)
  
  
  # stage outputs in working directory for overwrite
  # note: using first and last
  # note: look in test_job .. first is created and last is updated
  
  for ( xfile in c( test_file_refs[ c( 1, length(test_file_refs) ) ] ) ) {
    
    base::writeLines( paste( sample( c( letters, LETTERS, as.character(0:9) ), 40 ), collapse = "" ), 
                      con = file.path( wrkdir, xfile, fsep = "/" ) )
    
    if ( digest::digest( file.path( wrkdir, xfile, fsep = "/" ), algo = "sha1", file = TRUE ) ==
         digest::digest( file.path( wrkarea, xfile, fsep = "/" ), algo = "sha1", file = TRUE ) )
      testthat::fail( "Could not stage test files in working directory" )
    
  }
  
  
  
  # set up job
  
  test_action <- list( "type" = "program",
                       "files.created" = lapply( utils::head( sort(test_file_refs), n = 1 ), function(x) {
                         c( "path" = x,
                            "sha1" = digest::digest( file.path( wrkarea, x, fsep = "/"), algo = "sha1", file = TRUE ) )
                       }), 
                       "files.deleted" = lapply( utils::tail( sort(test_file_refs), n = length(test_file_refs) - 1), function(x) {
                         c( "path" = x,
                            "sha1" = digest::digest( file.path( wrkarea, x, fsep = "/"), algo = "sha1", file = TRUE ) )
                       }) )
  
  
  test_job <- list( "mode" = "localfs",
                    "working.directory" = wrkdir,
                    "work.area" = wrkarea,
                    "output.locations" = test_output_parent,
                    "actions" = list( test_action ),
                    "inputs" = list() )
  
  
  # -- test 
  
  result <- cxlib:::.cxlib_batch_localfs_publish( test_job )
 
  
  # -- expectations
  
  expected_outputs <- lapply( sort(test_file_refs), function(x) {
    c( "path" = x,
       "sha1" = digest::digest( file.path( wrkarea, x, fsep = "/"), algo = "sha1", file = TRUE ) )
  })
  
  expected_result <- list( "mode" = "localfs",
                           "working.directory" = wrkdir,
                           "work.area" = wrkarea,
                           "output.locations" = test_output_parent,
                           "actions" = list(test_action),
                           "inputs" = list(), 
                           "outputs" = utils::head( expected_outputs, n = 1),
                           "deleted" = utils::tail( expected_outputs, n = 1) )
  
  # -- assertions 
  
  # result
  testthat::expect_equal( result, expected_result )
  
  
  # files in work area exists
  # note: I know the action deletes the file .. just to make sure that it is deleted in the right place by publish
  testthat::expect_true( all( file.exists( file.path( wrkarea, base::substring( test_files, base::nchar(wrkarea) + 2 ), fsep = "/" )) ) )
  
  
  # files in work area are copied to working directory
  testthat::expect_true( all( file.exists( file.path( wrkdir, base::substring( utils::head( sort(test_files), n = 1), base::nchar(wrkarea) + 2 ), fsep = "/" )) ) )
  
  # file to be deleted is
  testthat::expect_false( all( file.exists(file.path( wrkdir, base::substring( utils::tail( sort(test_files), n = 2), base::nchar(wrkarea) + 2 ), fsep = "/" )) ) )
  
  
  # verify SHA-1 from work area
  for ( xout in expected_result[["outputs"]] ) 
    testthat::expect_equal( digest::digest( file.path( wrkdir, xout["path"], fsep = "/" ), algo = "sha1", file = TRUE ),
                            unname(xout["sha1"]) )
  
})


