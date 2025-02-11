#
#  Tests for cxlib::cxlib_batch()
#
#
#
#
#
# TODO
#
#  batch.localComplexProgramsAltLogDirLogSuffix
#   - Dig into each action and trace inputs, outputs and deleted for each action
#
  


testthat::test_that( "batch.localSingleProgram", {

    
  # -- stage

  current_wd <- base::getwd()
  
  test_root <- base::gsub( "\\\\", "/", base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit({
    base::setwd( current_wd )
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Could not stage test root" )

  # set test root as working directory
  base::setwd( test_root )

  
  # test reference
  test_reference <- paste( base::sample( c( letters, LETTERS, as.character(0:9) ) , 40 ), collapse = "" )

  
  # test programs
  test_program_parent <- file.path( "some", "path", "to", "programs", fsep = "/" )
  
  if ( dir.exists( file.path( test_root, test_program_parent, fsep = "/" ) ) ||
       ! dir.create( file.path( test_root, test_program_parent, fsep = "/" ), recursive = TRUE ) )
      stop( "Could not stage test program parent directory" )
  
  
  test_program <- base::gsub( "\\\\", "/", base::tempfile( pattern = "test-program-", tmpdir = file.path( test_root, test_program_parent, fsep = "/" ), fileext = ".R" ) )
  
  pgm <- c( "# test program",
            paste0( "cat( \"--->", test_reference, "<---\", sep = \"\")") )
  
  base::writeLines( pgm, con = test_program )

  if ( ! file.exists( test_program ) )
    testthat::fail( "Unexpected test program does not exist" )

    
  test_program_ref <- base::substring( test_program, base::nchar(test_root) + 2 )
  
  
  
  # -- test
  result <- cxlib::cxlib_batch( test_program_ref, silent = TRUE )
  


  # -- expectations
  
  expected_program <- c( "path" = test_program_ref,
                         "sha1" = digest::digest( test_program, algo = "sha1", file = TRUE) )
  
  expected_logfile <-  c( "path" = paste0( tools::file_path_sans_ext( test_program_ref ), ".Rout" ), 
                          "sha1" = digest::digest( paste0( tools::file_path_sans_ext( test_program ), ".Rout" ), algo = "sha1", file = TRUE ) )
  
    
  expected_action <- list( "type" = "program",
                           "path" = unname(expected_program["path"]),
                           "sha1" = unname(expected_program["sha1"]),
                           "log" = c( "path" = unname(expected_logfile["path"]), 
                                      "sha1" = unname(expected_logfile["sha1"]),
                                      "reference.sha1" = NA ),
                           "files.input" = list( expected_program ),
                           "files.created" = list( expected_logfile ),
                           "files.updated" = list(),
                           "files.deleted" = list()
                         ) 
                            

  expected_inputs <- list( expected_program )  
  expected_outputs <- list( expected_logfile )
                           

  # -- assertions
  #    note: cherry-picking checks to ignore dynamic elements
  
  testthat::expect_equal( result[["mode"]], "localfs" )
  testthat::expect_length( result[["output.locations"]], 0 )

  testthat::expect_equal( result[["actions"]][[1]][ sort(names(expected_action)) ], expected_action[ sort(names(expected_action)) ] )  
  
  testthat::expect_equal( result[["inputs"]], expected_inputs )
  testthat::expect_equal( result[["outputs"]], expected_outputs )
  
  
})




testthat::test_that( "batch.localSingleProgramAltLog", {
  
  
  # -- stage
  
  current_wd <- base::getwd()
  
  test_root <- base::gsub( "\\\\", "/", base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit({
    base::setwd( current_wd )
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Could not stage test root" )
  
  # set test root as working directory
  base::setwd( test_root )
  
  
  # test reference
  test_reference <- paste( base::sample( c( letters, LETTERS, as.character(0:9) ) , 40 ), collapse = "" )
  
  
  # test programs
  test_program_parent <- file.path( "some", "path", "to", "programs", fsep = "/" )
  
  if ( dir.exists( file.path( test_root, test_program_parent, fsep = "/" ) ) ||
       ! dir.create( file.path( test_root, test_program_parent, fsep = "/" ), recursive = TRUE ) )
    stop( "Could not stage test program parent directory" )
  
  
  test_program <- base::gsub( "\\\\", "/", base::tempfile( pattern = "test-program-", tmpdir = file.path( test_root, test_program_parent, fsep = "/" ), fileext = ".R" ) )
  
  pgm <- c( "# test program",
            paste0( "cat( \"--->", test_reference, "<---\", sep = \"\")") )
  
  base::writeLines( pgm, con = test_program )
  
  if ( ! file.exists( test_program ) )
    testthat::fail( "Unexpected test program does not exist" )
  
  
  test_program_ref <- base::substring( test_program, base::nchar(test_root) + 2 )
  
  
  
  # -- test
  result <- cxlib::cxlib_batch( test_program_ref, options = list( "log.fileext" = "log" ), silent = TRUE )
  
  
  
  # -- expectations
  
  expected_program <- c( "path" = test_program_ref,
                         "sha1" = digest::digest( test_program, algo = "sha1", file = TRUE) )
  
  expected_logfile <-  c( "path" = paste0( tools::file_path_sans_ext( test_program_ref ), ".log" ), 
                          "sha1" = digest::digest( paste0( tools::file_path_sans_ext( test_program ), ".log" ), algo = "sha1", file = TRUE ) )
  
  
  expected_action <- list( "type" = "program",
                           "path" = unname(expected_program["path"]),
                           "sha1" = unname(expected_program["sha1"]),
                           "log" = c( "path" = unname(expected_logfile["path"]), 
                                      "sha1" = unname(expected_logfile["sha1"]),
                                      "reference.sha1" = NA ),
                           "files.input" = list( expected_program ),
                           "files.created" = list( expected_logfile ),
                           "files.updated" = list(),
                           "files.deleted" = list()
  ) 
  
  
  expected_inputs <- list( expected_program )  
  expected_outputs <- list( expected_logfile )
  
  
  # -- assertions
  #    note: cherry-picking checks to ignore dynamic elements
  
  testthat::expect_equal( result[["mode"]], "localfs" )
  testthat::expect_length( result[["output.locations"]], 0 )
  
  testthat::expect_equal( result[["actions"]][[1]][ sort(names(expected_action)) ], expected_action[ sort(names(expected_action)) ] )  
  
  testthat::expect_equal( result[["inputs"]], expected_inputs )
  testthat::expect_equal( result[["outputs"]], expected_outputs )
  
  
})



testthat::test_that( "batch.localSingleProgramAltLogDir", {
  
  
  # -- stage
  
  current_wd <- base::getwd()
  
  test_root <- base::gsub( "\\\\", "/", base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit({
    base::setwd( current_wd )
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Could not stage test root" )
  
  # set test root as working directory
  base::setwd( test_root )
  
  
  # test reference
  test_reference <- paste( base::sample( c( letters, LETTERS, as.character(0:9) ) , 40 ), collapse = "" )
  
  
  # test programs
  test_program_parent <- file.path( "some", "path", "to", "programs", fsep = "/" )
  
  if ( dir.exists( file.path( test_root, test_program_parent, fsep = "/" ) ) ||
       ! dir.create( file.path( test_root, test_program_parent, fsep = "/" ), recursive = TRUE ) )
    stop( "Could not stage test program parent directory" )
  
  
  test_program <- base::gsub( "\\\\", "/", base::tempfile( pattern = "test-program-", tmpdir = file.path( test_root, test_program_parent, fsep = "/" ), fileext = ".R" ) )
  
  pgm <- c( "# test program",
            paste0( "cat( \"--->", test_reference, "<---\", sep = \"\")") )
  
  base::writeLines( pgm, con = test_program )
  
  if ( ! file.exists( test_program ) )
    testthat::fail( "Unexpected test program does not exist" )
  
  
  test_program_ref <- base::substring( test_program, base::nchar(test_root) + 2 )
  
  
  # log
  test_log_parent <- file.path( "some", "path", "to", "logs", fsep = "/" )
  
  if ( dir.exists( file.path( test_root, test_log_parent, fsep = "/" ) ) ||
       ! dir.create( file.path( test_root, test_log_parent, fsep = "/" ), recursive = TRUE ) )
    stop( "Could not stage test log parent directory" )
  
  
  
  # -- test
  result <- cxlib::cxlib_batch( test_program_ref, options = list( "logs" = test_log_parent ), silent = TRUE )
  
  
  
  # -- expectations
  
  expected_program <- c( "path" = test_program_ref,
                         "sha1" = digest::digest( test_program, algo = "sha1", file = TRUE) )
  
  expected_logfile_path <- file.path( test_log_parent, base::basename( paste0( tools::file_path_sans_ext( test_program_ref ), ".Rout" ) ), fsep = "/")
  
  expected_logfile <-  c( "path" = expected_logfile_path, 
                          "sha1" = digest::digest( file.path( test_root, expected_logfile_path, fsep = "/"), algo = "sha1", file = TRUE ) )
  
  
  expected_action <- list( "type" = "program",
                           "path" = unname(expected_program["path"]),
                           "sha1" = unname(expected_program["sha1"]),
                           "log" = c( "path" = unname(expected_logfile["path"]), 
                                      "sha1" = unname(expected_logfile["sha1"]),
                                      "reference.sha1" = NA ),
                           "files.input" = list( expected_program ),
                           "files.created" = list( expected_logfile ),
                           "files.updated" = list(),
                           "files.deleted" = list()
  ) 
  
  
  expected_inputs <- list( expected_program )  
  expected_outputs <- list( expected_logfile )
  
  
  # -- assertions
  #    note: cherry-picking checks to ignore dynamic elements
  
  testthat::expect_equal( result[["mode"]], "localfs" )
  testthat::expect_length( result[["output.locations"]], 0 )
  
  testthat::expect_equal( result[["actions"]][[1]][ sort(names(expected_action)) ], expected_action[ sort(names(expected_action)) ] )  
  
  testthat::expect_equal( result[["inputs"]], expected_inputs )
  testthat::expect_equal( result[["outputs"]], expected_outputs )
  
})



testthat::test_that( "batch.localSingleProgramAltLogDirLogSuffix", {
  
  
  # -- stage
  
  current_wd <- base::getwd()
  
  test_root <- base::gsub( "\\\\", "/", base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit({
    base::setwd( current_wd )
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Could not stage test root" )
  
  # set test root as working directory
  base::setwd( test_root )
  
  
  # test reference
  test_reference <- paste( base::sample( c( letters, LETTERS, as.character(0:9) ) , 40 ), collapse = "" )
  
  
  # test programs
  test_program_parent <- file.path( "some", "path", "to", "programs", fsep = "/" )
  
  if ( dir.exists( file.path( test_root, test_program_parent, fsep = "/" ) ) ||
       ! dir.create( file.path( test_root, test_program_parent, fsep = "/" ), recursive = TRUE ) )
    stop( "Could not stage test program parent directory" )
  
  
  test_program <- base::gsub( "\\\\", "/", base::tempfile( pattern = "test-program-", tmpdir = file.path( test_root, test_program_parent, fsep = "/" ), fileext = ".R" ) )
  
  pgm <- c( "# test program",
            paste0( "cat( \"--->", test_reference, "<---\", sep = \"\")") )
  
  base::writeLines( pgm, con = test_program )
  
  if ( ! file.exists( test_program ) )
    testthat::fail( "Unexpected test program does not exist" )
  
  
  test_program_ref <- base::substring( test_program, base::nchar(test_root) + 2 )
  
  
  # log
  test_log_parent <- file.path( "some", "path", "to", "logs", fsep = "/" )
  
  if ( dir.exists( file.path( test_root, test_log_parent, fsep = "/" ) ) ||
       ! dir.create( file.path( test_root, test_log_parent, fsep = "/" ), recursive = TRUE ) )
    stop( "Could not stage test log parent directory" )
  
  
  
  # -- test
  result <- cxlib::cxlib_batch( test_program_ref, options = list( "logs" = test_log_parent, "log.fileext" = "log" ), silent = TRUE )
  
  
  
  # -- expectations
  
  expected_program <- c( "path" = test_program_ref,
                         "sha1" = digest::digest( test_program, algo = "sha1", file = TRUE) )
  
  expected_logfile_path <- file.path( test_log_parent, base::basename( paste0( tools::file_path_sans_ext( test_program_ref ), ".log" ) ), fsep = "/")
  
  expected_logfile <-  c( "path" = expected_logfile_path, 
                          "sha1" = digest::digest( file.path( test_root, expected_logfile_path, fsep = "/"), algo = "sha1", file = TRUE ) )
  
  
  expected_action <- list( "type" = "program",
                           "path" = unname(expected_program["path"]),
                           "sha1" = unname(expected_program["sha1"]),
                           "log" = c( "path" = unname(expected_logfile["path"]), 
                                      "sha1" = unname(expected_logfile["sha1"]),
                                      "reference.sha1" = NA ),
                           "files.input" = list( expected_program ),
                           "files.created" = list( expected_logfile ),
                           "files.updated" = list(),
                           "files.deleted" = list()
  ) 
  
  
  expected_inputs <- list( expected_program )  
  expected_outputs <- list( expected_logfile )
  
  
  # -- assertions
  #    note: cherry-picking checks to ignore dynamic elements
  
  testthat::expect_equal( result[["mode"]], "localfs" )
  testthat::expect_length( result[["output.locations"]], 0 )
  
  testthat::expect_equal( result[["actions"]][[1]][ sort(names(expected_action)) ], expected_action[ sort(names(expected_action)) ] )  
  
  testthat::expect_equal( result[["inputs"]], expected_inputs )
  testthat::expect_equal( result[["outputs"]], expected_outputs )
  
})




testthat::test_that( "batch.localComplexProgramsAltLogDirLogSuffix", {

  
  # -- stage
  
  current_wd <- base::getwd()
  
  test_root <- base::gsub( "\\\\", "/", base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit({
    base::setwd( current_wd )
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Could not stage test root" )
  
  # set test root as working directory
  base::setwd( test_root )
  
  
  
  # - test inputs 
  
  test_input_parent <- file.path( "some", "path", "to", "inputs", fsep = "/" )
  
  if ( dir.exists( file.path( test_root, test_input_parent, fsep = "/" ) ) ||
       ! dir.create( file.path( test_root, test_input_parent, fsep = "/" ), recursive = TRUE ) )
    stop( "Could not stage test input parent directory" )
  
  
  test_input_files <-  base::gsub( "\\\\", "/", base::replicate( 300, 
                                                                 base::tempfile( pattern = "test-input-", tmpdir = file.path( test_root, test_input_parent, fsep = "/" ), fileext = ".txt" ),
                                                                 simplify = TRUE ) )
  
  for ( xpath in test_input_files ) {
    
    base::writeLines( c( "# test input",
                         paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9)), 25 ), collapse = "") ), 
                      con = xpath )
    
    if ( ! file.exists( xpath ) )
      testthat::fail( "Could not stage input file" )
    
  }
  
  test_input_refs <- base::substring( test_input_files, base::nchar( test_root ) + 2 )
  
  
  
  # - test outputs

  test_output_parent <- file.path( "some", "path", "to", "outputs", fsep = "/" )
  
  if ( dir.exists( file.path( test_root, test_output_parent, fsep = "/" ) ) ||
       ! dir.create( file.path( test_root, test_output_parent, fsep = "/" ), recursive = TRUE ) )
    stop( "Could not stage test output parent directory" )
  
  
  #   note: seed with 10 existing outputs
  test_output_files <-  base::gsub( "\\\\", "/", base::replicate( 600, 
                                                                 base::tempfile( pattern = "test-output-", tmpdir = file.path( test_root, test_output_parent, fsep = "/" ), fileext = ".txt" ),
                                                                 simplify = TRUE ) )
  

  for ( xpath in test_output_files ) {
    
    base::writeLines( c( "# test input",
                         paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9)), 25 ), collapse = "") ), 
                      con = xpath )
    
    if ( ! file.exists( xpath ) )
      testthat::fail( "Could not stage output file" )
    
  }


  test_output_file_refs <- base::substring( test_output_files, base::nchar(test_root) + 2 )
  

  
  # - test programs
  #   note: creating 4 programs
  
  test_program_parent <- file.path( "some", "path", "to", "programs", fsep = "/" )
  
  if ( dir.exists( file.path( test_root, test_program_parent, fsep = "/" ) ) ||
       ! dir.create( file.path( test_root, test_program_parent, fsep = "/" ), recursive = TRUE ) )
    stop( "Could not stage test program parent directory" )
  
  
  test_programs <- character(0)

  for ( xcontext in c( "random", "create", "update", "delete") )
    test_programs <- append( test_programs, 
                             base::gsub( "\\\\", "/", base::tempfile( pattern = paste0( "test-program-", xcontext, "-"), tmpdir = file.path( test_root, test_program_parent, fsep = "/" ), fileext = ".R" ) ) )
  
  
  #   program 1:  random string in log
  
  pgm <- c( "# test program 1", 
            "# random string", 
            "cat( paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9)), 25 ), collapse = \"\"), sep = \"\\n\")" ) 
  
  base::writeLines( pgm, con = test_programs[1] )

  # cat( c( "--- program 1 ---", pgm, "---x---" ), sep = "\n" )
  
  
  #   program 2:  create new output from an input
  
  pgm <- c( "# test program 2",
            "# create new output from an input",
            paste( "# @cx.input", utils::head( test_input_refs, n = 1 ) ),
            paste0( "txt_input <- base::readLines( con = \"", utils::head( test_input_refs, n = 1 ), "\" )" ),
            paste( "# @cx.output", test_output_parent),
            paste0( "out <- base::gsub( \"\\\\\\\\\", \"/\", base::tempfile( pattern = \"output-\", tmpdir = \"", test_output_parent, "\", fileext = \".txt\") )"), 
            "cat( txt_input, sep = \"\\n\", file = out )" ) 
  
  base::writeLines( pgm, con = test_programs[2] )
  
  # cat( c( "--- program 2 ---", pgm, "---x---" ), sep = "\n" )
  
    
  #   program 3:  update existing output

  pgm <- c( "# test program 3",
            "# update an existing output",
            paste( "# @cx.input", utils::head( test_output_file_refs, n = 1 )),
            paste( "# @cx.output", test_output_parent),
            paste0( "out <- \"", utils::head( test_output_file_refs, n = 1 ), "\"" ), 
            "cat( paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9)), 25 ), collapse = \"\"), sep = \"\\n\", file = out )" ) 
  
  base::writeLines( pgm, con = test_programs[3] )
  
  # cat( c( "--- program 3 ---", pgm, "---x---" ), sep = "\n" )
  
  
  #   program 4:  delete an input
  
  pgm <- c( "# test program 4",
            "# delete an existing input",
            paste( "# @cx.input", utils::tail( test_input_refs, n = 1 ) ),
            paste( "# @cx.output", test_input_parent),
            paste0( "to_delete <- \"", utils::tail( test_input_refs, n = 1 ), "\"" ), 
            "base::unlink( to_delete, force = TRUE )" ) 
  
  base::writeLines( pgm, con = test_programs[4] )
  
  # cat( c( "--- program 4 ---", pgm, "---x---" ), sep = "\n" )
  
  
  if ( ! all( file.exists( test_programs ) ) )
    testthat::fail( "Could not stage programs")


  test_program_refs <- base::substring( test_programs, base::nchar(test_root) + 2 )

    
  
  # log
  test_log_parent <- file.path( "some", "path", "to", "logs", fsep = "/" )
  
  if ( dir.exists( file.path( test_root, test_log_parent, fsep = "/" ) ) ||
       ! dir.create( file.path( test_root, test_log_parent, fsep = "/" ), recursive = TRUE ) )
    stop( "Could not stage test log parent directory" )
  
  
  # reference inventory of staged working directory
  test_input_inv <- sapply( list.files( test_root, recursive = TRUE, full.names = FALSE, include.dirs = FALSE ), function(x) {
   digest::digest( file.path( test_root, x, fsep = "/"), algo = "sha1", file = TRUE )
  }, USE.NAMES = TRUE )
  
  
  
  # -- test
  results <- cxlib::cxlib_batch( test_program_refs, options = list( "logs" = test_log_parent, "log.fileext" = "log" ), silent = TRUE )
  

  # -- expectations

  post_inv <- sapply( list.files( test_root, recursive = TRUE, full.names = FALSE, include.dirs = FALSE ), function(x) {
    digest::digest( file.path( test_root, x, fsep = "/"), algo = "sha1", file = TRUE )
  }, USE.NAMES = TRUE )
  
  post_inv_files <- names(post_inv)
  


  # actions
  expected_action_order <- test_program_refs


  # logs
  expected_log_files <- file.path( test_log_parent, paste0( base::basename(tools::file_path_sans_ext( test_program_refs)), ".log" ), fsep = "/" )
  
  expected_logs <- lapply( expected_log_files, function(x) {
    c( "path" = x,
       "sha1" = unname(post_inv[x]), 
       "reference.sha1" = NA )
  })
  
  
  # inputs

  expected_input_files <- c( test_program_refs, 
                             utils::head( test_input_refs, n = 1 ),
                             utils::head( test_output_file_refs, n = 1 ),
                             utils::tail( test_input_refs, n = 1 )
                            )

  expected_inputs <- lapply( sort(expected_input_files), function(x) {
    c( "path" = x,
       "sha1" = unname(test_input_inv[x]) )
  })
  

  # outputs
  
  expected_output_files <- c( post_inv_files[ grepl( paste0( "^", test_output_parent, "/output-[a-z0-9]*\\.txt$"), post_inv_files, perl = TRUE, ignore.case = TRUE ) ],
                              utils::head( test_output_file_refs, n = 1 ),
                              post_inv_files[ grepl( paste0( "^some/path/to/logs/test-program-[a-z]*-[a-z0-9]*\\.log$"), post_inv_files, perl = TRUE, ignore.case = TRUE ) ]
                            )
  
  expected_outputs <- lapply( sort(expected_output_files), function(x) {
    c( "path" = x,
       "sha1" = unname(post_inv[x]) )
  })

  

  # deleted
  
  expected_deleted_files <- utils::tail( test_input_refs, n = 1 )
  
  expected_deleted <- lapply( sort(expected_deleted_files), function(x) {
    c( "path" = x,
       "sha1" = unname(test_input_inv[x]) )
  })
  
  
  
  # unchanged
  
  inv_common_files <- intersect( post_inv_files, names(test_input_inv) )
  
  expected_unchanged_files <- inv_common_files[ ! inv_common_files %in% utils::head( test_output_file_refs, n = 1 ) ]
  
  

  # -- assertions

  # unchanged files
  # note: this covers programs not changed
  testthat::expect_equal( test_input_inv[ sort(expected_unchanged_files) ], post_inv[ sort(expected_unchanged_files) ] )

  # actions
  testthat::expect_length( results[["actions"]], length(expected_action_order) )
  
  action_lst <- sapply( results[["actions"]], function(x) {
    unname(x[["path"]])
  }, USE.NAMES = FALSE)
  
  testthat::expect_equal( action_lst, expected_action_order )
  
  
  # logs
  log_lst <- lapply( results[["actions"]], function(x) {
    x[["log"]]
  })
  
  testthat::expect_equal( log_lst, expected_logs)
  
  
  # input files 
  testthat::expect_equal( results[["inputs"]], expected_inputs )
  testthat::expect_true( all( file.exists( file.path( test_root, expected_input_files[ ! expected_input_files %in% expected_deleted_files ], fsep = "/" ) ) ) )

  # output files
  # note: file exist is derived from post inventory
  # note: correct hash is derived from post inventory
  testthat::expect_equal( results[["outputs"]], expected_outputs )
  testthat::expect_true( all( file.exists( file.path( test_root, expected_output_files, fsep = "/" ) ) ) )
  
  # deleted files
  testthat::expect_equal( results[["deleted"]], expected_deleted )
  testthat::expect_false( all( file.exists( file.path( test_root, expected_deleted_files, fsep = "/" ) ) ) )
  
})




