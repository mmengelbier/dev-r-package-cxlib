#
#   Tests for cxlib:::.cxlib_batch_localfs_stage()
#
#   input annotations
#
#




testthat::test_that( "batchlocalfs-input.programNoInput", {
  
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
  
  
  expected_inputs <- lapply( sort(list.files( test_root, recursive = TRUE, include.dirs = FALSE, full.names = FALSE)), function(x) {
    c( "path" = x, 
       "sha1" = digest::digest( file.path( test_root, x, fsep = "/"), algo = "sha1", file = TRUE ) )
    
  })
  
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
  testthat::expect_true( "inputs" %in% names(result) )
  testthat::expect_equal( result[["inputs"]], expected_inputs )  

})




testthat::test_that( "batchlocalfs-input.programInputNotExist", {
  
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
  
  # test input
  test_input <- base::gsub( "\\\\", "/", base::tempfile( pattern = "test-input-", tmpdir = file.path( test_root, "some", "path", "to", "input", fsep = "/" ), fileext = ".txt" ) )
  
  if ( file.exists( test_input ) )
    testthat::fail( "Test input exists" )
  
  if ( ! dir.exists( base::dirname(test_input) ) && ! dir.create( base::dirname(test_input), recursive = TRUE) )
    testthat::fail( "Could not create parent directory for non-existant input" )
  
  test_input_ref <- base::substring( test_input, base::nchar( test_root ) + 2 )
  
  
  # test program
  test_program <- base::gsub( "\\\\", "/", base::tempfile( pattern = "test-program-", tmpdir = file.path( test_root, "some", "path", "to", "programs", fsep = "/" ), fileext = ".R" ) )
  
  if ( dir.exists( base::dirname( test_program ) ) || ! dir.create( base::dirname( test_program ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  pgm <- c( "# test program",
            paste( "# @cx.input", test_input_ref) )

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
  
  
  expected_inputs <- lapply( sort(list.files( test_root, recursive = TRUE, include.dirs = FALSE, full.names = FALSE)), function(x) {
    c( "path" = x, 
       "sha1" = digest::digest( file.path( test_root, x, fsep = "/"), algo = "sha1", file = TRUE ) )
    
  })
  
  
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
  testthat::expect_true( "inputs" %in% names(result) )
  testthat::expect_equal( result[["inputs"]], expected_inputs )  
  
  
})






testthat::test_that( "batchlocalfs-input.programInputFile", {
  
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
  
  
  # test input
  test_input <- base::gsub( "\\\\", "/", base::tempfile( pattern = "test-input-", tmpdir = file.path( test_root, "some", "path", "to", "input", fsep = "/" ), fileext = ".txt" ) )
  
  if ( file.exists( test_input ) )
    testthat::fail( "Test input exists" )
  
  if ( ! dir.exists( base::dirname(test_input) ) && ! dir.create( base::dirname(test_input), recursive = TRUE) )
    testthat::fail( "Could not create parent directory for input" )

  base::writeLines( paste( base::sample( base::letters, 20, replace = TRUE), collapse = "" ), con = test_input )
  
    
  test_input_ref <- base::substring( test_input, base::nchar( test_root ) + 2 )
  
  
  
  # test program
  test_program <- base::gsub( "\\\\", "/", base::tempfile( pattern = "test-program-", tmpdir = file.path( test_root, "some", "path", "to", "programs", fsep = "/" ), fileext = ".R" ) )
  
  if ( dir.exists( base::dirname( test_program ) ) || ! dir.create( base::dirname( test_program ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )

  pgm <- c( "# test program",
            paste( "# @cx.input", test_input_ref) )  
    
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
  
  expected_inputs <- lapply( sort(list.files( test_root, recursive = TRUE, include.dirs = FALSE, full.names = FALSE)), function(x) {
    c( "path" = x, 
       "sha1" = digest::digest( file.path( test_root, x, fsep = "/"), algo = "sha1", file = TRUE ) )
    
  })
  
  
  
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
  
  
  # input annotations
  testthat::expect_true( "inputs" %in% names(result) )
  testthat::expect_equal( result[["inputs"]], expected_inputs )
  
})






testthat::test_that( "batchlocalfs-input.programInputDirectory", {
  
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
  
  
  # test input
  
  test_inputs <- character(0)
  
  for ( i in 1:10 )
    test_inputs <- append( test_inputs,
                           base::gsub( "\\\\", "/", base::tempfile( pattern = "test-input-", tmpdir = file.path( test_root, "some", "path", "to", "inputs", fsep = "/" ), fileext = ".txt" ) ) )
  
  if ( any(file.exists( test_inputs )) )
    testthat::fail( "Test inputs exists" )
  
  test_inputs_parent <- base::unique(base::dirname(test_inputs))
    
  if ( ! dir.exists( test_inputs_parent ) && ! dir.create( test_inputs_parent, recursive = TRUE) )
    testthat::fail( "Could not create parent directory for inputs" )
  
  for ( xfile in test_inputs ) 
    base::writeLines( paste( base::sample( base::letters, 20, replace = TRUE), collapse = "" ), con = xfile )
  
  test_inputs_ref <- base::substring( test_inputs, base::nchar( test_root ) + 2 )
  
  
  
  # test program
  test_program <- base::gsub( "\\\\", "/", base::tempfile( pattern = "test-program-", tmpdir = file.path( test_root, "some", "path", "to", "programs", fsep = "/" ), fileext = ".R" ) )
  
  if ( dir.exists( base::dirname( test_program ) ) || ! dir.create( base::dirname( test_program ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  pgm <- c( "# test program",
            paste( "# @cx.input", base::substring( test_inputs_parent, base::nchar( test_root ) + 2 ) ) )  
  
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
  
  expected_inputs <- lapply( sort(list.files( test_root, recursive = TRUE, include.dirs = FALSE, full.names = FALSE)), function(x) {
    c( "path" = x, 
       "sha1" = digest::digest( file.path( test_root, x, fsep = "/"), algo = "sha1", file = TRUE ) )
    
  })


  
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
  
  
  # input annotations
  testthat::expect_true( "inputs" %in% names(result) )
  testthat::expect_length( result[["inputs"]], length( expected_inputs ) )
  testthat::expect_equal( result[["inputs"]], expected_inputs )
  
})





testthat::test_that( "batchlocalfs-input.programMultiInput", {
  
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
  
  
  # test input
  
  test_inputdir_parent <- file.path( test_root, "some", "path", "to", "multi-inputs", fsep = "/" )

  if ( ! dir.exists( test_inputdir_parent ) && ! dir.create( test_inputdir_parent, recursive = TRUE) )
    testthat::fail( "Could not create parent directory for input" )
  
    
  test_inputdir_files <- replicate( 5, 
                                    base::gsub( "\\\\", "/", base::tempfile( pattern = "test-input-", tmpdir = test_inputdir_parent , fileext = ".txt" ) ),
                                    simplify = TRUE  )
  
  if ( any(file.exists( test_inputdir_files )) )
    testthat::fail( "Test input exists" )

    
  for ( xfile in test_inputdir_files ) 
    base::writeLines( paste( base::sample( base::letters, 20, replace = TRUE), collapse = "" ), con = xfile )
  
  
  test_inputdir_ref <- base::substring( test_inputdir_parent, base::nchar( test_root ) + 2 )
    
  
  
  test_input_file <- base::gsub( "\\\\", "/", base::tempfile( pattern = "test-input-", tmpdir = file.path( test_root, "some", "path", "to", "input", fsep = "/" ), fileext = ".txt" ) )
  
  if ( file.exists( test_input_file ) )
    testthat::fail( "Test input exists" )
  
  if ( ! dir.exists( base::dirname(test_input_file) ) && ! dir.create( base::dirname(test_input_file), recursive = TRUE) )
    testthat::fail( "Could not create parent directory for input" )
  
  base::writeLines( paste( base::sample( base::letters, 20, replace = TRUE), collapse = "" ), con = test_input_file )
  
  
  test_input_file_ref <- base::substring( test_input_file, base::nchar( test_root ) + 2 )
  
  
  
  # test program
  test_program <- base::gsub( "\\\\", "/", base::tempfile( pattern = "test-program-", tmpdir = file.path( test_root, "some", "path", "to", "programs", fsep = "/" ), fileext = ".R" ) )
  
  if ( dir.exists( base::dirname( test_program ) ) || ! dir.create( base::dirname( test_program ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  pgm <- c( "# test program",
            "# input file",
            paste( "# @cx.input", test_input_file_ref),
            "# some more inputs",
            "# input directory",
            paste( "# @cx.input", test_inputdir_ref) )  
  
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
  

  expected_inputs <- lapply( sort(list.files( test_root, recursive = TRUE, include.dirs = FALSE, full.names = FALSE)), function(x) {
    c( "path" = x, 
       "sha1" = digest::digest( file.path( test_root, x, fsep = "/"), algo = "sha1", file = TRUE ) )
    
  })
  
  
  
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
  
  
  # input annotations
  testthat::expect_true( "inputs" %in% names(result) )
  testthat::expect_length( result[["inputs"]], length(expected_inputs) )
  testthat::expect_equal( result[["inputs"]], expected_inputs )
  
})




testthat::test_that( "batchlocalfs-input.multiProgramInputWithDuplicates", {
  
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
  
  
  # test input
  
  test_inputdir_parent <- file.path( test_root, "some", "path", "to", "multi-inputs", fsep = "/" )
  
  if ( ! dir.exists( test_inputdir_parent ) && ! dir.create( test_inputdir_parent, recursive = TRUE) )
    testthat::fail( "Could not create parent directory for input" )
  
  
  test_inputdir_files <- replicate( 5, 
                                    base::gsub( "\\\\", "/", base::tempfile( pattern = "test-input-", tmpdir = test_inputdir_parent , fileext = ".txt" ) ),
                                    simplify = TRUE  )
  
  if ( any(file.exists( test_inputdir_files )) )
    testthat::fail( "Test input exists" )
  
  
  for ( xfile in test_inputdir_files ) 
    base::writeLines( paste( base::sample( base::letters, 20, replace = TRUE), collapse = "" ), con = xfile )
  
  
  test_inputdir_ref <- base::substring( test_inputdir_parent, base::nchar( test_root ) + 2 )
  
  
  
  test_input_file <- base::gsub( "\\\\", "/", base::tempfile( pattern = "test-input-", tmpdir = file.path( test_root, "some", "path", "to", "input", fsep = "/" ), fileext = ".txt" ) )
  
  if ( file.exists( test_input_file ) )
    testthat::fail( "Test input exists" )
  
  if ( ! dir.exists( base::dirname(test_input_file) ) && ! dir.create( base::dirname(test_input_file), recursive = TRUE) )
    testthat::fail( "Could not create parent directory for input" )
  
  base::writeLines( paste( base::sample( base::letters, 20, replace = TRUE), collapse = "" ), con = test_input_file )
  
  
  test_input_file_ref <- base::substring( test_input_file, base::nchar( test_root ) + 2 )
  
  
  
  # test program
  test_program_dir <- file.path( test_root, "some", "path", "to", "programs" , fsep = "/" )
  
  test_programs <- replicate( 5, 
                              base::gsub( "\\\\", "/", base::tempfile( pattern = "test-program-", tmpdir = test_program_dir, fileext = ".R" ) ),
                              simplify = TRUE )
  
  if ( dir.exists( test_program_dir ) || ! dir.create( test_program_dir, recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  
  for ( xpgm in utils::head( test_programs, n = 3 ) ) {
    
    pgm <- c( "# test program",
              "# input file",
              paste( "# @cx.input", test_input_file_ref) )  
    
    writeLines( pgm, con = xpgm )
    
    if ( ! file.exists( xpgm ) )
      testthat::fail( "Could not stage test program" )
    
  }
  
  
  for ( xpgm in utils::tail( test_programs, n = 2 ) ) {
    
    pgm <- c( "# test program",
              "# input directory",
              paste( "# @cx.input", test_inputdir_ref) )  
    
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
  

  expected_inputs <- lapply( sort(list.files( test_root, recursive = TRUE, include.dirs = FALSE, full.names = FALSE)), function(x) {
    c( "path" = x, 
       "sha1" = digest::digest( file.path( test_root, x, fsep = "/"), algo = "sha1", file = TRUE ) )
    
  })
  
  
  
  expected_program_files <- file.path( result[["work.area"]], test_programs_relpath, fsep = "/" )
  
  
  # -- assertions
  
  # work area
  testthat::expect_true( "work.area" %in% names(result) )
  testthat::expect_false( base::startsWith( result[["work.area"]] , test_root) )
  
  # program action
  testthat::expect_true( "actions" %in% names(result) )
  testthat::expect_length( result[["actions"]], length(test_programs) )
  testthat::expect_equal( result[["actions"]], expected_actions )
  
  
  # staged program
  testthat::expect_true( all(file.exists( expected_program_files )) )
  testthat::expect_true( all( sapply( expected_actions, function(x) {
    ( x[["sha1"]] == digest::digest( file.path( result[["work.area"]], x[["path"]], fsep = "/"), algo = "sha1", file = TRUE ) )
  }) ) ) 
  
  
  # input annotations
  testthat::expect_true( "inputs" %in% names(result) )
  testthat::expect_length( result[["inputs"]], length(expected_inputs) )
  testthat::expect_equal( result[["inputs"]], expected_inputs )
  
})

