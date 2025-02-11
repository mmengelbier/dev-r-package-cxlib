# note: not loading library cxlib and testthat as package should be able to be
#       used without library() or require()
# library(cxlib)
# library(testthat)

testthat::test_check("cxlib")