<<<<<<< HEAD
test_that("Check number of rows for 2013 accidents file", {
  testthat::expect_equal(nrow(fars_read_years(2013)[[1]]), 30202)
=======
test_that("Check number of rows for 2013 accidents file", {
  testthat::expect_equal(nrow(fars_read_years(2013)[[1]]), 30202)
>>>>>>> 4f9a9043af615a92f9cb7b080c3637ec4ac6f5b8
})