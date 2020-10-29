<<<<<<< HEAD
test_that(".csv data files are available", {
  testthat::expect_equal(list.files(system.file("extdata", package = "fars")),
               c("accident_2013.csv.bz2",
                 "accident_2014.csv.bz2",
                 "accident_2015.csv.bz2"))
=======
test_that(".csv data files are available", {
  testthat::expect_equal(list.files(system.file("extdata", package = "fars")),
               c("accident_2013.csv.bz2",
                 "accident_2014.csv.bz2",
                 "accident_2015.csv.bz2"))
>>>>>>> 4f9a9043af615a92f9cb7b080c3637ec4ac6f5b8
})