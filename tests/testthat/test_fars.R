test_that("filename works", {
  expect_equal(make_filename(2013), "accident_2013.csv.bz2")
})
