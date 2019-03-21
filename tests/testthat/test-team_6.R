context("test-team_6")

test_that("creating map for a country works", {
  expect_warning(team_6(tolerance = 'a'),"argument is not numeric or logical: returning NA")
  expect_equal(is.data.frame(team_6(tolerance = 0.1)),TRUE) 
})
