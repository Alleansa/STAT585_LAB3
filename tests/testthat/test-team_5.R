context("test-team_5")

test_that("multiplication works", {
  expect_warning(Team_5(tolerance = 'a'),"argument is not numeric or logical: returning NA")
  expect_equal(is.data.frame(Team_5(tolerance = 0.1)),TRUE) 
})
