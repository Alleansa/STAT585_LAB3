context("test-team_5")

test_that("multiplication works", {
  expect_warning(Team_5(file=1),'the file does not exist: returning NA')
  expect_warning(Team_5(tolerance = NA),"argument is not numeric or logical: returning NA")
  expect_warning(Team_5(file=1,tolerance = 'a'),'the file does not exist: returning NA')
  expect_equal(is.data.frame(Team_5()),TRUE) # CHECK THE OUTPUT
})
