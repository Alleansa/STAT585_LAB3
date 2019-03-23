context("test-team_6")

test_that("test for team 6 :creating map works", {
  
  #test returning warning if file path not provided by user and set to default path
  expect_warning(team_6(tolerance = 'c'),"file path does not provided by user, set to default file path, gadm36_AUS_shp/gadm36_AUS_1.shp")
  
  #test returning warning if file path does not exist and set to default path
  expect_warning(team_6(file="s",tolerance = 'c'),"file does not exist, set to default file, Australia map")
  
  #test the tolerance , returning warning if is not numeric and greater than 1 and less than 0
  expect_warning(team_6(system.file("gadm36_AUS_shp/gadm36_AUS_0.shp", package="Team5"),tolerance = 'c'),"argument is not numeric or logical: returning NA")
  expect_error(team_6(system.file("gadm36_AUS_shp/gadm36_AUS_0.shp", package="Team5"),3))
  
  #test the result is data frame
  expect_s3_class(team_6(system.file("gadm36_AUS_shp/gadm36_AUS_0.shp", package="Team5"),0.01),"data.frame")

})
