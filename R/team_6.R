#' solution of Lab 2 of team 6.
#' 
#' @param file A path having shape file of selected country.
#' @param tolerance A number for tolerence.
#' @import tidyverse
#' @import purr
#' @import sf
#' @export 
#' @return The dataframe
#' @examples
#' file<-"./data/gadm36_AUS_shp/gadm36_AUS_1.shp"
#' tolerance <- 0.1
#' team_6(file,tolerance) %>% ggplot(aes(x = long, y = lat, group = group)) + geom_polygon(fill="white",color="black",lwd=1)



team_6 <- function(file, tolerance){
  ozbig <- read_sf(file)
  oz_st <- maptools::thinnedSpatialPoly(as(ozbig, "Spatial"), tolerance = tolerance, minarea = 0.001, topologyPreserve = TRUE)
  oz <- st_as_sf(oz_st)
  mat2df <- function(mat) {
    df <- data.frame(long = mat[, 1],
                     lat  = mat[, 2], 
                     temporary.group = sum(mat[,1]+mat[,2] *nrow(mat) + rnorm(1, 100, 10)), 
                     order= 1:nrow(mat))
    df
  }
  
  
  # flatten oz$geometry twice to obtain a list of matrices
  # this is because we have two layers of lists ahead of matrices
  oz.geometry.flattened <- oz$geometry %>% flatten() %>% flatten()
  # Create a dataframe ozplus from the geometry variable using purrr
  # map_df applies a function to elements of a list and bind the dataframes together
  # Instead of temporary.group variable by the function mat2df, 
  # group variable consisting of the index corresponding each matrix will be used.
  ozplus <- oz.geometry.flattened %>% purrr::map_df(.x = ., .id ="group", .f = mat2df)
  return (ozplus)
  
}

#file<-"./data/gadm36_AUS_shp/gadm36_AUS_1.shp"
#file="./data/gadm36_DEU_shp/gadm36_DEU_4.shp"
# plot the result
#team_6(file,0.1) %>% ggplot(aes(x = long, y = lat, group = group)) + geom_polygon(fill="white",color="black",lwd=1)

