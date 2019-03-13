#' Function from Team 5
#'
#'@param file is the location of the data
#'@param tolerance is the value used for thinning the polygon
#'
#'@import ggplot2
#'@return ozplus is a data frame about how to plot the map of Australia
#'
Team_5 <- function(file="./data/gadm36_AUS_shp/gadm36_AUS_1.shp",tolerance=0.1){
  if(!file.exists(as.character(file))){
    warning('the file does not exist: returning NA')
    return(NA)
  }
  if(!is.numeric(tolerance)){
    warning('argument is not numeric or logical: returning NA')
    return(NA)
  }
  ozbig <- sf::read_sf(file)
  oz_st <- maptools::thinnedSpatialPoly(as(ozbig, "Spatial"), tolerance , minarea = 0.001, topologyPreserve = TRUE)
  oz <- sf::st_as_sf(oz_st)
  Mat2Df <- function(Mat){
    long <- Mat[,1]
    lat <- Mat[,2]
    order <- 1:nrow(Mat)
    group <- rep(rnorm(1),nrow(Mat))
    df <- data.frame(long=long,lat=lat,group=group,order=order)
    df
  }
  oz_flatten <- purrr::flatten(purrr::flatten(oz$geometry))
  ozplus <- purrr::map_df(.x=oz_flatten,.f=Mat2Df)
  return(ozplus)
}
