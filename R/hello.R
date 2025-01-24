
#' @export
#' @importFrom raster raster
df2ras <- function (dd)
{
  rasterDF <- vector("list", length(ncol(dd) - 2))
  k <- 1
  for (i in 3:ncol(dd)) {
    spg <- dd[, c(1, 2, i)]
    coordinates(spg) <- ~x + y
    gridded(spg) <- TRUE
    rasterDF[[k]] <- raster::raster(spg, )
    k <- k + 1
  }
  RS <- stack(unlist(rasterDF))
  crs(RS) <- "+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs"
  return(RS)
}

#' @export
#' @importFrom sp SpatialPoints SpatialPointsDataFrame
df2sdf <- function(sp){
  coords = sp[,c("coords.x1", "coords.x2")]
  spoi = sp::SpatialPoints(coords)
  # make spatial data frame
  df <- data.frame("Occurrence" = sp$Occurrence)
  spdf = sp::SpatialPointsDataFrame(spoi, df)
  return(spdf)
}
