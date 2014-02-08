###############################################################################
# WINGS spatial
###############################################################################


# libraries
  require(maptools)
  require(sp)
  require(xlsx)
  require(plyr)

# import
  #shp.district <- readShapeSpatial("input/spatial/shapefiles/district/district.shp")
  villages <- read.csv("input/spatial/villages.csv")
  villages <- villages[order(villages$SiteID),]  # sort
  tc <- read.csv("input/spatial/tradingcenters.csv")
# add "tc" to trading centers
  tc$TC <- paste(tc$TC, "tc", sep=".")
# append trading centers to sites
  villages <- rbind.fill(villages, tc)
# subset coordinates
  ll <- c("Longitude", "Latitude")
  coords <- villages[ll]
# calculate distance in km
  dist <- apply(coords, 1, 
                function(eachPoint) spDistsN1(as.matrix(coords),
                                              eachPoint, longlat=TRUE))
  dist <- data.frame(dist)
  names(dist) <- villages$SiteID
  row.names(dist) <- villages$SiteID
# export
  write.xlsx(x = dist, file = "output/distance.xlsx",
             sheetName = "villages", row.names = TRUE)
