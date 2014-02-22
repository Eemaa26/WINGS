###############################################################################
# WINGS spatial
###############################################################################

# setup -----------------------------------------------------------------------
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
# calculate village-to-village distance matrix --------------------------------
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
# calculate village-to-market distance matrix ---------------------------------
# add "tc" to trading centers
  tc$TC <- paste(tc$TC, "tc", sep=".")
# append trading centers to sites
  villages.tc <- rbind.fill(villages, tc)
# subset coordinates
  ll <- c("Longitude", "Latitude")
  coords <- villages.tc[ll]
# calculate distance in km
  dist.vtc <- apply(coords, 1, 
                    function(eachPoint) spDistsN1(as.matrix(coords),
                                                  eachPoint, longlat=TRUE))
  dist.vtc <- data.frame(dist.vtc)
  names(dist.vtc) <- villages.tc$SiteID
  row.names(dist.vtc) <- villages.tc$SiteID
# limit cols to TCs and rows to villages
  dist.vtc <- dist.vtc[,-c(1:120)]
  dist.vtc <- dist.vtc[1:120,]
# export
  write.xlsx(x = dist.vtc, file = "output/distance.xlsx",
             sheetName = "tc", row.names = TRUE,
             append=TRUE)
# gravity ---------------------------------------------------------------------
  dist.mat <- data.matrix(dist)
# create distance matrix
  dist.mat <- data.matrix(dist)
# columns will be village-specific vectors of attraction to every other village 
  head(dist.mat[,1:6])
# create treatment weight matrix
  p1 <- villages$phase_1
  dist.mat.w <- (dist.mat*p1)/dist.mat
  head(dist.mat.w[,1:6])
# weight by inverse distance 
  grav.mat <- dist.mat.w/exp(dist.mat*.1813)
  grav.mat[grav.mat==Inf] <- 0
  head(grav.mat[,1:6])
# sum columns
  gravity <- colSums(grav.mat, na.rm = TRUE)
  gravity
  hist(gravity)

