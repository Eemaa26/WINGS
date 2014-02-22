WINGS spatial
========================================================

Here we import spatial coordinates of all 120 WINGS villages (i) and calculate a euclidian distance matrix. Then we contruct a gravity index (see [here](http://cityform.mit.edu/files/UNA_help_v1_1.pdf)) that measures the influence of treatment villages on i. This index is proportional to the weight of the surrounding villages (j) and inversely proportional to the straight line distances between i and j. 

### Setup


```r
# libraries
  require(knitr)
  require(maptools)
  require(sp)
  require(xlsx)
  require(plyr)
# knitr options
  opts_knit$set(root.dir=normalizePath('../'))
  opts_chunk$set(warning=FALSE, message=FALSE, tidy=FALSE)
```

### Import

```r
  #shp.district <- readShapeSpatial("input/spatial/shapefiles/district/district.shp")
  villages <- read.csv("input/spatial/villages.csv")
  villages <- villages[order(villages$SiteID),]  # sort
  tc <- read.csv("input/spatial/tradingcenters.csv")
```


### Calculate village-to-village distance matrix

```r
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
```


### Calculate village-to-trading center distance matrix

```r
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
```


### Calculate gravity index

By dividing the weight of surrounding village j by the exponent of the distance between j and i, we assume that the inverse effect of distance decreases exponentially. By first multiplying distances by 0.1813, we modify the shape of the distance decay in a manner that assumes walking is the main mode of travel. We include all j's when constructing the index; no maximum search radius is used. We do not weight according to village population.


```r
# convert data frame to matrix
  dist.mat <- data.matrix(dist)
# columns will be village-specific vectors of attraction to every other village
# showing here distance matrix for villages 1-6
  head(dist.mat[,1:6])
```

```
##       1     2     3      4     5      6
## 1 0.000 5.173 2.287 6.6146 8.539 7.2375
## 2 5.173 0.000 5.484 2.8566 5.256 3.8023
## 3 2.287 5.484 0.000 5.8586 7.282 6.2190
## 4 6.615 2.857 5.859 0.0000 2.406 0.9471
## 5 8.539 5.256 7.282 2.4058 0.000 1.4783
## 6 7.238 3.802 6.219 0.9471 1.478 0.0000
```

```r
# create treatment weight matrix
  p1 <- villages$phase_1
  p1
```

```
##   [1] 1 0 0 1 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 1 1 1 0 0 1 1 0 1 1 1 1 1 1 0
##  [36] 0 0 1 0 1 1 1 0 0 0 0 0 0 0 0 1 1 1 0 0 0 1 1 0 1 0 1 1 1 1 1 0 0 1 0
##  [71] 1 1 1 1 1 0 1 0 1 1 1 0 0 1 1 1 1 0 1 1 1 1 0 0 0 0 1 0 0 0 0 0 1 0 1
## [106] 1 0 0 0 1 1 0 0 0 0 1 0 0 0 0
```

```r
# village 1 is a treatment village
# village 2 is a control village and has weight of 0 on other villages
  dist.mat.w <- (dist.mat*p1)/dist.mat
  head(dist.mat.w[,1:6])
```

```
##     1   2   3   4   5   6
## 1 NaN   1   1   1   1   1
## 2   0 NaN   0   0   0   0
## 3   0   0 NaN   0   0   0
## 4   1   1   1 NaN   1   1
## 5   0   0   0   0 NaN   0
## 6   0   0   0   0   0 NaN
```

```r
# weight by inverse distance 
#   From http://cityform.mit.edu/files/UNA_help_v1_1.pdf:
#   "The inverse effect of distance specified in the Gravity index decreases exponentially. The exact shape of the distance decay can be controlled with the exponent and the corresponding shape of distance decay should be derived from the assumed mode of travel; for walking measured in “minutes”, for instance, researchers have found to fall around 0.1813 (Handy and Niemeier 1997). 
  grav.mat <- dist.mat.w/exp(dist.mat*.1813)
  grav.mat[grav.mat==Inf] <- 0
  head(grav.mat[,1:6])
```

```
##        1      2      3      4      5      6
## 1    NaN 0.3914 0.6606 0.3014 0.2127 0.2692
## 2 0.0000    NaN 0.0000 0.0000 0.0000 0.0000
## 3 0.0000 0.0000    NaN 0.0000 0.0000 0.0000
## 4 0.3014 0.5958 0.3457    NaN 0.6465 0.8422
## 5 0.0000 0.0000 0.0000 0.0000    NaN 0.0000
## 6 0.0000 0.0000 0.0000 0.0000 0.0000    NaN
```

```r
# sum columns
  gravity <- colSums(grav.mat, na.rm = TRUE)
  gravity
```

```
##      1      2      3      4      5      6      7      8      9     10 
## 2.1938 3.8969 2.4660 2.5181 2.6564 3.1214 1.9239 3.8945 2.8810 3.4655 
##     11     12     13     14     15     16     17     18     19     20 
## 3.7150 3.7143 2.4048 4.4065 4.1930 4.3536 4.2209 3.9843 4.7196 5.7113 
##     21     22     23     24     25     26     27     28     29     30 
## 4.9975 5.0418 4.8877 6.0826 5.2802 5.0844 2.6145 5.0357 4.3041 4.1235 
##     31     32     33     34     35     36     37     38     39     40 
## 3.6220 4.1261 4.7328 2.1333 3.1342 3.3720 4.3847 3.9466 3.5614 1.7898 
##     41     42     43     44     45     46     47     48     49     50 
## 1.7243 4.0341 3.5297 4.0598 4.2202 1.2157 1.1561 0.7475 1.4913 1.2975 
##     51     52     53     54     55     56     57     58     59     60 
## 1.4009 3.2000 4.2890 4.1536 2.7356 2.3783 3.5608 2.5735 4.5591 3.5726 
##     61     62     63     64     65     66     67     68     69     70 
## 3.3674 2.8330 4.1455 3.8159 4.2389 2.4383 2.6804 3.0313 2.2055 4.5271 
##     71     72     73     74     75     76     77     78     79     80 
## 3.1309 4.1907 1.0343 3.2906 0.6152 2.7214 1.7730 2.9079 2.8154 2.8354 
##     81     82     83     84     85     86     87     88     89     90 
## 2.3482 1.8897 3.3401 2.8372 3.8811 3.2901 3.8908 4.7097 3.6565 3.7202 
##     91     92     93     94     95     96     97     98     99    100 
## 3.7988 3.6533 2.6020 0.6632 0.7353 0.6803 2.2467 3.1194 2.7344 2.3081 
##    101    102    103    104    105    106    107    108    109    110 
## 3.1186 2.3038 3.0283 3.2251 2.4475 1.5405 3.1521 3.7049 3.2063 3.0613 
##    111    112    113    114    115    116    117    118    119    120 
## 2.1383 3.4152 3.6423 4.2197 3.7278 2.4830 3.5206 3.2138 2.8349 2.9094
```

```r
  hist(gravity)
```

![plot of chunk gravity](figure/gravity.png) 

