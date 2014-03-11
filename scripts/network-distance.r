library(xlsx)
library(data.table)

routes <- read.csv("output/origin-destination-0311.csv")
routes <- routes[c(2,6)]
routes2 <- data.frame(do.call('rbind', 
                              strsplit(as.character(routes[,1]),
                                       ' - ',fixed=TRUE)))
routes <- cbind(routes, routes2)
remove(routes2)
routes[1] <- NULL
names(routes) <- c("km", "origin", "destination")
routes$km <- routes$km/1000
routes$origin <- as.numeric(levels(routes$origin))[routes$origin]
routes$destination <- as.numeric(levels(routes$destination))[routes$destination]
routes <- routes[order(routes$origin, routes$destination),] 

routes.w <- reshape(routes, 
                    timevar = "destination",
                    idvar = c("origin"),
                    direction = "wide")
row.names(routes.w) <- routes.w$origin
routes.w$origin <- NULL
remove(routes)
names(routes.w) <- gsub("km.", "", names(routes.w))

write.xlsx(x = routes.w, file = "output/distance.xlsx",
           sheetName = "villages-road", row.names = TRUE, append=TRUE)

# import straight line
  straight <- read.xlsx("output/distance.xlsx", sheetName="villages")
  straight[1] <- NULL
  names(straight) <- seq(1:120)

  s <- data.matrix(straight, rownames.force = NA)
  r <- data.matrix(routes.w, rownames.force = NA)
  d <- r-s
  diff <- data.frame(d)
  names(diff) <- seq(1:120)
  write.xlsx(x = diff, file = "output/distance.xlsx",
           sheetName = "difference", row.names = TRUE, append=TRUE)
  mean.diff <- data.frame(sapply(diff, mean, na.rm=TRUE))
  names(mean.diff) <- "mean.diff"
  write.xlsx(x = mean.diff, file = "output/distance.xlsx",
           sheetName = "mean-diff", row.names = TRUE, append=TRUE)
  mean(mean.diff$mean.diff, na.rm=TRUE)