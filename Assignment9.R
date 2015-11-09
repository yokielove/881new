library(ggplot2)
library(ggmap)
library(tigris)
library(sp)
library(rgeos)
library(spdep)
qmap(location="boston")

ma = tracts(state='MA')
plot(ma)

# Boston, MA zip code areas
uas <- urban_areas()

dfb <- zctas(cb=T, starts_with = "02")
boston_ua <- uas[grep("Boston, MA", uas$NAME10), ]
bos_zcta <- dfb[as.vector(gIntersects(dfb, boston_ua, byid = TRUE)), ]
plot(bos_zcta)

#+++++++++++++++++++++++++++++++++++++++++
Boston_Murders <- read.csv("F:/MSSP/2015FALL/MA 881/Boston_Murders.csv",header = T, stringsAsFactors = F)

usa<-urban_areas()

dfh<-zctas(cb=T,starts_with= "77")

bos<-get_map(location="Boston,MA",zoom=11,maptype = "roadmap")
ggmap(bos)

loc <- data.frame(
  as.numeric(str_sub(Boston_Murders$Location,2,12)),
  as.numeric(str_sub(Boston_Murders$Location,14,26)))

colnames(loc) <- c("lat","lon")

bos <- get_map(location = "Boston, MA",zoom=12, maptype = "roadmap")
ggmap(bos)

chk <-is.na(loc)
sum(chk)
dim(chk)
tmp <- loc
tmp1 <- na.omit(tmp)
chk1 <- is.na(tmp1)
sum(chk1)

dim(tmp1)
head(tmp1)
head(loc)
loc <- tmp1

bos1 <- get_map(location = c(lon = -71.05, lat = 42.32), zoom = 12,scale = "auto", maptype = "roadmap")
ggmap(bos1) + geom_point(aes(x=lon, y=lat), data=loc, color='red',size=3)

bos1 <- get_map(location = c(lon = -71.06, lat = 42.36), zoom = 14,scale = "auto", maptype = "roadmap")
ggmap(bos1) + geom_point(aes(x=lon, y=lat), data=loc, color='red',size=3)

Boston <- "Boston university"
qmap(Boston, zoom = 14)
qmap(Boston, zoom = 14, source = "osm")

set.seed(500)
df <- round(data.frame(
  x = jitter(rep(-95.36, 50), amount = .3),
  y = jitter(rep( 29.76, 50), amount = .3)
), digits = 2)
map <- get_googlemap('Boston', markers = df, path = df, scale = 2)
ggmap(map, extent = 'device')

qmap(Boston, zoom = 14, source = "stamen", maptype = "watercolor")
qmap(Boston, zoom = 14, source = "stamen", maptype = "toner")

boston <- get_map(location = "boston")
str(boston)
ggmap(boston, extent = "normal")


