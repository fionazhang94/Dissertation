library(sp)
library(rgdal)
library(raster)
library(ggplot2)
library(viridis)
library(rasterVis)
library(scales)

ness <- raster("loch ness.tif")
s_ness <- brick('loch ness.tif')
# Get properties of the Ness raster
ness

# Blue
b2 <- raster('loch ness.tif', band=2)
# Green
b3 <- raster('loch ness.tif', band=3)
#red
b4 <- raster('loch ness.tif', band=4)
# Near Infrared (NIR)
b8 <- raster('loch ness.tif', band=8)


par(mfrow = c(2,2))
plot(b2, main = "Blue", col = viridis_pal(option="D")(10)) 
plot(b3, main = "Green", col = viridis_pal(option="D")(10)) 
plot(b4, main = "Red", col = viridis_pal(option="D")(10)) 
plot(b8, main = "NIR", col = viridis_pal(option="D")(10))



image(b8, col= viridis_pal(option="D")(10), main="Sentinel 2 image of Loch Ness")

par(mfrow = c(1,2))

nessRGB <- stack(list(b4, b3, b2))              # creates raster stack
plotRGB(nessRGB, axes = TRUE, stretch = "lin", main = "Landsat RGB colour composite")

landsatFCC <- stack(list(b8, b4, b3))
plotRGB(landsatFCC, axes=TRUE, stretch="lin", main="Landsat False Color Composite")


# NDVI

# Created a VI function (vegetation index)
VI <- function(img, k, i) {
  bk <- img[[k]]
  bi <- img[[i]]
  vi <- (bk - bi) / (bk + bi)
  return(vi)
}

# For Sentinel 2, the relevant bands to use are:
# NIR = 8, red = 4

ndvi <- VI(s_ness, 8, 4)
# 8 and 4 refer to the bands we'll use

png('ndviplot.png', width = 4, height = 4, units = "in", res = 300)
plot(ndvi, col = rev(terrain.colors(10)), main = 'Sentinel 2, Loch Ness-NDVI')
dev.off()

# Create histogram of NDVI data

png('ndvihist.png', width = 4, height = 4, units = "in", res = 300)
hist(ndvi,
     main = "Distribution of NDVI values",
     xlab = "NDVI",
     ylab= "Frequency",
     col = "aquamarine3",
     xlim = c(-0.5, 1),
     breaks = 30,
     xaxt = 'n')
axis(side = 1, at = seq(-0.5,1, 0.05), labels = seq(-0.5,1, 0.05))
dev.off()
# Mask cells that have NDVI of less than 0.4 (less likely to be vegetation)

png('ndvimask.png', width = 4, height = 4, units = "in", res = 300)

veg <- reclassify(ndvi, cbind(-Inf, 0.4, NA))
# We are reclassifying our object and making all values between
# negative infinity and 0.4 be NAs

plot(veg, main = 'Veg cover')
dev.off()

# convert the raster to vector/matrix ('getValues' converts the RasterLAyer to array) )

nr <-getValues(ndvi)
str(nr)

# important to set the seed generator because `kmeans` initiates the centres in random locations
# the seed generator just generates random numbers

set.seed(99)

# create 10 clusters, allow 500 iterations, start with 5 random sets using 'Lloyd' method

kmncluster <- kmeans(na.omit(nr), centers = 10, iter.max = 500,
                     nstart = 5, algorithm = "Lloyd")

# kmeans returns an object of class 'kmeans'

str(kmncluster)
# First create a copy of the ndvi layer
knr <- ndvi

# Now replace raster cell values with kmncluster$cluster
# array
knr[] <- kmncluster$cluster

# Alternative way to achieve the same result
values(knr) <- kmncluster$cluster
knr
par(mfrow = c(1, 2))
plot(ndvi, col = viridis_pal(option = "D")(10), main = "NVDI")
plot(knr, main = "Kmeans", col = viridis_pal(option = "D")(10))

