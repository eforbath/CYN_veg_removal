########### Cherkskiy North Veg Removal Plots #########3

getwd()
setwd("/Users/elenaforbath/Downloads/loranty_lab/data")

install.packages("dplyr")
install.packages("ggplot2")

library(dplyr)
library(ggplot2)

## NDVI analyses ##

## read in tiff file
install.packages("tiff")
install.packages("rtiff")
install.packages("raster")
install.packages("sp")
install.packages("rgdal")
library(tiff)
library(rtiff)
library(raster)
library(sp)
library(rgdal)

FL016 <- raster("CYN_TR1_FL016M/RU_CYN_TR1_FL016B_index_ndvi.tif")
FL016b <- projectRaster(FL016, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
                        method = "bilinear", 
                        alignOnly = FALSE)
FL016b
plot(FL016b)

FL020 <- raster("CYN_TR1_FL020M/NDVI.data.tif")
FL020b <- projectRaster(FL020, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs", 
                        method = "bilinear", 
                        alignOnly = FALSE)
FL020b
plot(FL020b)

### maybe try this one
## "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

hist(FL016b)
hist(FL020b, add = TRUE)

## extracting values from GPS points
GPS <- na.omit(read.csv("CYN_plot_centers.csv") )

GPS2 <- subset(GPS, select= -c(plot, elevation))

GPS2

colnames(GPS2)

## re order columns so longitude is first
GPS_order <- GPS2[,c("longitude", "latitude")]

## turn into spatial points
GPS_order2 <- SpatialPoints(GPS_order,
                            proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

## actually extract NDVI values from each flight 
ndvi_FL016 <- extract(FL016b, GPS_order2, 
                      method = 'bilinear',
                      df = TRUE, 
                      along = TRUE, 
                      sp = TRUE)
names(ndvi_FL016)[names(ndvi_FL016) == "RU_CYN_TR1_FL016B_index_ndvi"] <- "FL016_ndvi"
ndvi_FL016b <- as.data.frame(ndvi_FL016)


ndvi_FL020 <- extract(FL020b, 
                      GPS_order2,
                      method = 'bilinear', 
                      df = TRUE,
                      along = TRUE,
                      sp = TRUE)
names(ndvi_FL020)[names(ndvi_FL020) == "NDVI.data"] <- "FL020_ndvi"
ndvi_FL020b <- as.data.frame(ndvi_FL020)

## plot of data points on ndvi maps

plot(FL016b)
points(GPS_order$longitude, GPS_order$latitude, pch=20)

plot(FL020b)
points(GPS_order$longitude, GPS_order$latitude, pch=20)

## combine data frames by lat and long

ndvi <- merge(ndvi_FL016b, ndvi_FL020b, by = c("longitude", "latitude"))
ndvi

ndvi_plots <- merge(ndvi, GPS, by = c("longitude", "latitude"))
ndvi_plots

## rearrage column orders
ndvi_plots2 <- ndvi_plots[,c("plot", "longitude", "latitude", "elevation", "FL016_ndvi", "FL020_ndvi")]


## plotting NDVI differences
df<-ndvi_plots2[, c("FL016_ndvi", "FL020_ndvi")]
barplot(cbind(FL016_ndvi, FL020_ndvi) ~ plot, 
        ndvi_plots2,
        beside = TRUE,
        main = "Changes in NDVI by plot", 
        xlab = "Plot", ylab = "NDVI", 
        ylim = c(-0.5, 1.5), 
        col = c("light green", "orange"), 
        las = 2, 
        cex.lab = 1.2)
legend("topright", legend = c("pre-clipping", "post-clipping"), fill = c("light green", "orange"))


## add treatments to table
treatments <- read.csv("treatments.csv")
treatments 

ndvi <- merge(ndvi_plots2, treatments, by = "plot")
ndvi


## plot by treatment (separate plots)
ndvi_CT <- subset(ndvi, treatment == "CT")
ndvi_GR <- subset(ndvi, treatment == "GR")
ndvi_SH <- subset(ndvi, treatment == "SH")
ndvi_GS <- subset(ndvi, treatment == "GS")

## control ##
barplot(cbind(FL016_ndvi, FL020_ndvi) ~ plot, 
        ndvi_CT,
        beside = TRUE,
        main = "Changes in NDVI by plot (control)", 
        xlab = "", ylab = "NDVI", 
        ylim = c(-0.5, 1.5), 
        col = c("light green", "orange"), 
        las = 2, 
        cex.lab = 1.2)
mtext("Plot", side = 1, line = 5, cex = 1.2)
legend("topright", 
       legend = c("pre-clipping", "post-clipping"),
       fill = c("light green", "orange"))


## grass ##
barplot(cbind(FL016_ndvi, FL020_ndvi) ~ plot, 
        ndvi_GR,
        beside = TRUE,
        main = "Changes in NDVI by plot (grass)", 
        xlab = "", ylab = "NDVI", 
        ylim = c(-0.5, 1.5), 
        col = c("light green", "orange"), 
        las = 2, 
        cex.lab = 1.2)
mtext("Plot", side = 1, line = 5, cex = 1.2)
legend("topright", 
       legend = c("pre-clipping", "post-clipping"),
       fill = c("light green", "orange"))

## shrub ##
barplot(cbind(FL016_ndvi, FL020_ndvi) ~ plot, 
        ndvi_SH,
        beside = TRUE,
        main = "Changes in NDVI by plot (shrub)", 
        xlab = "", ylab = "NDVI", 
        ylim = c(-0.5, 1.5), 
        col = c("light green", "orange"), 
        las = 2, 
        cex.lab = 1.2)
mtext("Plot", side = 1, line = 5, cex = 1.2)
legend("topright", 
       legend = c("pre-clipping", "post-clipping"),
       fill = c("light green", "orange"))


## grass and shrub ##
barplot(cbind(FL016_ndvi, FL020_ndvi) ~ plot, 
        ndvi_GS,
        beside = TRUE,
        main = "Changes in NDVI by plot (grass and shrub)", 
        xlab = "", ylab = "NDVI", 
        ylim = c(-0.5, 1.5), 
        col = c("light green", "orange"), 
        las = 2, 
        cex.lab = 1.2)
mtext("Plot", side = 1, line = 5, cex = 1.2)
legend("topright", 
       legend = c("pre-clipping", "post-clipping"),
       fill = c("light green", "orange"))



## compare ndvi values by treatment (anova or lm??)
lm <- lm(FL016_ndvi ~ treatment, data = ndvi)
summary(lm)
aov <- aov(FL016_ndvi ~ treatment, data = ndvi) 
summary(aov)

lm <- lm(FL020_ndvi ~ treatment, data = ndvi)
summary(lm)
aov <- aov(FL020_ndvi ~ treatment, data = ndvi)
summary(aov)

## subtract pre- and post- + linear regression 
ndvi$ndvi_diff <- (ndvi$FL016_ndvi - ndvi$FL020_ndvi)
lm <- lm(ndvi_diff ~ treatment, data = ndvi)
summary(lm)

## subtracting pre- and post-clipping rasters 
difference <- (FL020b - FL016b)
 ## Error in compareRaster(e1, e2, extent = FALSE, rowcol = FALSE, crs = TRUE,  : 
  ## different resolution

## crop and change resolution of FL020
FL020_crop <- crop(FL020b, FL016b)
plot(FL020_crop)

FL020_crop2 <- resample(FL020_crop, FL016b)

difference <- FL020_crop2 - FL016b

jpeg("ndvi_diff.jpg")
plot(difference, 
     main = "Raster of NDVI Difference (FL020 - FL016)", 
     xlab = "Longitude", 
     ylab = "Latitude")
dev.off()

jpeg("pre-ndvi.jpg")
plot(FL016b, 
     main = "Pre-Clipping NDVI", 
     xlab = "Longitude", 
     ylab = "Latitude")
dev.off()

jpeg("post-ndvi.jpg")
plot(FL020b, 
     main = "Post-Clipping NDVI)", 
     xlab = "Longitude", 
     ylab = "Latitude")
dev.off()








