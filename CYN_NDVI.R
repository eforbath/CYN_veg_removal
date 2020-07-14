########### Cherkskiy North Veg Removal Plots ###########

getwd()
setwd("/Users/elenaforbath/Downloads/loranty_lab/data")

install.packages("dplyr")
install.packages("ggplot2")
install.packages("tiff")
install.packages("rtiff")
install.packages("raster") 
install.packages("sp")
install.packages("rgdal")

library(dplyr)
library(ggplot2)
library(tiff)
library(rtiff)
library(raster)
library(sp)
library(rgdal)


########### NDVI analyses########### 

## read in tiff file and create raster plots
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


## histogram of pre/post NDVI values (overlapping values)

c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")

jpeg("ndvi_hist.jpg")
hist(FL016b,
     main = "Histogram of NDVI Values",
     xlab = "NDVI",
     ylab = "Frequency",
     xlim = c(-0.5, 1),
     ylim = c(0, 20000),
     col = c1)
hist(FL020b, col = c2, add = TRUE)
legend("top", legend = c("pre-clipping", "post-clipping"), fill = c(c1, c2))
dev.off()

## extracting values from GPS points
GPS <- na.omit(read.csv("CYN_plot_centers.csv"))

GPS2 <- subset(GPS, select= -c(plot, elevation))

## re order columns so longitude is first
GPS_order <- GPS2[,c("longitude", "latitude")]

## turn into spatial points
GPS_order2 <- SpatialPoints(GPS_order,
                            proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

## actually extract NDVI values from each flight 
ndvi_FL016 <- extract(FL016b, GPS_order2,
                      buffer = 0.25,
                      fun = mean,
                      df = TRUE, 
                      along = TRUE, 
                      sp = TRUE)
names(ndvi_FL016)[names(ndvi_FL016) == "RU_CYN_TR1_FL016B_index_ndvi"] <- "FL016_ndvi"
ndvi_FL016b <- as.data.frame(ndvi_FL016)


ndvi_FL020 <- extract(FL020b, 
                      GPS_order2,
                      buffer = 0.25,
                      fun = mean,
                      df = TRUE,
                      along = TRUE,
                      sp = TRUE)
names(ndvi_FL020)[names(ndvi_FL020) == "NDVI.data"] <- "FL020_ndvi"
ndvi_FL020b <- as.data.frame(ndvi_FL020)


## plot data points on ndvi maps
plot(FL016b, 
     main = "Pre-Clipping NDVI", 
     xlab = "Longitude", 
     ylab = "Latitude")
points(GPS_order$longitude, GPS_order$latitude, pch=20)

plot(FL020b, 
     main = "Post-Clipping NDVI", 
     xlab = "Longitude", 
     ylab = "Latitude")
points(GPS_order$longitude, GPS_order$latitude, pch=20)

## combine pre- and post-clipping data frames by lat and long

ndvi <- merge(ndvi_FL016b, ndvi_FL020b, by = c("longitude", "latitude")) ## just pre- and post-

ndvi_plots <- merge(ndvi, GPS, by = c("longitude", "latitude"))## add GPS points


## rearrage column orders for ease of reading
ndvi_plots2 <- ndvi_plots[,c("plot", "longitude", "latitude", "elevation", "FL016_ndvi", "FL020_ndvi")]


## plotting NDVI changes (all plots)
df <- ndvi_plots2[, c("FL016_ndvi", "FL020_ndvi")] ##need this???

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

jpeg("barplot_control.jpg")
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
dev.off()

## grass ##
jpeg("barplot_grass.jpg")
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
dev.off()

## shrub ##
jpeg("barplot_shrub.jpg")
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
dev.off()


## grass and shrub ##
jpeg("barplot_both.jpg")
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
dev.off()




########### compare ndvi values by treatment (anova or lm??) ########### 
lm <- lm(FL016_ndvi ~ treatment, data = ndvi)
summary(lm)
aov <- aov(FL016_ndvi ~ treatment, data = ndvi) 
summary(aov)

plot(FL016_ndvi ~ treatment, 
     data = ndvi, 
     xlab = "Treatment", 
     ylab = "NDVI", 
     main = "Pre-Clipping NDVI by Treatment")

lm <- lm(FL020_ndvi ~ treatment, data = ndvi)
summary(lm)
aov <- aov(FL020_ndvi ~ treatment, data = ndvi)
summary(aov)

plot(FL020_ndvi ~ treatment, 
     data = ndvi, 
     xlab = "Treatment", 
     ylab = "NDVI",
     main = "Post-Clipping NDVI by Treatment")

## subtract pre- and post- & linear regression 
ndvi$ndvi_diff <- (ndvi$FL020_ndvi - ndvi$FL016_ndvi)
lm <- lm(ndvi_diff ~ treatment, data = ndvi)
summary(lm)


## subtracting pre- and post-clipping rasters 

## crop and change resolution of FL020
FL020_crop <- crop(FL020b, FL016b)
plot(FL020_crop)

FL020_crop2 <- resample(FL020_crop, FL016b) ## needed to resample the plot to change the resolution
                                            ## for the subtraction of rasters to work 
                                            ## both rasters needed to have the same resolution 

difference <- FL020_crop2 - FL016b

## plot and export difference raster
jpeg("ndvi_diff.jpg")
plot(difference, 
     main = "Raster of NDVI Difference (FL020 - FL016)", 
     xlab = "Longitude", 
     ylab = "Latitude")
dev.off()

### plot and export rasters of pre/post clipping
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

## histogram of difference?
hist(difference, 
     maxpixels = 100000,
     main = "Histogram of NDVI Value Differences", 
     xlab = "NDVI difference", 
     col = "lightblue")



########### biomass removal vs ndvi difference #######

bio_removal <- na.omit(read.csv("CYN_bio_removal.csv"))
names(bio_removal)[names(bio_removal) == "Plot.ID"] <- "plot"

ndvi2 <- sub("P", " " , ndvi$plot) ## take out "P" in all plot numbers
ndvi$plot = gsub("P", "",ndvi$plot )


bio_removal2 <- aggregate(bio_removal$BIOMASS..g., by = list(plot=bio_removal$plot), FUN = sum)
names(bio_removal2)[names(bio_removal2) == "x"] <- "bio_removed"


ndvi_br <- na.omit(merge(ndvi, bio_removal2, by = c("plot"), all.x = TRUE, all.y = TRUE)) 

barplot(bio_removed ~ plot, data = ndvi_br,
     main = "Biomass Removed by Plot",
     xlab = "Plot", 
     ylab = "Biomass Removed (g)", 
     ylim = c(0,600), 
     col = "lightgreen")

lm <- lm(bio_removed ~ ndvi_diff, data = ndvi_br)
summary(lm)
anova(lm)

plot(bio_removed ~ ndvi_diff, data = ndvi_br, 
     xlab = "NDVI Difference", 
     ylab = "Biomass Removed (g)", 
     pch = 19, 
     col = "red")
abline(lm)


## biomass removal by treatment ##
GR <- subset(ndvi_br, treatment == "GR")
SH <- subset(ndvi_br, treatment == "SH")
GS <- subset(ndvi_br, treatment == "GS")


plot(bio_removed ~ ndvi_diff, 
     data = GR, 
     xlab = "NDVI Difference", 
     ylab = "Biomass Removed (g)", 
     main = "NDVI Difference vs Biomass Removed", 
     pch = 19, 
     col = "green")
points(bio_removed ~ ndvi_diff, 
       data = SH, 
       pch = 19, 
       col = "red")
points(bio_removed ~ ndvi_diff, 
       data = GS, 
       pch = 19, 
       col = "red")
legend()


########### percent cover #########
percent_cover <- na.omit(read.csv("percent_cover.csv"))
names(percent_cover)[names(percent_cover) == "Plot.ID"] <- "plot"

## aggregate data by functional group
percent_cover2 <- aggregate(percent_cover$percent.cover, 
                 by = list(percent_cover$plot, percent_cover$Functional.group), 
                 FUN = sum)
names(percent_cover2)[names(percent_cover2) == "Group.1"] <- "plot"
names(percent_cover2)[names(percent_cover2) == "Group.2"] <- "functional_group"
names(percent_cover2)[names(percent_cover2) == "x"] <- "percent_cover"


## subset by treatment 
pc_GR <- subset(percent_cover, Treatment == "GR")
pc_SH <- subset(percent_cover, Treatment == "SH")
pc_GS <- subset(percent_cover, Treatment == "G+S")

ndvi_GR$plot = gsub("P", "",ndvi_GR$plot)
ndvi_GS$plot = gsub("P", "",ndvi_GS$plot)
ndvi_SH$plot = gsub("P", "",ndvi_SH$plot)

## merge ndvi data with pc data (not sure if this is necessary)
ndviGR <- merge(ndvi_GR, pc_GR, by = c("plot")) 
ndviSH <- merge(ndvi_SH, pc_SH, by = c("plot"))
ndviGS <- merge(ndvi_GS, pc_GS, by = c("plot"))



par(xpd = T, mar = par()$mar + c(0,0,0,9))
barplot(percent.cover ~ Functional.group + plot,
        data = ndviGR,
        xlab = "Plot", 
        ylab = "Percent Cover",
        ylim = c(0, 120),
        las = 2,
        cex.names = 0.85,
        col = c("red", "orange", "yellow", "lightgreen", "darkgreen", "lightblue", "darkblue", 
                "purple", "pink", "brown"))
title("Percent Cover by Functional Group (Grass Treatment)", adj = 0.05, line = 1.5)
legend(10, 100, c("conifer", "evergreen shrub", "deciduous shurb", "Graminoid", "forb", 
                     "coarse woody debris", "lichen", "bare ground", "litter", "Equisetum spp"), 
       fill = c("red", "orange", "yellow", "lightgreen", "darkgreen", "lightblue", "darkblue", 
                "purple", "pink", "brown"), cex = 0.75)


barplot(percent.cover ~ Functional.group + plot,
        data = ndviSH,
        xlab = "Plot", 
        ylab = "Percent Cover",
        ylim = c(0, 120), 
        las = 2,
        cex.names = 0.75,
        col = c("red", "orange", "yellow", "lightgreen", "darkgreen", "lightblue", "darkblue", 
                "purple", "pink", "brown"))
title("Percent Cover by Functional Group (Shrub Treatment)", adj = 0.05, line = 1.5)
legend(10, 100, c("conifer", "evergreen shrub", "deciduous shurb", "Graminoid", "forb", 
                  "coarse woody debris", "lichen", "bare ground", "litter", "Equisetum spp"), 
       fill = c("red", "orange", "yellow", "lightgreen", "darkgreen", "lightblue", "darkblue", 
                "purple", "pink", "brown"))

barplot(percent.cover ~ Functional.group + plot,
        data = ndviGS,
        xlab = "Plot", 
        ylab = "Percent Cover",
        ylim = c(0, 120), 
        las = 2,
        cex.names = 0.75,
        col = c("red", "orange", "yellow", "lightgreen", "darkgreen", "lightblue", "darkblue", 
                "purple", "pink", "brown"))
title("Percent Cover by Functional Group (Grass+Shrub Treatment)", adj = 0.05, line = 1.5)
legend(10, 100, c("conifer", "evergreen shrub", "deciduous shurb", "Graminoid", "forb", 
                  "coarse woody debris", "lichen", "bare ground", "litter", "Equisetum spp"), 
       fill = c("red", "orange", "yellow", "lightgreen", "darkgreen", "lightblue", "darkblue", 
                "purple", "pink", "brown"))




########### bar plot with all plots and percent cover ###########  
dev.off()
par(xpd = T, mar = par()$mar + c(0,0,0,10))
barplot(percent.cover ~ Functional.group + plot,
        data = percent_cover,
        xlab = "Plot", 
        ylab = "Percent Cover",
        ylim = c(0, 120), 
        las = 2,
        cex.names = 0.75,
        col = c("red", "orange", "yellow", "lightgreen", "darkgreen", "lightblue", "darkblue", 
                "purple", "pink", "brown"))
title("Percent Cover by Functional Group", adj = 0.05, line = 1.5)
legend(50, 100, c("conifer", "evergreen shrub", "deciduous shurb", "Graminoid", "forb", 
                    "coarse woody debris", "lichen", "bare ground", "litter", "Equisetum spp"), 
       fill = c("red", "orange", "yellow", "lightgreen", "darkgreen", "lightblue", "darkblue", 
                "purple", "pink", "brown"))



########### subset percent cover by funtional group ########
con <- subset(percent_cover, Functional.group == "CON")
evsh <- subset(percent_cover, Functional.group == "EVSH")
desh <- subset(percent_cover, Functional.group == "DESH")
gram <- subset(percent_cover, Functional.group == "GRAM")
forb <- subset(percent_cover, Functional.group == "FORB")
cwd <- subset(percent_cover, Functional.group == "CWD")
moss <- subset(percent_cover, Functional.group == "MOSS")
lichen <- subset(percent_cover, Functional.group == "LICH")
brg <- subset(percent_cover, Functional.group == "BRG")
litr <- subset(percent_cover, Functional.group == "LITR")
equ <- subset(percent_cover, Functional.group == "EQU")


### does change in NDVI correlate with veg type (ie does NDVI inc/dec with specific veg types)
pc_br <- merge(percent_cover, ndvi, by = c("plot")) ## merging percent cover with NDVI

con <- subset(pc_br, Functional.group == "CON")
lm <- lm(FL016_ndvi ~ percent.cover, data = con)
summary(lm)

plot(FL016_ndvi ~ percent.cover, 
     data = con,
     main = "NDVI vs Percent Cover (Conifers)",
     xlab = "Percent Cover", 
     ylab = "NDVI (pre-clipping)",
     pch = 19, 
     col = "darkgreen")
abline(lm)


ev <- subset(pc_br, Functional.group == "EVSH")
lm <- lm(FL016_ndvi ~ percent.cover, data = ev)
summary(lm)

plot(FL016_ndvi ~ percent.cover, 
     data = ev,
     main = "NDVI vs Percent Cover (Evergreen Shrub)",
     xlab = "Percent Cover", 
     ylab = "NDVI (pre-clipping)",
     pch = 19, 
     col = "darkgreen")
abline(lm)


desh <- subset(pc_br, Functional.group == "DESH")
lm <- lm(FL016_ndvi ~ percent.cover, data = desh)
summary(lm)

plot(FL016_ndvi ~ percent.cover, 
     data = desh,
     main = "NDVI vs Percent Cover (Deciduous Shrub)",
     xlab = "Percent Cover", 
     ylab = "NDVI (pre-clipping)",
     pch = 19, 
     col = "darkgreen")
abline(lm)



gram <- subset(pc_br, Functional.group == "GRAM")
lm <- lm(FL016_ndvi ~ percent.cover, data = gram)
summary(lm)

plot(FL016_ndvi ~ percent.cover, 
     data = gram,
     main = "NDVI vs Percent Cover (Graminoid)",
     xlab = "Percent Cover", 
     ylab = "NDVI (pre-clipping)",
     pch = 19, 
     col = "darkgreen")
abline(lm)



forb <- subset(pc_br, Functional.group == "FORB")
lm <- lm(FL016_ndvi ~ percent.cover, data = forb)
summary(lm)

plot(FL016_ndvi ~ percent.cover, 
     data = forb,
     main = "NDVI vs Percent Cover (Forb)",
     xlab = "Percent Cover", 
     ylab = "NDVI (pre-clipping)",
     pch = 19, 
     col = "darkgreen")
abline(lm)



cwd <- subset(pc_br, Functional.group == "CWD")
lm <- lm(FL016_ndvi ~ percent.cover, data = cwd)
summary(lm)

plot(FL016_ndvi ~ percent.cover, 
     data = cwd,
     main = "NDVI vs Percent Cover (CWD)",
     xlab = "Percent Cover", 
     ylab = "NDVI (pre-clipping)",
     pch = 19, 
     col = "darkgreen")
abline(lm)



moss <- subset(pc_br, Functional.group == "MOSS")
lm <- lm(FL016_ndvi ~ percent.cover, data = moss)
summary(lm)

plot(FL016_ndvi ~ percent.cover, 
     data = moss,
     main = "NDVI vs Percent Cover (Moss)",
     xlab = "Percent Cover", 
     ylab = "NDVI (pre-clipping)",
     pch = 19, 
     col = "darkgreen")
abline(lm)



lich <- subset(pc_br, Functional.group == "LICH")
lm <- lm(FL016_ndvi ~ percent.cover, data = lich)
summary(lm)

plot(FL016_ndvi ~ percent.cover, 
     data = lich,
     main = "NDVI vs Percent Cover (Lichen)",
     xlab = "Percent Cover", 
     ylab = "NDVI (pre-clipping)",
     pch = 19, 
     col = "darkgreen")
abline(lm)



brg <- subset(pc_br, Functional.group == "BRG") ### irrelevant bc all plots have 0% cover for brg
lm <- lm(FL016_ndvi ~ percent.cover, data = brg)
summary(lm)

plot(FL016_ndvi ~ percent.cover, 
     data = brg,
     main = "NDVI vs Percent Cover (Bare Ground)",
     xlab = "Percent Cover", 
     ylab = "NDVI (pre-clipping)",
     pch = 19, 
     col = "darkgreen")
abline(lm)
### error : 
## Error in int_abline(a = a, b = b, h = h, v = v, untf = untf, ...) : 
##  'a' and 'b' must be finite



litr <- subset(pc_br, Functional.group == "LITR")
lm <- lm(FL016_ndvi ~ percent.cover, data = litr)
summary(lm)

plot(FL016_ndvi ~ percent.cover, 
     data = litr,
     main = "NDVI vs Percent Cover (Litter)",
     xlab = "Percent Cover", 
     ylab = "NDVI (pre-clipping)",
     pch = 19, 
     col = "darkgreen")
abline(lm)
### statistically significant?!?!?!



equ <- subset(pc_br, Functional.group == "EQU")
lm <- lm(FL016_ndvi ~ percent.cover, data = equ)
summary(lm)

plot(FL016_ndvi ~ percent.cover, 
     data = equ,
     main = "NDVI vs Percent Cover (Equisetum spp.)",
     xlab = "Percent Cover", 
     ylab = "NDVI (pre-clipping)",
     pch = 19, 
     col = "darkgreen")
abline(lm)






########### point intercept data ########### 
## more accurate, all add up to 100% BUT missing some functional groups
pt_int <- na.omit(read.csv("pt_intercept.csv"))

treatments$plot = gsub("P", "", treatments$plot) ## remove all P from plots
pt_int2 <- merge(treatments, pt_int, by = c("plot")) ## merge to add treaments for all plots

point_CT <- subset(pt_int2, treatment == "CT") ## Subset by treatments
point_GR <- subset(pt_int2, treatment == "GR")
point_SH <- subset(pt_int2, treatment == "SH")
point_GS <- subset(pt_int2, treatment == "GS")

## control ##
dev.off()
par(xpd = T, mar = par()$mar + c(0,0,0,9))
barplot(percent_composition ~ functional_groups + plot,
        data = point_CT,
        xlab = "Plot", 
        ylab = "Percent Cover",
        ylim = c(0, 100),
        las = 2,
        cex.names = 0.85,
        col = c("red", "orange", "yellow", "lightgreen", "darkgreen", "lightblue", "darkblue", 
                "purple", "pink", "brown"))
title("Percent Cover by Functional Group (Control)", adj = 0.05, line = 1.5)
legend(12.5, 100, c("conifer", "evergreen shrub", "deciduous shurb", "Graminoid", "forb", 
                  "coarse woody debris", "lichen", "bare ground", "litter", "Equisetum spp"), 
       fill = c("red", "orange", "yellow", "lightgreen", "darkgreen", "lightblue", "darkblue", 
                "purple", "pink", "brown"), cex = 0.75)

## grass ##
barplot(percent_composition ~ functional_groups + plot,
        data = point_GR,
        xlab = "Plot", 
        ylab = "Percent Cover",
        ylim = c(0, 100),
        las = 2,
        cex.names = 0.85,
        col = c("red", "orange", "yellow", "lightgreen", "darkgreen", "lightblue", "darkblue", 
                "purple", "pink", "brown"))
title("Percent Cover by Functional Group (Grass Treatment)", adj = 0.05, line = 1.5)
legend(12.5, 100, c("conifer", "evergreen shrub", "deciduous shurb", "Graminoid", "forb", 
                  "coarse woody debris", "lichen", "bare ground", "litter", "Equisetum spp"), 
       fill = c("red", "orange", "yellow", "lightgreen", "darkgreen", "lightblue", "darkblue", 
                "purple", "pink", "brown"))

## shrubs ##
barplot(percent_composition ~ functional_groups + plot,
        data = point_SH,
        xlab = "Plot", 
        ylab = "Percent Cover",
        ylim = c(0, 100), 
        las = 2,
        cex.names = 0.75,
        col = c("red", "orange", "yellow", "lightgreen", "darkgreen", "lightblue", "darkblue", 
                "purple", "pink", "brown"))
title("Percent Cover by Functional Group (Shrub Treatment)", adj = 0.05, line = 1.5)
legend(12.5, 100, c("conifer", "evergreen shrub", "deciduous shurb", "Graminoid", "forb", 
                    "coarse woody debris", "lichen", "bare ground", "litter", "Equisetum spp"), 
       fill = c("red", "orange", "yellow", "lightgreen", "darkgreen", "lightblue", "darkblue", 
                "purple", "pink", "brown"))

## grass and shrubs ##
barplot(percent_composition ~ functional_groups + plot,
        data = point_GS,
        xlab = "Plot", 
        ylab = "Percent Cover",
        ylim = c(0, 100), 
        las = 2,
        cex.names = 0.75,
        col = c("red", "orange", "yellow", "lightgreen", "darkgreen", "lightblue", "darkblue", 
                "purple", "pink", "brown"))
title("Percent Cover by Functional Group (Grass+Shrub Treatment)", adj = 0.05, line = 1.5)
legend(12.5, 100, c("conifer", "evergreen shrub", "deciduous shurb", "Graminoid", "forb", 
                    "coarse woody debris", "lichen", "bare ground", "litter", "Equisetum spp"), 
       fill = c("red", "orange", "yellow", "lightgreen", "darkgreen", "lightblue", "darkblue", 
                "purple", "pink", "brown"))



########### subset percent cover by functional group using POINT INTERCEPT DATA ########### 
con <- subset(pt_int2, functional_groups == "CON")
evsh <- subset(pt_int2, functional_groups == "EVSH")
desh <- subset(pt_int2, functional_groups == "DESH")
gram <- subset(pt_int2, functional_groups == "GRAM")
forb <- subset(pt_int2, functional_groups == "FORB")
cwd <- subset(pt_int2, functional_groups == "CWD")
moss <- subset(pt_int2, functional_groups == "MOSS")
lichen <- subset(pt_int2, functional_groups == "LICH")
brg <- subset(pt_int2, functional_groups == "BRG")
litr <- subset(pt_int2, functional_groups == "LITR")
equ <- subset(pt_int2, functional_groups == "EQU")


### does change in NDVI correlate with veg type (ie does NDVI inc/dec with specific veg types)
point_ndvi <- merge(pt_int2, ndvi, by = c("plot"))

con <- subset(point_ndvi, functional_groups == "CON")
lm <- lm(FL016_ndvi ~ percent_composition, data = con)
summary(lm)

plot(FL016_ndvi ~ percent_composition, 
     data = con,
     main = "NDVI vs Percent Cover (Conifers)",
     xlab = "Percent Cover", 
     ylab = "NDVI (pre-clipping)",
     pch = 19, 
     col = "darkgreen")
abline(lm)



ev <- subset(point_ndvi, functional_groups == "EVSH")
lm <- lm(FL016_ndvi ~ percent_composition, data = ev)
summary(lm)

plot(FL016_ndvi ~ percent_composition, 
     data = ev,
     main = "NDVI vs Percent Cover (Evergreen Shrub)",
     xlab = "Percent Cover", 
     ylab = "NDVI (pre-clipping)",
     pch = 19, 
     col = "darkgreen")
abline(lm)



desh <- subset(point_ndvi, functional_groups == "DESH")
lm <- lm(FL016_ndvi ~ percent_composition, data = desh)
summary(lm)

plot(FL016_ndvi ~ percent_composition, 
     data = desh,
     main = "NDVI vs Percent Cover (Deciduous Shrub)",
     xlab = "Percent Cover", 
     ylab = "NDVI (pre-clipping)",
     pch = 19, 
     col = "darkgreen")
abline(lm)




gram <- subset(point_ndvi, functional_groups == "GRAM")
lm <- lm(FL016_ndvi ~ percent_composition, data = gram)
summary(lm)

plot(FL016_ndvi ~ percent_composition, 
     data = gram,
     main = "NDVI vs Percent Cover (Graminoid)",
     xlab = "Percent Cover", 
     ylab = "NDVI (pre-clipping)",
     pch = 19, 
     col = "darkgreen")
abline(lm)



forb <-subset(point_ndvi, functional_groups == "FORB")
lm <- lm(FL016_ndvi ~ percent_composition, data = forb)
summary(lm)

plot(FL016_ndvi ~ percent_composition, 
     data = forb,
     main = "NDVI vs Percent Cover (Forb)",
     xlab = "Percent Cover", 
     ylab = "NDVI (pre-clipping)",
     pch = 19, 
     col = "darkgreen")
abline(lm)



cwd <- subset(point_ndvi, functional_groups == "CWD")
lm <- lm(FL016_ndvi ~ percent_composition, data = cwd)
summary(lm)

plot(FL016_ndvi ~ percent_composition, 
     data = cwd,
     main = "NDVI vs Percent Cover (CWD)",
     xlab = "Percent Cover", 
     ylab = "NDVI (pre-clipping)",
     pch = 19, 
     col = "darkgreen")
abline(lm)


#### skipped moss, no moss in point intercept data


lich <- subset(point_ndvi, functional_groups == "LICH")
lm <- lm(FL016_ndvi ~ percent_composition, data = lich)
summary(lm)

plot(FL016_ndvi ~ percent_composition, 
     data = lich,
     main = "NDVI vs Percent Cover (Lichen)",
     xlab = "Percent Cover", 
     ylab = "NDVI (pre-clipping)",
     pch = 19, 
     col = "darkgreen")
abline(lm)



brg <- subset(point_ndvi, functional_groups == "BRG") ### irrelevant bc all plots have 0% cover for brg
lm <- lm(FL016_ndvi ~ percent_composition, data = brg)
summary(lm)

plot(FL016_ndvi ~ percent_composition, 
     data = brg,
     main = "NDVI vs Percent Cover (Bare Ground)",
     xlab = "Percent Cover", 
     ylab = "NDVI (pre-clipping)",
     pch = 19, 
     col = "darkgreen")
abline(lm)
### error : 
## Error in int_abline(a = a, b = b, h = h, v = v, untf = untf, ...) : 
##  'a' and 'b' must be finite


## no litter in point intercept data
## very confused ##


equ <- subset(point_ndvi, functional_groups == "EQU")
lm <- lm(FL016_ndvi ~ percent_composition, data = equ)
summary(lm)

plot(FL016_ndvi ~ percent_composition, 
     data = equ,
     main = "NDVI vs Percent Cover (Equisetum spp.)",
     xlab = "Percent Cover", 
     ylab = "NDVI (pre-clipping)",
     pch = 19, 
     col = "darkgreen")
abline(lm)





###############








