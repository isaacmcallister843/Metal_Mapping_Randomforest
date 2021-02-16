setwd("C:/Users/dmcallister/Desktop/Chidliak_Processed/Data")
library(class)
library(reshape)
library(ggplot2)
library(raster)
require(rgdal)
library(ggpubr)
library(rfUtilities)
library(ranger)

library(randomForest)
library(e1071)
library(caTools)
library(ranger)

rsq <- function (x, y) cor(x, y) ^ 2

# ---- Load in
chid_met <- read.csv("HallSoil_Points.csv")

combined_raster = raster("combined.tif")
info_frame = read.csv("save.csv", row.names=NULL)
info_frame <- info_frame[,c(2:ncol(info_frame))]
chid_met <- read.csv("HallSoil_Points.csv")

met_data <- as.data.frame(cbind(chid_met$POINT_X, chid_met$POINT_Y, chid_met$Cd_PPM))
colnames(met_data) <- c("x","y","val")

# -------- Filter and plot
filter <- (met_data$x > max(info_frame$x)) | (met_data$x < min(info_frame$x)) | (met_data$y > max(info_frame$y)) | (met_data$y < min(info_frame$y))
met_data <- met_data[!filter, ]
met_data <- met_data[complete.cases(met_data),]

# --------------- Get Elevation values
x <- seq(from=1, to=nrow(info_frame), by = 43)

reso = 300 
met_features <- c()

for (i in 1:nrow(met_data)){
  coords <- as.numeric((met_data[i, ][c(1,2)]))
  
  box <- info_frame[info_frame[x,]$x < (coords[1]+reso) &
                      info_frame[x,]$y < (coords[2]+reso) &
                      info_frame[x,]$y > (coords[2]-reso) &
                      info_frame[x,]$x > (coords[1]-reso)
                    ,] 
  met_features = rbind(met_features, cbind(mean(box$elev),mean(box$aspect), 
                                           mean(box$slope)))
}

met_elev <- cbind(met_data, met_features)
met_elev <- met_elev[complete.cases(met_elev),]
colnames(met_elev)[4:6] <- c("elev","aspect","slope")
met_elev <- met_elev[,c(1,2,4,5,6,3)]

# ------------- Modeling
metal_data = as.matrix(cbind(scale(met_elev[,-6]), met_elev[6]))
center <- c(mean(met_elev$x), mean(met_elev$y))
distance <- (met_elev$x - center[1])^2 + (met_elev$y - center[2])^2 
distance_scale <- distance/max(distance)
distance_center_inverse <- (1 - distance_scale)


# ------------ Plotting
weights <- distance_center_inverse / sum(distance_center_inverse)

rf <- ranger(as.formula("val ~ ."), data = met_elev, 
             num.trees = 100, classification = TRUE, case.weights=weights)

pred <- predict(rf, data = info_frame)
y_pred_standarized = as.numeric(as.character(pred$predictions))
test_frame = as.data.frame(cbind(info_frame, y_pred_standarized))

rf_final <- ggplot(test_frame[x,], aes(x=x, y=y)) +
  geom_point(aes(color = y_pred_standarized)) +
  ggtitle("Random Forest Classifications - Cadmium")+
  scale_colour_gradient(low ="#001c80", high = "#ff0000", 
                        breaks=seq(0,1,by=.2),name = "PPM" )+
  xlab("Easting")+
  ylab("Northing")
rf_final

raster_test <- rasterFromXYZ(test_frame[,c(1,2,6)], crs = "+init=epsg:32619")
writeRaster(raster_test, "Cadmium_map.tif")










