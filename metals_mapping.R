setwd("C:/Users/dmcallister/Desktop/Chidliak_Processed/Data")
chid_met <- read.csv("HallSoil_Points.csv")

# Arsenic, Lead 

# Purple - Yellow 

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

rsq <- function (x, y) cor(x, y) ^ 2

# ------------------- DEM 
# As, Ca, Pb, Cu, S,
dem <- raster("ArcticDEM_10m_UTM.tif")
slope <- raster("Slope_proj.tif")
aspect <- raster("Aspect_proj.tif")


dem_agg <- aggregate(dem, fact = 5) # aggregate output
slope_agg <- aggregate(slope, fact = 5)
aspect_agg <- aggregate(aspect, fact = 5)

r3resampled <- projectRaster(aspect_agg,dem_agg,method = 'bilinear')
r4resampled <- projectRaster(slope_agg,dem_agg,method = 'bilinear')

combined_raster <- stack(dem_agg, r3resampled,r4resampled)

info_frame <- as.data.frame(combined_raster, xy=TRUE)
info_frame <- info_frame[complete.cases(info_frame), ]
colnames(info_frame)[3:5] <- c("elev", "slope", "aspect")

writeRaster(combined_raster, "combined.tif",overwrite=TRUE)
write.csv(info_frame, "save.csv")

# ---------------- Assemble Data in standard form 

met_data <- as.data.frame(cbind(chid_met$POINT_X, chid_met$POINT_Y, chid_met$Li_PPM))
colnames(met_data) <- c("x","y","val")

# -------- Filter and plot
filter <- (met_data$x > max(info_frame$x)) | (met_data$x < min(info_frame$x)) | (met_data$y > max(info_frame$y)) | (met_data$y < min(info_frame$y))
met_data <- met_data[!filter, ]

# --------------- Get Elevation values
reso = 300 
met_features <- c()

for (i in 1:nrow(met_data)){
  coords <- as.numeric((met_data[i, ][c(1,2)]))
  
  box <- info_frame[info_frame$x < (coords[1]+reso) &
                      info_frame$y < (coords[2]+reso) &
                      info_frame$y > (coords[2]-reso) &
                      info_frame$x > (coords[1]-reso)
                      ,] 
  met_features = rbind(met_features, cbind(mean(box$elev),mean(box$aspect), 
                                   mean(box$slope)))
  
}

met_elev <- cbind(met_data, met_features)
met_elev <- met_elev[complete.cases(met_elev),]
colnames(met_elev)[4:6] <- c("elev","aspect","slope")
met_elev <- met_elev[,c(1,2,4,5,6,3)]

write.csv(met_elev, "met.csv")

met_elev <- read.csv("met.csv")
met_elev <- met_elev[,c(2:ncol(met_elev))]

# -------------- Reload Everything 

met_elev <- read.csv("met.csv")
met_elev <- met_elev[,c(2:ncol(met_elev))]
combined_raster = raster("combined.tif")
info_frame = read.csv("save.csv", row.names=NULL)
info_frame <- info_frame[,c(2:ncol(info_frame))]
chid_met <- read.csv("HallSoil_Points.csv")

# ------------- Modeling
metal_data = as.matrix(cbind(scale(met_elev[,-6]), met_elev[6]))
validation_set = scale(info_frame)

#classifier = randomForest(x = metal_data[, -6],
#                          y = metal_data[, 6],
#                          ntree = 100,
#                          importance = TRUE,
#                          do.trace = TRUE,
#                          corr.bias = TRUE)

#y_pred_map = predict(classifier, newdata = validation_set)
#y_pred_standarized = as.numeric(as.character(y_pred_map))
#test_frame = as.data.frame(cbind(info_frame, y_pred_standarized))

#ggplot(test_frame[x,], aes(x=x, y=y)) +
#  geom_point(aes(color = y_pred_standarized)) +
#  ggtitle("Random Forest Classifications")+
#  scale_colour_gradient(low ="#001c80", high = "#ff0000", limits=c(0, 100),name = "PPM" )

# ---------- Ranger
library(ranger)
x <- seq(from=1, to=nrow(info_frame), by = 43)

# -------------------- Distance Weights 

center <- c(mean(met_elev$x), mean(met_elev$y))
distance <- (met_elev$x - center[1])^2 + (met_elev$y - center[2])^2 
distance_scale <- distance/max(distance)

distance_exp <- 1/(10 - 1) * (10^(distance_scale)-1)
distance_quad <- -4*distance_scale^2+4*distance_scale 

distance_band_filter <- distance_scale/((2.5-10*distance_scale^2)^2+
                                          distance_scale^2)^.5
distance_center_linear <- distance_scale
distance_center_inverse <- (1 - distance_scale)


# no weighting 
rf <- ranger(as.formula("val ~ ."), data = met_elev, 
             num.trees = 100, classification = TRUE, case.weights=NULL)
pred <- predict(rf, data = info_frame[x,])
y_pred_standarized = as.numeric(as.character(pred$predictions))
test_frame = as.data.frame(cbind(info_frame[x,], y_pred_standarized))

no_weighting <- ggplot(test_frame, aes(x=x, y=y)) +
  geom_point(aes(color = y_pred_standarized)) +
  ggtitle("Random Forest Classifications - Baseline")+
  scale_colour_gradient(low ="#001c80", high = "#ff0000", limits=c(0, 100),name = "PPM" )

# quad Weighting
weights <- distance_quad / sum(distance_quad)
rf <- ranger(as.formula("val ~ ."), data = met_elev, 
             num.trees = 100, classification = TRUE, case.weights=weights)
pred <- predict(rf, data = info_frame[x,])

y_pred_standarized = as.numeric(as.character(pred$predictions))
test_frame = as.data.frame(cbind(info_frame[x,], y_pred_standarized))

quad_rf <- ggplot(test_frame, aes(x=x, y=y)) +
  geom_point(aes(color = y_pred_standarized)) +
  ggtitle("Random Forest Classifications - Quadratic")+
  scale_colour_gradient(low ="#001c80", high = "#ff0000", limits=c(0, 100),name = "PPM" )

# exponential Weighting
weights <- distance_exp / sum(distance_exp)
rf <- ranger(as.formula("val ~ ."), data = met_elev, 
             num.trees = 100, classification = TRUE, case.weights=weights)
pred <- predict(rf, data = info_frame[x,])
y_pred_standarized = as.numeric(as.character(pred$predictions))
test_frame = as.data.frame(cbind(info_frame[x,], y_pred_standarized))

exp_rf <- ggplot(test_frame, aes(x=x, y=y)) +
  geom_point(aes(color = y_pred_standarized)) +
  ggtitle("Random Forest Classifications - Expotential")+
  scale_colour_gradient(low ="#001c80", high = "#ff0000", limits=c(0, 100),name = "PPM" )

# band Weighting
weights <- distance_band_filter / sum(distance_band_filter)
rf <- ranger(as.formula("val ~ ."), data = met_elev, 
             num.trees = 100, classification = TRUE, case.weights=weights)
pred <- predict(rf, data = info_frame[x,])
y_pred_standarized = as.numeric(as.character(pred$predictions))
test_frame = as.data.frame(cbind(info_frame[x,], y_pred_standarized))

band_rf <- ggplot(test_frame, aes(x=x, y=y)) +
  geom_point(aes(color = y_pred_standarized)) +
  ggtitle("Random Forest Classifications - Filter")+
  scale_colour_gradient(low ="#001c80", high = "#ff0000", limits=c(0, 100),name = "PPM" )

# linear Weighting
weights <- distance_center_linear / sum(distance_center_linear)
rf <- ranger(as.formula("val ~ ."), data = met_elev, 
             num.trees = 100, classification = TRUE, case.weights=weights)
pred <- predict(rf, data = info_frame[x,])
y_pred_standarized = as.numeric(as.character(pred$predictions))
test_frame = as.data.frame(cbind(info_frame[x,], y_pred_standarized))

linear_rf <- ggplot(test_frame, aes(x=x, y=y)) +
  geom_point(aes(color = y_pred_standarized)) +
  ggtitle("Random Forest Classifications - Linear")+
  scale_colour_gradient(low ="#001c80", high = "#ff0000", limits=c(0, 100),name = "PPM" )

# inverse linear Weighting
weights <- distance_center_inverse / sum(distance_center_inverse)
rf <- ranger(as.formula("val ~ ."), data = met_elev, 
             num.trees = 100, classification = TRUE, case.weights=weights)
pred <- predict(rf, data = info_frame[x,])
y_pred_standarized = as.numeric(as.character(pred$predictions))
test_frame = as.data.frame(cbind(info_frame[x,], y_pred_standarized))

inv_linear_rf <- ggplot(test_frame, aes(x=x, y=y)) +
  geom_point(aes(color = y_pred_standarized)) +
  ggtitle("Random Forest Classifications -Inverse Linear")+
  scale_colour_gradient(low ="#001c80", high = "#ff0000", limits=c(0, 100),name = "PPM" )


ggarrange(no_weighting, linear_rf, inv_linear_rf,
          quad_rf, exp_rf, band_rf, ncol=2, nrow = 3)

# ------------- KNN
x <- seq(from=1, to=nrow(info_frame), by = 43)


# ---- All
k_val = 5

y_pred_map = knn(train = metal_data[, c(-6)],
                 test = validation_set[x,],
                 cl = metal_data[, 6],
                 k = k_val,
                 use.all = FALSE)

y_pred_standarized = as.numeric(as.character(y_pred_map))
test_frame = as.data.frame(cbind(info_frame[x,], y_pred_standarized))

knn_all <- ggplot(test_frame, aes(x=x, y=y)) +
  geom_point(aes(color = y_pred_standarized)) +
  scale_colour_gradient(low ="#001c80", high = "#ff0000", limits=c(0, 100),name = "PPM" )+
  ggtitle("KNN - All Predictors")

# ---- No Aspect
y_pred_map = knn(train = metal_data[, c(-6,-4)],
                 test = validation_set[x,-5],
                 cl = metal_data[, 6],
                 k = k_val,
                 use.all = FALSE)

y_pred_standarized = as.numeric(as.character(y_pred_map))
test_frame = as.data.frame(cbind(info_frame[x,], y_pred_standarized))

knn_no_aspect <- ggplot(test_frame, aes(x=x, y=y)) +
  geom_point(aes(color = y_pred_standarized)) +
  scale_colour_gradient(low ="#001c80", high = "#ff0000", limits=c(0, 100),name = "PPM" )+
  ggtitle("KNN - No Aspect")

# ----- No Slope

y_pred_map = knn(train = metal_data[, c(-6,-5)],
                 test = validation_set[x,-4],
                 cl = metal_data[, 6],
                 k = k_val,
                 use.all = FALSE)

y_pred_standarized = as.numeric(as.character(y_pred_map))
test_frame = as.data.frame(cbind(info_frame[x,], y_pred_standarized))

knn_no_slope <- ggplot(test_frame, aes(x=x, y=y)) +
  geom_point(aes(color = y_pred_standarized)) +
  scale_colour_gradient(low ="#001c80", high = "#ff0000", limits=c(0, 100),name = "PPM" )+
  ggtitle("KNN - No Slope")

# ----- No Slope and no aspect
y_pred_map = knn(train = metal_data[, c(-6,-5,-4)],
                 test = validation_set[x,c(-4,-5)],
                 cl = metal_data[, 6],
                 k = k_val,
                 use.all = FALSE)
y_pred_standarized = as.numeric(as.character(y_pred_map))
test_frame = as.data.frame(cbind(info_frame[x,], y_pred_standarized))

knn_no_slope_aspect <- ggplot(test_frame, aes(x=x, y=y)) +
  geom_point(aes(color = y_pred_standarized)) +
  scale_colour_gradient(low ="#001c80", high = "#ff0000", limits=c(0, 100),name = "PPM" )+
  ggtitle("KNN - No Slope or Aspect")


ggarrange(knn_all, knn_no_aspect, knn_no_slope, knn_no_slope_aspect)



# ------ Final Maps
# inv_lin  Weighting
weights <- distance_center_inverse / sum(distance_center_inverse)
rf <- ranger(as.formula("val ~ ."), data = met_elev, 
             num.trees = 100, classification = TRUE, case.weights=weights)
pred <- predict(rf, data = info_frame[x,])
y_pred_standarized = as.numeric(as.character(pred$predictions))
test_frame = as.data.frame(cbind(info_frame[x,], y_pred_standarized))

rf_final <- ggplot(test_frame, aes(x=x, y=y)) +
  geom_point(aes(color = y_pred_standarized)) +
  ggtitle("Random Forest Classifications - Lithium")+
  scale_colour_gradient(low ="#001c80", high = "#ff0000", 
                        limits=c(0, 100),name = "PPM" )+
  xlab("Easting")+
  ylab("Northing")


# KNN 

y_pred_map = knn(train = metal_data[, c(-6,-5,-4)],
                 test = validation_set[x,c(-4,-5)],
                 cl = metal_data[, 6],
                 k = 8,
                 use.all = FALSE)
y_pred_standarized = as.numeric(as.character(y_pred_map))
test_frame = as.data.frame(cbind(info_frame[x,], y_pred_standarized))

knn_final <- ggplot(test_frame, aes(x=x, y=y)) +
  geom_point(aes(color = y_pred_standarized)) +
  scale_colour_gradient(low ="#001c80", high = "#ff0000", limits=c(0, 100),name = "PPM" )+
  ggtitle("KNN - Lithium")+
  xlab("Easting")+
  ylab("Northing")


# DEM PLOT 

DEM_PLOT <- ggplot(data=info_frame[x,], aes(x=x, y=y))+
  geom_point(aes(color=elev))+
  geom_point(data=met_elev, color="blue", size=.5)+
  ggtitle("DEM and Sample Points")+
  scale_colour_gradient(low ="#FF5733", high = "#581845",name = "Elevation (m)" )


ggarrange(DEM_PLOT,rf_final, knn_final, ncol=3, nrow=1)


# ------------- Accuracy Testing 

split = sample.split(met_elev[,5] , SplitRatio = 0.8)
training_set = subset(met_elev, split == TRUE)
test_set = subset(met_elev, split == FALSE)

weights <- distance_center_inverse / sum(distance_center_inverse)
weights = subset(weights, split==TRUE)

# ---- Random Forest

rf <- ranger(as.formula("val ~ ."), data = training_set, 
             num.trees = 100, classification = TRUE, case.weights=weights, 
             verbose= TRUE)
rf$prediction.error
pred <- predict(rf, data = test_set[,-6])
rsq(test_set[,6],pred$predictions)
plot(test_set[,6],pred$predictions)

# ---- KNN 

y_pred = knn(train = scale(training_set[, -6]),
             test = scale(test_set[, -6]),
             cl = training_set[, 6],
             k = 10,
             prob = TRUE)

y_pred_test = as.numeric(as.character(y_pred))

rsq(y_pred_test, test_set[,6])

rf_2 <- ranger(as.formula("y ~ ."), data=as.data.frame(cbind(y=y_pred_test,x=test_set[,6])), 
               num.trees = 100, classification = TRUE)
pred_correction <- predict(rf_2, test_set)

plot(y_pred_test, test_set[,6])

rsq_KNN <- 0.04255594
rsq_randomforest <- 0.9816929
# --------------- Further Mapping

met_data <- as.data.frame(cbind(chid_met$POINT_X, chid_met$POINT_Y, chid_met$Pb_PPM))
colnames(met_data) <- c("x","y","val")

# -------- Filter and plot

x <- seq(from=1, to=nrow(info_frame), by = 43)

filter <- (met_data$x > max(info_frame$x)) | (met_data$x < min(info_frame$x)) | (met_data$y > max(info_frame$y)) | (met_data$y < min(info_frame$y))
met_data <- met_data[!filter, ]

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
  print(i/nrow(met_data)*100)
}

met_elev <- cbind(met_data, met_features)
met_elev <- met_elev[complete.cases(met_elev),]
colnames(met_elev)[4:6] <- c("elev","aspect","slope")
met_elev <- met_elev[,c(1,2,4,5,6,3)]

# --- Plotting
x <- seq(from=1, to=nrow(info_frame), by = 1)

center <- c(mean(met_elev$x), mean(met_elev$y))
distance <- (met_elev$x - center[1])^2 + (met_elev$y - center[2])^2 
distance_scale <- distance/max(distance)
distance_center_inverse <- (1 - distance_scale)

weights <- distance_center_inverse / sum(distance_center_inverse)

rf <- ranger(as.formula("val ~ ."), data = met_elev, 
             num.trees = 100, classification = TRUE, case.weights=weights)

pred <- predict(rf, data = info_frame[x,])
y_pred_standarized = as.numeric(as.character(pred$predictions))
test_frame = as.data.frame(cbind(info_frame[x,], y_pred_standarized))


raster_test <- rasterFromXYZ(test_frame[,c(1,2,6)], crs = "+init=epsg:32619")
plot(raster_test)
writeRaster(raster_test, "Lead_map.tif")
