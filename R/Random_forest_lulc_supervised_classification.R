############################################################################################################
# -*- coding: utf-8 -*-
#Created on Wed Mar 06 12:55:41 2019
#@author: chinmay deval 
#Object: Perform supervised classification using random forest algorithm on landsat 
#       spectral indices 
############################################################################################################


###############################clear environment and console###############################

rm(list = ls())
cat("\014")

###############################set working dir###############################

dir.create("D:/Chinmay/Pune/Random_forest_outputs/")

setwd("D:/Chinmay/Pune/Random_forest_outputs/")

###############################Load required packages###############################

library(raster)
library(rgdal)
library(caret)
library(RStoolbox)

###############################Load image###############################

ndvi<- raster("D:/Chinmay/Pune/Analysis_06_06_2018/NITK_RSGIS_20180824_154257/Outputs/LT05_L1TP_147047_19900318_20170131_01_T1.tar/LT5[147_047](1990-03-18_04-48)NDVI.TIF")
ndbi<-raster("D:/Chinmay/Pune/Analysis_06_06_2018/NITK_RSGIS_20180824_154257/Outputs/LT05_L1TP_147047_19900318_20170131_01_T1.tar/LT5[147_047](1990-03-18_04-48)NDBI_01.TIF")
ndbai<- raster("D:/Chinmay/Pune/Analysis_06_06_2018/NITK_RSGIS_20180824_154257/Outputs/LT05_L1TP_147047_19900318_20170131_01_T1.tar/LT5[147_047](1990-03-18_04-48)NDBaI_01.TIF")
ndwi<- raster("D:/Chinmay/Pune/Analysis_06_06_2018/NITK_RSGIS_20180824_154257/Outputs/LT05_L1TP_147047_19900318_20170131_01_T1.tar/LT5[147_047](1990-03-18_04-48)NDWI.TIF")
mndwi<-raster("D:/Chinmay/Pune/Analysis_06_06_2018/NITK_RSGIS_20180824_154257/Outputs/LT05_L1TP_147047_19900318_20170131_01_T1.tar/LT5[147_047](1990-03-18_04-48)MNDWI_01.TIF")

img<- stack(ndvi,ndbi,ndbai,ndwi,mndwi)
names(img) <- c ("NDVI", "NDBI", "NDBAI", "NDWI", "MNDWI")

###############################Load image###############################
trainData <- shapefile("D:/OneDrive - University of Idaho/Pune/training.shp")
responseCol <- "ClassID"

################extract values for each band for each training poly######################

dfAll = data.frame(matrix(vector(), nrow = 0, ncol = length(names(img)) + 1))   

for (i in 1:length(unique(trainData[[responseCol]]))){
  category <- unique(trainData[[responseCol]])[i]
  categorymap <- trainData[trainData[[responseCol]] == category,]
  dataSet <- extract(img, categorymap)
  if(is(trainData, "SpatialPointsDataFrame")){
    dataSet <- cbind(dataSet, class = as.numeric(rep(category, nrow(dataSet))))
    dfAll <- rbind(dfAll, dataSet[complete.cases(dataSet),])
  }
  if(is(trainData, "SpatialPolygonsDataFrame")){
    dataSet <- dataSet[!unlist(lapply(dataSet, is.null))]
    dataSet <- lapply(dataSet, function(x){cbind(x, class = as.numeric(rep(category, nrow(x))))})
    df <- do.call("rbind", dataSet)
    dfAll <- rbind(dfAll, df)
  }
}

###############################Fit Model###############################

nsamples <- 20486
sdfAll <- dfAll[sample(1:nrow(dfAll), nsamples), ]

beginCluster()
modFit_rf <- train(as.factor(class) ~ NDVI+NDBI+NDBAI+NDWI+MNDWI, method = "rf", data = sdfAll)

###############################Predict/ Classify image###############################


preds_rf <- clusterR(img, raster::predict, args = list(model = modFit_rf))
endCluster()

###############################writ classified image to a raster ##############

writeRaster(preds_rf, filename = "predictedRFclassification.TIF", overwrite=TRUE )


