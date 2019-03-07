############################################################################################################
# -*- coding: utf-8 -*-
#Created on Wed Mar 06 12:55:41 2019
#@author: chinmay deval 
#Object: """"""""""RSToolbox APPROACH """"""""""
#       Perform supervised classification using random forest algorithm on landsat 
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

## Fit classifier (splitting training into 70% training data, 30% validation data)
beginCluster()
SupC <- superClass(img, trainData, responseCol = "ClassID", 
                       model = "rf", tuneLength = 1, trainPartition = 0.7)
endCluster()

####
writeRaster(SupC$map, filename = "predictedRFclassification.TIF", overwrite=TRUE )


