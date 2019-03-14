############################################################################################################
# -*- coding: utf-8 -*-
#Created on Wed Mar 06 12:55:41 2019
#@author: chinmay deval 
#Object: """"""""""RSToolbox APPROACH """"""""""
#       Preprocess remotesensing data
############################################################################################################


###############################clear environment and console###############################

rm(list = ls())
cat("\014")

###############################set working dir###############################

# dir.create("D:/Chinmay/Pune/test_del_Soon/")

setwd("D:/Chinmay/Pune/test_del_Soon/")

###############################Load required packages###############################

library(raster)
library(rgdal)
library(caret)
library(RStoolbox)

###############################Load image###############################

## preprocess 
beginCluster()
meta1990 <- readMeta("D:/Chinmay/Pune/to_be_used_ldsat/LT05_L1TP_147047_19900318_20170131_01_T1/LT05_L1TP_147047_19900318_20170131_01_T1_MTL.txt")
meta1999 <- readMeta("D:/Chinmay/Pune/to_be_used_ldsat/LT05_L1TP_147047_19990327_20161218_01_T1/LT05_L1TP_147047_19990327_20161218_01_T1_MTL.txt")
meta2017 <- readMeta("D:/Chinmay/Pune/to_be_used_ldsat/LC08_L1TP_147047_20170328_20170414_01_T1/LC08_L1TP_147047_20170328_20170414_01_T1_MTL.txt")

summary(meta1990)
summary(meta1999)
summary(meta2017)


## stack bands along with the metadata

TM1990 <- stackMeta(meta1990)
TM1999 <- stackMeta(meta1999)
OLI2017 <- stackMeta(meta2017)

### clip to the area mask

mask <- readOGR("D:/OneDrive - University of Idaho/Pune/Pune_Admin/Pune_City_Admin_feature_to_poly_WGS84UTM43N.shp")
dem <- raster ("D:/Chinmay/Pune/DEM/srtm_51_09/srtm_51_09.tif")
dem <- projectRaster(dem,TM1990)

dem_clip1 <- crop(dem, extent(mask))
dem_clip <- mask(dem_clip1, mask)

TM1990_clip1 <- crop(TM1990, extent(mask))
TM1990_clip <- mask(TM1990_clip1, mask)

TM1999_clip1 <- crop(TM1999, extent(mask))
TM1999_clip <- mask(TM1999_clip1, mask)

OLI2017_clip1 <- crop(OLI2017, extent(mask))
OLI2017_clip <- mask(OLI2017_clip1, mask)

### convert to TOA Radiance values

# TM1990_rad <- radCor(TM1990, meta1990, method = "rad")
# TM1999_rad <- radCor(TM1999, meta1999, method = "rad")
# OLI2017_rad <- radCor(OLI2017, meta2017, method = "rad")

### also calculate TOA reflectance and brightness temp values
# 
# TM1990_ref <- radCor(TM1990_clip, meta1990, method = "apref")
# TM1999_ref <- radCor(TM1999_clip, meta1999, method = "apref")
# OLI2017_ref <- radCor(OLI2017_clip, meta2017, method = "apref")

### Atmospheric correction using DOS 
#(correct DN to at-surface-reflecatance with DOS (Chavez decay model))

TM1990_dos <- radCor(TM1990_clip, meta1990, method = "dos", darkProp = 0.01)
TM1999_dos <- radCor(TM1999_clip, meta1999, method = "dos", darkProp = 0.01)
OLI2017_dos <- radCor(OLI2017_clip, meta2017, method = "dos", darkProp = 0.01)

### Topographic correction

TM1990_dos_topcor <-topCor(TM1990_dos, dem_clip, metaData = meta1990, method = "C")
TM1999_dos_topcor <-topCor(TM1999_dos, dem_clip, metaData = meta1999, method = "C")
OLI2017_dos_topcor <- topCor(OLI2017_dos, dem_clip, metaData = meta2017, method = "C")


### Subset to RGBNIR bands
# TM1990_sub <- TM1990_dos[[c(1:4)]]
# TM1999_sub <- TM1999_dos[[c(1:4)]]
# OLI2017_sub <- OLI2017_dos[[c(2:5)]]

### write pre processed images to a raster file

writeRaster(TM1990_dos_topcor, "preprocessed_TM1990_dos_topcor.TIF")

writeRaster(TM1999_dos_topcor, "preprocessed_TM1999_dos_topcor.TIF")

writeRaster(OLI2017_dos_topcor, "preprocessed_OLI2017_dos_topcor.TIF")

#### Unsup Classification
# # 
# # set.seed(6)
# # TM1990_UC <- unsuperClass(TM1990_dos, nClasses = 5, norm = TRUE)
# 
# ### Kmeans classification
# TM1990_sub_kmeans <- kmeans(na.omit(as.matrix(TM1990_sub)), centers = 5, iter.max = 100,
#                             nstart = 10)
# ## create empty raster of same specification as 
# ### the input to kmeans.
# TM1990_UC_kmeans <- raster(TM1990_clip)
# 
# ### write results of kmeans to the empty raster
# TM1990_UC_kmeans[] <- TM1990_sub_kmeans$cluster

endCluster()