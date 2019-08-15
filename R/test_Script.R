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

dir.create("D:/Chinmay/Pune/test_del_Soon/06_23_2019/")

setwd("D:/Chinmay/Pune/test_del_Soon/06_23_2019")

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
meta2019 <- readMeta("D:/Chinmay/Pune/to_be_used_ldsat/LC08_L1TP_147047_20190318_20190325_01_T1/LC08_L1TP_147047_20190318_20190325_01_T1_MTL.txt")


summary(meta1990)
summary(meta1999)
summary(meta2017)
summary(meta2019)

## stack bands along with the metadata

TM1990 <- stackMeta(meta1990)
TM1999 <- stackMeta(meta1999)
OLI2017 <- stackMeta(meta2017)
OLI2019 <- stackMeta(meta2019)

### clip to the area mask

mask <- readOGR("D:/OneDrive - University of Idaho/Pune/Pune_Admin/Pune_City_Admin_feature_to_poly_WGS84UTM43N.shp")
dem <- raster ("D:/Chinmay/Pune/DEM/srtm_51_09/srtm_51_09.tif")
dem <- projectRaster(dem,TM1999)

dem_clip1 <- crop(dem, extent(mask))
dem_clip <- mask(dem_clip1, mask)

TM1990_clip1 <- crop(TM1990, extent(mask))
TM1990_clip <- mask(TM1990_clip1, mask)

TM1999_clip1 <- crop(TM1999, extent(mask))
TM1999_clip <- mask(TM1999_clip1, mask)

OLI2017_clip1 <- crop(OLI2017, extent(mask))
OLI2017_clip <- mask(OLI2017_clip1, mask)

OLI2019_clip1 <- crop(OLI2019, extent(mask))
OLI2019_clip <- mask(OLI2019_clip1, mask)


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

### estimate haze

TM1990_dos <- radCor(TM1990_clip, meta1990, method = "dos", darkProp = 0.01)
TM1999_dos <- radCor(TM1999_clip, meta1999, method = "dos", darkProp = 0.01)
OLI2017_dos <- radCor(OLI2017_clip, meta2017, method = "dos", darkProp = 0.01)
OLI2019_dos <- radCor(OLI2019_clip, meta2019, method = "dos", darkProp = 0.01)

### Topographic correction

TM1990_dos_topcor <-topCor(TM1990_dos, dem_clip, metaData = meta1990, method = "C")
TM1999_dos_topcor <-topCor(TM1999_dos, dem_clip, metaData = meta1999, method = "C")
OLI2017_dos_topcor <- topCor(OLI2017_dos, dem_clip, metaData = meta2017, method = "C")
OLI2019_dos_topcor <- topCor(OLI2019_dos, dem_clip, metaData = meta2019, method = "C")

### Calculate Spectral Indices
SI1990 <- spectralIndices(TM1990_dos_topcor, blue = 1, green = 2, red = 3, nir = 4, swir2 = 5, swir3 = 7 )
NDBI1990 <- (TM1990_dos_topcor[[5]] - TM1990_dos_topcor[[4]])/(TM1990_dos_topcor[[5]] + TM1990_dos_topcor[[4]])
NDBaI1990 <-(TM1990_dos_topcor[[5]] - TM1990_dos_topcor[[6]])/(TM1990_dos_topcor[[5]] + TM1990_dos_topcor[[6]])

SI1999 <- spectralIndices(TM1999_dos_topcor, blue = 1, green = 2, red = 3, nir = 4, swir2 = 5, swir3 = 7 )
NDBI1999 <- (TM1999_dos_topcor[[5]] - TM1999_dos_topcor[[4]])/(TM1999_dos_topcor[[5]] + TM1999_dos_topcor[[4]])
NDBaI1999 <-(TM1999_dos_topcor[[5]] - TM1999_dos_topcor[[6]])/(TM1999_dos_topcor[[5]] + TM1999_dos_topcor[[6]])

SI2017 <- spectralIndices(OLI2017_dos_topcor, blue = 2, green = 3, red = 4, nir = 5,swir2 = 6,swir3 = 7)
NDBI2017 <-(OLI2017_dos_topcor[[5]] - OLI2017_dos_topcor[[6]])/(OLI2017_dos_topcor[[5]] + OLI2017_dos_topcor[[6]])
NDBaI2017 <- (OLI2017_dos_topcor[[6]] - OLI2017_dos_topcor[[9]]) /  (OLI2017_dos_topcor[[6]] + OLI2017_dos_topcor[[9]])
NDBaI22017 <- (OLI2017_dos_topcor[[6]] - OLI2017_dos_topcor[[10]]) /  (OLI2017_dos_topcor[[6]] + OLI2017_dos_topcor[[10]])
  
SI2019 <- spectralIndices(OLI2019_dos_topcor, blue = 2, green = 3, red = 4, nir = 5,swir2 = 6,swir3 = 7)
NDBI2019 <-(OLI2019_dos_topcor[[5]] - OLI2019_dos_topcor[[6]])/(OLI2019_dos_topcor[[5]] + OLI2019_dos_topcor[[6]])
NDBaI2019 <- (OLI2019_dos_topcor[[6]] - OLI2019_dos_topcor[[9]]) /  (OLI2019_dos_topcor[[6]] + OLI2019_dos_topcor[[9]])
NDBaI22019 <- (OLI2019_dos_topcor[[6]] - OLI2019_dos_topcor[[10]]) /  (OLI2019_dos_topcor[[6]] + OLI2019_dos_topcor[[10]])
# PCB67 <- rasterPCA(stack(OLI2019_dos_topcor[[6]],OLI2019_dos_topcor[[7]]))
# summary(PCB67$model)
# plot(PCB67$map)
# PCB1011 <- rasterPCA(stack(OLI2019_dos_topcor[[9]],OLI2019_dos_topcor[[10]]))
# summary(PCB1011$model)
# plot(PCB1011$map)
# 
# NDBI2019 <- ((PCB67$map[[1]] + PCB1011$map[[1]]) - OLI2019_dos_topcor[[5]])/((PCB67$map[[1]] + PCB1011$map[[1]]) + OLI2019_dos_topcor[[5]])

writeRaster(SI1990[[7]], "MNDWI_1990.TIF", overwrite=TRUE)
writeRaster(SI1990[[11]], "NDVI_1990.TIF", overwrite=TRUE)
writeRaster(SI1990[[13]], "NDWI_1990.TIF", overwrite=TRUE)
writeRaster(NDBI1990, "NDBI_1990.TIF", overwrite=TRUE)
writeRaster(NDBaI1990, "NDBaI_1990.TIF", overwrite=TRUE)


writeRaster(SI1999[[7]], "MNDWI_1999.TIF", overwrite=TRUE)
writeRaster(SI1999[[11]], "NDVI_1999.TIF", overwrite=TRUE)
writeRaster(SI1999[[13]], "NDWI_1999.TIF", overwrite=TRUE)
writeRaster(NDBI1999, "NDBI_1999.TIF", overwrite=TRUE)
writeRaster(NDBaI1999, "NDBaI_1999.TIF", overwrite=TRUE)


writeRaster(SI2017[[7]], "MNDWI_2017.TIF", overwrite=TRUE)
writeRaster(SI2017[[11]], "NDVI_2017.TIF", overwrite=TRUE)
writeRaster(SI2017[[13]], "NDWI_2017.TIF", overwrite=TRUE)
writeRaster(NDBI2017, "NDBI_2017.TIF", overwrite=TRUE)
writeRaster(NDBaI2017, "NDBaI_2017.TIF", overwrite=TRUE)
writeRaster(NDBaI22017, "NDBaI_2017_2.TIF", overwrite=TRUE)

writeRaster(SI2019[[7]], "MNDWI_2019.TIF", overwrite=TRUE)
writeRaster(SI2019[[11]], "NDVI_2019.TIF", overwrite=TRUE)
writeRaster(SI2019[[13]], "NDWI_2019.TIF", overwrite=TRUE)
writeRaster(NDBI2019, "NDBI_2019.TIF", overwrite=TRUE)
writeRaster(NDBaI2019, "NDBaI_2019.TIF", overwrite=TRUE)
writeRaster(NDBaI22019, "NDBaI_2019_2.TIF", overwrite=TRUE)


# stackedIndices_1999 <- stack(SI1999[[7]], SI1999[[11]], SI1999[[13]],NDBI1999, NDBaI1999)
# names(stackedIndices_1999)[[4]] <- "NDBI"
# names(stackedIndices_1999)[[5]] <- "NDBaI"
# writeRaster(stackedIndices_1999, "stackedIndices_1999.TIF", overwrite=TRUE)
# 
# stackedIndices_2019 <- stack(SI2019[[7]], SI2019[[11]], SI2019[[13]],NDBI2019, NDBaI22019)
# names(stackedIndices_2019)[[4]] <- "NDBI"
# names(stackedIndices_2019)[[5]] <- "NDBaI"
# writeRaster(stackedIndices_2019, "stackedIndices_2019.TIF", overwrite=TRUE)


### write pre processed images to a raster file

# # writeRaster(TM1990_dos_topcor, "preprocessed_TM1990_dos_topcor.TIF", overwrite=TRUE)
# 
# # writeRaster(TM1999_dos_topcor, "preprocessed_TM1999_dos_topcor.TIF", format="GTiff")
# 
# # writeRaster(OLI2017_dos_topcor, "preprocessed_OLI2017_dos_topcor.TIF", overwrite=TRUE)
# 
# # writeRaster(OLI2019_dos_topcor, "preprocessed_OLI2019_dos_topcor.TIF", format="GTiff")


endCluster()






##############################LST#######################################

# For calculating of surface temperature we use formula:

# LST(°C)=Bt/[1+(w???Bt/p)???ln(e)]???273.15
# Where:

# Bt is At satellite temperature

# w is wavelength of emitted radiance

# p is h???c/s, where h is Planc's constant, c is velocity of light, s is Boltzmann constant. p is equal to 14388

# e is electromagnetic emissivity. For urban areas I use formula from Stathopolou (2007):

# e=0.017???PV+0.963
# In formula of electromagnetic emissivity PV is "Proportion of Vegetation". For PV calculation is used the NDVI.
# 
# PV=[(NDVI???NDVImin)/(NDVImax???NDVImin)]2


#########PV
# 
# PV2019 <- ((stackedIndices_2019[[2]] - minValue(stackedIndices_2019[[2]])) /
#              (maxValue(stackedIndices_2019[[2]]) + (minValue(stackedIndices_2019[[2]]))))^2
# plot(PV2019)
# 
# PV1999 <- ((stackedIndices_1999[[2]] - minValue(stackedIndices_1999[[2]])) /
#              (maxValue(stackedIndices_1999[[2]]) + (minValue(stackedIndices_1999[[2]]))))^2
# plot(PV1999)
# 
# 
# ### emissivity
# # 
# emissiv2019 <- 0.017 * PV2019 + 0.963
# emissiv1999 <- 0.017 * PV1999 + 0.963
# 
# 
# ### LST
# bt10_2019 <- raster("D:/Chinmay/Pune/to_be_used_ldsat/NITK_RSGIS_20190725_145349/Outputs/LC08_L1TP_147047_20190318_20190325_01_T1.tar/LC8[147_47](2019-03-18_05-27)Brightness_Temp_B10.TIF")
# bt10_20191 <- crop(bt10_2019, extent(mask))
# bt10_2019 <- mask(bt10_20191, mask)
# 
# 
# bt10_1999 <- raster("D:/Chinmay/Pune/to_be_used_ldsat/NITK_RSGIS_20190728_140027/Outputs/LT05_L1TP_147047_19990327_20161218_01_T1.tar/LT5[147_047](1999-03-27_05-06)Brightness_Temp.TIF")
# bt10_19991 <- crop(bt10_1999, extent(mask))
# bt10_1999 <- mask(bt10_19991, mask)
# 
# 
# # LST2019 <- OLI2019_dos_topcor$B10_bt / 
# # (1 + 10.8 * (OLI2019_dos_topcor$B10_bt/14388) * log(emissiv2019)) - 273.15
# # 
# # plot(LST2019)
# 
# LST1999 <- bt10_1999 / 
#   (1 + 10.8 * (bt10_1999/14388) * log(emissiv1999)) 
# plot(LST1999)
# 
# LST2019 <- bt10_2019 / 
#   (1 + 10.8 * (bt10_2019/14388) * log(emissiv2019)) 
# plot(LST2019)
# 
# lststack <- stack(LST1999, LST2019)
# levelplot(lststack)
# 
# emiss <- stack(emissiv1999, emissiv2019)
# levelplot(emiss)
# 
# s <- stack(bt10_1999, bt10_2019) 
# names(s) <- c("BT1999", "BT2019")
# levelplot(s)

s <- stack(SI1990[[11]],SI1999[[11]],SI2017[[11]],SI2019[[11]])
names(s) <- c("1990","1999", "2017", "2019")
levelplot(s)

w <- stack(SI1990[[13]],SI1999[[13]],SI2017[[13]],SI2019[[13]])
names(w) <- c("1990","1999", "2017", "2019")
plot(w)
