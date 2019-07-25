############################################################################################################
# -*- coding: utf-8 -*-
#Created on Wed Jul 25 12:55:41 2019
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

ndvi2019 <- raster("D:/Chinmay/Pune/test_del_Soon/06_23_2019/NDVI_2019.tif")