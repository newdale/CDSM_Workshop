############################################################################################
############################################################################################
###########                   COVARIATE REDUCTION STRATEGIES                     ###########
###########               CANADIAN DIGITAL SOIL MAPPING WORKSHOP                 ###########
###########                                                                      ###########
############################################################################################
############################################################################################


# FIRST STEP IS TO CALL IN THE LIBRARIES REQUIRED TO PROCESS THE DATA
library(terra)
library(onsoilsurvey)
library(fmsb)
library(corrplot)

# SECOND, SET THE WORKING DIRECTORY AND CREATE AN OBJECT THAT IS THE PATH FOR THE LOCATION OF THE DEM
setwd('C:/CDSM/3_EnvironmentalCovariates/')

# NOW WE NEED TO CREATE A RASTER STACK WITH OUR COVARIATES THAT WE JUST GENERATED
covs<- rast(list.files(path="Data/VIF", pattern=".tif", full.names = TRUE))

# AND WE WANT TO CONVERT THE COVARIATES TO A DATA FRAME
covs_df<- as.data.frame(covs)

# WE CAN GENERATE A CORRELATION MATRIX TO VISUALIZE CORRELATION IN COVARIATES
corrplot(cor(covs_df, use="pairwise.complete.obs"), type="lower")


#############################################
# VARIANCE INFLATION FACTOR (VIF)

vif_results<- oss.seqVIF(cov_df = covs_df ,thresh=5, trace=T, show.R2.vals=T)

# and we can now subset the covariate stack and write them to a new folder
vif_layers <- subset(covs, vif_results[[1]])
names(vif_layers)


#writeRaster(vif_layers, 
#            filename = paste0("C:/CDSM/5_Machine Learning Models/5a_Continuous Soil Properties/Data/Covariates/", 
#                              names(vif_layers), '.tif'), 
#            filetype="GTiff", 
#            overwrite=TRUE)




