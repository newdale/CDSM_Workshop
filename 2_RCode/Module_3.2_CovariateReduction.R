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
wd<- setwd("C:/CDSM_Workshop/2_RCode")
setwd(wd)

# NOW WE NEED TO CREATE A RASTER STACK WITH OUR COVARIATES THAT WE JUST GENERATED
covs<- rast(list.files(path="Data/Covariates", pattern=".tif", full.names = TRUE))

# AND WE WANT TO CONVERT THE COVARIATES TO A DATA FRAME
covs_df<- as.data.frame(covs)

# WE CAN GENERATE A CORRELATION MATRIX TO VISUALIZE CORRELATION IN COVARIATES
corrplot(cor(covs_df, use="pairwise.complete.obs"), type="lower", diag=FALSE)


#############################################
# VARIANCE INFLATION FACTOR (VIF)

vif_results<- oss.seqVIF(cov_df = covs_df ,thresh=5, trace=T, show.R2.vals=T)

# and we can now subset the covariate stack
vif_layers <- subset(covs, vif_results[[1]])
names(vif_layers)

# we can store this list of VIF-selected covariates as a CSV for later
write.csv(vif_results[[1]],"Outputs/vif_layers.csv", row.names = FALSE)
