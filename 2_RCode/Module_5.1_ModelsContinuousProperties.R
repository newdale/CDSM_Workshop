############################################################################################
############################################################################################
###########                 MODELS FOR CONTINUOUS SOIL PROPERTIES                ###########
###########               CANADIAN DIGITAL SOIL MAPPING WORKSHOP                 ###########
###########                                                                      ###########
############################################################################################
############################################################################################


################################################################################################################
################################################################################################################
#                                        Prepare the Data for Modelling                                        #
################################################################################################################
################################################################################################################
library(ithir)
library(terra)
library(onsoilsurvey)

# set working directory - need to set this to wd once in the room where the data will be for users to access
wd<- setwd("C:/CDSM_Workshop/2_RCode")
setwd(wd)

# let's call in the analytical data table
MyData<- read.csv('Data/Analytical.csv')
str(MyData)

### here we set the variable being predicted to an object
v<- "OC"

# Run the equal area spline to harmonize the soil depth interval for prediction
# ea_spline requires the first 3 columns of data to be the site identifier, upper and lower
spline <- ea_spline(MyData, var.name= v,d = t(c(0,5,15,30,60,100)),lam = 0.1, vlow=0.01, vhigh = 100, show.progress=FALSE)
str(spline)

# let's look at the harmonised data table
View(spline$harmonised)

# we can pull just the harmonized table and name it to an object for further work
FieldData <- spline$harmonised

# we now need to attach coordinates for our sites
Coords <- unique.data.frame(MyData[,c("SITE_ID","x","y")])
str(Coords)
FieldData$x<- Coords[match(Coords$SITE_ID,FieldData$id),"x"]
FieldData$y<- Coords[match(Coords$SITE_ID,FieldData$id),"y"]
str(FieldData)

# i don't like the column headers assigned by the EA Spline, so here I change them fpr columns 2 to 6
colnames(FieldData)[2] <- paste0(v,"005")
colnames(FieldData)[3] <- paste0(v,"015")
colnames(FieldData)[4] <- paste0(v,"030")
colnames(FieldData)[5] <- paste0(v,"060")
colnames(FieldData)[6] <- paste0(v,"100")
str(FieldData)

# now I can assign the x and y coordinates, and the coordinate reference system (crs)
FieldData<- vect(FieldData, geom=c("x","y"), crs="epsg:26917")
crs(FieldData)

# Create a rasterstack that will be used to intersect with sample points from field work to build models

grid.name.list <- list.files(path="Data/Covariates", pattern = "tif$", full.names = TRUE) 
grid20<-rast(grid.name.list)
grid20
crs(grid20)
names(grid20)
nlyr(grid20)

# let us call back the list of VIF-selected covariates from Module 3.2 to subset the covariates
vif_layers<- read.csv("Outputs/vif_layers.csv")
grid20<- subset(grid20, vif_layers[,1])
names(grid20)
nlyr(grid20)

# now we can plot a raster layer from the stack and the sample points to make sure they are aligned as we expect
plot(grid20,3)
plot(FieldData,add=T)

# Extract the covariate values from the Covariate RasterStack based on the coordinates of the sample locations,
Model_data <- terra::extract(grid20, FieldData, method = "simple")
Model_data <- cbind(as.data.frame(FieldData),Model_data)
str(Model_data)


################################################################################################################
################################################################################################################
#                                        Train the predictive Models
################################################################################################################
################################################################################################################

library(caret)

# the caret package interfaces with numerous models, and we can look this up
list_of_models<-modelLookup()
head(list_of_models)
nrow(list_of_models)

# we can look at tuning parameters for models
# and can find these by:
modelLookup(model="cubist")
modelLookup(model="rf")

# we can take advantage of parallel processing for running the caret model training
library(doParallel)
library(foreach)

cores<- detectCores()/2
registerDoParallel(cores=cores)

# we must select validation technique and paramaterize/tune the models
# in this case we have decided to use Repeated Cross Validation with 10-fold CV repeated 2 times

fitControl <- trainControl(
  method = "repeatedcv", number=5, repeats=2, allowParallel = TRUE, 
  returnResamp = "all", savePredictions = TRUE)


# Cubist tuning parameters are committees and neighbors, here we create a matrix of combinations to try
Cubist_tune <- expand.grid(committees = c(1,10,50,100), neighbors = c(0,1,5,9))
Cubist_tune

# the random forest tuning parameter is mtry
mtry=c(seq(2,10, by = 2))
RF_tune <- data.frame(mtry=mtry)
RF_tune

# we can generate a model equation to use in the model training below
ML_Equation<- as.formula(paste(names(Model_data)[2], "~", paste(names(grid20), collapse="+"),sep=""))
ML_Equation

# here we use set seed so that everyone gets the same answer, not required unless you want to
# make the results reproducible
# below set seed is the model training for Cubist where we must save the output of the model to an object
# and specify in the train function the independent variables or predictors, the dependent variable, tuning grid and
# the training control we created above
set.seed(17)
CU005 <- train(data=Model_data,
               ML_Equation,
               method = "cubist",
               tuneGrid = Cubist_tune,
               trControl = fitControl)

# calling the model shows us the cross-validation results
CU005

# provides the goodness of fit for model training and standard deviations since we ran repeated CV
# this can provide information about model stability
CU005$results

# we can use the getCCC function from onsoilsurvey to see the concordance
oss.getCCC(CU005)

# summary provides a printout of the model, rules and outcomes for each node of the tree
summary(CU005)

# we can also look at variable importance
varImp(CU005)
plot(varImp(CU005))

# same as above, we use set seed for reproducibility
# notice that caret is quite useful because the syntax/code is the same
# except the method and tune grid
set.seed(123)
RF005 <- train(data=Model_data,
               ML_Equation,
               method = "rf", 
               importance=TRUE, # to get variable importance from RF we need to add this argument
               tuneGrid = RF_tune,
               trControl = fitControl)

# calling the model shows us the cross-validation results
RF005

# provides the goodness of fit for model training and standard deviations since we ran repeated CV
# this can provide information about model stability
RF005$results

# we can use the getCCC function from onsoilsurvey to see the concordance
oss.getCCC(RF005)

# we can also look at variable importance
varImp(RF005)
plot(varImp(RF005))


################################################################################################################
################################################################################################################
#                                   Apply the Models to the study area
################################################################################################################
################################################################################################################

# now we can apply our models and create the predicted maps
# this can be done directly to raster

# Applying the models directly to raster
# This writes the raster directly to the hard drive 
# It also assigns them to objects in the environment variables for easy plotting

OC005_cu<- predict(object= grid20,
                   model= CU005,
                   na.rm=TRUE,
                   cores=1,
                   filename="Outputs/OC005_cu.tif",
                   filetype="GTiff",
                   overwrite=TRUE)

OC005_rf<- predict(object= grid20,
                   model= RF005,
                   na.rm=TRUE,
                   cores=1,
                   filename="Outputs/OC005_rf.tif",
                   filetype="GTiff",
                   overwrite=TRUE)

################################################################################################################
################################################################################################################
#                                        Plotting the output maps
################################################################################################################
################################################################################################################


# we can plot these individually, but they are hard to compare since the ranges are different
plot(OC005_cu, main="Soil Organic Carbon (0-5cm) - Cubist")
plot(OC005_rf, main="Soil Organic Carbon (0-5cm) - Random Forest")

# we can visualize the maps side-by-side with the same scale using tmap package
library(tmap)
library(sf)

OCA<- tm_shape(OC005_cu) +
      tm_raster(breaks=c(0,2,4,6,8,10,15,17,25,30,45,60),palette="YlOrBr",legend.show=TRUE) +
      tm_layout(title="Cubist", legend.position = c("right", "bottom"))
OCB<- tm_shape(OC005_rf) +
      tm_raster(breaks=c(0,2,4,6,8,10,15,17,25,30,45,60),palette="YlOrBr",legend.show=TRUE) +
      tm_layout(title="RF", legend.position = c("right", "bottom"))

tmap_arrange(OCA,OCB,ncol=2,nrow=1)



################################################################################################################
################################################################################################################
#                                        Recursive Feature Elimination
################################################################################################################
################################################################################################################

# identifies covariates that are not contributing to the model
# relationship between independent variable and predictors

# we can set up control file for Recursive Feature Elimination in caret
# here we are using caret's built in rfFuncs, which uses Random Forest
control<- rfeControl(functions=rfFuncs, method="repeatedcv", number=5, repeats=2, allowParallel = TRUE) 

# we will just run this with the Random Forest model
rfe_rf<- rfe(data=Model_data,
              ML_Equation,
              sizes=c(1:27),
              rfeControl=control)

# we can plot the RMSE as a function of variables
plot(rfe_rf, type=c("g", "o"))

# this will tell us which covariates to retain
rfe_rf

# now we can create a new equation for the model with selected covariates
ML_opt<- as.formula(as.formula(paste(names(Model_data)[2], "~", paste(rfe_rf$optVariables, collapse = "+"))))

# and now we can train a new model
set.seed(22)
RF005Opt <- train(data=Model_data,
                  ML_opt,
                  method = "rf", 
                  importance=TRUE, # to get variable importance from RF we need to add this argument
                  tuneGrid = RF_tune,
                  trControl = fitControl)

# how did we do compared to using all covariates?
oss.getCCC(RF005)$ccc_optimal_model
oss.getCCC(RF005Opt)$ccc_optimal_model

# this improved the model quite a bit
# RMSE from      8.6  -> 7.9
# Rsquared       0.47 -> 0.56
# CCC            0.54 -> 0.63


# END