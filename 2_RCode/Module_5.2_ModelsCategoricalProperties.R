############################################################################################
############################################################################################
###########                 MODELS FOR CATEGORICAL SOIL PROPERTIES               ###########
###########               CANADIAN DIGITAL SOIL MAPPING WORKSHOP                 ###########
###########                                                                      ###########
############################################################################################
############################################################################################

# set working directory - need to set this to wd once in the room where the data will be for users to access
wd<- setwd("C:/CDSM_Workshop/2_RCode")
setwd(wd)

### STEP 1: Load Data ###

# Load point data as a shapefile
library(terra)
Training_Points <- vect("Data/Categorical/LFV_Great_Groups_Training_Points.shp")

# Load covariates as a stack. GeoTIFF file format is the best thing to use.
Covariates <- list.files(path= "Data/Categorical/Covariates/", pattern= "\\.tif$", full.names = TRUE)      
Covariates <- rast(Covariates)

# Set projection system (see https://spatialreference.org/ for codes)
crs(Training_Points) <- "epsg:3005"
crs(Covariates) <- "epsg:3005"


### STEP 2: Create Training Data ###

# Extract covariate values to points

Training_Data<- extract(Covariates,Training_Points)
Training_Points<- cbind(as.data.frame(Training_Points),Training_Data)
rm(Training_Data)

# remove points with NA values
Training_Points <- Training_Points[complete.cases(Training_Points),]
Training_Points$G_Group<-as.factor(Training_Points$G_Group)

### STEP 3: Train Random Forest Algorithm ###

library(caret)

modelLookup(model="rf")

# we must select validation technique and paramaterize/tune the models
# in this case we have decided to use Repeated Cross Validation with 10-fold CV repeated 2 times
fitControl <- trainControl(
  method = "repeatedcv", number=10, repeats=2, allowParallel = TRUE, 
  returnResamp = "all", savePredictions = TRUE 
)

# the random forest tuning parameter is mtry
#mtry=c(2, 4, 6, 8, 10, 12, 14, 16, 17)  # 16 works well for this dataset
mtry=16
RF_tune <- data.frame(mtry=mtry)

ML_Equation <- as.formula(paste("G_Group ~", paste(names(Covariates), collapse="+")))   #Creates an equation that will be fed into the train function

# enable parallel processing (only for windows)
library(doParallel)
library(foreach)

cores<- detectCores()/2
cl<- makePSOCKcluster(cores)
registerDoParallel(cl) # set number of virtual cores - don't use too many if your computer can't handle it (3-5 would be ok)

# Train the Random Forest model using caret
RF <- train(
  data = Training_Points,
  ML_Equation,
  method = "rf",                  #Model of Interest - rf=randomForest package
  tuneGrid = RF_tune,
  ntree = 500,
  trControl = fitControl)

stopCluster(cl)

# Output confusion matrix
Confusion <- (confusionMatrix(RF))
write.csv(Confusion$table, "./Outputs/Confusion Matrix.csv")

### STEP 4: Predict Target Variable for the Study Extent ###

# Generate Map Legend
legend<- data.frame(levels(Training_Points$G_Group))
colnames(legend)[1] <- "Class"
legend$Code <- seq.int(nrow(legend))
legend<- legend[,c(2,1)]
write.csv(legend, "./Outputs/Map Legend.csv")

# Generate Hardened Map
GGroup<- writeRaster(predict(object= Covariates, 
                             model=RF,
                             na.rm=TRUE), 
                     filename="Outputs/LFV_Great_Group.tif", 
                     filetype="GTiff", 
                     overwrite=TRUE)

# We can visualize the map by converting it to a factor map and creating a color palette
library(RColorBrewer)
GGroup<- as.factor(GGroup)
levels(GGroup)<- legend
region<- colorRampPalette(brewer.pal(12, "Accent"))(15)
plot(GGroup, col=region)


# Generate Class Probabilities (only for Random Forest)
dir.create("Outputs/Class Probability")

writeRaster(predict(object= Covariates, 
                    model= RF, 
                    index=1:nlevels(Training_Points$G_Group), 
                    type='prob',
                    na.rm=TRUE), 
            filename=paste("Outputs/Class Probability/GGroup_",levels(Training_Points$G_Group),".tif", sep=""), 
            filetype="GTiff", 
            overwrite=TRUE)

### STEP 5: Produce Uncertainty ###

probabilities <- list.files(path="Outputs/Class Probability" ,pattern= "\\.tif$", full.names = TRUE)      # http://stackoverflow.com/questions/4876813/using-r-to-list-all-files-with-a-specified-extension
probabilities <- rast(probabilities)
names(probabilities)<- legend$Class
names(probabilities)
plot(probabilities, 1:4)
plot(probabilities,7:10)

# These lines will calculate entropy uncertainty. It ranges from 0(high certainty) to 1 (low certainty)
n <- nlyr(probabilities)
f1 <- function(x){x*log(x+0.000001)}
probabilities <- app(probabilities, fun=f1, cores=cores)
entropy <- app(probabilities, fun=sum, cores=cores)
entropy <- -1*entropy/log(n)
entropy[entropy<0] <- 0
plot(entropy)

# Output Entropy Uncerainty Map
writeRaster(entropy, 
            filename="Outputs/Class Probability/Entropy Uncertainty.tif", 
            filetype="GTiff", overwrite=TRUE)

### STEP 6: Post-Hoc Analysis of Model ###

# Produce a  variable importance plot
plot(varImp(RF))
