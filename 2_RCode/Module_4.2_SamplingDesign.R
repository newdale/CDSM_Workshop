############################################################################################
############################################################################################
###########                 TECHNIQUES FOR SAMPLING DESIGN FOR DSM               ###########
###########               CANADIAN DIGITAL SOIL MAPPING WORKSHOP                 ###########
###########                                                                      ###########
############################################################################################
############################################################################################


################################################################################################################
################################################################################################################
#                                        Prepare the Data for Sample Design                                    #
################################################################################################################
################################################################################################################

# start by setting a working directory
wd<- setwd("C:/CDSM_Workshop/2_RCode")
setwd(wd)

# and load the libraries we will need
library(terra)
library(onsoilsurvey)

# We will start by bringing in some environmental covariates
# Create a rasterstack that will be used for creating sampling designs
grid.name.list <- list.files(path="Data/Covariates/", pattern = "tif$", full.names = TRUE) 
grid20<-rast(grid.name.list)
grid20
crs(grid20)
names(grid20)

#let's add a classified crop raster
crop2016<- rast("Data/crop2016.tif")
grid20<- c(grid20,crop2016)
names(grid20)


################################################################################################################
################################################################################################################
#                                                    Sampling Designs                                          #
################################################################################################################
################################################################################################################


# 1. Simple Random Sampling
# all samples have equal probability of being selected
# samples selected only guaranteed equal n for rectangular or square study areas
SRS<- spatSample(x= grid20, size=50, method="random", as.points=TRUE, na.rm=TRUE)

# now we can visualize
plot(grid20,'DEM')
points(SRS,pch=19)
nrow(geom(SRS))

# and finally export the sampling plan as required
write.csv(SRS,"Outputs/01SRS.csv")
writeVector(SRS, filename="Outputs/01SRS.shp", overwrite=TRUE)

########################################################################################################################
#2. Stratified Random Sampling - Equal Sampling within Strata

# we will use crop 2016 as the strata, therefore we need to specify this
unique(grid20[['crop2016']])

# assign the classes to an oject and then drop the NA because we do not need it
crop2016<- unique(grid20[['crop2016']])
crop2016<- crop2016[c(1:9),]

StRS<- spatSample(x= grid20[['crop2016']], size=6, method="stratified", as.points=TRUE, na.rm=TRUE)

# now we can visualize
plot(grid20,"crop2016",legend=FALSE)
points(StRS,pch=19)
nrow(geom(StRS))

# but this leaves with only the point locations, we want the covariate values as well
StRS<- extract(grid20,StRS, method="simple", xy=TRUE)
StRS<- vect(StRS, geom=c("x","y"), crs=crs(grid20))

# and finally export the sampling plan as required
write.csv(StRS,"Outputs/02StRS.csv")
writeVector(StRS, filename = "Outputs/02StRS.shp")
table(StRS$crop2016)

########################################################################################################################
#3. Stratified Random Sampling - Proportional Sampling within Strata

#compute stratum sample sizes for proportional allocation
Nh<- freq(grid20[['crop2016']])[,3]
wh <- Nh/sum(Nh)
sum(Nh)
sum(wh)

# we can look at the class weighths
rec<- cbind(crop2016,wh)
rec

# and now we do the sampling
StRS2<- spatSample(x= grid20[['crop2016']], size=50, method="weights", as.points=TRUE, na.rm=TRUE)

# but this leaves with only the point locations, we want the covariate values as well
StRS2<- extract(grid20,StRS2, method="simple", xy=TRUE)
StRS2<- vect(StRS2, geom=c("x","y"), crs=crs(grid20))

# and finally export the sampling plan as required
write.csv(StRS2,"Outputs/03StRS2.csv")
writeVector(StRS2, filename = "Outputs/03StRS2.shp", overwrite=TRUE)

# now we can visualize
plot(grid20,"crop2016",legend=FALSE)
points(StRS2,pch=19)
nrow(geom(StRS2))
table(StRS2$crop2016)

########################################################################################################################
#4. Grid Sampling

# samples selected only guaranteed equal n for rectangular or square study areas
set.seed(100)
GRID<- spatSample(grid20[[1]], size=50, method="regular", as.points=TRUE, na.rm=TRUE)
nrow(geom(GRID))

#above only gave us 30 because it looks at the full extent of the raster
# so we can increase the sample size
GRID<- spatSample(grid20[[1]], size=100, method="regular", as.points=TRUE, na.rm=TRUE)
nrow(geom(GRID))

# now we can visualize
plot(grid20,"DEM")
points(GRID,pch=19)

# but this leaves with only the point locations, we want the covariate values as well
GRID<- extract(grid20,GRID, method="simple", xy=TRUE)
GRID<- vect(GRID, geom=c("x","y"), crs=crs(grid20))

# and finally export the sampling plan as required
write.csv(GRID,"Outputs/04GRID.csv")
writeVector(GRID, filename = "Outputs/04GRID.shp", overwrite=TRUE)

#######################################################################################################################
#5. Conditioned Latin Hypercube Sampling
library(clhs)

# this works on data frame, Spatial Points Data Frame and Raster
grid_df<- as.data.frame(grid20, xy=TRUE, na.rm=TRUE)

# and we specify crop2016 as factor
grid_df$crop2016<- as.factor(grid_df$crop2016)

lhs<- clhs(grid_df[,c(3:34)], size=50, iter=1000, progress=TRUE, simple=FALSE) # index starting at column 3 to drop the x and y

# we can plot the evolution of the objective function against the iterations
plot(lhs)

# let's grab the sample sites and make them spatial
lhs_plan<- grid_df[lhs$index_samples,]
lhs_plan<- vect(lhs_plan, geom=c("x","y"), crs=crs(grid20))
nrow(geom(lhs_plan))

# now we can visualize
plot(grid20,"DEM")
points(lhs_plan, pch=19)

# and finally export the sampling plan as required
write.csv(lhs_plan,"Outputs/05cLHS.csv")
writeVector(lhs_plan, filename = "Outputs/05cLHS.shp")


########################################################################################################################
#
# we can look at how well each sampling design represents the original distribution of a covariate
# let's look at elevation

# first step here is a bit of data cleaning cause our DEM has a lake and was hydro-flattened
s0<- rbind(data.frame(method = "Original", DEM=grid_df$DEM))
s0<- s0[s0$DEM>187.2600,]

# and we extract from the cLHS object just the spatial points data frame
lhsdata<- as.data.frame(lhs_plan)


# now we bind all the sampling datasets together specifically for the DEM values into a singlke dataframe
s1 <- rbind(data.frame(method = "SRS", DEM=SRS$DEM),
           data.frame(method = "StRS", DEM=StRS$DEM),
           data.frame(method = "StRS2", DEM=StRS2$DEM),
           data.frame(method = "Grid", DEM=GRID$DEM),
           data.frame(method = "cLHS", DEM=lhs_plan$DEM))
           
# using ggplot we can now plot the original covariate DEM values as histogram bars and the sampling distributions as curves
library(ggplot2)
ggplot() + geom_histogram(data=s0, aes(x=DEM,fill="red",y=after_stat(density)),binwidth=.5, alpha=.5, position="identity") + geom_density(data = s1, aes(x = DEM, col = method))



# let's look at sagawi
s00<- rbind(data.frame(method = "Original",TWI=grid_df$TWI))

s2 <- rbind(data.frame(method = "SRS", TWI= SRS$TWI),
            data.frame(method = "StRS", TWI= StRS$TWI),
            data.frame(method = "StRS2", TWI= StRS2$TWI),
            data.frame(method = "Grid", TWI= GRID$TWI),
            data.frame(method = "cLHS", TWI= lhs_plan$TWI)
            
)
ggplot() + geom_histogram(data=s00, aes(x=TWI,fill="red",y=after_stat(density)),binwidth=.5, alpha=.5, position="identity") + geom_density(data = s2, aes(x = TWI, col = method))



# an alternative way is to look at the Cumulative Distribution Functions of the data
s3 <- rbind(s0,s1)


ggplot(s3,aes(x=DEM)) + stat_ecdf(aes(colour=method), linewidth=1) +
  scale_color_manual(breaks=c("cLHS","Grid","Original","SRS","StRS","StRS2"),
                     values=c("blue", "red", "black", "orange", "dark green", "brown"))

# we can also compare the distributions using the Kolmogorov-Smirnov (KS) test
# the null hypothesis for the K-S test is that both samples come from the same distribution
# we reject the null hypothesis when p-value < 0.05

# for the DEM distributions
ks.test(s0$DEM,SRS$DEM)
ks.test(s0$DEM,StRS$DEM)
ks.test(s0$DEM,StRS2$DEM)
ks.test(s0$DEM,GRID$DEM)
ks.test(s0$DEM,lhsdata$DEM)

# END
