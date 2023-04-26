############################################################################################
############################################################################################
###########     CREATING DEM DERIVATIVES USING RSAGA AND WHITEBOX TOOLS          ###########
###########               CANADIAN DIGITAL SOIL MAPPING WORKSHOP                 ###########
###########                                                                      ###########
############################################################################################
############################################################################################


# FIRST STEP IS TO CALL IN THE LIBRARIES REQUIRED TO PROCESS THE DATA
library(RSAGA)
library(terra)
library(whitebox)


# SECOND, SET THE WORKING DIRECTORY AND CREATE AN OBJECT THAT IS THE PATH FOR THE LOCATION OF THE DEM
setwd('C:/CDSM/3_EnvironmentalCovariates/')

# And set an object to the value of the working directory
wd=setwd('C:/CDSM/3_EnvironmentalCovariates/')

# Paste the name of the working directory and the location of the DEM to be called later
DEM = paste(wd,'/Data/DEM.tif',sep='')

# create directories to store outputs
dir.create("Data/SGRDS")
dir.create("Data/TIFFS")


# WE CAN LOOK AT HOW RSAGA IS STRUCTURED INTO LIBRARIES AND MODULES
rsaga.get.libraries()
rsaga.get.modules('ta_compound')
rsaga.get.usage('ta_compound',0)
rsaga.get.modules('ta_morphometry')
rsaga.get.usage('ta_morphometry',0)



# import DEM to SAGA by converting from TIFF to SGRD format
rsaga.import.gdal(DEM, 'Data/SGRDS/DEM.sgrd')


##################### Geoprocessing #######################

# we create an object that holds the RSAGA environment variables that we call in when running all the tools
work_env <- rsaga.env()

# Run the Basic Terrain Analysis Compound Tool
rsaga.geoprocessor('ta_compound', 0, env = work_env, param = list(ELEVATION = 'Data/SGRDS/DEM.sgrd', 
                                                                  SHADE = 'Data/SGRDS/hillshade',
                                                                  SLOPE = "Data/SGRDS/sloperad",
                                                                  ASPECT = 'Data/SGRDS/aspect',
                                                                  HCURV = 'Data/SGRDS/plancurv', 
                                                                  VCURV = 'Data/SGRDS/profcurv',
                                                                  CONVERGENCE = 'Data/SGRDS/conv',
                                                                  #SINKS = 'Data/SGRDS/SINKS',
                                                                  FLOW = 'Data/SGRDS/catchment',
                                                                  WETNESS = 'Data/SGRDS/TWI',
                                                                  LSFACTOR = 'Data/SGRDS/LS',
                                                                  #CHANNELS = 'Data/SGRDS/channels',
                                                                  #BASINS = 'Data/SGRDS/basins',
                                                                  #CHNL_BASE = 'Data/SGRDS/CNBL',
                                                                  #CHNL_DIST = 'Data/SGRDS/CNDIST',
                                                                  VALL_DEPTH = 'Data/SGRDS/vdepth',
                                                                  RSP = 'Data/SGRDS/RSP',
                                                                  THRESHOLD = 5))


# Run Topographic Position Index
rsaga.geoprocessor('ta_morphometry', 18, env = work_env, param = list(DEM = 'Data/SGRDS/DEM.sgrd', 
                                                                      TPI = 'Data/SGRDS/TPI'))

# Create catchment areas
rsaga.geoprocessor('garden_learn_to_program', 7, env = work_env, param = list(ELEVATION = 'Data/SGRDS/DEM.sgrd',
                                                                            AREA = 'Data/SGRDS/CATCHMENT',
                                                                            METHOD = 1))

# Run Topographic Wetness Index
rsaga.geoprocessor('ta_hydrology', 20, env = work_env, param = list(SLOPE = 'Data/SGRDS/sloperad.sgrd', 
                                                                    AREA = 'Data/SGRDS/CATCHMENT.sgrd',
                                                                    TWI = 'Data/SGRDS/TWI',
                                                                    CONV = 0,
                                                                    METHOD = 0))


######## Convert sgrds to tiff #############

# CREATE A RASTER STACK OF THE COVARIATES WE JUST CREATED
files <- list.files(path="Data/SGRDS", pattern="*.sdat$", full.names=T, recursive=FALSE)
sgrd_stack <-rast(files)
sgrd_stack
names(sgrd_stack)
crs(sgrd_stack)

# WE CAN VISUALIZE SOME OF THESE LAYERS IN THE RASTER STACK
plot(sgrd_stack,'sloperad')
plot(sgrd_stack,'TWI')
plot(sgrd_stack, 10)


# WRITE THE FILES IN THE RASTER STACK AS GEOTIFFS AND SAVE THEM TO THE TIFF FOLDER
outName = paste("Data/TIFFS/", names(sgrd_stack), '.tif', sep = "")
outName
writeRaster(sgrd_stack, filename = outName, filetype="GTiff", overwrite=TRUE)

# SINCE RSAGA SAVES THE SGRDS TO THE COMPUTER, WE CAN NOW DELETE THEM SINCE THE TIFFS ARE CREATED
unlink('Data/SGRDS',recursive = TRUE)



##############################################################
# Another great library for generating covariates is WhiteboxTools
# Let's try a couple

# elevation percentile using 11 x 11 window
print(wbt_tool_help("ElevPercentile"))
wbt_elev_percentile(dem = DEM, output = paste0("./Data/TIFFS/ep11.tif"), filterx=11, filtery=11, sig_digits = 2)


# deviation from mean elevation using 11 x 11 window
print(wbt_tool_help("DevFromMeanElev"))
wbt_dev_from_mean_elev(dem = DEM, output = paste0("./Data/TIFFS/deme11.tif"), filterx=11, filtery=11)

# Maximum Elevation Deviation should be run 3 times at various filter ranges: 3 to f2, f2 to f3 and f3 to f4
print(wbt_tool_help("MaxElevationDeviation"))

# check the raster width and height
ncol(sgrd_stack); nrow(sgrd_stack)

# we can run maximum elevation deviation at 3 different "scales"
wbt_max_elevation_deviation(dem= DEM, out_mag= paste0("./Data/TIFFS/med33.tif"),
                            out_scale= paste0("./Data/TIFFS/meds33.tif"), min_scale= 3, max_scale= 33, step= 2)

wbt_max_elevation_deviation(dem= DEM, out_mag= paste0("./Data/TIFFS/med99.tif"),
                            out_scale= paste0("./Data/TIFFS/meds99.tif"), min_scale= 33, max_scale= 99, step= 3)

wbt_max_elevation_deviation(dem= DEM, out_mag= paste0("./Data/TIFFS/med400.tif"),
                            out_scale= paste0("./Data/TIFFS/meds400.tif"), min_scale= 99, max_scale=399, step= 5)


# we can have one more look at the new covariates
files <- rast(list.files(path="Data/TIFFS", pattern="*.tif$", full.names=T, recursive=FALSE))


plot(files,"med33")
plot(files,"med99")
plot(files,"med400")
plot(files,c("med33","med99",'med400'))


#######################################################################################################################
# Calculate northness and eastness from the aspect data
# we cannot use aspect directly as a covariate, it is circular
#######################################################################################################################
asp<- rast("Data/TIFFS/aspect.tif")
eastness<- cos(asp)
northness<- sin(asp)
names(eastness)<- "eastness"
names(northness)<- "northness"
writeRaster(eastness,filename = "Data/TIFFS/northness.tif", filetype="GTiff", overwrite=TRUE)
writeRaster(northness,filename = "Data/TIFFS/eastness.tif", filetype="GTiff", overwrite=TRUE)
rm(asp)
file.remove('Data/TIFFS/aspect.tif')

# END
