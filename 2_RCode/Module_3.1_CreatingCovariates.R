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


# SECOND, CREATE AN OBJECT THAT IS THE PATH TO THE WORKING DIRECTORY AND SET THE WORKING DIRECTORY
wd<- setwd("C:/CDSM_Workshop/2_RCode")
setwd(wd)

# Paste the name of the working directory and the location of the DEM to be called later
DEM = paste(wd,'/Data/DEM.tif',sep='')

# create directories to store outputs
dir.create("Outputs/SGRDS")
dir.create("Outputs/TIFFS")


# WE CAN LOOK AT HOW RSAGA IS STRUCTURED INTO LIBRARIES AND MODULES
rsaga.get.libraries()
rsaga.get.modules('ta_compound')
rsaga.get.usage('ta_compound',0)
rsaga.get.modules('ta_morphometry')
rsaga.get.usage('ta_morphometry',0)



# import DEM to SAGA by converting from TIFF to SGRD format
rsaga.import.gdal(DEM, 'Outputs/SGRDS/DEM.sgrd')


##################### Geoprocessing #######################

# we create an object that holds the RSAGA environment variables that we call in when running all the tools
work_env <- rsaga.env()

# Run the Basic Terrain Analysis Compound Tool
rsaga.geoprocessor('ta_compound', 0, env = work_env, param = list(ELEVATION = 'Outputs/SGRDS/DEM.sgrd', 
                                                                  SHADE = 'Outputs/SGRDS/hillshade',
                                                                  SLOPE = "Outputs/SGRDS/sloperad",
                                                                  ASPECT = 'Outputs/SGRDS/aspect',
                                                                  HCURV = 'Outputs/SGRDS/plancurv', 
                                                                  VCURV = 'Outputs/SGRDS/profcurv',
                                                                  CONVERGENCE = 'Outputs/SGRDS/conv',
                                                                  #SINKS = 'Outputs/SGRDS/SINKS',
                                                                  FLOW = 'Outputs/SGRDS/catchment',
                                                                  WETNESS = 'Outputs/SGRDS/TWI',
                                                                  LSFACTOR = 'Outputs/SGRDS/LS',
                                                                  #CHANNELS = 'Outputs/SGRDS/channels',
                                                                  #BASINS = 'Outputs/SGRDS/basins',
                                                                  #CHNL_BASE = 'Outputs/SGRDS/CNBL',
                                                                  #CHNL_DIST = 'Outputs/SGRDS/CNDIST',
                                                                  VALL_DEPTH = 'Outputs/SGRDS/vdepth',
                                                                  RSP = 'Outputs/SGRDS/RSP',
                                                                  THRESHOLD = 5))


# Run Topographic Position Index
rsaga.geoprocessor('ta_morphometry', 18, env = work_env, param = list(DEM = 'Outputs/SGRDS/DEM.sgrd', 
                                                                      TPI = 'Outputs/SGRDS/TPI'))

# Create catchment areas
rsaga.geoprocessor('garden_learn_to_program', 7, env = work_env, param = list(ELEVATION = 'Outputs/SGRDS/DEM.sgrd',
                                                                            AREA = 'Outputs/SGRDS/CATCHMENT',
                                                                            METHOD = 1))

# Run Topographic Wetness Index
rsaga.geoprocessor('ta_hydrology', 20, env = work_env, param = list(SLOPE = 'Outputs/SGRDS/sloperad.sgrd', 
                                                                    AREA = 'Outputs/SGRDS/CATCHMENT.sgrd',
                                                                    TWI = 'Outputs/SGRDS/TWI',
                                                                    CONV = 0,
                                                                    METHOD = 0))


######## Convert sgrds to tiff #############

# CREATE A RASTER STACK (SpatRaster) OF THE COVARIATES WE JUST CREATED
files <- list.files(path="Outputs/SGRDS", pattern="*.sdat$", full.names=T, recursive=FALSE)
sgrd_stack <- rast(files)
sgrd_stack
names(sgrd_stack)
crs(sgrd_stack)

# WE CAN VISUALIZE SOME OF THESE LAYERS IN THE RASTER STACK
plot(sgrd_stack,'sloperad')
plot(sgrd_stack,'TWI')
plot(sgrd_stack, 10)


# WRITE THE FILES IN THE RASTER STACK AS GEOTIFFS AND SAVE THEM TO THE TIFF FOLDER
outName = paste("Outputs/TIFFS/", names(sgrd_stack), '.tif', sep = "")
outName
writeRaster(sgrd_stack, filename = outName, filetype="GTiff", overwrite=TRUE)

# SINCE RSAGA SAVES THE SGRDS TO THE COMPUTER, WE CAN NOW DELETE THEM SINCE THE TIFFS ARE CREATED
unlink('Outputs/SGRDS',recursive = TRUE)



##############################################################
# Another great library for generating covariates is WhiteboxTools
# Let's try a couple

# elevation percentile using 11 x 11 window
print(wbt_tool_help("ElevPercentile"))
wbt_elev_percentile(dem = DEM, output = paste0("./Outputs/TIFFS/ep11.tif"), filterx=11, filtery=11, sig_digits = 2)


# deviation from mean elevation using 11 x 11 window
print(wbt_tool_help("DevFromMeanElev"))
wbt_dev_from_mean_elev(dem = DEM, output = paste0("./Outputs/TIFFS/deme11.tif"), filterx=11, filtery=11)

# Maximum Elevation Deviation should be run 3 times at various filter ranges: 3 to f2, f2 to f3 and f3 to f4
print(wbt_tool_help("MaxElevationDeviation"))

# check the raster width and height
ncol(sgrd_stack); nrow(sgrd_stack)

# we can run maximum elevation deviation at 3 different "scales"
wbt_max_elevation_deviation(dem= DEM, out_mag= paste0("./Outputs/TIFFS/med33.tif"),
                            out_scale= paste0("./Outputs/TIFFS/meds33.tif"), min_scale= 3, max_scale= 33, step= 2)

wbt_max_elevation_deviation(dem= DEM, out_mag= paste0("./Outputs/TIFFS/med99.tif"),
                            out_scale= paste0("./Outputs/TIFFS/meds99.tif"), min_scale= 33, max_scale= 99, step= 3)

wbt_max_elevation_deviation(dem= DEM, out_mag= paste0("./Outputs/TIFFS/med400.tif"),
                            out_scale= paste0("./Outputs/TIFFS/meds400.tif"), min_scale= 99, max_scale=399, step= 5)


# we can have one more look at the new covariates
files <- rast(list.files(path="Outputs/TIFFS", pattern="*.tif$", full.names=T, recursive=FALSE))
names(files)

plot(files,"med33")
plot(files,"med99")
plot(files,"med400")
plot(files,c("med33","med99",'med400'))


#######################################################################################################################
# Calculate northness and eastness from the aspect data
# we cannot use aspect directly as a covariate, it is circular
#######################################################################################################################
asp<- rast("Outputs/TIFFS/aspect.tif")
eastness<- cos(asp)
northness<- sin(asp)
names(eastness)<- "eastness"
names(northness)<- "northness"
writeRaster(eastness,filename = "Outputs/TIFFS/northness.tif", filetype="GTiff", overwrite=TRUE)
writeRaster(northness,filename = "Outputs/TIFFS/eastness.tif", filetype="GTiff", overwrite=TRUE)
rm(asp)
file.remove('Outputs/TIFFS/aspect.tif')

# END
