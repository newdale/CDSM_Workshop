############################################################################################
############################################################################################
###########             INTRODUCTION TO R FOR DIGITAL SOIL MAPPING               ###########
###########               CANADIAN DIGITAL SOIL MAPPING WORKSHOP                 ###########
###########                                                                      ###########
############################################################################################
############################################################################################

###################################################
### DEMO 1: INSTALLING & LOADING LIBRARIES IN R ###
###################################################
# R packages contain all the functions needed to carry out day-to-day activities of a soil mapper
# Best place to get information on the packages is to google the package name
# There should be supporting documentation that describes the use of each function within the package
# Using the Plot Viewer & File Manager Window, you can search and install packages

#install all required packages
install.packages("caret")
install.packages("clhs")
install.packages("Cubist")
install.packages("doParallel")
install.packages("ggplot2")
install.packages("randomForest")
install.packages("terra")
install.packages("RColorBrewer")
install.packages("RSAGA")
install.packages("sf")
install.packages("snowfall")
install.packages("tmap")
install.packages("devtools")
install.packages("usethis")
install.packages("corrplot")
library(devtools)

# whitebox let's use use WhiteboxTools in R
install.packages("whitebox")
# once the package is installed, we need to also install WhiteboxTools binaries
whitebox::install_whitebox()

#ithir requires manual installation and some dependencies
install.packages("aqp")
install.packages("matrixStats")
#install.packages("C:/CDSM/R Packages & Documentations/Packages/ithir_1.0.tar.gz", repos = NULL, type = "source")
devtools::install_bitbucket("brendo1001/ithir/pkg")
devtools::install_github("newdale/onsoilsurvey")

library(terra)
library(sf)

# If you are ever unsure about what the functions or packages in R are doing, you can use ?function to get the documentation

?sf              # shows what the sp package does
?terra          # shows what the rast() function does

##############################################################
### DEMO 2: SETTING UP YOUR WORKING DRIVE & ACCESSING DATA ###
##############################################################
# All of your data will be analyzed through, and produced in your working drive
# It is really important to remember that almost everything in R is processed by the MEMORY
# So if you are processing large amounts of data, you might need to upgrade the amount of RAM in your computer
# For example, if you are working with a 1GB datatable, it will require roughly 1GB of RAM to manage that data
# Of course, there are some exceptions to this (e.g. Raster Package), where the package will run things off hard drive

setwd("C:/CDSM_Workshop/2_RCode")

# Sometimes you will access some data in a SUB-DIRECTORY of the WORKING DRIVE
# "Data/" means that we will be accessing a .csv file from the Data subdirectory
# We will be creating an R object based off the .csv file

MyData <- read.csv('Data/Analytical.csv')         

# The View() function will allow us to take a look inside the datatable

View(MyData)

# Sometimes you may want to create a new directory for your outputs.

dir.create("Outputs")

######################################
### DEMO 3: BASIC DATA EXPLORATION ###
######################################
# The data is a Soil Layer File with soil attribute data on a horizon-by-horizon basis

# Let's calculate the average Sand % of the field observations
# We need to specify which column we are averaging using the MyDATA$xxx. 

mean(MyData$S)

# We can also perform some basic calculations.
# Let's add up the Sand, Silt, and Clay % to make sure they all add up to 100% for each point
# By specifying a new coloumn, the added values will be put into MyData$Texture_Sum

MyData$Texture_Sum <- MyData$S + MyData$SI + MyData$C

# R can also be used to make some simple plots for data visualization.
# Let's make a set of boxplots that show OC by horizon

?boxplot

boxplot(OC~HORIZONID, data=MyData) # The formula shows OC as a function (~) of Horizon.

# We can then write the modified data into a new csv file

write.csv(MyData, "./Outputs/NewData.csv")

# Often times, we need to clean up our workspace. To remove an R object, use the rm() function.
# Remember, each R object is committed to memory...

rm(MyData)

