############################################################################################
############################################################################################
###########       DEPTH FUNCTIONS FOR HARMONIZING PREDICTION INTERVALS           ###########
###########               CANADIAN DIGITAL SOIL MAPPING WORKSHOP                 ###########
###########                                                                      ###########
############################################################################################
############################################################################################


# Load some libraries required for the work
library(ithir)

wd<- setwd("C:/CDSM_Workshop/2_RCode")
setwd(wd)

# Load some data
MyData<- read.csv('Data/MyData.csv')
View(MyData)

# let's visualize the data for the profile
plot_soilProfile(MyData,vals=MyData$OC, depths = MyData[,2:3], label = 'Organic C (%)')

# let's apply the equal area spline from the ithir package
# here we use the GlobalSoilMap.net intervals of 0,5,15,30,60,100, but you can use whatever you want
spl1<- ea_spline(MyData,var.name='OC',lam=0.1, d= t(c(0,5,15,30,60,100)), vlow=0.0, vhigh=100, show.progress=FALSE)
str(spl1)

# now we can look at the spline

#set up margins for the plotting window
par(mar=c(1,1,1,1))

#split the plotting window into 3 rows, 1 colun for plotting
par(mfrow=c(3,1))

# the for loop runs the plot_ea_spline function 3 times, once for each plot type (1,2,3) available
# the loop replaces the i in the function for each loop
for (i in 1:3){
plot_ea_spline(splineOuts=spl1, d= t(c(0,5,15,30,60,100)), maxd=100, type=i, label='Organic C (%)')
}

#plot Type = 1: Observed soil data plus spline
#plot Type = 2: Observed data plus averages of the spline at the selected depth intervals
#plot Type = 3: Observed data, spline averages and continuous spline

# here we use the same intervals as the sampling to see
spl2<- ea_spline(MyData,var.name='OC',lam=0.1, d= t(c(0,24,43,58,92,100)), vlow=0.0, vhigh=100, show.progress=FALSE)
str(spl2)

# now we can look at the spline
par(mar=c(1,1,1,1))
par(mfrow=c(3,1))
for (i in 1:3){
  plot_ea_spline(splineOuts=spl2, d= t(c(0,24,43,58,92,100)), maxd=100, type=i, label='Organic C (%)')
}


#########################################################################################################################
# we can try for another property, say clay content...imagine a luvisol with Bt enriched in clay

# let's visualize the data for the profile
plot_soilProfile(MyData,vals=MyData$C, depths = MyData[,2:3], label = 'Clay (%)')

# let's apply the equal area spline from the ithir package
# here we use the GlobalSoilMap.net intervals of 0,5,15,30,60,100, but you can use whatever you want
spl1<- ea_spline(MyData,var.name='C',lam=0.1, d= t(c(0,5,15,30,60,100)), vlow=0.0, vhigh=100, show.progress=FALSE)
str(spl1)

# now we can look at the spline
plot_ea_spline(splineOuts=spl1, d= t(c(0,5,15,30,60,100)), maxd=100, type=3, label='Clay (%)')

