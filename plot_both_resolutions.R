#=============================================================================#
# Name   : plots_layout_comparison_resolution
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    : 
# URL    : This plot a layout [1 , 2 , 3]
#=============================================================================#
source('source/recruitment_age.R')
source('source/recruitment_area.R')
source('source/recruitment_bathy.R')
source('source/recruitment_behavior.R')
source('source/recruitment_day.R')
source('source/recruitment_depth.R')
source('source/recruitment_temp.R')
source('source/recruitment_year.R')
source('source/recruitment_zone.R')
source('source/recruitment_eps.R')

file1 <- 'E:/ICHTHYOP/peru02km/ichthyopPeruBathy/out/results/ichthyop_output.csv'
file2 <- 'D:/ICHTHYOP/peru10km/ichthyopPeruBathy/out/results/ichthyop_output.csv'
PNG <- paste0('C:/Users/jflores/Desktop/ichthyopPeruBathy.png')

res01 <- read.table(file1, sep=';', header = T)
# res01 <- subset(res01, res01$Eps == 1e-09)

res02 <- read.table(file2, sep=';', header = T)
# res02 <- subset(res02, res02$Eps == 1e-09)

legend_text = c('02 km', '10 km')
ymax = 65
ylab = 'Local Retention (%)'
col_bars <- c('grey20','grey80')
legpos <- 'top'

#--------------- Don't change anything after here ---------------#
meses = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
          'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

# Grupo de datos 1
res01_dataage       <- recruitment_age(res01)
res01_dataarea      <- recruitment_area(res01)
res01_databathy     <- recruitment_bathy(res01)
res01_databehavior  <- recruitment_behavior(res01)
res01_dataday       <- recruitment_day(res01)
res01_datadepth     <- recruitment_depth(res01)
res01_datatemp      <- recruitment_temp(res01)
res01_datayear      <- recruitment_year(res01)
res01_datazone      <- recruitment_zone(res01)
res01_dataeps       <- recruitment_eps(res01)

# Grupo de datos 2
res02_dataage       <- recruitment_age(res02)
res02_dataarea      <- recruitment_area(res02)
res02_databathy     <- recruitment_bathy(res02)
res02_databehavior  <- recruitment_behavior(res02)
res02_dataday       <- recruitment_day(res02)
res02_datadepth     <- recruitment_depth(res02)
res02_datatemp      <- recruitment_temp(res02)
res02_datayear      <- recruitment_year(res02)
res02_datazone      <- recruitment_zone(res02)
res02_dataeps       <- recruitment_eps(res02)

# DATA AGE ----------------------------------------------------------------
agemean      <- cbind(res01_dataage[,1],res02_dataage[,1]); rownames(agemean) <- rownames(res01_dataage)
ageemin      <- cbind(res01_dataage[,2],res02_dataage[,2])
ageemax      <- cbind(res01_dataage[,3],res02_dataage[,3])

# DATA AREA ---------------------------------------------------------------
areamean      <- cbind(res01_dataarea[,1],res02_dataarea[,1]); rownames(areamean) <- rownames(res01_dataarea)
areaemin      <- cbind(res01_dataarea[,2],res02_dataarea[,2])
areaemax      <- cbind(res01_dataarea[,3],res02_dataarea[,3])

# DATA BATHY --------------------------------------------------------------
bathymean        <- cbind(res01_databathy[,1],res02_databathy[,1]); rownames(bathymean) <- rownames(res01_databathy)
bathyemin        <- cbind(res01_databathy[,2],res02_databathy[,2])
bathyemax        <- cbind(res01_databathy[,3],res02_databathy[,3])

# DATA BEHAVIOUR ----------------------------------------------------------
behaviormean     <- cbind(res01_databehavior[,1],res02_databehavior[,1]); rownames(behaviormean) <- rownames(res01_databehavior)
behavioremin     <- cbind(res01_databehavior[,2],res02_databehavior[,2])
behavioremax     <- cbind(res01_databehavior[,3],res02_databehavior[,3])

# DATA DAY ----------------------------------------------------------------
daymean          <- cbind(res01_dataday[,1],res02_dataday[,1]); rownames(daymean) <- rownames(res01_dataday)
dayemin          <- cbind(res01_dataday[,2],res02_dataday[,2])
dayemax          <- cbind(res01_dataday[,3],res02_dataday[,3])

# DATA DEPTH --------------------------------------------------------------
depthmean        <- cbind(res01_datadepth[,1],res02_datadepth[,1]); rownames(depthmean) <- rownames(res01_datadepth)
depthemin        <- cbind(res01_datadepth[,2],res02_datadepth[,2])
depthemax        <- cbind(res01_datadepth[,3],res02_datadepth[,3])

# DATA TEMP ---------------------------------------------------------------
tempmean        <- cbind(res01_datatemp[,1],res02_datatemp[,1]); rownames(tempmean) <- rownames(res01_datatemp)
tempemin        <- cbind(res01_datatemp[,2],res02_datatemp[,2])
tempemax        <- cbind(res01_datatemp[,3],res02_datatemp[,3])

# DATA YEAR ---------------------------------------------------------------
yearmean      <- cbind(res01_datayear[,1],res02_datayear[,1]); rownames(yearmean) <- rownames(res01_datayear)
yearemin      <- cbind(res01_datayear[,2],res02_datayear[,2])
yearemax      <- cbind(res01_datayear[,3],res02_datayear[,3])

# DATA ZONE NAME ----------------------------------------------------------
zonemean      <- cbind(res01_datazone[,1],res02_datazone[,1]); rownames(zonemean) <- rownames(res01_datazone)
zoneemin      <- cbind(res01_datazone[,2],res02_datazone[,2])
zoneemax      <- cbind(res01_datazone[,3],res02_datazone[,3])

# MAKE PLOTS --------------------------------------------------------------
png(PNG, width = 1050, height = 1050, res=120)
mat <- matrix(1:4, 2, 2, byrow = TRUE)
nf <- layout(mat, widths = c(8,8), height = c(8,8), TRUE)


# # PLOT AS FUNCTION OF AGE MINIMUN -----------------------------------------
# par(mar=c(4, 4, 1 , 0.3))
# ageplot <- barplot(t(agemean), beside = T, xlab='', ylab='', ylim = c(0,ymax),
#                      axes = FALSE, cex.names=.8, col = col_bars)
# arrows(x0 = ageplot[1,], y0 = ageemax[,1], x1 = ageplot[1,], y1 = ageemin[,1],
#        angle=90,code=3,length=0.025, lwd = 2)
# arrows(x0 = ageplot[2,], y0 = ageemax[,2], x1 = ageplot[2,], y1 = ageemin[,2],
#        angle=90,code=3,length=0.025, lwd = 2)
# mtext(ylab, side=2, line=2.5 , cex=1.2)
# axis(2, lwd = 3, cex.axis = 1.4, las = 2)
# mtext('Age Minimun (days)', side=1, line=2.5, cex=0.9)
# legend(legpos, legend = legend_text, bty = 'n', fill = col_bars)


# # PLOT AS FUNCTION OF SPAWNING AREA ---------------------------------------
# par(mar=c(4, 4, 1 , 0.3))
# areaplot <- barplot(t(areamean), beside = T, xlab='', ylab='', ylim = c(0,ymax),
#                    axes = FALSE, cex.names=.8, col = col_bars)
# arrows(x0 = areaplot[1,], y0 = areaemax[,1], x1 = areaplot[1,], y1 = areaemin[,1],
#        angle=90,code=3,length=0.025, lwd = 2)
# arrows(x0 = areaplot[2,], y0 = areaemax[,2], x1 = areaplot[2,], y1 = areaemin[,2],
#        angle=90,code=3,length=0.025, lwd = 2)
# mtext(ylab, side=2, line=2.5 , cex=1.2)
# axis(2, lwd = 3, cex.axis = 1.4, las = 2)
# mtext('Release Area', side=1, line=2.5, cex=0.9)
# legend(legpos, legend = legend_text, bty = 'n', fill = col_bars)


# PLOT AS FUNCTION OF SPAWNING BATHYMETRY ---------------------------------
par(mar=c(4, 4, 1 , 0.3))
bathyplot <- barplot(t(bathymean), beside = T, xlab='', ylab='', ylim = c(0,ymax),
                     axes = FALSE, cex.names=.8, col = col_bars)
arrows(x0 = bathyplot[1,], y0 = bathyemax[,1], x1 = bathyplot[1,], y1 = bathyemin[,1],
       angle=90,code=3,length=0.025, lwd = 2)
arrows(x0 = bathyplot[2,], y0 = bathyemax[,2], x1 = bathyplot[2,], y1 = bathyemin[,2],
       angle=90,code=3,length=0.025, lwd = 2)
mtext(ylab, side=2, line=2.5 , cex=1.2)
axis(2, lwd = 3, cex.axis = 1.4, las = 2)
mtext('Bathymetry (m)', side=1, line=2.5, cex=0.9)
legend('top', legend = legend_text, bty = 'n', fill = col_bars)


# # PLOT AS FUNCTION OF BEHAVIOR --------------------------------------------
# par(mar=c(4, 4, 1 , 0.3))
# behaviorplot <- barplot(t(behaviormean), beside = T, xlab='', ylab='', ylim = c(0,ymax),
#                      axes = FALSE, cex.names=.8, col = col_bars)
# arrows(x0 = behaviorplot[1,], y0 = behavioremax[,1], x1 = behaviorplot[1,], y1 = behavioremin[,1],
#        angle=90,code=3,length=0.025, lwd = 2)
# arrows(x0 = behaviorplot[2,], y0 = behavioremax[,2], x1 = behaviorplot[2,], y1 = behavioremin[,2],
#        angle=90,code=3,length=0.025, lwd = 2)
# mtext(ylab, side=2, line=2.5 , cex=1.2)
# axis(2, lwd = 3, cex.axis = 1.4, las = 2)
# mtext('Behavior', side=1, line=2.5, cex=0.9)
# legend('top', legend = legend_text, bty = 'n', fill = col_bars)


# PLOT AS FUNCTION OF SPAWNING MONTH --------------------------------------
par(mar=c(4, 4, 1 , 0.3))
dayplot   <- barplot(t(daymean), beside = T, xlab='', ylab= '' ,ylim = c(0,ymax),
                     axes = FALSE, axisnames = FALSE, col = col_bars)
arrows(x0 = dayplot[1,], y0 = dayemax[,1], x1 = dayplot[1,], y1 = dayemin[,1],
       angle=90,code=3,length=0.025, lwd = 2)
arrows(x0 = dayplot[2,], y0 = dayemax[,2], x1 = dayplot[2,], y1 = dayemin[,2],
       angle=90,code=3,length=0.025, lwd = 2)
at <- (dayplot[1,] + dayplot[2,])/2
axis(1, at=at, labels = FALSE, tick = FALSE)
mtext(ylab, side=2, line=2.5 , cex=1.2)
axis(2, lwd = 3, cex.axis = 1.4, las = 2)
text(at, par('usr')[3], labels = meses, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.9)
mtext('Spawning Month', side=1, line=2.5 , cex=0.9)
legend('top', legend = legend_text, bty = 'n', fill = col_bars)


# PLOT AS FUNCTION OF SPAWNING DEPTH --------------------------------------
par(mar=c(4, 4, 1 , 0.3))
depthplot <- barplot(t(depthmean), beside = T, xlab='', ylab='', ylim = c(0,ymax),
                        axes = FALSE, cex.names=.8, col = col_bars)
arrows(x0 = depthplot[1,], y0 = depthemax[,1], x1 = depthplot[1,], y1 = depthemin[,1],
       angle=90,code=3,length=0.025, lwd = 2)
arrows(x0 = depthplot[2,], y0 = depthemax[,2], x1 = depthplot[2,], y1 = depthemin[,2],
       angle=90,code=3,length=0.025, lwd = 2)
mtext(ylab, side=2, line=2.5 , cex=1.2)
axis(2, lwd = 3, cex.axis = 1.4, las = 2)
mtext('Release Depth (m)', side=1, line=2.5, cex=0.9)
legend('top', legend = legend_text, bty = 'n', fill = col_bars)


# # PLOT AS FUNCTION OF LETHAL TEMPERATURE ----------------------------------
# par(mar=c(4, 4, 1 , 0.3))
# tempplot <- barplot(t(tempmean), beside = T, xlab='', ylab='', ylim = c(0,ymax),
#                         axes = FALSE, cex.names=.8, col = col_bars)
# arrows(x0 = tempplot[1,], y0 = tempemax[,1], x1 = tempplot[1,], y1 = tempemin[,1],
#        angle=90,code=3,length=0.025, lwd = 2)
# arrows(x0 = tempplot[2,], y0 = tempemax[,2], x1 = tempplot[2,], y1 = tempemin[,2],
#        angle=90,code=3,length=0.025, lwd = 2)
# mtext(ylab, side=2, line=2.5 , cex=1.2)
# axis(2, lwd = 3, cex.axis = 1.4, las = 2)
# mtext('Lethal Temperature (°C)', side=1, line=2.5, cex=0.9)
# legend('top', legend = legend_text, bty = 'n', fill = col_bars)


# PLOT AS FUNCTION OF SPAWNING YEAR ---------------------------------------
par(mar=c(4, 4, 1 , 0.3))
yearplot   <- barplot(t(yearmean[,c(1,2)]), beside = T, xlab='', ylab= '' ,ylim = c(0,ymax),
                     axes = FALSE, col = col_bars)
arrows(x0 = yearplot[1,], y0 = yearemax[,1], x1 = yearplot[1,], y1 = yearemin[,1],
       angle=90,code=3,length=0.025, lwd = 2)
arrows(x0 = yearplot[2,], y0 = yearemax[,2], x1 = yearplot[2,], y1 = yearemin[,2],
       angle=90,code=3,length=0.025, lwd = 2)
at <- (yearplot[1,] + yearplot[2,])/2
axis(1, at=at, labels = FALSE, tick = FALSE)
mtext(ylab, side=2, line=2.5 , cex=1.2)
axis(2, lwd = 3, cex.axis = 1.4, las = 2)
mtext('Spawning Year', side=1, line=2.5 , cex=0.9)
legend('top', legend = legend_text, bty = 'n', fill = col_bars)


# # PLOT AS FUNCTION OF ZONE NAME -------------------------------------------
# par(mar=c(4, 4, 1 , 0.3))
# zoneplot <- barplot(t(zonemean), beside = T, xlab='', ylab='', ylim = c(0,ymax),
#                     axes = FALSE, cex.names=.8, col = col_bars)
# arrows(x0 = zoneplot[1,], y0 = zoneemax[,1], x1 = zoneplot[1,], y1 = zoneemin[,1],
#        angle=90,code=3,length=0.025, lwd = 2)
# arrows(x0 = zoneplot[2,], y0 = zoneemax[,2], x1 = zoneplot[2,], y1 = zoneemin[,2],
#        angle=90,code=3,length=0.025, lwd = 2)
# mtext(ylab, side=2, line=2.5 , cex=1.2)
# axis(2, lwd = 3, cex.axis = 1.4, las = 2)
# mtext('Zone', side=1, line=2.5, cex=0.9)
# legend('top', legend = legend_text, bty = 'n', fill = col_bars)

dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#