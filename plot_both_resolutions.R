#=============================================================================#
# Name   : plots_layout_comparison_resolution
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    : 
# URL    : This plot a layout [1 , 2 , 3]
#=============================================================================#
source('F:/GitHub/ichthyop_analysis/R/source/recruitment_year.R')
source('F:/GitHub/ichthyop_analysis/R/source/recruitment_day.R')
source('F:/GitHub/ichthyop_analysis/R/source/recruitment_depth.R')
source('F:/GitHub/ichthyop_analysis/R/source/recruitment_bathy.R')
source('F:/GitHub/ichthyop_analysis/R/source/recruitment_zone.R')
source('F:/GitHub/ichthyop_analysis/R/source/recruitment_age.R')

out_path <- 'C:/Users/ASUS/Desktop/'

res01 <- read.table('C:/Users/ASUS/Desktop/02km/perubathy/ichthyop_output.csv', sep=';', header = T)
res02 <- read.table('C:/Users/ASUS/Desktop/10km/perubathy/ichthyop_output.csv', sep=';', header = T)

legend_text = c('02 km', '10 km')
PNG <- paste0(out_path, 'peru_resolution_bathy.png')
sort_bathy <- c(1,4,2,3,5) # Orden de filas de batimetria

#--------------- Don't change anything after here ---------------#
ymax = 55
ylab = 'Local Retention (%)'
meses = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
          "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

legend <- 0  # legend == 1 turn on legend, if 0, legned turn off
# legend.cex <- 1 # size of the legend inside the plot

res01_datayear  <- recruitment_year (res01)
res01_dataday   <- recruitment_day  (res01)
res01_datadepth <- recruitment_depth(res01)
res01_databathy <- recruitment_bathy(res01)
res01_datazone  <- recruitment_zone (res01)
res01_dataage   <- recruitment_age  (res01)

res02_datayear  <- recruitment_year (res02)
res02_dataday   <- recruitment_day  (res02)
res02_datadepth <- recruitment_depth(res02)
res02_databathy <- recruitment_bathy(res02)
res02_datazone  <- recruitment_zone (res02)
res02_dataage   <- recruitment_age  (res02)

# DATA YEAR ---------------------------------------------------------------
datayear       <- cbind(res01_datayear[,1],res02_datayear[,1])

year_mean <- NULL
for(i in 1:dim(datayear)[1]){
  a <- res01_datayear[i,2]
  b <- res02_datayear[i,2]
  d <- c(a,b)
  year_mean <- c(year_mean, d)
}

year_sem <- NULL
for(i in 1:dim(datayear)[1]){
  a <- res01_datayear[i,2]
  b <- res02_datayear[i,3]
  d <- c(a,b)
  year_sem <- c(year_sem, d)
}

# DATA DAY ----------------------------------------------------------------
dataday        <- cbind(res01_dataday[,1],res02_dataday[,1])

day_mean <- NULL
for(i in 1:dim(dataday)[1]){
  a <- res01_dataday[i,2]
  b <- res02_dataday[i,2]
  d <- c(a,b)
  day_mean <- c(day_mean, d)
}

day_sem <- NULL
for(i in 1:dim(dataday)[1]){
  a <- res01_dataday[i,2]
  b <- res02_dataday[i,3]
  d <- c(a,b)
  day_sem <- c(day_sem, d)
}


# DATA DEPTH --------------------------------------------------------------
datadepth        <- cbind(res01_datadepth[,1],res02_datadepth[,1])

depth_mean <- NULL
for(i in 1:dim(datadepth)[1]){
  a <- res01_datadepth[i,2]
  b <- res02_datadepth[i,2]
  d <- c(a,b)
  depth_mean <- c(depth_mean, d)
}

depth_sem <- NULL
for(i in 1:dim(datadepth)[1]){
  a <- res01_datadepth[i,2]
  b <- res02_datadepth[i,3]
  d <- c(a,b)
  depth_sem <- c(depth_sem, d)
}

# DATA BATHY --------------------------------------------------------------
databathy        <- cbind(res01_databathy[,1],res02_databathy[,1])

bathy_mean <- NULL
for(i in 1:dim(databathy)[1]){
  a <- res01_databathy[i,2]
  b <- res02_databathy[i,2]
  d <- c(a,b)
  bathy_mean <- c(bathy_mean, d)
}

bathy_sem <- NULL
for(i in 1:dim(databathy)[1]){
  a <- res01_databathy[i,2]
  b <- res02_databathy[i,3]
  d <- c(a,b)
  bathy_sem <- c(bathy_sem, d)
}

# DATA ZONE NAME ----------------------------------------------------------
datazone        <- cbind(res01_datazone[,1],res02_datazone[,1])

zone_mean <- NULL
for(i in 1:dim(datazone)[1]){
  a <- res01_datazone[i,2]
  b <- res02_datazone[i,2]
  d <- c(a,b)
  zone_mean <- c(zone_mean, d)
}

zone_sem <- NULL
for(i in 1:dim(datazone)[1]){
  a <- res01_datazone[i,2]
  b <- res02_datazone[i,3]
  d <- c(a,b)
  zone_sem <- c(zone_sem, d)
}

# DATA AGE ----------------------------------------------------------------
# dataage        <- cbind(res01_dataage[,1],res02_dataage[,1])
# 
# age_mean <- NULL
# for(i in 1:dim(dataage)[1]){
#   a <- res01_dataage[i,2]
#   b <- res02_dataage[i,2]
#   d <- c(a,b)
#   age_mean <- c(age_mean, d)
# }
# 
# age_sem <- NULL
# for(i in 1:dim(dataage)[1]){
#   a <- res01_dataage[i,2]
#   b <- res02_dataage[i,3]
#   d <- c(a,b)
#   age_sem <- c(age_sem, d)
# }


# PLOT --------------------------------------------------------------------
col_bars <- c('grey20','grey80')
png(PNG ,width = 1050 , height = 1050 , res=120)

mat <- matrix(1:4, 2, 2, byrow = TRUE)
nf <- layout(mat, widths = c(8,8), height = c(8,8), TRUE)

# PLOT AS FUNCTION OF SPAWNING YEAR ---------------------------------------
par(mar=c(4, 4, 1 , 0.3))
yearplot   <- barplot(t(datayear[,c(1,2)]), beside = T, xlab="", ylab= "" ,ylim = c(0,ymax),
                     axes = FALSE, col = col_bars)
arrows(yearplot[1,],100*(res01_datayear[,2]+res01_datayear[,3]),
       yearplot[1,],100*(res01_datayear[,2]-res01_datayear[,3]),
       angle=90,code=3,length=0.025)
arrows(yearplot[2,],100*(res02_datayear[,2]+res02_datayear[,3]),
       yearplot[2,],100*(res02_datayear[,2]-res02_datayear[,3]),
       angle=90,code=3,length=0.025)
at <- (yearplot[1,] + yearplot[2,])/2
axis(1, at=at, labels = FALSE, tick = FALSE)
mtext(ylab, side=2, line=2.5 , cex=1.2)
axis(2, lwd = 3, cex.axis = 1.4, las = 2)
# text(at, par("usr")[3], labels = meses, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.9)
mtext("Month of spawning", side=1, line=2.5 , cex=0.9)
legend('top', legend = legend_text, bty = 'n', fill = col_bars)

# PLOT AS FUNCTION OF SPAWNING MONTH --------------------------------------
par(mar=c(4, 4, 1 , 0.3))
dayplot   <- barplot(t(dataday[,c(1,2)]), beside = T, xlab="", ylab= "" ,ylim = c(0,ymax),
                     axes = FALSE, axisnames = FALSE, col = col_bars)
arrows(dayplot[1,],100*(res01_dataday[,2]+res01_dataday[,3]),
       dayplot[1,],100*(res01_dataday[,2]-res01_dataday[,3]),
       angle=90,code=3,length=0.025)
arrows(dayplot[2,],100*(res02_dataday[,2]+res02_dataday[,3]),
       dayplot[2,],100*(res02_dataday[,2]-res02_dataday[,3]),
       angle=90,code=3,length=0.025)
at <- (dayplot[1,] + dayplot[2,])/2
axis(1, at=at, labels = FALSE, tick = FALSE)
mtext(ylab, side=2, line=2.5 , cex=1.2)
axis(2, lwd = 3, cex.axis = 1.4, las = 2)
text(at, par("usr")[3], labels = meses, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.9)
mtext("Month of spawning", side=1, line=2.5 , cex=0.9)
legend('top', legend = legend_text, bty = 'n', fill = col_bars)

# PLOT AS FUNCTION OF SPAWNING DEPTH --------------------------------------
par(mar=c(4, 4, 1 , 0.3))
depthplot <- barplot(t(datadepth[,c(1,2)]), beside = T, xlab="", ylab="", ylim = c(0,ymax),
                     axes = FALSE, cex.names=.8, col = col_bars)
# arrows(depthplot[1,],100*(res01_datadepth[,2]+res01_datadepth[,3]),
#        depthplot[1,],100*(res01_datadepth[,2]-res01_datadepth[,3]),
#        angle=90,code=3,length=0.025)
# arrows(depthplot[2,],100*(res02_datadepth[,2]+res02_datadepth[,3]),
#        depthplot[2,],100*(res02_datadepth[,2]-res02_datadepth[,3]),
#        angle=90,code=3,length=0.025)
mtext(ylab, side=2, line=2.5 , cex=1.2)
axis(2, lwd = 3, cex.axis = 1.4, las = 2)
mtext("Spawning depth (m)", side=1, line=2.5, cex=0.9)
legend('top', legend = legend_text, bty = 'n', fill = col_bars)
# mtext(ylab, side=2, line=2.5 , cex=1.2)

# PLOT AS FUNCTION OF SPAWNING BATHYMETRY ---------------------------------
par(mar=c(4, 4, 1 , 0.3))
bathyplot <- barplot(t(databathy[sort_bathy,c(1,2)]), beside = T, xlab="", ylab="", ylim = c(0,ymax),
                     axes = FALSE, cex.names=.8, col = col_bars)
arrows(bathyplot[1,],100*(res01_databathy[sort_bathy,2]+res01_databathy[sort_bathy,3]),
       bathyplot[1,],100*(res01_databathy[sort_bathy,2]-res01_databathy[sort_bathy,3]),
       angle=90,code=3,length=0.025)
arrows(bathyplot[2,],100*(res02_databathy[sort_bathy,2]+res02_databathy[sort_bathy,3]),
       bathyplot[2,],100*(res02_databathy[sort_bathy,2]-res02_databathy[sort_bathy,3]),
       angle=90,code=3,length=0.025)
mtext(ylab, side=2, line=2.5 , cex=1.2)
axis(2, lwd = 3, cex.axis = 1.4, las = 2)
mtext("Bathymetry (m)", side=1, line=2.5, cex=0.9)
legend('top', legend = legend_text, bty = 'n', fill = col_bars)
# mtext(ylab, side=2, line=2.5 , cex=1.2)


# ### PLOT AS FUNCTION OF AGE MINIMUN TO SETTLEMENT
# par(mar=c(4, 4, 1 , 0.3))
# 
# ageplot   <- barplot(t(dataage[,c(1,2)]), beside = T, xlab="",ylab= "", ylim = c(0,ymax),
#                      axes = FALSE, cex.names=.8, col = col_bars)
# arrows(ageplot[1,],100*(res01_dataage[,2]+res01_dataage[,3]),
#        ageplot[1,],100*(res01_dataage[,2]-res01_dataage[,3]),
#        angle=90,code=3,length=0.025)
# arrows(ageplot[2,],100*(res02_dataage[,2]+res02_dataage[,3]),
#        ageplot[2,],100*(res02_dataage[,2]-res02_dataage[,3]),
#        angle=90,code=3,length=0.025)
# mtext(ylab, side=2, line=2.5 , cex=1.2)
# axis(2, lwd = 3, cex.axis = 1.4)
# mtext('Minimum age to settlement (d)', side=1, line=2.5, cex=0.9)
# legend('topright', legend = legend, bty = 'n', fill = col_bars)
# # mtext(ylab, side=2, line=2.5 , cex=1.2)
# 
# if(legend == 1){
#   legend("topleft", "C)", bty = "n", cex = legend.cex)
# }
dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#