# KIT-IMK-IFU Flux Measurement Fundamentals course material (TERENO GAP site subset).
# Contact: Matthias Zeeman <matthias.zeeman@kit.edu>
# Version: 20140805


# Make a Hovmueller-like plot of the data, showing hour vs day. Sometimes refered to as 'fingerprint' plot. 
DOYHOD_matrix <- function(vtime,var,plot=FALSE) {
  ## Convert a variable (var) to a matrix of time (HourOfDay) to time (DayOfYear).
  ## Note: Use only on continuous records (e.g. timeseries).
  ## Written by Matthias Zeeman <matthias.zeeman@kit.edu>
  ## Version 20140805
  DOY  <- as.numeric(format(vtime,format="%j")) # not year safe
  HOD  <- as.numeric(format(vtime,format="%H")) + as.numeric(format(vtime,format="%M"))/60
  HODs <- min(abs(diff(HOD)))   # time step
  HODm <- diff(c(min(HOD),1))   # in case 00:00 is not 24:00 we have a HOD value of 0, that cannot be used as index
  HODi <- sapply(HOD, FUN = function(x) { ( x / HODs) + HODm } )  # should be 1..48 for 30-min data
  
  m    <- matrix(NA, nrow = length(unique(HOD)), ncol = diff(range(DOY))+1 )
  for (t in seq_along(vtime)) {
    m[HODi[t],DOY[t]] <- var[t]
  }
  return(m)
}

# make the plots
DOY  <- as.numeric(format(GAPtime,format="%j")) # not year safe
HOD  <- as.numeric(format(GAPtime,format="%H")) + as.numeric(format(GAPtime,format="%M"))/60
for ( var in c( "soiltemp_Avg", "PAR_Avg") ) {
  m <- DOYHOD_matrix(GAPtime,GAPmodelinput[[ var ]])
  filled.contour(x = unique(DOY), # or POSIX data: min(GAPtime) + (unique(DOY) - 1)*60*60*24
                 y = unique(HOD),  
                 z = t(m),
                 color.palette = topo.colors,
                 xlab= "Time (day of year)", ylab = "Time (hour of day)", main = var)
} 

for ( var in c( "Flag.ustar","Flag.wCO2") ) {
  m <- DOYHOD_matrix(GAPtime,GAPmodelinput[[ var ]])
  filled.contour(x = unique(DOY), 
                 y = unique(HOD),  
                 z = t(m),  
                 levels= c(0,.5,1.5,2),
                 color.palette = colorRampPalette(c("white","gray70","darkblue")),
                 xlab= "Time (day of year)", ylab = "Time (hour of day)", main = var)
}

m <- DOYHOD_matrix(GAPtime,GAPmodelinput[['NEE']])
filled.contour(x = unique(DOY), 
               y = unique(HOD),  
               z = t(m),
               levels = c(-30,-15,-5,-1,1,5,15,30),
               color.palette = colorRampPalette(c("darkblue","green","yellow","orange","sienna4")),
               xlab= "Time (day of year)", ylab = "Time (hour of day)", main = "NEE")



