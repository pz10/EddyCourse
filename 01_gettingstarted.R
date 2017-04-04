# KIT-IMK-IFU Flux Measurement Fundamentals course material (TERENO GAP site subset).
# Contact: Matthias Zeeman <matthias.zeeman@kit.edu>
# Version: 20140805


# read data file
GAPdata = read.table( file = '/tmp/TERENO_Garmisch_2013_FMF.csv',
                      sep = ',', 
                      na.strings = '-9999', 
                      header = TRUE )

# check 
head(GAPdata)
class(GAPdata[['MODELTIME']])

# convert date information
GAPdata[['MODELTIME']] <- as.POSIXct( GAPdata[['MODELTIME']] )
class(GAPdata[['MODELTIME']])

# generate means, e.g.,
GAPtime <- GAPdata[['MODELTIME']]
GAPmean <- aggregate( GAPdata, 
                      by = list('YDOY'= format(GAPtime,'%Y%j' )), 
                      FUN = mean, na.rm=T )

# check means
head(GAPmean)

# plot means
plot( as.POSIXct(GAPmean[['YDOY']], format="%Y%j"), # convert date information!
      GAPmean[['soiltemp_Avg']],
      ty='l',xlab="time",ylab="T", main="GAP 2013, daily mean" )
