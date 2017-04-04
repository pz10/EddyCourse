# KIT-IMK-IFU Flux Measurement Fundamentals course material (TERENO GAP site subset).
# Contact: Matthias Zeeman <matthias.zeeman@kit.edu>
# Version: 20140805


# plot NEE data
NEElim = range(GAPdata[['NEE']],na.rm=T)

plot( GAPtime, 
      GAPdata[['NEE']],
      xlab="time",ylab="NEE", 
      ty='p',col="azure3",pch=19,cex=.5,
      main = "GAP 2013, NEE",
      ylim = NEElim )

# add daily means, NEE
par(new=TRUE) # add another plot on top
plot( as.POSIXct(GAPmean[['YDOY']], format="%Y%j"),
      GAPmean[['NEE']],
      ty='l', lwd=2, col = 'black',
      xlab="time",ylab="NEE", main=" ",
      ylim = NEElim )

# add weekly means
GAPwmean <- aggregate( GAPdata, 
                       by = list('YWEEK'= format(GAPtime,'%Y%W' )), 
                       FUN = mean, na.rm=T )

par(new=TRUE) # add another plot on top
plot( as.POSIXct(GAPwmean[['YWEEK']], format="%Y%j"),
      GAPwmean[['NEE']],
      ty='s', lwd= 4, col = 'coral3',
      xlab="time",ylab="NEE", main=" ", 
      ylim = NEElim )



# Hourly mean, per month
GAPHmean <- aggregate( GAPdata, 
                       by = list('YMONHOD'= format(GAPtime,'%Y%m%H' )),
                       FUN = median, na.rm=T )

GAPHmean.month = as.numeric( substr( GAPHmean[['YMONHOD']],5,6) )
GAPHmean.hour  = as.numeric( substr( GAPHmean[['YMONHOD']],7,8) )

for ( month in unique(GAPHmean.month ) ) {
  thismonth =  GAPHmean.month == month 
  plot( c( NA, GAPHmean.hour[ thismonth ]),
        c( NA, GAPHmean[['NEE']][ thismonth  ]),
        col = colorRampPalette(c('darkblue','gray','red'))(24)[ round(GAPHmean[['soiltemp_Avg']][ thismonth  ] ) ],
        ylim = NEElim, xlim = range( GAPHmean.hour ),
        ty='l',lwd=2,
        ylab = 'NEE', xlab='Time (hour of day)',
        main = c('GAP 2013, NEE; hourly mean per month','','(+ color indicates soiltemp)'))
  par(new=TRUE)
}
par(new=FALSE)