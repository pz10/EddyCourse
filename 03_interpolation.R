# KIT-IMK-IFU Flux Measurement Fundamentals course material (TERENO GAP site subset).
# Contact: Matthias Zeeman <matthias.zeeman@kit.edu>
# Version: 20140805


sum(is.na(GAPdata$PAR_Avg))
# [1] 17
sum(is.na(GAPdata$soiltemp_Avg))
# [1] 15
sum(is.na(GAPdata$NEE))
# [1] 10412

# where are the gaps (rle black magic)
missing_data <- function(boolean_input) {
  ## Search for locations of TRUE (e.g. missing are is.na() == TRUE) in a variable (BOOLEAN).
  ## Note: Use only on continuous timeseries.
  ## Written by Matthias Zeeman <matthias.zeeman@kit.edu>
  ## Version 20140805
  gaprle <- rle(boolean_input)
  gaps   <- data.frame ( start  = cumsum( gaprle[['lengths']] )[  gaprle[['values']] ] -
                                        ( gaprle[['lengths']] )[  gaprle[['values']] ] + 1,
                         end    = cumsum( gaprle[['lengths']] )[  gaprle[['values']] ],
                         length =       ( gaprle[['lengths']] )[  gaprle[['values']] ]  )
  return(gaps)
}

# if the gaps are an hour or less, we interpolate (i.e. max 2 30-min values are missing)
linear_interpolation <- function(var, max=2, verbose=FALSE ) {
  ## Linear interpolate missing numbers
  ## Note: Use only on continuous timeseries.
  ## Written by Matthias Zeeman <matthias.zeeman@kit.edu>
  ## Version 20140805
  gaps <- missing_data( is.na( var ) )
  for (i in seq(1,dim(gaps)[1])) {
    gaplen <- gaps[i,'length']
    if ( gaplen <= max) {
      for (n in seq( length.out = gaplen) ) {
        indexleft  <- ifelse( gaps[i,'start']==1          ,gaps[i,'end']+1  , gaps[i,'start']-1) 
        indexright <- ifelse( gaps[i,'end']  ==length(var),gaps[i,'start']-1, gaps[i,'end']+1) 
        var[ gaps[i,'start'] + n - 1 ] <- 
          var[indexleft] + (var[indexright]-var[indexleft]) / (gaplen + 1) * n
      }
      if (verbose) { message(paste( c( i, " : ", round( var[indexleft:indexright] )), collaps=" " )) }
    }
  }
  return(var)
}

# for big gaps we use the same time of day on neighbouring days
daily_interpolation <- function(var,time,max = c(-3,3)) {
  
}

## copy to a model input datatable
GAPmodelinput <- GAPdata

# linear interpolation
for ( var in c( "soiltemp_Avg", "PAR_Avg", "NEE") ) {
  GAPmodelinput[[ var ]]  <- linear_interpolation( GAPmodelinput[[ var ]] )
}

# diurnal interpolation
for ( var in c( "soiltemp_Avg", "PAR_Avg") ) {
  GAPmodelinput[[ var ]]  <- linear_interpolation( GAPmodelinput[[ var ]] ,  max= 20 , verbose=TRUE)
}


# test:
sum(is.na(GAPmodelinput$PAR_Avg))
# [1] 0
sum(is.na(GAPmodelinput$soiltemp_Avg))
# [1] 0
