# KIT-IMK-IFU Flux Measurement Fundamentals course material (TERENO GAP site subset).
# Contact: Matthias Zeeman <matthias.zeeman@kit.edu>
# Version: 20140805

# define a model
RE_model <- function(RE,Tsoil,Rref) {
  ## Function from Lloyd and Taylor, 1994.
  ##
  ## Written by Matthias Zeeman <matthias.zeeman@kit.edu>
  ## Version 20140805
  T_0 = 227.13
  E_0 = 308.56
  REfun <- as.formula( paste("F_R ~ Rref * exp(",E_0," * ( ( 1/ (283 - ", T_0,") ) - ( 1 / (Tsoil -", T_0,") )) )"  ))
  REnls <- try(nls(REfun, data = list(F_R = RE, Tsoil=Tsoil), 
                    start = list(Rref=Rref), trace=FALSE))
  if (class(REnls) == "try-error" ) { REnls = NULL }
  return(REnls)
}


# we need to define what data is considered missing, the nighttime values and a reference temperature (see Lloyd-Taylor)
flag.gaps  = is.na( GAPmodelinput[['NEE']] ) | !is.finite( GAPmodelinput[['NEE']] ) |
  (GAPmodelinput[['Flag.ustar']] > 1 & !is.na( GAPmodelinput[['Flag.ustar']] )) |
  (GAPmodelinput[['Flag.wCO2']] > 1 & !is.na( GAPmodelinput[['Flag.wCO2']] )) 

# what is night and what is the reference temperature for the reference respiration in Lloyd and Taylor
flag.night = GAPmodelinput[['PAR_Avg']] < 30 # or 10, or?
flag.Tref  = GAPmodelinput[['soiltemp_Avg']] > 9.5 & GAPmodelinput[['soiltemp_Avg']] <= 10.5

# Model!
GAPRref  <- mean( GAPmodelinput[ flag.night & flag.Tref, "NEE" ], na.rm=T)
GAPREmod <- RE_model( GAPmodelinput[ flag.night, "NEE" ] ,
                      GAPmodelinput[ flag.night, "soiltemp_Avg" ] + 273.15,
                      GAPRref )

# Model output
GAPmodeloutput <- subset(GAPmodelinput,select="MODELTIME")
GAPmodeloutput[['RE.model']] <- predict( GAPREmod, 
                                   newdata = data.frame( Tsoil= GAPmodelinput[['soiltemp_Avg']]+ 273.15))

# RE is the modelled RE...
GAPmodeloutput[['RE']] <- GAPmodeloutput[['RE.model']]

# ... except where we have measurements of NEE at night.
GAPmodeloutput[ !flag.gaps & flag.night, 'RE'] <-  GAPmodelinput[!flag.gaps & flag.night, 'NEE']


# RE (only modelled) and RE (combination of modelled data and NEE data)
plot(subset(GAPmodeloutput,select=c('MODELTIME','RE')),
     ty="p",pch=19,cex=0.5,col="azure3", lwd=4,
     ylim=NEElim, xlab="Time",ylab="RE.model and RE")
par(new=T)
plot(subset(GAPmodeloutput,select=c('MODELTIME','RE.model')),
     ty="l",pch=19,cex=0.5,col="coral4", lwd=4,
     ylim=NEElim, xlab="Time",ylab="RE.model and RE")
abline(h=0)

# compare RE and NEE data against Tsoil
plot(GAPmodelinput[ !flag.gaps & flag.night,'soiltemp_Avg' ],
     GAPmodelinput[ !flag.gaps & flag.night,'NEE' ],
     ty="p",pch=19,cex=0.5,col="azure3",
     ylim=NEElim, xlab="Temperature (of the soil)",ylab="RE.model and NEE (at night)")
par(new=T)
plot(GAPmodelinput[  !flag.gaps & flag.night,'soiltemp_Avg' ],
     GAPmodeloutput[ !flag.gaps & flag.night,'RE.model' ],
     ty="l",pch=19,cex=0.5,col="coral4", lwd=4,
     ylim=NEElim, xlab="Temperature (of the soil)",ylab="RE.model and NEE (at night)")
abline(h=0)

# compare RE and NEE data against Tsoil
plot(GAPmodeloutput[ !flag.gaps & flag.night,'RE.model' ], 
     GAPmodelinput[ !flag.gaps & flag.night,'NEE' ],
     xlab= "RE", ylab="NEE (night)", 
     xlim=NEElim,ylim=NEElim,pch=19,cex=0.5,col="azure3")
abline(v=0,h=0)
