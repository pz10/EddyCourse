# KIT-IMK-IFU Flux Measurement Fundamentals course material (TERENO GAP site subset).
# Contact: Matthias Zeeman <matthias.zeeman@kit.edu>
# Version: 20140805


# initially, GPP can be calculated from NEE and RE and set to 0 at night.
GAPmodeloutput[['GPP.model']]            <- GAPmodeloutput[['RE']] - GAPmodelinput[['NEE']]
GAPmodeloutput[ flag.night, 'GPP.model'] <- 0

# define a model
GPP_model <- function(GPP, QPPFD, QPPFDlow=400, QPPFDopt=1200, alpha=NULL) {
  ## Function as in Falge, 2001.
  ##
  ## Written by Matthias Zeeman <matthias.zeeman@kit.edu>
  ## Version 20140805
  
  # we estimate an initial alpha from lowlight conditions.
  alpha <- mean(   GPP[QPPFD > QPPFDlow & QPPFD < QPPFDopt] / 
                 QPPFD[QPPFD > QPPFDlow & QPPFD < QPPFDopt], na.rm=T)
  # we estimate A2000 from optimal light conditions.
  lmA2000 <- lm( F_A ~ QPPFD, 
                 data   = data.frame( F_A = GPP, QPPFD = QPPFD ), 
                 subset = (QPPFD > QPPFDopt) )
  A2000   <- predict(lmA2000, newdata=data.frame(QPPFD=2000))

  GPPfun <- as.formula( paste("F_A ~  alpha * QPPFD / (1 - (QPPFD / 2000 ) + QPPFD * alpha / ",A2000," )"  ))
  GPPnls <- try(nls(GPPfun, data = list(F_A = GPP, QPPFD=QPPFD), 
                   start = list(alpha=alpha), trace=FALSE, subset = ))
  if (class(GPPnls) == "try-error" ) { GPPnls = NULL }
  return(GPPnls)
}


# Model!
GAPGPPmod <- GPP_model( GAPmodeloutput[ !flag.night, "GPP.model" ] ,
                        GAPmodelinput[ !flag.night, "PAR_Avg" ])
GAPmodeloutput[ !flag.night , 
                'GPP.model'] <- predict( GAPGPPmod, 
                                         newdata = data.frame( QPPFD= GAPmodelinput[!flag.night,'PAR_Avg']))

# GPP is the modelled GPP...
GAPmodeloutput[['GPP']] <- GAPmodeloutput[['GPP.model']]

# ... except where we have measurements of NEE during the day.
GAPmodeloutput[ !flag.gaps & !flag.night, 'GPP'] <-  GAPmodeloutput[ !flag.gaps & !flag.night ,'RE'] - 
                                                     GAPmodelinput[  !flag.gaps & !flag.night, 'NEE']


# GPP (only modelled) and GPP (combination of modelled GPP, modelled RE and NEE data)
plot(subset(GAPmodeloutput,select=c('MODELTIME','GPP')),
     ty="p",pch=19,cex=0.5,col="azure3", lwd=4,
     ylim=NEElim, xlab="Time",ylab="GPP.model and GPP")
par(new=T)
plot(subset(GAPmodeloutput,select=c('MODELTIME','GPP.model')),
     ty="p",pch=19,cex=0.1,col="coral4", lwd=4,
     ylim=NEElim, xlab="Time",ylab="GPP.model and GPP")
abline(h=0)

# compare RE and NEE data against Tsoil
plot(GAPmodelinput[ !flag.gaps & !flag.night,'PAR_Avg' ],
     GAPmodeloutput[ !flag.gaps & !flag.night,'GPP' ],
     ty="p",pch=19,cex=0.5,col="azure3",
     ylim=NEElim, xlab="QPPFD",ylab="GPP and GPP.model")
par(new=T)
plot(GAPmodelinput[  !flag.gaps & !flag.night,'PAR_Avg' ],
     GAPmodeloutput[ !flag.gaps & !flag.night,'GPP.model' ],
     ty="p",pch=19,cex=0.5,col="coral4", lwd=4,
     ylim=NEElim, xlab="QPPFD",ylab="GPP and GPP.model")
abline(h=0)

# compare GPP.model and GPP data
plot(GAPmodeloutput[ !flag.gaps & !flag.night,'GPP.model' ], 
     GAPmodeloutput[ !flag.gaps & !flag.night, 'GPP' ],
     xlab= "GPP.model", ylab="GPP", 
     xlim=NEElim,ylim=NEElim,pch=19,cex=0.5,col="azure3")
abline(v=0,h=0)


