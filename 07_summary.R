# KIT-IMK-IFU Flux Measurement Fundamentals course material (TERENO GAP site subset).
# Contact: Matthias Zeeman <matthias.zeeman@kit.edu>
# Version: 20140805

# We can now construct the missing values of NEE as well.
GAPmodeloutput[['NEE']]         <- GAPmodelinput[['NEE']] 
GAPmodeloutput[flag.gaps,'NEE'] <- GAPmodeloutput[flag.gaps,'RE'] - GAPmodeloutput[flag.gaps,'GPP']

# NEE fingerprint
m <- DOYHOD_matrix(GAPtime,GAPmodeloutput[['NEE']])
filled.contour(x = unique(DOY), 
               y = unique(HOD),  
               z = t(m),
               levels = c(-30,-15,-5,-1,1,5,15,30),
               color.palette = colorRampPalette(c("darkblue","green","yellow","orange","sienna4")),
               xlab= "Time (day of year)", ylab = "Time (hour of day)", main = "NEE (data + model)")

# Any gaps left?
any(is.na(GAPmodeloutput[['NEE']]))

# What is the annual sum?
GAPdt     <- unique(diff(as.numeric(GAPtime))) # time step in seconds, also a good test to see if the series is continuous :^)

GAPNEEtot <- sum(GAPmodeloutput[["NEE"]],na.rm=T) * GAPdt * 10^{-6} * 12
GAPGPPtot <- sum(GAPmodeloutput[["GPP"]],na.rm=T) * GAPdt * 10^{-6} * 12
GAPREtot  <- sum(GAPmodeloutput[["RE"]],na.rm=T)  * GAPdt * 10^{-6} * 12

# Cumulative sums?
plot(GAPtime, cumsum(GAPmodeloutput[['NEE']] * GAPdt * 10^{-6} * 12), # where did this factor come from?
     ty="l",pch=19,cex=0.5,col="coral4", lwd=4,
     ylim=c(-330,30), xlab="Time",ylab="Cumulative sum of flux (g m-2)", main = "NEE")
abline(h=0,v=GAPtime[1])
abline(h=GAPNEEtot,v=rev(GAPtime)[1])
text(x=GAPtime[1], y=GAPNEEtot, 
     labels=sprintf('   NEE:  %.0f g m-2 yr-1',GAPNEEtot),
     adj=c(0,2))
text(x=GAPtime[1], y=GAPNEEtot, 
     labels=sprintf('   GPP: %.0f g m-2 yr-1',GAPGPPtot),
     adj=c(0,-2))
text(x=GAPtime[1], y=GAPNEEtot, 
     labels=sprintf('   RE   : %.0f g m-2 yr-1',GAPREtot),
     adj=c(0,-5))
abline(h=0,v=phenotime)
