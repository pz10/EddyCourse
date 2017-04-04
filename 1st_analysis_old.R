dd <-fread("TOA5_2833.Table3.dat", sep=",")
data <-fread("TOA5_2833.ts_data.dat", sep=",")
span <- 30*60
hz <- 20

data <- data[3:nrow(data)]
data[, time:= as.POSIXct(TIMESTAMP, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
data[, time:= time+7200]
data[, aux1:= unclass(time)]
data[, aux2:=1:.N, by=time]
data[, aux2:= as.numeric(aux2)/hz]
data[, time1 := as.numeric(aux1 + aux2)]
setkey(data, time1)

setnames(data, "li7500_co2_mg_m3", "CO2")
setnames(data, "li7500_h2o_g_m3", "H2O")
setnames(data, "Air_Temp_Avg", "Ta")

str(data)
data[,Uz:= as.numeric(Uz)]
data[,Air_Temp_Avg:= as.numeric(Air_Temp_Avg)]
data[,li7500_co2_mg_m3:= as.numeric(li7500_co2_mg_m3)]
data[,T:= as.numeric(T)]


# setkey(data, time)
# data[,order:=1:.N, by=time]
# data[,order:= as.numeric(order)/hz]
# data[, time:= as.numeric(unclass(time))]
# data[, time:= time+order]

# data[,m.Uz:= rollapply(Uz, span, mean, fill=NA)]
# data[,m.T:= rollapply(Air_Temp_Avg, span, mean, fill=NA)]
# data[,i.Uz:= Uz - m.Uz]
# data[,i.T:= Air_Temp_Avg - m.T]

# 2015-07-27 15:23:45
# 2015-07-28 06:47:07
c1 <- unclass(as.POSIXct("2015-07-27 15:15:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC"))[1]
c2 <- unclass(as.POSIXct("2015-07-28 07:15:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC"))[1]

cuts <- seq(from= c1, to=c2 , by= span)
data[, stretch:= cut(time1, breaks = cuts, labels = FALSE, include.lowest= T)]

data[, m.Uz:= mean(Uz, na.rm=T), by = stretch]
data[, m.T:= mean(Air_Temp_Avg, na.rm=T), by = stretch]
data[, i.Uz:= Uz - m.Uz]
data[, i.T:= Air_Temp_Avg - m.T]
data[abs(i.T) > 2, i.T:= NA]


# quantile(data$i.T, probs=seq(from=0.05, to=1, by=0.05), na.rm=T)
# 
# boxplot(data$i.T, na.rm=T)
# 
# with(data[stretch==15], plot(i.T ~ i.Uz))
# abline(h=0, col="red")
# abline(v=0, col="red")

mydata <- data[stretch %in% 4:31]
g.1 <- ggplot(mydata, aes(x= i.T, y = i.Uz))
g.1 <- (g.1
        + facet_wrap( ~ stretch, scales = "fixed", ncol=7)
         + theme_bw(base_size = 8)
         + theme(plot.margin= unit(c(0, 0.5, 0, 0.1), "lines"),
                 axis.ticks = element_line(size = 0.1),
                 legend.position =   "none",
                 panel.margin =       unit(-0.4, "lines"),
                 strip.background =   element_blank(),
                 strip.text.x =       element_blank(),
                 plot.title = element_text(hjust = 0)
         )
#          + ylab("occurrence by rain event [%]")
#          + xlab("WFPS  [%]")
#          #         + labs(title = "(a)")
#          + coord_cartesian(xlim= c(0, 100), ylim=c(0,100))
#          + scale_x_continuous(breaks = c(0, 25, 50, 75, 100))
)

g.1 <- (g.1
         + geom_point(alpha=0.1)
#          + geom_ribbon(aes(ymin=0, ymax=100), fill="grey92")
#          + geom_ribbon(aes(ymin=0, ymax=Ndens.event.2), fill="grey85")
#          + geom_ribbon(aes(ymin=0, ymax=Ndens.event.1), fill="grey68")
#          
#          
#          #          + geom_line(aes(x= WFPS, y = Ndens.NO.1), col="#74276D")
#          #          + geom_line(aes(x= WFPS, y = myNO.2), col="#74276D", linetype="dotted")         
#          #          + geom_line(aes(x= WFPS, y = Ndens.N2O.1), col="#8EA336")
#          #          + geom_line(aes(x= WFPS, y = myN2O.2), col="#8EA336", linetype="dotted")
#          + geom_line(aes(x= WFPS, y = Ndens.NO.1), col="#E69F00")
#          + geom_line(aes(x= WFPS, y = myNO.2), col="#E69F00", linetype="dotted")         
#          + geom_line(aes(x= WFPS, y = Ndens.N2O.1), col="#12329C")
#          + geom_line(aes(x= WFPS, y = myN2O.2), col="#12329C", linetype="dotted")
#          
#          #          + geom_line(aes(x= WFPS, y = Ndens.event.2))
#          
#          + geom_hline(yintercept=c(25,50,75,100), col="white", size = 0.2* 8/12, alpha=0.3)
#          + geom_hline(yintercept=c(25,50,75,100)-12.5, col="white", size = 0.5* 8/12, alpha=0.1)
#          + geom_vline(xintercept=c(25,50,75,100), col="white", size = 0.2* 8/12, alpha=0.3)
#          + geom_vline(xintercept=c(25,50,75,100)-12.5, col="white", size = 0.5* 8/12, alpha=0.1)
#          
#          + geom_text(data=labels, aes(x=x, y=y, label=label), hjust= -0.1, vjust=0, size=2.8, color=labels$color)
)
g.1
myplot <- "i.T_vs_i.Uz.png"
png(filename=myplot,  width = 160, height = 80, units = "mm", res=1200)
print(g.1)

dev.off()



myd <- data[stretch==30]
# with(myd, plot(li7500_co2_mg_m3 ~time))

g.1 <- ggplot(myd, aes(x= i.T, y = i.Uz))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0, 0.5, 0, 0.1), "lines"),
                axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                panel.margin =       unit(-0.4, "lines"),
                strip.background =   element_blank(),
                strip.text.x =       element_blank(),
                plot.title = element_text(hjust = 0)
        )
                 + ylab("Uz' [m/s]")
                 + xlab("T' [Celsius]")
                 + labs(title = "05:45 - 06:15 am")
        #          + coord_cartesian(xlim= c(0, 100), ylim=c(0,100))
        #          + scale_x_continuous(breaks = c(0, 25, 50, 75, 100))
)

g.1 <- (g.1
        + geom_point(alpha=0.1)
)
g.1
myplot <- "i.T_vs_i.Uz_0545.0615.png"
png(filename=myplot,  width = 160, height = 80, units = "mm", res=1200)
print(g.1)

dev.off()


# RAW # Tue Jul 28 17:03:05 2015 ------------------------------
# UZ
g.1 <- ggplot(myd, aes(x= time, y = Uz))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0, 0.5, 0, 0.1), "lines"),
                axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                panel.margin =       unit(-0.4, "lines"),
                strip.background =   element_blank(),
                strip.text.x =       element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab("Uz [m/s]")
        + xlab("time")
        + labs(title = "05:45 - 06:15 am")
        #          + coord_cartesian(xlim= c(0, 100), ylim=c(0,100))
        #          + scale_x_continuous(breaks = c(0, 25, 50, 75, 100))
)

g.1 <- (g.1
        + geom_line(alpha=1)
)
g.1
##

# T
g.2 <- ggplot(myd, aes(x= time, y = Air_Temp_Avg))
g.2 <- (g.2
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0, 0.5, 0, 0.1), "lines"),
                axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                panel.margin =       unit(-0.4, "lines"),
                strip.background =   element_blank(),
                strip.text.x =       element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab("T [celsius]")
        + xlab("time")
        + labs(title = "05:45 - 06:15 am")
        #          + coord_cartesian(xlim= c(0, 100), ylim=c(0,100))
        #          + scale_x_continuous(breaks = c(0, 25, 50, 75, 100))
)

g.2 <- (g.2
        + geom_line(alpha=1)
)
g.2

# CO2
g.3 <- ggplot(myd, aes(x= time1, y = li7500_co2_mg_m3))
g.3 <- (g.3
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0, 0.5, 0, 0.1), "lines"),
                axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                panel.margin =       unit(-0.4, "lines"),
                strip.background =   element_blank(),
                strip.text.x =       element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab("CO2 [celsius]")
        + xlab("time")
        + labs(title = "05:45 - 06:15 am")
        #          + coord_cartesian(xlim= c(0, 100), ylim=c(0,100))
        #          + scale_x_continuous(breaks = c(0, 25, 50, 75, 100))
)

g.3 <- (g.3
        + geom_point(alpha=1)
)
g.3



