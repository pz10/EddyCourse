dd <-fread("TOA5_2833.Table3.dat", sep=",")
# data <-fread("TOA5_2833.ts_data.dat", sep=",")
data <-fread("TOA5_2835.dat", sep=",")

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


data[,Uz:= as.numeric(Uz)]
data[,Ta:= as.numeric(Ta)]
data[,RH:= as.numeric(RH)]
data[,CO2:= as.numeric(CO2)]
data[,H2O:= as.numeric(H2O)]
str(data)

# make 30 minutes stretches
# 2015-07-27 15:23:45
# 2015-07-28 06:47:07
# c1 <- unclass(as.POSIXct("2015-07-27 17:15:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC"))[1]
# c2 <- unclass(as.POSIXct("2015-07-28 09:15:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC"))[1]
# 
# 2015-07-29 08:01:47.9
# 2015-07-30 06:41:14.95
c1 <- unclass(as.POSIXct("2015-07-29 08:00:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC"))[1]
c2 <- unclass(as.POSIXct("2015-07-30 06:30:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC"))[1]

cuts <- seq(from= c1, to=c2 , by= span)

data[, stretch:= cut(time1, breaks = cuts, labels = FALSE, include.lowest= T)]

data[, stretch.label:= cut(time1, breaks = cuts, labels = cuts[-1], include.lowest= T)]
data[, stretch.label:= as.numeric(as.character(stretch.label))]
data[, stretch.label:= as.POSIXct(stretch.label - span/2, origin = "1960-01-01")]
data[, stretch.label:= substring(stretch.label, 12, last = 16)]
data[, file.label:= gsub(":", "",stretch.label)]
# data[, stretch.label:= paste(stretch, stretch.label, sep="-")]



# get mean 30-minute-value
data[, m.Uz:= mean(Uz, na.rm=T), by = stretch]
data[, m.Ta:= mean(Ta, na.rm=T), by = stretch]
data[, m.RH:= mean(RH, na.rm=T), by = stretch]
data[, m.CO2:= mean(CO2, na.rm=T), by = stretch]
data[, m.H2O:= mean(H2O, na.rm=T), by = stretch]

data[, i.Uz:= Uz - m.Uz]
data[, i.Ta:= Ta - m.Ta]
data[, i.RH:= RH - m.RH]
data[, i.CO2:= CO2 - m.CO2]
data[, i.H2O:= H2O - m.H2O]

# with(data, plot(i.Uz))
# with(data, plot(i.Ta, type="l"))
# data[i.Ta>10, unique(stretch)]
# 
# with(data, plot(i.RH, type="l"))
# data[i.RH>10, unique(stretch)]
# 
# with(data, plot(i.CO2, type="l"))
# with(data[stretch>4], plot(i.H2O, type="l")) #more tricky

mydata <- data[stretch %in% c(4:31)]
label.order <- unique(mydata$stretch.label)
mydata[, stretch.label:= factor(stretch.label, levels = label.order, labels=label.order)]

################################################################################
# w' vs T'
g.1 <- ggplot(mydata, aes( y = i.Uz, x= i.Ta))
g.1 <- (g.1
        + facet_wrap( ~ stretch.label, scales = "fixed", ncol=7)
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0, 0.5, 0, 0.1), "lines"),
                axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                panel.margin =       unit(-0.4, "lines"),
                strip.background =   element_blank(),
                strip.text.x =       element_text(size=4),
                # strip.switch.pad.wrap = unit(0.05, "cm"),
                plot.title = element_text(hjust = 0)
        )
        + xlab("T'")
        + ylab("w'")
        #          #         + labs(title = "(a)")
        #          + coord_cartesian(xlim= c(0, 100), ylim=c(0,100))
        + scale_x_continuous(breaks = c(-1,0,1))
        + scale_y_continuous(breaks = c(-2,0,2))
)

g.1 <- (g.1
        + geom_point(alpha=0.1, size=0.25)
        + geom_vline(xintercept = 0, col="red", size = 0.25)
        + geom_hline(yintercept = 0, col="red", size = 0.25)
        
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
# g.1
myplot <- "i.Ta_vs_i.Uz.png"
png(filename=myplot,  width = 160, height = 80, units = "mm", res=1200)
print(g.1)

dev.off()
rm(list="g.1")

################################################################################
# w' vs CO2'
g.1 <- ggplot(mydata, aes(y =i.Uz, x= i.CO2))
g.1 <- (g.1
        + facet_wrap( ~ stretch.label, scales = "fixed", ncol=7)
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0, 0.5, 0, 0.1), "lines"),
                axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                panel.margin =       unit(-0.4, "lines"),
                strip.background =   element_blank(),
                strip.text.x =       element_text(size=4),
                # strip.switch.pad.wrap = unit(0.05, "cm"),
                plot.title = element_text(hjust = 0)
        )
        + xlab("CO2'")
        + ylab("w'")
        #          #         + labs(title = "(a)")
        #          + coord_cartesian(xlim= c(0, 100), ylim=c(0,100))
        # + scale_y_continuous(breaks = c(-1,0,1))
        + scale_y_continuous(breaks = c(-2,0,2))
)

g.1 <- (g.1
        + geom_point(alpha=0.1, size=0.25)
        + geom_vline(xintercept = 0, col="red", size = 0.25)
        + geom_hline(yintercept = 0, col="red", size = 0.25)
)
# g.1
myplot <- "i.CO2_vs_i.Uz.png"
png(filename=myplot,  width = 160, height = 80, units = "mm", res=1200)
print(g.1)

dev.off()
rm(list="g.1")

# rm(list="mydata")
mytargets <- unique(mydata$file.label)
for(i in mytargets){
        target <- i
        myd <- mydata[file.label==target]
        source("TS_plots.R")
}
# 
# target <- "0600"
# myd <- mydata[file.label==target]
# source("TS_plots.R")
# 
# ################################################################################
# # 05:45 - 06:15
# ################################################################################
# myd <- data[stretch==26]
# myd
# 
# # w' vs T'
# g.1 <- ggplot(mydata, aes( y = i.Uz, x= i.Ta))
# g.1 <- (g.1
#         + theme_bw(base_size = 8)
#         + theme(plot.margin= unit(c(0, 0.5, 0, 0.1), "lines"),
#                 axis.ticks = element_line(size = 0.1),
#                 legend.position =   "none",
#                 panel.margin =       unit(-0.4, "lines"),
#                 strip.background =   element_blank(),
#                 strip.text.x =       element_text(size=4),
#                 # strip.switch.pad.wrap = unit(0.05, "cm"),
#                 plot.title = element_text(hjust = 0)
#         )
#         + xlab("T'")
#         + ylab("w'")
#         #          #         + labs(title = "(a)")
#         #          + coord_cartesian(xlim= c(0, 100), ylim=c(0,100))
#         + scale_x_continuous(breaks = c(-1,0,1))
#         + scale_y_continuous(breaks = c(-2,0,2))
# )
# 
# g.1 <- (g.1
#         + geom_point(alpha=0.1, size=1.5)
#         + geom_vline(xintercept = 0, col="red", size = 0.25)
#         + geom_hline(yintercept = 0, col="red", size = 0.25)
# )
# # g.1
# myplot <- "0545.0615_i.Ta_vs_i.Uz.png"
# png(filename=myplot,  width = 160, height = 80, units = "mm", res=300)
# print(g.1)
# 
# dev.off()
# rm(list="g.1")
# 
# 
# # w' vs CO2'
# g.1 <- ggplot(myd, aes(y =i.Uz, x= i.CO2))
# g.1 <- (g.1
#         + theme_bw(base_size = 8)
#         + theme(plot.margin= unit(c(0, 0.5, 0, 0.1), "lines"),
#                 axis.ticks = element_line(size = 0.1),
#                 legend.position =   "none",
#                 panel.margin =       unit(-0.4, "lines"),
#                 strip.background =   element_blank(),
#                 strip.text.x =       element_text(size=4),
#                 # strip.switch.pad.wrap = unit(0.05, "cm"),
#                 plot.title = element_text(hjust = 0)
#         )
#         + xlab("CO2'")
#         + ylab("w'")
#         #          #         + labs(title = "(a)")
#         #          + coord_cartesian(xlim= c(0, 100), ylim=c(0,100))
#         # + scale_y_continuous(breaks = c(-1,0,1))
#         + scale_y_continuous(breaks = c(-2,0,2))
# )
# 
# g.1 <- (g.1
#         + geom_point(alpha=0.1, size=1.5)
#         + geom_vline(xintercept = 0, col="red", size = 0.25)
#         + geom_hline(yintercept = 0, col="red", size = 0.25)
# )
# # g.1
# myplot <- "0545.0615_i.CO2_vs_i.Uz.png"
# png(filename=myplot,  width = 160, height = 80, units = "mm", res=300)
# print(g.1)
# 
# dev.off()
# rm(list="g.1")
# 
# ################################################################################
# # RAW
# # UZ
# g.1 <- ggplot(myd, aes(x= time, y = Uz))
# g.1 <- (g.1
#         + theme_bw(base_size = 8)
#         + theme(plot.margin= unit(c(0, 0.5, 0, 0.1), "lines"),
#                 axis.ticks = element_line(size = 0.1),
#                 axis.title.x = element_blank(),
#                 axis.text.x =element_blank(),
#                 legend.position =   "none",
#                 panel.margin =       unit(-0.4, "lines"),
#                 strip.background =   element_blank(),
#                 strip.text.x =       element_blank(),
#                 plot.title = element_text(hjust = 0)
#         )
#         + ylab("w [m/s]")
#         + xlab("time")
#         #          + coord_cartesian(xlim= c(0, 100), ylim=c(0,100))
#         #          + scale_x_continuous(breaks = c(0, 25, 50, 75, 100))
# )
# 
# g.1 <- (g.1
#         + geom_line(size=0.2)
# )
# g.1
# ##
# 
# # T
# g.2 <- ggplot(myd, aes(x= time, y = Ta))
# g.2 <- (g.2
#         + theme_bw(base_size = 8)
#         + theme(plot.margin= unit(c(0, 0.5, 0, 0.1), "lines"),
#                 axis.ticks = element_line(size = 0.1),
#                 axis.title.x = element_blank(),
#                 axis.text.x =element_blank(),
#                 legend.position =   "none",
#                 panel.margin =       unit(-0.4, "lines"),
#                 strip.background =   element_blank(),
#                 strip.text.x =       element_blank(),
#                 plot.title = element_text(hjust = 0)
#         )
#         + ylab("T [celsius]")
#         + xlab("time")
#         #          + coord_cartesian(xlim= c(0, 100), ylim=c(0,100))
#         #          + scale_x_continuous(breaks = c(0, 25, 50, 75, 100))
# )
# 
# g.2 <- (g.2
#         + geom_line(size=0.2)
# )
# g.2
# 
# # CO2
# g.3 <- ggplot(myd, aes(x= time, y = CO2))
# g.3 <- (g.3
#         + theme_bw(base_size = 8)
#         + theme(plot.margin= unit(c(0, 0.5, 0, 0.1), "lines"),
#                 axis.ticks = element_line(size = 0.1),
#                 legend.position =   "none",
#                 panel.margin =       unit(-0.4, "lines"),
#                 strip.background =   element_blank(),
#                 strip.text.x =       element_blank(),
#                 plot.title = element_text(hjust = 0)
#         )
#         + ylab("CO2 [mg-CO2/m3]")
#         + xlab("time")
#         #          + coord_cartesian(xlim= c(0, 100), ylim=c(0,100))
#         #          + scale_x_continuous(breaks = c(0, 25, 50, 75, 100))
# )
# 
# g.3 <- (g.3
#         + geom_line(size=0.2)
# )
# g.3
# 
# myplot <- "RAW_0545.0615.png"
# png(filename=myplot,  width = 160, height = 80, units = "mm", res=300)
# 
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(3, 1)))
# vplayout <- function(x, y)
#         viewport(layout.pos.row = x, layout.pos.col = y)
# 
# print(g.1, vp = vplayout(1, 1))
# print(g.2, vp = vplayout(2, 1))
# print(g.3, vp = vplayout(3, 1))
# 
# dev.off()
# 
# 
# ################################################################################
# # deviations
# # uz'
# g.1 <- ggplot(myd, aes(x= time, y = i.Uz))
# g.1 <- (g.1
#         + theme_bw(base_size = 8)
#         + theme(plot.margin= unit(c(0, 0.5, 0, 0.1), "lines"),
#                 axis.ticks = element_line(size = 0.1),
#                 axis.title.x = element_blank(),
#                 axis.text.x =element_blank(),
#                 legend.position =   "none",
#                 panel.margin =       unit(-0.4, "lines"),
#                 strip.background =   element_blank(),
#                 strip.text.x =       element_blank(),
#                 plot.title = element_text(hjust = 0)
#         )
#         + ylab("w' [m/s]")
#         + xlab("time")
#         #          + coord_cartesian(xlim= c(0, 100), ylim=c(0,100))
#         #          + scale_x_continuous(breaks = c(0, 25, 50, 75, 100))
# )
# 
# g.1 <- (g.1
#         + geom_line(size=0.2)
#         + geom_hline(yintercept=0, size=0.5, col="red")
# 
# )
# g.1
# ##
# 
# # T'
# g.2 <- ggplot(myd, aes(x= time, y = i.Ta))
# g.2 <- (g.2
#         + theme_bw(base_size = 8)
#         + theme(plot.margin= unit(c(0, 0.5, 0, 0.1), "lines"),
#                 axis.ticks = element_line(size = 0.1),
#                 axis.title.x = element_blank(),
#                 axis.text.x =element_blank(),
#                 legend.position =   "none",
#                 panel.margin =       unit(-0.4, "lines"),
#                 strip.background =   element_blank(),
#                 strip.text.x =       element_blank(),
#                 plot.title = element_text(hjust = 0)
#         )
#         + ylab("T' [celsius]")
#         + xlab("time")
#         #          + coord_cartesian(xlim= c(0, 100), ylim=c(0,100))
#         #          + scale_x_continuous(breaks = c(0, 25, 50, 75, 100))
# )
# 
# g.2 <- (g.2
#         + geom_line(size=0.2)
#         + geom_hline(yintercept=0, size=0.5, col="red")
# 
# )
# g.2
# 
# # CO2'
# g.3 <- ggplot(myd, aes(x= time, y = i.CO2))
# g.3 <- (g.3
#         + theme_bw(base_size = 8)
#         + theme(plot.margin= unit(c(0, 0.5, 0, 0.1), "lines"),
#                 axis.ticks = element_line(size = 0.1),
#                 legend.position =   "none",
#                 panel.margin =       unit(-0.4, "lines"),
#                 strip.background =   element_blank(),
#                 strip.text.x =       element_blank(),
#                 plot.title = element_text(hjust = 0)
#         )
#         + ylab("CO2' [mg-CO2/m3]")
#         + xlab("time")
#         #          + coord_cartesian(xlim= c(0, 100), ylim=c(0,100))
#         #          + scale_x_continuous(breaks = c(0, 25, 50, 75, 100))
# )
# 
# g.3 <- (g.3
#         + geom_line(size=0.2)
#         + geom_hline(yintercept=0, size=0.5, col="red")
# 
# )
# g.3
# 
# myplot <- "DEV_0545.0615.png"
# png(filename=myplot,  width = 160, height = 80, units = "mm", res=300)
# 
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(3, 1)))
# vplayout <- function(x, y)
#         viewport(layout.pos.row = x, layout.pos.col = y)
# 
# print(g.1, vp = vplayout(1, 1))
# print(g.2, vp = vplayout(2, 1))
# print(g.3, vp = vplayout(3, 1))
# 
# dev.off()
# 
# ################################################################################
# # edyy-covariance
# # T'
# g.2 <- ggplot(myd, aes(x= time, y = i.Ta * i.Uz))
# g.2 <- (g.2
#         + theme_bw(base_size = 8)
#         + theme(plot.margin= unit(c(0, 0.5, 0, 0.1), "lines"),
#                 axis.ticks = element_line(size = 0.1),
#                 axis.title.x = element_blank(),
#                 axis.text.x =element_blank(),
#                 legend.position =   "none",
#                 panel.margin =       unit(-0.4, "lines"),
#                 strip.background =   element_blank(),
#                 strip.text.x =       element_blank(),
#                 plot.title = element_text(hjust = 0)
#         )
#         + ylab("T' * w'")
#         + xlab("time")
#         #          + coord_cartesian(xlim= c(0, 100), ylim=c(0,100))
#         #          + scale_x_continuous(breaks = c(0, 25, 50, 75, 100))
# )
# 
# g.2 <- (g.2
#         + geom_line(size=0.2)
#         + geom_hline(yintercept=0, size=0.5, col="red")
#         
# )
# g.2
# 
# # CO2'
# g.3 <- ggplot(myd, aes(x= time, y = i.CO2 * i.Uz))
# g.3 <- (g.3
#         + theme_bw(base_size = 8)
#         + theme(plot.margin= unit(c(0, 0.5, 0, 0.1), "lines"),
#                 axis.ticks = element_line(size = 0.1),
#                 legend.position =   "none",
#                 panel.margin =       unit(-0.4, "lines"),
#                 strip.background =   element_blank(),
#                 strip.text.x =       element_blank(),
#                 plot.title = element_text(hjust = 0)
#         )
#         + ylab("CO2' * w'")
#         + xlab("time")
#         #          + coord_cartesian(xlim= c(0, 100), ylim=c(0,100))
#         #          + scale_x_continuous(breaks = c(0, 25, 50, 75, 100))
# )
# 
# g.3 <- (g.3
#         + geom_line(size=0.2)
#         + geom_hline(yintercept=0, size=0.5, col="red")
#         
# )
# g.3
# 
# myplot <- "eddy_cov_0545.0615.png"
# png(filename=myplot,  width = 160, height = 80, units = "mm", res=300)
# 
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(2, 1)))
# vplayout <- function(x, y)
#         viewport(layout.pos.row = x, layout.pos.col = y)
# 
# print(g.2, vp = vplayout(1, 1))
# print(g.3, vp = vplayout(2, 1))
# 
# dev.off()
# 
# 
