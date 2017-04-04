################################################################################
# a given stretch
################################################################################
# w' vs T'
g.1 <- ggplot(myd, aes( y = i.Uz, x= i.Ta))
g.1 <- (g.1
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
        + geom_point(alpha=0.1, size=1.5)
        + geom_vline(xintercept = 0, col="red", size = 0.25)
        + geom_hline(yintercept = 0, col="red", size = 0.25)
)
# g.1
myplot <- paste0("1st_analysis/",target,"_i.Ta_vs_i.Uz.png")

png(filename=myplot,  width = 160, height = 80, units = "mm", res=300)
print(g.1)

dev.off()
rm(list="g.1")


# w' vs CO2'
g.1 <- ggplot(myd, aes(y =i.Uz, x= i.CO2))
g.1 <- (g.1
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
        + geom_point(alpha=0.1, size=1.5)
        + geom_vline(xintercept = 0, col="red", size = 0.25)
        + geom_hline(yintercept = 0, col="red", size = 0.25)
)
# g.1
myplot <- paste0("1st_analysis/",target,"_i.CO2_vs_i.Uz.png")
png(filename=myplot,  width = 160, height = 80, units = "mm", res=300)
print(g.1)

dev.off()
rm(list="g.1")

################################################################################
# RAW
# UZ
g.1 <- ggplot(myd, aes(x= time, y = Uz))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0, 0.5, 0, 0.1), "lines"),
                axis.ticks = element_line(size = 0.1),
                axis.title.x = element_blank(),
                axis.text.x =element_blank(),
                legend.position =   "none",
                panel.margin =       unit(-0.4, "lines"),
                strip.background =   element_blank(),
                strip.text.x =       element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab("w [m/s]")
        + xlab("time")
        #          + coord_cartesian(xlim= c(0, 100), ylim=c(0,100))
        #          + scale_x_continuous(breaks = c(0, 25, 50, 75, 100))
)

g.1 <- (g.1
        + geom_line(size=0.2)
)
g.1
##

# T
g.2 <- ggplot(myd, aes(x= time, y = Ta))
g.2 <- (g.2
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0, 0.5, 0, 0.1), "lines"),
                axis.ticks = element_line(size = 0.1),
                axis.title.x = element_blank(),
                axis.text.x =element_blank(),
                legend.position =   "none",
                panel.margin =       unit(-0.4, "lines"),
                strip.background =   element_blank(),
                strip.text.x =       element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab("T [celsius]")
        + xlab("time")
        #          + coord_cartesian(xlim= c(0, 100), ylim=c(0,100))
        #          + scale_x_continuous(breaks = c(0, 25, 50, 75, 100))
)

g.2 <- (g.2
        + geom_line(size=0.2)
)
g.2

# CO2
g.3 <- ggplot(myd, aes(x= time, y = CO2))
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
        + ylab("CO2 [mg-CO2/m3]")
        + xlab("time")
        #          + coord_cartesian(xlim= c(0, 100), ylim=c(0,100))
        #          + scale_x_continuous(breaks = c(0, 25, 50, 75, 100))
)

g.3 <- (g.3
        + geom_line(size=0.2)
)
g.3

myplot <- paste0("1st_analysis/RAW_",target,".png")
png(filename=myplot,  width = 160, height = 80, units = "mm", res=300)

grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 1)))
vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)

print(g.1, vp = vplayout(1, 1))
print(g.2, vp = vplayout(2, 1))
print(g.3, vp = vplayout(3, 1))

dev.off()


################################################################################
# deviations
# uz'
g.1 <- ggplot(myd, aes(x= time, y = i.Uz))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0, 0.5, 0, 0.1), "lines"),
                axis.ticks = element_line(size = 0.1),
                axis.title.x = element_blank(),
                axis.text.x =element_blank(),
                legend.position =   "none",
                panel.margin =       unit(-0.4, "lines"),
                strip.background =   element_blank(),
                strip.text.x =       element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab("w' [m/s]")
        + xlab("time")
        #          + coord_cartesian(xlim= c(0, 100), ylim=c(0,100))
        #          + scale_x_continuous(breaks = c(0, 25, 50, 75, 100))
)

g.1 <- (g.1
        + geom_line(size=0.2)
        + geom_hline(yintercept=0, size=0.5, col="red")
        
)
g.1
##

# T'
g.2 <- ggplot(myd, aes(x= time, y = i.Ta))
g.2 <- (g.2
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0, 0.5, 0, 0.1), "lines"),
                axis.ticks = element_line(size = 0.1),
                axis.title.x = element_blank(),
                axis.text.x =element_blank(),
                legend.position =   "none",
                panel.margin =       unit(-0.4, "lines"),
                strip.background =   element_blank(),
                strip.text.x =       element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab("T' [celsius]")
        + xlab("time")
        #          + coord_cartesian(xlim= c(0, 100), ylim=c(0,100))
        #          + scale_x_continuous(breaks = c(0, 25, 50, 75, 100))
)

g.2 <- (g.2
        + geom_line(size=0.2)
        + geom_hline(yintercept=0, size=0.5, col="red")
        
)
g.2

# CO2'
g.3 <- ggplot(myd, aes(x= time, y = i.CO2))
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
        + ylab("CO2' [mg-CO2/m3]")
        + xlab("time")
        #          + coord_cartesian(xlim= c(0, 100), ylim=c(0,100))
        #          + scale_x_continuous(breaks = c(0, 25, 50, 75, 100))
)

g.3 <- (g.3
        + geom_line(size=0.2)
        + geom_hline(yintercept=0, size=0.5, col="red")
        
)
g.3

myplot <- paste0("1st_analysis/DEV_",target,".png")
png(filename=myplot,  width = 160, height = 80, units = "mm", res=300)

grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 1)))
vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)

print(g.1, vp = vplayout(1, 1))
print(g.2, vp = vplayout(2, 1))
print(g.3, vp = vplayout(3, 1))

dev.off()

################################################################################
# edyy-covariance
# T'
g.2 <- ggplot(myd, aes(x= time, y = i.Ta * i.Uz))
g.2 <- (g.2
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0, 0.5, 0, 0.1), "lines"),
                axis.ticks = element_line(size = 0.1),
                axis.title.x = element_blank(),
                axis.text.x =element_blank(),
                legend.position =   "none",
                panel.margin =       unit(-0.4, "lines"),
                strip.background =   element_blank(),
                strip.text.x =       element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab("T' * w'")
        + xlab("time")
        #          + coord_cartesian(xlim= c(0, 100), ylim=c(0,100))
        #          + scale_x_continuous(breaks = c(0, 25, 50, 75, 100))
)

g.2 <- (g.2
        + geom_line(size=0.2)
        + geom_hline(yintercept=0, size=0.5, col="red")
        
)
g.2

# CO2'
g.3 <- ggplot(myd, aes(x= time, y = i.CO2 * i.Uz))
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
        + ylab("CO2' * w'")
        + xlab("time")
        #          + coord_cartesian(xlim= c(0, 100), ylim=c(0,100))
        #          + scale_x_continuous(breaks = c(0, 25, 50, 75, 100))
)

g.3 <- (g.3
        + geom_line(size=0.2)
        + geom_hline(yintercept=0, size=0.5, col="red")
        
)
g.3

myplot <- paste0("1st_analysis/COV_",target,".png")
png(filename=myplot,  width = 160, height = 80, units = "mm", res=300)

grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 1)))
vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)

print(g.2, vp = vplayout(1, 1))
print(g.3, vp = vplayout(2, 1))

dev.off()