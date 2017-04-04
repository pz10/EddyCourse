data <- fread("TERENO_Garmisch_2013_FMF.csv")
data[, time:= as.POSIXct(MODELTIME, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]

data[data==-9999]<-NA
str(data)
setnames(data, "soiltemp_Avg", "Tsoil")

# check flags 
table(data$Flag.ustar)
table(data$Flag.wCO2)

# check NA values
mean(is.na(data$Flag.ustar))
mean(is.na(data$Flag.wCO2))
mean(is.na(data$NEE))
mean(is.na(data$Tsoil))
mean(is.na(data$PAR_Avg))

# take daily
data[,day:= format(time, "%Y-%m-%d")]
data[,day.NEE:= mean(NEE, na.rm=T), by=day]
data[,day.Tsoil:= mean(Tsoil, na.rm=T), by=day]
data[,day.PAR_Avg:= mean(PAR_Avg, na.rm=T), by=day]

daily <- data[,lapply(.SD, mean, na.rm=T), by=day, .SDcols= c("NEE", "Tsoil", "PAR_Avg")]
daily[is.na(daily)]<- NA

# take weekly
data[,week:= format(time, "%W")]
data[,week.NEE:= mean(NEE, na.rm=T), by=week]
data[,week.Tsoil:= mean(Tsoil, na.rm=T), by=week]
data[,week.PAR_Avg:= mean(PAR_Avg, na.rm=T), by=week]

weekly <- data[,lapply(.SD, mean, na.rm=T), by=week, .SDcols= c("NEE", "Tsoil", "PAR_Avg")]
weekly[is.na(weekly)]<- NA

# take hourly
data[,month.hour:= format(time, "%Y-%m %H")]
data[,mh.NEE:= mean(NEE, na.rm=T), by=month.hour]
data[,mh.Tsoil:= mean(Tsoil, na.rm=T), by=month.hour]
data[,mh.PAR_Avg:= mean(PAR_Avg, na.rm=T), by=month.hour]

hourly <- data[,lapply(.SD, mean, na.rm=T), by=month.hour, .SDcols= c("NEE", "Tsoil", "PAR_Avg")]
hourly[is.na(hourly)]<- NA



with(data, plot(m.Tsoil ~ time , type="l", main="daily mean"))
