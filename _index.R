# cleanning of workspace and time recording
rm(list = ls())
print(Sys.time())
Sys.setenv(TZ='UTC') #--> very important

#1)   set common target input folder and target output folder (Use /, not \).
# input_path <- "G:/BioAtmo/zuazo-p/General/eclaire/Automatic system/OUTPUTS/drying_wetting"
# input_path_RAW <-  "G:/BioAtmo/zuazo-p/RAW data/drying_wetting"
# 
# output_path <- "G:/BioAtmo/zuazo-p/General/eclaire/Automatic system/OUTPUTS/drying_wetting/_all_fluxes"
# dir.create(output_path)


#2)   set desire date and time format (date_format) and date origin (date_origin) (IDASW and PICARRO)
#     see (http://www.stat.berkeley.edu/classes/s133/dates.html) for date formats
date_format<-"%Y-%m-%d %H:%M:%S"
date_origin<-"1970-01-01"
#     UTC (Coordinated Universal Time) offset of the winter-time of our time zone
#     data must be without DST(day saving time ) transition (e.g. always winter-time)
UTC_offset <- 3600


require(data.table)
require(flux)
require(lattice)
require(ggplot2)
require(multcomp)
require(car)
require(gridExtra)


require(reshape2)
require(zoo)
require(grid)