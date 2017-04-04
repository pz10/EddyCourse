REM This script should be run after every successful scheduled LoggerNet data collection run.
REM
REM It does the following things
REM
REM 1) Splits the raw fast data file into single-hour data file(s), changing the cr5000 time stamp into a doy,hhmm,sec time stamp
REM 2) Splits the raw slow-data file into single-day data file(s), changing the cr5000 time stamp into a doy,hhmm time stamp
REM 3) writes a log file that keeps track of the time of the last data copied. In this way no duplicated data are saved
REM
REM
REM this works on the fast raw data
REM inp  : original raw data file
REM out  : resulting hourly data file name. The special character $ indicates that the name of the resulting file has time stamp information in it.
REM	 : $y2 : 2 digits year
REM	 : $jd : 3 digits DOY
REM      : $ho : 2 digits hour
REM      : time stamp information are taken from the cr5000 time stamp.
REM col	 : the column to be copied from the raw data into the new hourly files, starting from 1. For example, with a CSAT and a LI7500 you want to copy columns
REM	   from 3 to the end (11?) (column 1  is the cr5000 timstamp and col 2 is the cr5000 counter)
REM its	 : describes the raw data time stamp. In your case, "$c5" means cr5000 time stamp. And it should be in the first column.
REM ots  : output time stamp.
REM	 : $jd	: 3 digit hour
REM	 : $ho  : 2 digit hour
REM      : $mi  : 2 digit minute
REM      : $se  : 2 digit second
REM	 : the time stamp is always copied in the first columns
REM del  : 1 the original file is deleted after have been splited, 0= the original file is not deleted
REM log  : writes a log file that keeps track of the time of the last data copied. This may go togheter with the next parameter
REM ctm  : check time option. If 1, the program copies only lines that have a time stamp higher than the the previous data.
REM	   if 2, the program also saves the rejected lines in a file, wHICH name must be given as ctm=2,"filename". TimeStamp field characher $ can be used
REM
REM
 
HourlySplitData inp=G:\BioAtmo\zuazo-p\General\R\EddyCourse\TOA5_2833.ts_data.dat log=HSplitlogB.txt out=G:\BioAtmo\zuazo-p\General\R\EddyCourse\batched_data\Gap_$y2_$jd_$ho.dat col=3,4,5,6,7,8,9,10,11,12,13 its="$c5" ots="$c5," del=0 ctm=1

REM split row slow data files in daily data file(s)
REM same option as in HourlySplitData

REM DailySplitData inp=C:\Daten\Ro_LoggerC_Table3.dat log=DSplitlogB.txt out=C:\Daten\Rm_$y2$jd.dat col=3-79 its="$c5" ots="$c5," ctm=1, C:\Daten_Trash\Rm_$y2$jd.dat del=0

REM G:\BioAtmo\zuazo-p\General\R\EddyCourse