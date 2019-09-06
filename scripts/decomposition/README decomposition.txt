Before running the R code

Set up the input file in the same way as the template. There should be 20 rows, 1 for each age group. The first 6 columns should be sex, agegroup, population, 
deaths from all causes, area and time period. The following columns are for deaths by cause, the number of causes can vary but they must sum to the deaths
by all cause column.

In the R-script, set the working directory, input file amd specify the number of columns on each sheet of the input file. input the number of years being 
compared for each decomposition. e.g. if decomposition 1 covers 2000-2002 to 2012-2014 and decomposition 2 covers 2012-2014 to 2015-2017 then 

yrsfirstperiod<-12
yrssecondperiod<-3

run the rest of the code. The results of the decomposition will appear in the working directory