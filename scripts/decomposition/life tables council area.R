library(data.table)
library(dplyr)
library(reshape2)

#set the working directory where the input files are stored
setwd("P:/DATAPROD/LIFE TABLES/2015-2017/processing/running LE and HLE")

#read in the population and deaths file (for three years)
CA<-read.csv("popdeaths/CApopdeaths.csv")

####life expectancy processing####

CA$Period<-"2015-2017"

##ax is the assumed proportion of life spent in an age interval before death. 
##It is assumed that half the age interval is lived before death. Hence ax=0.5
##ax is set to 0.1 for under 1's to be consistent with official ONS Life Expectancy calculation.
#if statement. similar to IF in excel: If ageband is '1' then ax is '0.1' else ax is '0.5'
CA$ax <- ifelse (CA$x == "1", 0.1, 0.5)


#Age-specific death rate.
CA$mx <- CA$deaths/CA$population

# n is the Width of the age intervals used in this abridged life table.
CA$n <- ifelse (CA$x ==1, 1, 
                ifelse (CA$x == 2, 4, 
                        ifelse (CA$x == 20, 2/CA$mx,5 )))




# Conditional probability that an individual who has survived to start of the age interval will die in the interval.
CA$qx <- ifelse (CA$x == 20, 1, (CA$n * CA$mx)/(1 + CA$n *(1-CA$ax)*CA$mx))

# Conditional probability that an individual entering the age interval will survive the age interval.
CA$px <- 1 - CA$qx

##lx is an arbitrary starting population. This is often assumed to be 100,000 at birth 
##(or any given age we wish to start the lifetables at).The probablility of survival 
##is applied to lx to predict the number of people surviving to successive age intervals from a given age
#if statement to initialise the variable lx
CA$lx <- ifelse(CA$x == 1, 100000,0) #if ageband = 1 then set to 100000, else 0

# the rest of the variable CA lx is populated using a loop and uses the SHIFT function. This allows you to use the data in the line above. 
for (i in 1:20){CA$lx <-ifelse(CA$x == 1,CA$lx, shift(CA$px, 1, type = "lag")*shift(CA$lx, 1, type = "lag"))}

#re-sort the data in descending agebands, it is necessary to calculate person years at risk.  
CA<-CA[with(CA,order(Geog,Sex_Code,-x)), ]

# dx= Number of life table deaths in the age interval
CA$dx <- ifelse(CA$x ==20, CA$lx, 0)
for (i in 1:20){CA$dx <-ifelse(CA$x == 20,CA$lx, CA$lx-shift(CA$lx, 1, type = "lag"))}

# Person years lived at age x;

CA$Lx <- ifelse(CA$x ==20, CA$lx/CA$mx, 0)
for (i in 1:20){CA$Lx <-ifelse(CA$x == 20,CA$Lx, CA$n*((shift(CA$lx, 1, type = "lag"))+(CA$ax*CA$dx)))}

# Total person years lived. 
CA$Tx <- ifelse(CA$x ==20, CA$Lx, NA)
for (i in 1:20){CA$Tx <-ifelse(CA$x == 20,CA$Tx, CA$Lx+shift(CA$Tx, 1, type = "lag"))}

# Life expectancy 

CA$ex<- CA$Tx/CA$lx

#\\\\\\\\\\\\\\\\\\\\calculating confidence intervals of LE \\\\\\\\\\\\\\\\\\\\\\\\\\\\

CA$varqx <- ifelse (CA$x == 20, (4/(CA$deaths*(CA$mx^2))),(CA$n^2*CA$mx*(1-CA$ax*CA$n*CA$mx))/(CA$population*(1+(1-CA$ax)*CA$n*CA$mx)^3))



CA$P1 <- ifelse(CA$x == 20, (((CA$lx/2)^2)*CA$varqx),(CA$lx^2)*(((1-CA$ax)*CA$n+shift(CA$ex, 1, type = "lag"))^2)*CA$varqx)

for (i in 1:20) {CA$P2 <- ifelse(CA$x == 20, CA$P1, CA$P1+shift(CA$P2, 1, type = "lag"))}



CA$varex <-CA$P2/CA$lx^2

CA$SE <- sqrt(CA$varex)

CA$Lower_LE <- round(CA$ex-CA$SE*1.96, digits = 5)

CA$Upper_LE <- round(CA$ex+CA$SE*1.96, digits = 5)

CA<-CA[with(CA,order(Period,Geog,Sex_Code,x)), ]

