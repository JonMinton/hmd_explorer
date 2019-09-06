library(readxl)
library(tidyverse)
library(data.table)
library(writexl)

### 1. set to the directory where the input file is saved
setwd("P:/DATAPROD/LIFE TABLES/2015-2017/decomposition")

### 2. change to the name of the input file
dataset <- "decomposition input males.xlsx"

### 3. change nc to the number of columns in each sheet of the input file- they must all be the same. 
###do not include column names, read_excel automatically does this
nc <- c( 32, 32, 32)
nr <- c(20, 20, 20)

### 4. change these to the number of years between each dataset. i.e. 2000-02 to 2012-14= 12 years
yrsfirstperiod<-12
yrssecondperiod<-3

## reading in the data
for (i in 1:(length(excel_sheets(dataset)))) {
  print(paste(i, excel_sheets(dataset)[i]))
  assign(excel_sheets(dataset)[i],
         read_excel(dataset, sheet = i)[1:nr[i], 1:nc[i]])
}
firstperiod<-paste(year1$period[1]," to ",year2$period[1])  
secondperiod<-paste(year2$period[1]," to ",year3$period[1])

##creating dataframes with deaths by cause as a proportion
y<- ncol(year1)
propyear1=year1
for (i in c(7:y)) {
  propyear1[,i]<-year1[,i]/year1$deaths
}
propyear2=year2
for (i in c(7:y)) {
  propyear2[,i]<-year2[,i]/year2$deaths
}
propyear3=year3
for (i in c(7:y)) {
  propyear3[,i]<-year3[,i]/year3$deaths
}

##calculate life tables for 1st time period
year1<- arrange(year1,agegroup)
year1$mx<-year1$deaths/year1$population
year1$qx<-ifelse(year1$agegroup==1,year1$mx,
              ifelse(year1$agegroup==2,(2*4*year1$mx)/(2+(4*year1$mx)),
                     ifelse(year1$agegroup==20,1,(2*5*year1$mx)/(2+(5*year1$mx)) )))
year1$px<-1-year1$qx
year1$lx<-NA
year1$lx[1]=100000
for (i in 1:20){
  year1$lx<-ifelse(year1$agegroup==1,year1$lx, lag(year1$px, 1)*lag(year1$lx,1))}
year1$dx<-ifelse(year1$agegroup==20,year1$lx,year1$lx-lead(year1$lx))
year1$Lx<-ifelse(year1$agegroup==20,year1$lx/year1$mx,
              ifelse(year1$agegroup==1,(0.9*lead(year1$lx,1))+(0.1*year1$lx),
                     ifelse(year1$agegroup==2,(year1$lx+lead(year1$lx,1))*2,(year1$lx+lead(year1$lx,1))*2.5)))
year1$Tx<-ifelse(year1$agegroup==20,year1$Lx,NA)
for (i in 20:1) {
  year1$Tx<-ifelse(year1$agegroup==20,year1$Tx,year1$Lx+lead(year1$Tx,1))}
year1$ex<-year1$Tx/year1$lx

##calculate life tables for 2nd time period
year2<- arrange(year2,agegroup)
year2$mx<-year2$deaths/year2$population
year2$qx<-ifelse(year2$agegroup==1,year2$mx,
              ifelse(year2$agegroup==2,(2*4*year2$mx)/(2+(4*year2$mx)),
                     ifelse(year2$agegroup==20,1,(2*5*year2$mx)/(2+(5*year2$mx)) )))
year2$px<-1-year2$qx
year2$lx<-NA
year2$lx[1]=100000
for (i in 1:20){
  year2$lx<-ifelse(year2$agegroup==1,year2$lx, lag(year2$px, 1)*lag(year2$lx,1))}
year2$dx<-ifelse(year2$agegroup==20,year2$lx,year2$lx-lead(year2$lx))
year2$Lx<-ifelse(year2$agegroup==20,year2$lx/year2$mx,
              ifelse(year2$agegroup==1,(0.9*lead(year2$lx,1))+(0.1*year2$lx),
                     ifelse(year2$agegroup==2,(year2$lx+lead(year2$lx,1))*2,(year2$lx+lead(year2$lx,1))*2.5)))
year2$Tx<-ifelse(year2$agegroup==20,year2$Lx,NA)
for (i in 20:1) {
  year2$Tx<-ifelse(year2$agegroup==20,year2$Tx,year2$Lx+lead(year2$Tx,1))}
year2$ex<-year2$Tx/year2$lx


##calculate life tables for 3rd time period
year3<- arrange(year3,agegroup)
year3$mx<-year3$deaths/year3$population
year3$qx<-ifelse(year3$agegroup==1,year3$mx,
              ifelse(year3$agegroup==2,(2*4*year3$mx)/(2+(4*year3$mx)),
                     ifelse(year3$agegroup==20,1,(2*5*year3$mx)/(2+(5*year3$mx)) )))
year3$px<-1-year3$qx
year3$lx<-NA
year3$lx[1]=100000
for (i in 1:20){
  year3$lx<-ifelse(year3$agegroup==1,year3$lx, lag(year3$px, 1)*lag(year3$lx,1))}
year3$dx<-ifelse(year3$agegroup==20,year3$lx,year3$lx-lead(year3$lx))
year3$Lx<-ifelse(year3$agegroup==20,year3$lx/year3$mx,
              ifelse(year3$agegroup==1,(0.9*lead(year3$lx,1))+(0.1*year3$lx),
                     ifelse(year3$agegroup==2,(year3$lx+lead(year3$lx,1))*2,(year3$lx+lead(year3$lx,1))*2.5)))
year3$Tx<-ifelse(year3$agegroup==20,year3$Lx,NA)
for (i in 20:1) {
  year3$Tx<-ifelse(year3$agegroup==20,year3$Tx,year3$Lx+lead(year3$Tx,1))}
year3$ex<-year3$Tx/year3$lx


##begin decomposition 1 (between year1 and year2)
decomp1=propyear1
decomp1$all_causes<-ifelse(decomp1$agegroup==20,(year1$lx/100000)*((year2$Tx/year2$lx)-(year1$Tx/year1$lx)),
                         (year1$lx/100000)*((year2$Lx/year2$lx)-(year1$Lx/year1$lx))+(lead(year2$Tx,1)/100000)*((year1$lx/year2$lx)-(lead(year1$lx,1)/lead(year2$lx,1))))
y<-ncol(propyear1)
for (i in c(7:y)) {
  decomp1[,(i)]<-(decomp1$all_causes*(((propyear2[,i]*year2$mx)-(propyear1[,i]*year1$mx))/(year2$mx-year1$mx)))}
decomp1[,c(1,3,4,5,6)]=NULL

y<-ncol(decomp1)
totals<-colSums(decomp1[,c(2:y)])
totals<-append(0,totals)        
decomp1<-rbind(decomp1,totals)
decomp1$agegroup[21]<-"all ages"

##change decomposition to weeks per annum rather than years
y<-ncol(decomp1)
decomp1[2:y]<-(decomp1[2:y]*52)/yrsfirstperiod
decomp1$period<-firstperiod


#begin decomposition 2 (between year2 and year3)
decomp2=propyear2
decomp2$all_causes<-ifelse(decomp2$agegroup==20,(year2$lx/100000)*((year3$Tx/year3$lx)-(year2$Tx/year2$lx)),
                         (year2$lx/100000)*((year3$Lx/year3$lx)-(year2$Lx/year2$lx))+(lead(year3$Tx,1)/100000)*((year2$lx/year3$lx)-(lead(year2$lx,1)/lead(year3$lx,1))))
y<-ncol(propyear2)
for (i in c(7:y)) {
  decomp2[,(i)]<-(decomp2$all_causes*(((propyear3[,i]*year3$mx)-(propyear2[,i]*year2$mx))/(year3$mx-year2$mx)))}
decomp2[,c(1,3,4,5,6)]=NULL

y<-ncol(decomp2)
totals<-colSums(decomp2[,c(2:y)])
totals<-append(0,totals)        
decomp2<-rbind(decomp2,totals)
decomp2$agegroup[21]<-"all ages"

##change decomposition to weeks per annum rather than years
y<-ncol(decomp2)
decomp2[2:y]<-(decomp2[2:y]*52)/yrssecondperiod
decomp2$period<-secondperiod


##output files
y<-length(year1)
life_table_1<-year1[,-(7:(y-8))]
life_table_2<-year2[,-(7:(y-8))]
life_table_3<-year3[,-(7:(y-8))]

dc_by_age<-data.frame(decomp1$agegroup,decomp1$all_causes,decomp2$all_causes)
names(dc_by_age)<-c("agegroup",firstperiod,secondperiod)
dc_by_age$difference<- dc_by_age[,3]- dc_by_age[,2]

y=length(decomp1)
cause<-names(decomp1[2:(y-2)])
period1<-unname(unlist(decomp1[21,c(2:(y-2))]))
period2<-unname(unlist(decomp2[21,c(2:(y-2))]))
dc_by_cause<-data.frame(cause,period1,period2)
names(dc_by_cause)<-c("cause of death",firstperiod,secondperiod)
dc_by_cause$difference<-dc_by_cause[,3]- dc_by_cause[,2]

write_xlsx(list(life_table_1=life_table_1,life_table_2=life_table_2,life_table_3=life_table_3, decomp1=decomp1,decomp2=decomp2,dc_by_age=dc_by_age,dc_by_cause=dc_by_cause),"decomposition output males.xlsx")
