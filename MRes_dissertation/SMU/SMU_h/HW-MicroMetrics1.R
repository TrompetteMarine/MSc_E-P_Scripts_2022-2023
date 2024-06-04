#############################################################
# Homework: Applied MicroEconometrics A- Cleaning and stuff#
############################################################

#Libraries & Loading data
#-----------------------------------------------

#Libraries
library(tidyverse)
library(readxl)

#Loading data
Dataset <- read_excel("Desktop/HW-MicroMetrics/Data/InputData/Dataset.xlsx")
Dt <- read_excel("Desktop/HW-MicroMetrics/Data/InputData/Dataset.xlsx")
#Descriptive Statistics and Exploration Analysis
#-----------------------------------------------

Dataset$year   =as.numeric(Dataset$year)
Dataset$codgeo =as.numeric(Dataset$codgeo)
name= Dataset$libgeo
Dataset$libgeo=NULL

#Clean the data:
df=Dataset
#Missing values
sapply(df, function(x) sum(is.na(x)))
m=dim(Dataset)[2]
f= vector("numeric",m)

for(i in 1:m){
  if(sum(is.na(Dataset[,i]))>12){
    f[i]=i
  }else{f[i]=NA}
}
f=f[!is.na(f)]

df=Dataset[,-f]
df$name = name
df=drop_na(df,codgeo)
df$codgeo=NULL

#Exploration
reg= df
reg$name= NULL
reg$year=NULL
reg$No_pathology_identified=NULL

Npi=df$No_pathology_identified
year= df$year
dep= df$name
dset= data.frame(year,reg,dep)
Dep= dep[1:97]


#Aggregation of cases by department and by year: 

N=df$name[df$year==2015]


df2015= df[df$year==2015,]
df2015$year=NULL
df2015$name=NULL
df2015$case2015=rowSums(df2015)

df2016= df[df$year==2016,]
df2016$year=NULL
df2016$name=NULL
df2016$case2016=rowSums(df2016)

df2017= df[df$year==2017,]
df2017$year=NULL
df2017$name=NULL
df2017$case2017=rowSums(df2017)

df2018= df[df$year==2018,]
df2018$year=NULL
df2018$name=NULL
df2018$case2018=rowSums(df2018)

df2019= df[df$year==2019,]
df2019$year=NULL
df2019$name=NULL
df2019$case2019=rowSums(df2019)

df2020= df[df$year==2020,]
df2020$year=NULL
df2020$name=NULL
df2020$case2020=rowSums(df2020)

#Aggregated "Cases"
y2015=df2015$case2015
y2016=df2016$case2016
y2017=df2017$case2017
y2018=df2018$case2018
y2019=df2019$case2019
y2020=df2020$case2020

Death=vector("numeric",length=length(y2015))
for(i in 1:length(y2015)){
  Death[i]=(y2015[i]+y2016[i]+y2017[i]+y2018[i]+y2019[i]+y2020[i])/6
}

Tcase= data.frame(N,Death)

y2015=data.frame(N,y2015)
y2016=data.frame(N,y2016)
y2017=data.frame(N,y2017)
y2018=data.frame(N,y2018)
y2019=data.frame(N,y2019)
y2020=data.frame(N,y2020)
#Export CSV

write.csv(y2015,file=".../HW-MicroMetrics/Data/Interdata/y2015.csv")
write.csv(y2016,file=".../HW-MicroMetrics/Data/Interdata/y2016.csv")
write.csv(y2017,file=".../HW-MicroMetrics/Data/Interdata/y2017.csv")
write.csv(y2018,file=".../HW-MicroMetrics/Data/Interdata/y2018.csv")
write.csv(y2019,file=".../HW-MicroMetrics/Data/Interdata/y2019.csv")
write.csv(y2020,file=".../HW-MicroMetrics/Data/Interdata/y2020.csv")
write.csv(Tcase,file=".../HW-MicroMetrics/Data/InputData/Tcase.csv")
