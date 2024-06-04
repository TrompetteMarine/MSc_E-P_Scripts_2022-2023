####################################################################################
# Homework: Applied MicroEconometrics C-Econometric Modeling II : by year 2015-2020#
####################################################################################

#Libraries
library(tidyverse)
library(readr)
library(readxl)
library(bootstrap)

#Data Loading

#Year2016 <- read_excel("Desktop/HW-MicroMetrics/Data/InterData/DataByYear.xlsx", +sheet = "2016")
#Year2018 <- read_excel("Desktop/HW-MicroMetrics/Data/InterData/DataByYear.xlsx", +sheet = "2018")
#Year2020 <- read_excel("Desktop/HW-MicroMetrics/Data/InterData/DataByYear.xlsx", +sheet = "2020")
FrMap2 <- read_csv("Desktop/france-metrop-departements-from-voronoi-cog-2022-table.csv")

#Last manipulations
Year2016$D_id= NA
Year2016$H_id= NA
Year2016$Med_id= NA

Year2018$D_id= NA
Year2018$H_id= NA
Year2018$Med_id= NA

Year2020$D_id= NA
Year2020$H_id= NA
Year2020$Med_id= NA

#Indexes normalized by national average
for(i in 1:dim(Year2016)[1]){
  Year2016$D_id[i]  = (Year2016$AC[i]-min(Year2016$AC))/(mean(Year2016$AC)-min(Year2016$AC))
  Year2016$H_id[i]  = (Year2016$Hexp[i]-min(Year2016$Hexp))/(mean(Year2016$Hexp)-min(Year2016$Hexp))
  Year2016$Med_id[i]= (Year2016$MedRev[i]-min(Year2016$MedRev))/(mean(Year2016$MedRev)-min(Year2016$MedRev))
  
  Year2018$D_id[i]  = (Year2018$AC[i]-min(Year2018$AC))/(mean(Year2018$AC)-min(Year2018$AC))
  Year2018$H_id[i]  = (Year2018$Hexp[i]-min(Year2018$Hexp))/(mean(Year2018$Hexp)-min(Year2018$Hexp))
  Year2018$Med_id[i]= (Year2018$MedRev[i]-min(Year2018$MedRev))/(mean(Year2018$MedRev)-min(Year2018$MedRev))
  
  Year2020$D_id[i]  = (Year2020$AC[i]-min(Year2020$AC))/(mean(Year2020$AC)-min(Year2020$AC))
  Year2020$H_id[i]  = (Year2020$Hexp[i]-min(Year2020$Hexp))/(mean(Year2020$Hexp)-min(Year2020$Hexp))
  Year2020$Med_id[i]= (Year2020$MedRev[i]-min(Year2020$MedRev))/(mean(Year2020$MedRev)-min(Year2020$MedRev))
}

#Model 

Model2016=lm(D_id~H_id:N,data=Year2016)
Model2018=lm(D_id~H_id:N,data=Year2018)
Model2020=lm(D_id~H_id:N,data=Year2020)

Beta_hat2016=function(index,Year2016){
  boot_data= Year2016[index,]
  m2016= lm(D_id~H_id:N,data=boot_data)
  return(m2016$coefficients[2])
}

Beta_hat2018=function(index,Year2018){
  boot_data= Year2018[index,]
  m2018= lm(D_id~H_id:N,data=boot_data)
  return(m2018$coefficients[2])
}

Beta_hat2020=function(index,Year2020){
  boot_data= Year2020[index,]
  m= lm(D_id~H_id:N,data=boot_data)
  return(m2020$coefficients[2])
}
n=nrow(Year2016)
index= sample(1:n, replace=TRUE)

lm_boot2016=bootstrap(1:n,Beta_hat2016,nboot = 1000,Year2016)
lm_boot2018=bootstrap(1:n,Beta_hat2016,nboot = 1000,Year2018)
lm_boot2020=bootstrap(1:n,Beta_hat2016,nboot = 1000,Year2020)
#Bootstrap linear models for better estimations of CI:


mu2016=mean(lm_boot2016$thetastar)
sig2016=sd(lm_boot2016$thetastar)

mu2018=mean(lm_boot2018$thetastar)
sig2018=sd(lm_boot2018$thetastar)

mu2020=mean(lm_boot2020$thetastar)
sig2020=sd(lm_boot2020$thetastar)

#95 %CI is: 

CI2016=c(mu2016-1.96*sig2016,mu2016+1.96*sig2016)
CI2018=c(mu2018-1.96*sig2018,mu2018+1.96*sig2018)
CI2020=c(mu2020-1.96*sig2020,mu2020+1.96*sig2020)


#Extraction of the c parameter
c2016=as.vector(Model2016$coefficients[1:94]) # give the shape of the utility curve u(x)= [1-exp(cx)]/[1-exp(x)]
c2018=as.vector(Model2018$coefficients[1:94])
c2020=as.vector(Model2020$coefficients[1:94])


#computing utilty 
U2016= vector("numeric", n)
U2018= vector("numeric", n)
U2020= vector("numeric", n)

for(i in 1:n){
  U2016[i]= (1-exp(c2016[i]*Year2016$H_id[i]))/(1-exp(Year2016$H_id[i]))
  U2018[i]= (1-exp(c2018[i]*Year2018$H_id[i]))/(1-exp(Year2018$H_id[i]))
  U2020[i]= (1-exp(c2020[i]*Year2020$H_id[i]))/(1-exp(Year2020$H_id[i]))
}

#Marginal Utility
MU2016= vector("numeric", n)
MU2018= vector("numeric", n)
MU2020= vector("numeric", n)

for( i in 1:94){
  MU2016[i]=((c2016[i]-1)*exp(c2016[i]*Year2016$H_id[i]+Year2016$H_id[i])-c2016[i]*exp(c2016[i]*Year2016$H_id[i])+exp(Year2016$H_id[i]))/((1-exp(Year2016$H_id[i]))^2)
  MU2018[i]=((c2016[i]-1)*exp(c2018[i]*Year2018$H_id[i]+Year2018$H_id[i])-c2018[i]*exp(c2018[i]*Year2018$H_id[i])+exp(Year2018$H_id[i]))/((1-exp(Year2018$H_id[i]))^2)
  MU2020[i]=((c2020[i]-1)*exp(c2020[i]*Year2020$H_id[i]+Year2020$H_id[i])-c2020[i]*exp(c2020[i]*Year2020$H_id[i])+exp(Year2020$H_id[i]))/((1-exp(Year2020$H_id[i]))^2)
  
}

Mu2016=vector("numeric",96)
Mu2018=vector("numeric",96)
Mu2020=vector("numeric",96)
for( i in 1:28){
  Mu2016[i]=MU2016[i]
  Mu2018[i]=MU2018[i]
  Mu2020[i]=MU2020[i]
}
for(i in 29:29){
  Mu2016[i]=2/3*(MU2016[28]) #disaggregation of 2A and 2B
  Mu2018[i]=2/3*(MU2018[28])
  Mu2020[i]=2/3*(MU2020[28])
}

for(i in 30:30){
  Mu2016[i]=1/3*(MU2016[28]) #disaggregation of 2A and 2B
  Mu2018[i]=1/3*(MU2018[28])
  Mu2020[i]=1/3*(MU2020[28])
}

for(i in 31:85){
  Mu2016[i]=MU2016[i-2]
  Mu2018[i]=MU2018[i-2]
  Mu2020[i]=MU2020[i-2]
}

for(i in 86:87){
  Mu2016[i]=MU2016[85]/2 # Disagregation of Haute Vienne and Vienne
  Mu2018[i]=MU2018[85]/2
  Mu2020[i]=MU2020[85]/2
}

for(i in 88:96){
  Mu2016[i]=MU2016[i-2]
  Mu2018[i]=MU2018[i-2]
  Mu2020[i]=MU2020[i-2]
}

#Final Data outputs

FrMap2$MU2016=Mu2016
FrMap2$MU2018=Mu2018
FrMap2$MU2020=Mu2020


#Export Final Outputs:

write.csv(FrMap2,file="/Users/gabrielbontemps/desktop/FrMap2.csv")
