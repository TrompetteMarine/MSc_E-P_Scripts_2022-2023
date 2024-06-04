##############################################################
# Homework: Applied MicroEconometrics C-Econometric Modeling #
##############################################################

#Libraries
library(tidyverse)
library(readr)
library(bootstrap)
#Data Loading

Data <- read_csv("Desktop/Data.csv")
FrMap<- read_csv("Desktop/france-metrop-departements-from-voronoi-cog-2022-table.csv")
Data= Data[,-1]

#Last manipulations
Data$D_id= NA
Data$H_id= NA
Data$Med_id= NA

#Indexes normalized by national average
for(i in 1:dim(Data)[1]){
  Data$D_id[i] = (Data$Death[i]-min(Data$Death))/(mean(Data$Death)-min(Data$Death))
  Data$H_id[i] = (Data$Hexp[i]-min(Data$Hexp))/(mean(Data$Hexp)-min(Data$Hexp))
  Data$Med_id[i]= (Data$MedRev[i]-min(Data$MedRev))/(mean(Data$MedRev)-min(Data$MedRev))
}

#Model 

Model=lm(D_id~H_id:N,data=Data)

Beta_hat=function(index,Data){
  boot_data= Data[index,]
  m= lm(D_id~H_id:N,data=Data)
  return(m$coefficients[2])
}
n=nrow(Data)
index= sample(1:n, replace=TRUE)
lm_boot=bootstrap(1:n,Beta_hat,nboot = 1000,Data)


#Bootstrap linear models for better estimations of CI:


mean(lm_boot$thetastar)
sd(lm_boot$thetastar)

#95 %CI is: 

CI=c(mean(lm_boot$thetastar)-1.96*sd(lm_boot$thetastar),mean(lm_boot$thetastar)+1.96*sd(lm_boot$thetastar))

#Extraction of the c parameter
c=as.vector(Model$coefficients[1:94]) # give the shape of the utility curve u(x)= [1-exp(cx)]/[1-exp(x)]

#computing utilty 
U= vector("numeric", n)
for(i in 1:n){
  U[i]= (1-exp(c[i]*Data$H_id[i]))/(1-exp(Data$H_id[i]))
}
U[is.na(U)]= mean(U[!is.na(U)]) #getting rid of the NA without changing anything 
muU= (1-exp(mean(lm_boot$thetastar)*mean(Data$H_id)))/(1-exp(mean(Data$H_id)))

sdU=sd(U)

#Statistically significant deviant to the national average aka where MU of health expenses is really usefull
N=Data$N
t=(U-muU)/sdU
pnorm(t)

MU= vector("numeric",94) #First derivative of u(x)
for( i in 1:94){
  MU[i]=((c[i]-1)*exp(c[i]*Data$H_id[i]+Data$H_id[i])-c[i]*exp(c[i]*Data$H_id[i])+exp(Data$H_id[i]))/((1-exp(Data$H_id[i]))^2)
}

Mu=vector("numeric",96)
for( i in 1:28){
  Mu[i]=MU[i]
}
for(i in 29:29){
  Mu[i]=2/3*(MU[28]) #disaggregation of 2A and 2B
}

for(i in 30:30){
  Mu[i]=1/3*(MU[28]) #disaggregation of 2A and 2B
}

for(i in 31:85){
  Mu[i]=MU[i-2]
}

for(i in 86:87){
  Mu[i]=MU[85]/2 # Disagregation of Haute Vienne and Vienne
}

for(i in 88:96){
  Mu[i]=MU[i-2]
}

Mu[is.na(Mu)]= 0
Lib=FrMap$LIBGEO

FrMap$MU=Mu

write.csv(FrMap,file="/Users/gabrielbontemps/desktop/FrMap.csv")



