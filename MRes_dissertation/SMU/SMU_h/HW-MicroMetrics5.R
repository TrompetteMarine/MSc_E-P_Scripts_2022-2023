##############################################################################################
# Homework: Applied MicroEconometrics C-Econometric Modeling III : MicroSimulation Framework #
##############################################################################################

#Libraries
library(tidyverse)
library(readr)
library(readxl)
library(bootstrap)
library(ggplot2)
#Load Data & Manipulation
#-------------------------------
Data <- read_csv("Desktop/HW-MicroMetrics/Data/InputData/Data.csv")
FrMap3 <- read_csv("Desktop/HW-MicroMetrics/Data/InputData/france-metrop-departements-from-voronoi-cog-2022-table.csv")
#Assume median household is risk averse Regarding Health expenses estimation 
#from Budget santé des Ménages:
#Set up 
n= nrow(Data)
HexpHousehold= vector("numeric",n)

for(i in 1:n){
  if(Data$MedRev[i] <20000){
    HexpHousehold[i]= 385+ rnorm(1,10,5)
  }
  if(Data$MedRev[i] >=20000 & Data$MedRev[i] <21000){
    HexpHousehold[i]= 407+rnorm(1,10,5)
  }
  if(Data$MedRev[i] >=21000 & Data$MedRev[i] <22000){
    HexpHousehold[i]= 510+rnorm(1,10,5)
  }
  if(Data$MedRev[i] >=22000 & Data$MedRev[i] <23000){
    HexpHousehold[i]= 535+rnorm(1,10,5)
  }
  if(Data$MedRev[i] >=23000){
    HexpHousehold[i]= 625+rnorm(1,10,5)
  }
}

# Matching 
#-------------------------------
HHexp = vector("numeric",n+2)
AHexp = vector("numeric",n+2)
MdR = vector("numeric",n+2)
D = vector("numeric",n+2)


for( i in 1:28){
  HHexp[i]=HexpHousehold[i]
  AHexp[i]=Data$Hexp[i]
  MdR[i]= Data$MedRev[i]
  D[i]= Data$Death[i]
}
for(i in 29:29){ #disaggregation of 2A and 2B
  HHexp[i]=2/3*HexpHousehold[i]
  AHexp[i]=2/3*Data$Hexp[i]
  MdR[i]= 2/3*Data$MedRev[i]
  D[i]= 2/3*Data$Death[i] 
}

for(i in 30:30){#disaggregation of 2A and 2B
  HHexp[i]=1/3*HexpHousehold[i]
  AHexp[i]=1/3*Data$Hexp[i]
  MdR[i]= 1/3*Data$MedRev[i]
  D[i]= 1/3*Data$Death[i]
}

for(i in 31:85){
  HHexp[i]=HexpHousehold[i-2]
  AHexp[i]=Data$Hexp[i-2]
  MdR[i]= Data$MedRev[i-2]
  D[i]= Data$Death[i-2]
}

for(i in 86:87){
  HHexp[i]=HexpHousehold[85]/2
  AHexp[i]=Data$Hexp[85]/2
  MdR[i]= Data$MedRev[85]/2
  D[i]= Data$Death[85]/2 # Disagregation of Haute Vienne and Vienne
}

for(i in 88:96){
  HHexp[i]=HexpHousehold[i-2]
  AHexp[i]=Data$Hexp[i-2]
  MdR[i]= Data$MedRev[i-2]
  D[i]= Data$Death[i-2]
}


#Final set up 
FrMap3$MdR=MdR
FrMap3$HHexp=HHexp
FrMap3$D = D
FrMap3$AHexp =1000*AHexp

FrMap3$D_dv = (FrMap3$D-min(FrMap3$D))/(mean(FrMap3$D)-min(FrMap3$D))
FrMap3$Hl_iv = (FrMap3$AHexp/FrMap3$POPULATION+FrMap3$HHexp-min(FrMap3$AHexp/FrMap3$POPULATION+FrMap3$HHexp))/(mean(FrMap3$AHexp/FrMap3$POPULATION+FrMap3$HHexp)-min(FrMap3$AHexp/FrMap3$POPULATION+FrMap3$HHexp))

FrMap3$Treated = ifelse(FrMap3$UID%%2==0,1,0)
FrMap3$Control =1 -FrMap3$Treated 
FrMap3$Policy1 = ifelse(FrMap3$Treated ==1,HHexp+100,HHexp)
FrMap3$Policy2 = ifelse(FrMap3$Treated ==1,AHexp+100*FrMap3$POPULATION,AHexp)

#Index for Policy 1 & 2

FrMap3$Hl_ivP1 = (FrMap3$AHexp/FrMap3$POPULATION+FrMap3$Policy1-min(FrMap3$AHexp/FrMap3$POPULATION+FrMap3$Policy1))/(mean(FrMap3$AHexp/FrMap3$POPULATION+FrMap3$Policy1)-min(FrMap3$AHexp/FrMap3$POPULATION+FrMap3$Policy1))
FrMap3$Hl_ivP2 = (FrMap3$Policy2/FrMap3$POPULATION+FrMap3$HHexp-min(FrMap3$Policy2/FrMap3$POPULATION+FrMap3$HHexp))/(mean(FrMap3$Policy2/FrMap3$POPULATION+FrMap3$HHexp)-min(FrMap3$Policy2/FrMap3$POPULATION+FrMap3$HHexp))

 
#Micro simulation : Benchmark model 
#-------------------------------
Model=lm(D_dv~Hl_iv:LIBGEO,data=FrMap3)

BenchBetaT = Model$coefficients[-1]*FrMap3$Treated
BenchBetaT=BenchBetaT[BenchBetaT!=0]

BenchBetaC = Model$coefficients[-1]*FrMap3$Control
BenchBetaC=BenchBetaC[BenchBetaC!=0]

#Boostrap set up for standard error estimations
Beta_hat=function(index,FrMap3){
 boot_data= FrMap3[index,]
 m= lm(D_dv~Hl_iv:LIBGEO,data=boot_data)
 return(m$coefficients[2])}

n=nrow(FrMap3)
index= sample(1:n, replace=TRUE)
lm_boot=bootstrap(1:n,Beta_hat,nboot = 1000,FrMap3)

#check moments of bethat distriution
muhat=mean(lm_boot$thetastar)
sighat=sd(lm_boot$thetastar) #assume Betahat is normally distributed following N(9.404239,19.71434^2) ~> N(10,400)

#Check Asymptotic properties
mean((lm_boot$thetastar-mean(lm_boot$thetastar))/sd(lm_boot$thetastar))
sd((lm_boot$thetastar-mean(lm_boot$thetastar))/sd(lm_boot$thetastar))

#Micro simulation : Treatment models
#-------------------------------

#Policy1
ModelP1=lm(D_dv~Hl_ivP1:LIBGEO,data=FrMap3)

P1_t = ModelP1$coefficients[-1]*FrMap3$Treated #Treatment for P1
P1_t = P1_t[P1_t!=0]

P1_C = ModelP1$coefficients[-1]*FrMap3$Control #Control for P1
P1_C = P1_C[P1_C!=0]

#Policy2

ModelP2=lm(D_dv~Hl_ivP2:LIBGEO,data=FrMap3)

P2_t = ModelP2$coefficients[-1]*FrMap3$Treated #Treatment for P2
P2_t = P2_t[P2_t!=0]

P2_C = ModelP2$coefficients[-1]*FrMap3$Control #Control for P2
P2_C = P2_C[P2_C!=0]


#Translation into MU:
MU_bT  = vector("numeric",n/2)  #Benchmark Treatment
MU_bc  = vector("numeric",n/2)  #Benchmark control
MU_P1t = vector("numeric",n/2)  #Policy 1 Treatment
MU_P1c = vector("numeric",n/2)  #Policy 1 Control
MU_P2t = vector("numeric",n/2)  #Policy 2 Treatment 
MU_P2c = vector("numeric",n/2)  #Policy 2 Control 


for( i in 1:n/2){
  MU_bT[i]  = ((BenchBetaT[i]-1)*exp(BenchBetaT[i]*FrMap3$Hl_iv[2*i]+FrMap3$Hl_iv[2*i])-BenchBetaT[i]*exp(BenchBetaT[i]*FrMap3$Hl_iv[2*i])+exp(FrMap3$Hl_iv[2*i]))/((1-exp(FrMap3$Hl_iv[2*i]))^2) 
  MU_bc[i]  = ((BenchBetaC[i]-1)*exp(BenchBetaC[i]*FrMap3$Hl_iv[2*i]+FrMap3$Hl_iv[2*i])-BenchBetaC[i]*exp(BenchBetaC[i]*FrMap3$Hl_iv[2*i])+exp(FrMap3$Hl_iv[i]))/((1-exp(FrMap3$Hl_iv[i]))^2)  
  MU_P1t[i] = ((P1_t[i]-1)*exp(P1_t[i]*FrMap3$Hl_ivP1[2*i]+FrMap3$Hl_ivP1[2*i])-P1_t[i]*exp(P1_t[i]*FrMap3$Hl_ivP1[2*i])+exp(FrMap3$Hl_ivP1[2*i]))/((1-exp(FrMap3$Hl_ivP1[2*i]))^2)  
  MU_P1c[i] = ((P1_C[i]-1)*exp(P1_C[i]*FrMap3$Hl_ivP1[2*i]+FrMap3$Hl_ivP1[2*i])-P1_C[i]*exp(P1_C[i]*FrMap3$Hl_ivP1[2*i])+exp(FrMap3$Hl_ivP1[2*i]))/((1-exp(FrMap3$Hl_ivP1[2*i]))^2)  
  MU_P2t[i] = ((P2_t[i]-1)*exp(P2_t[i]*FrMap3$Hl_ivP2[2*i]+FrMap3$Hl_ivP2[2*i])-P2_t[i]*exp(P2_t[i]*FrMap3$Hl_ivP2[2*i])+exp(FrMap3$Hl_ivP2[2*i]))/((1-exp(FrMap3$Hl_ivP2[2*i]))^2)  
  MU_P2c[i] = ((P2_C[i]-1)*exp(P2_C[i]*FrMap3$Hl_ivP2[2*i]+FrMap3$Hl_ivP2[2*i])-P2_C[i]*exp(P2_C[i]*FrMap3$Hl_ivP2[2*i])+exp(FrMap3$Hl_ivP2[2*i]))/((1-exp(FrMap3$Hl_ivP2[2*i]))^2) 
}


#First difference models

Policy1= (MU_P1t-MU_bT)-(MU_P1c-MU_bc)
Policy1= Policy1[-35]



Policy2= (MU_P2t-MU_bT)-(MU_P2c-MU_bc)
Policy2=Policy2[-35]

#Stat significance of the model

t1=(Policy1[!is.na(Policy1)]-mean(lm_boot$thetastar))/sd(lm_boot$thetastar)
t2=(Policy2[!is.na(Policy2)]-mean(lm_boot$thetastar))/sd(lm_boot$thetastar)

dnorm(t1)
dnorm(t2)

#Differences in means because 
tmean1=(mean(Policy1[!is.na(Policy1)])-mean(lm_boot$thetastar))/sd(lm_boot$thetastar)
tmean2=(mean(Policy2[!is.na(Policy2)])-mean(lm_boot$thetastar))/sd(lm_boot$thetastar)

dnorm(tmean1)
dnorm(tmean2)



# Creation of plots and Graphs
#-------------------------------
# Creation of plots and Graphs
X=(FrMap3$AHexp/FrMap3$POPULATION+FrMap3$HHexp-min(FrMap3$AHexp/FrMap3$POPULATION+FrMap3$HHexp))/(mean(FrMap3$AHexp/FrMap3$POPULATION+FrMap3$HHexp)-min(FrMap3$AHexp/FrMap3$POPULATION+FrMap3$HHexp))
x= seq(0,95,1)
x1=seq(48,95,1)

bench= c(MU_bc,MU_bT)
bench=bench[-35]

yref= mean(bench[!is.na(bench)])

y=yref*x+yref
y1= abs(mean(Policy1[!is.na(Policy1)]))*x1+ yref
y2= abs(mean(Policy2[!is.na(Policy2)]))*x1+ yref

polA= c(rep(NA,48),y1)
polB= c(rep(NA,48),y2)


df1= data.frame(x,y,polA,polB)

ggplot(data=df1,aes(x=x))+geom_line(aes(x=x,y=y,color="Benchmark Line"))+
  geom_line(aes(x=x,y=polA,color="Policy 1"))+
  geom_line(aes(x = x,y=polB,color="Policy 2"))+
  geom_vline(xintercept = 48,color="red",linetype="dashed")+
  xlab("Health Expenses")+
  ylab("Marginal Utility of Health Expenses")+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position = c(0.2, 0.7),legend.background = element_rect(fill=0.5),legend.title = element_blank())







