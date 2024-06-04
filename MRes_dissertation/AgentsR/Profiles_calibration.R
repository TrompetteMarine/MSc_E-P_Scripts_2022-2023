
library(readr)
library(meanShiftR)
library(FactoMineR)
library(MASS)
library(bootstrap)
library(ggplot2)
library(marginaleffects)
library(modelsummary)

#data
Map2 <- read_csv("Desktop/MRes_paper/Data/MapData2.csv")

#functions

Tr= function(A){
  return(sum(diag(A)))
}

fd= function(X,Y){
  return(cor(X[["ind"]][["contrib"]],Y[["ind"]][["contrib"]]))
}

fD= function(X,Y){
  return(t(cor(t(X[["ind"]][["contrib"]]),t(Y[["ind"]][["contrib"]]))))
}

Prf= function(X){
  return(X[["ind"]][["contrib"]])
}
#set up

M1=Map2[,seq(from=6,to=16,by=2)]
M2=Map2[,seq(from=7,to=17,by=2)]

pop= Map2$Pop

# Estimation and Bootstrapping 

N=100

bootreps= list()
n= nrow(M1)

for(i in 1:N){
  index= sample(1:n,replace = TRUE)
  M1_boot= M1[index,]
  A=PCA(t(M1_boot),col.w = pop,graph=FALSE)
  bootreps[[i]]=as.matrix(A[["ind"]][["contrib"]])
}

Tn=data.frame(bootreps)


#Means
T1=rowSums(Tn[,seq(from=1,to=5*N,by=5)])/N
T2=rowSums(Tn[,seq(from=2,to=5*N,by=5)])/N
T3=rowSums(Tn[,seq(from=3,to=5*N,by=5)])/N
T4=rowSums(Tn[,seq(from=4,to=5*N,by=5)])/N
T5=rowSums(Tn[,seq(from=5,to=5*N,by=5)])/N

#Standards deviation 
#Profile 1

s11=sd(Tn[1,seq(from=1,to=5*N,by=5)])/sqrt(N)
s12=sd(Tn[2,seq(from=1,to=5*N,by=5)])/sqrt(N)
s13=sd(Tn[3,seq(from=1,to=5*N,by=5)])/sqrt(N)
s14=sd(Tn[4,seq(from=1,to=5*N,by=5)])/sqrt(N)
s15=sd(Tn[5,seq(from=1,to=5*N,by=5)])/sqrt(N)
s16=sd(Tn[6,seq(from=1,to=5*N,by=5)])/sqrt(N)

S1= c(s11,s12,s13,s14,s15,s16)
rm(s11,s12,s13,s14,s15,s16)

#Profile 2

s21=sd(Tn[1,seq(from=2,to=5*N,by=5)])/sqrt(N)
s22=sd(Tn[2,seq(from=2,to=5*N,by=5)])/sqrt(N)
s23=sd(Tn[3,seq(from=2,to=5*N,by=5)])/sqrt(N)
s24=sd(Tn[4,seq(from=2,to=5*N,by=5)])/sqrt(N)
s25=sd(Tn[5,seq(from=2,to=5*N,by=5)])/sqrt(N)
s26=sd(Tn[6,seq(from=2,to=5*N,by=5)])/sqrt(N)

S2= c(s21,s22,s23,s24,s25,s26)
rm(s21,s22,s23,s24,s25,s26)

#Profile 3

s31=sd(Tn[1,seq(from=3,to=5*N,by=5)])/sqrt(N)
s32=sd(Tn[2,seq(from=3,to=5*N,by=5)])/sqrt(N)
s33=sd(Tn[3,seq(from=3,to=5*N,by=5)])/sqrt(N)
s34=sd(Tn[4,seq(from=3,to=5*N,by=5)])/sqrt(N)
s35=sd(Tn[5,seq(from=3,to=5*N,by=5)])/sqrt(N)
s36=sd(Tn[6,seq(from=3,to=5*N,by=5)])/sqrt(N)

S3= c(s31,s32,s33,s34,s35,s36)
rm(s31,s32,s33,s34,s35,s36)

#Profile 4

s41=sd(Tn[1,seq(from=4,to=5*N,by=5)])/sqrt(N)
s42=sd(Tn[2,seq(from=4,to=5*N,by=5)])/sqrt(N)
s43=sd(Tn[3,seq(from=4,to=5*N,by=5)])/sqrt(N)
s44=sd(Tn[4,seq(from=4,to=5*N,by=5)])/sqrt(N)
s45=sd(Tn[5,seq(from=4,to=5*N,by=5)])/sqrt(N)
s46=sd(Tn[6,seq(from=4,to=5*N,by=5)])/sqrt(N)

S4= c(s41,s42,s43,s44,s45,s46)
rm(s41,s42,s43,s44,s45,s46)

#Profile 5

s51=sd(Tn[1,seq(from=5,to=5*N,by=5)])/sqrt(N)
s52=sd(Tn[2,seq(from=5,to=5*N,by=5)])/sqrt(N)
s53=sd(Tn[3,seq(from=5,to=5*N,by=5)])/sqrt(N)
s54=sd(Tn[4,seq(from=5,to=5*N,by=5)])/sqrt(N)
s55=sd(Tn[5,seq(from=5,to=5*N,by=5)])/sqrt(N)
s56=sd(Tn[6,seq(from=5,to=5*N,by=5)])/sqrt(N)

S5= c(s51,s52,s53,s54,s55,s56)
rm(s51,s52,s53,s54,s55,s56)

AgentParam= data.frame(T1,S1,T2,S2,T3,S3,T4,S4,T5,S5)

#Export Final Outputs
write.csv(AgentParam,file="Desktop/MRes_paper/Data/AgP.csv")
