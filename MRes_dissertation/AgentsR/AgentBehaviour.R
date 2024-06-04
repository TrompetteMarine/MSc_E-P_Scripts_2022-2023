library(readr)
library(MASS)
library(dplyr)
library(QZ,quiet=TRUE)
library(data.table)
##########
# Data
##########
AgentParam <- read_csv("Desktop/MRes_paper/Data/AgP.csv")
Map2 <- read_csv("Desktop/MRes_paper/Data/MapData2.csv")
V0 <- read_csv("Desktop/vopt.csv")

##########
#Param
##########

pi= 1+c(0.13,0.15,0.21,0.24,0.29,0.46)
w= c(0.54,0.21,0.09,0.07,0.09)


##########
# Function to decompose the difference between the distributions 
##########
tr= function(X){
  return(sum(diag(X)))
}

M= function(X,v,i){
  return(1+(1/pi[i]*X-X)%*%t(v))
}

cVar= function(M,r){
  r1=1+r
  A=1/r*tr(t(M)%*%(M))
  B= abs(1/r*tr(M))^2
  return((A-B)/r1)
}

Profile= function(X,v){
  alpha=sqrt(cVar(M(X,v,1),6))-1
  B1=alpha*X+(1-alpha)*colSums(M(X,v,1))
  
  alpha2= sqrt(cVar(M(B1,v,2),6))-1
  B2=alpha2*B1+(1-alpha2)*colSums(M(B1,v,2))
  
  alpha3=sqrt(cVar(M(B2,v,3),6))-1
  B3=alpha3*B2+(1-alpha3)*colSums(M(B2,v,3))
  
  alpha4 = sqrt(cVar(M(B3,v,4),6))-1
  B4=alpha4*B3+(1-alpha4)*colSums(M(B3,v,4))
  
  alpha5 = sqrt(cVar(M(B4,v,5),6))-1
  B5=alpha5*B3+(1-alpha5)*colSums(M(B4,v,5))
  
  alpha6= sqrt(cVar(M(B5,v,6),6))-1
  
  hP = c(X[1],B1[1],B2[1],B3[1],B4[1],B5[1])
  fP = c(X[2],B1[2],B2[2],B3[2],B4[2],B5[2])
  tP = c(X[3],B1[3],B2[3],B3[3],B4[3],B5[3])
  dP = c(X[4],B1[4],B2[4],B3[4],B4[4],B5[4])
  hlP= c(X[5],B1[5],B2[5],B3[5],B4[5],B5[5])
  oP = c(X[6],B1[6],B2[6],B3[6],B4[6],B5[6])
  
  ds = c(1+alpha,1+alpha2,1+alpha3,1+alpha4,1+alpha5,1+alpha6)
  
  x=1:6
  
  dset= data.frame(x,hP,fP,tP,dP,hlP,oP,ds)
  
  return(dset)
}

stacklist=function(X,i){
  
  X[[1]]=NULL # [[1]] is artificial for calibration purposes ergo not to be included in later analysis
  
  X= Reduce(rbind,X)
  
  X=X[-c(1,7,13,19,25),]
  #(2-X$ds[1])*X[c(1,7,13,19,25),]
  
  X$x= rep(seq(1:5),times=5,each=1)
  #X$x= rep(seq(1:5),times=6,each=1)
  
  X$G= rep(seq(1:5),times=1,each=5)
  #X$G= rep(seq(1:5),times=1,each=6)
  
  X$Meta= i
  
  return(X)
}

##########
#Quick Set Up 
##########
A=as.matrix(AgentParam[,seq(from=2,to=11,by=2)])
S=as.matrix(AgentParam[,seq(from=3,to=11,by=2)])
V=as.matrix(V0)
# How do agent react period 1 : inflation follows historic data  - without help (Gov Inter)

#Agents

p1= list()
p2= list()
p3= list()
p4= list()
p5= list()

for(i in 1:6){
  p1[[i]]=Profile(A[,1],V[,i])
  p2[[i]]=Profile(A[,2],V[,i])
  p3[[i]]=Profile(A[,3],V[,i])
  p4[[i]]=Profile(A[,4],V[,i])
  p5[[i]]=Profile(A[,5],V[,i])
}


P1= stacklist(p1,1)
P2= stacklist(p2,2)
P3= stacklist(p3,3)
P4= stacklist(p4,4)
P5= stacklist(p5,5)

##########
# Aggregates Responses & sd 
##########

# Response 
c1= colSums(P1[,c(2:8)])
c2= colSums(P2[,c(2:8)])
c3= colSums(P3[,c(2:8)])
c4= colSums(P4[,c(2:8)])
c5= colSums(P5[,c(2:8)])


C  = as.matrix(data.frame(c1,c2,c3,c4,c5))
C1 = as.matrix(data.frame(c1,c2,c3,c4,c5))
C1 = C1[-c(6,7),]
Ctn=  t(C1)/rowSums(t(C1))
Ctn
dS= S[-6,]
# Sd
deusXmachina=function(X1,X2,X3,X4,X5){
  u1  = X1[,c(2:7)]/rowSums(X1[,c(2:7)]) 
  u2  = X2[,c(2:7)]/rowSums(X2[,c(2:7)]) 
  u3  = X3[,c(2:7)]/rowSums(X3[,c(2:7)]) 
  u4  = X4[,c(2:7)]/rowSums(X4[,c(2:7)]) 
  u5  = X5[,c(2:7)]/rowSums(X5[,c(2:7)]) 
  
  dhP  = c(sd(u1$hP)/sqrt(5),sd(u2$hP)/sqrt(5),sd(u3$hP)/sqrt(5),sd(u4$hP)/sqrt(5),sd(u5$hP)/sqrt(5))
  dfP  = c(sd(u1$fP)/sqrt(5),sd(u2$fP)/sqrt(5),sd(u3$fP)/sqrt(5),sd(u4$fP)/sqrt(5),sd(u5$fP)/sqrt(5))
  dtP  = c(sd(u1$tP)/sqrt(5),sd(u2$tP)/sqrt(5),sd(u3$tP)/sqrt(5),sd(u4$tP)/sqrt(5),sd(u5$tP)/sqrt(5))
  ddP  = c(sd(u1$dP)/sqrt(5),sd(u2$dP)/sqrt(5),sd(u3$dP)/sqrt(5),sd(u4$dP)/sqrt(5),sd(u5$dP)/sqrt(5))
  dhlP = c(sd(u1$hlP)/sqrt(5),sd(u2$hlP)/sqrt(5),sd(u3$hlP)/sqrt(5),sd(u4$hlP)/sqrt(5),sd(u5$hlP)/sqrt(5))
  
  Y = data.frame(dhP,dfP,dtP,ddP,dhlP)
  return(Y)
}

dCtn= dS%*%Ctn/sqrt(25)

##########
# Agent Response Estimates
##########

#Behavioural Response Estimates
Br1=Br2=Br3=Br4=Br5=matrix(ncol=5,nrow=5)
for (i in 1:5){
  Br1[i,]=colSums(p1[[i]][,-c(1,7,8)])/sum(colSums(p1[[i]][,-c(1,7,8)]))
  Br2[i,]=colSums(p2[[i]][,-c(1,7,8)])/sum(colSums(p2[[i]][,-c(1,7,8)]))
  Br3[i,]=colSums(p3[[i]][,-c(1,7,8)])/sum(colSums(p3[[i]][,-c(1,7,8)]))
  Br4[i,]=colSums(p4[[i]][,-c(1,7,8)])/sum(colSums(p4[[i]][,-c(1,7,8)]))
  Br5[i,]=colSums(p5[[i]][,-c(1,7,8)])/sum(colSums(p5[[i]][,-c(1,7,8)]))
}

# sd Behavioural Response Estimates 



ichtus=function(U){
  X=U[,c(2:6)]/rowSums(U[,c(2:6)])
  X$G=U$G
  dhP=dfP=dtP=ddP=dhlP=vector("numeric",5)
  for(i in 1:5){
    dhP[i]=sd(X$hP[X$G==i])/sqrt(5)
    dfP[i]=sd(X$fP[X$G==i])/sqrt(5)
    dtP[i]=sd(X$tP[X$G==i])/sqrt(5)
    ddP[i]=sd(X$dP[X$G==i])/sqrt(5)
    dhlP[i]=sd(X$hlP[X$G==i])/sqrt(5)
  }
  
  Y= data.frame(dhP,dfP,dtP,ddP,dhlP)
  
  return(Y)
}

dBr1=ichtus(P1)
dBr2=ichtus(P2)
dBr3=ichtus(P3)
dBr4=ichtus(P4)
dBr5=ichtus(P5)


# Boring computation are do, ergo data mangement for plotting starts right here: 

# var | profile

hP  = c(Ctn[,1],Br1[,1],Br2[,1],Br3[,1],Br4[,1],Br5[,1])
fP  = c(Ctn[,2],Br1[,2],Br2[,2],Br3[,2],Br4[,2],Br5[,2])
tP  = c(Ctn[,3],Br1[,3],Br2[,3],Br3[,3],Br4[,3],Br5[,3])
dP  = c(Ctn[,4],Br1[,4],Br2[,4],Br3[,4],Br4[,4],Br5[,4])
hlP = c(Ctn[,5],Br1[,5],Br2[,5],Br3[,5],Br4[,5],Br5[,5])

dhP  = c(dCtn[,1],dBr1[,1],dBr2[,1],dBr3[,1],dBr4[,1],dBr5[,1])
dfP  = c(dCtn[,2],dBr1[,2],dBr2[,2],dBr3[,2],dBr4[,2],dBr5[,2])
dtP  = c(dCtn[,3],dBr1[,3],dBr2[,3],dBr3[,3],dBr4[,3],dBr5[,3])
ddP  = c(dCtn[,4],dBr1[,4],dBr2[,4],dBr3[,4],dBr4[,4],dBr5[,4])
dhlP = c(dCtn[,5],dBr1[,5],dBr2[,5],dBr3[,5],dBr4[,5],dBr5[,5])


# Profile | var


stk= function(X){
  Y = c(X[1,],X[2,],X[3,],X[4,],X[5,])
  return(Y)
}
Ag  = as.numeric(stk(Ctn))
Pr1 = as.numeric(stk(Br1))
Pr2 = as.numeric(stk(Br2))
Pr3 = as.numeric(stk(Br3))
Pr4 = as.numeric(stk(Br4))
Pr5 = as.numeric(stk(Br5))

dAg  = as.numeric((stk(dCtn)))
dPr1 = as.numeric((stk(dBr1)))
dPr2 = as.numeric((stk(dBr2)))
dPr3 = as.numeric((stk(dBr3)))
dPr4 = as.numeric((stk(dBr4)))
dPr5 = as.numeric((stk(dBr5)))

#Dsets

dtP = data.frame(hP,dhP,fP,dfP,tP,dtP,dP,ddP,hlP,dhlP)
dtP$Profile = rep(c("Aggregated","Profile 1","Profile 2","Profile 3","Profile 4","Profile 5"),times=1,each=5)
dtP$x= rep(seq(from=1,to=5,by=1),times=6,each=1)


dtC= data.frame(Ag,dAg,Pr1,dPr1,Pr2,dPr2,Pr3,dPr3,Pr4,dPr4,Pr5,dPr5)
dtC$Consumption = rep(c("hP","fP","tP","dP","hlP"),times=1,each=5)
dtC$x = rep(seq(from=1,to=5,by=1),times=5,each=1)

#Export data

write.csv(dtP, file= "Desktop/MRes_paper/data/dtP.csv")
write.csv(dtC, file = "Desktop/MRes_paper/data/dtC.csv")
