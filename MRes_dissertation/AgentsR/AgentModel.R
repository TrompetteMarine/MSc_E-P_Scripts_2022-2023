library(readr)
library(ggplot2)
library(FactoMineR)
library(tikzDevice)
library(reshape2)
AgentParam <- read_csv("Desktop/MRes_paper/Data/AgentParam.csv")
Map2 <- read_csv("Desktop/MRes_paper/Data/MapData2.csv")

#Function 
Prf= function(X){
  return(X[["ind"]][["contrib"]])
}
#Deriving agent adjustment to shock strategies via PCA Stuff: avg 2017-2022 benchmark

M1=Map2[,seq(from=6,to=16,by=2)]
M2=Map2[,seq(from=7,to=17,by=2)]

pop= Map2$Pop
  
PCA1= PCA(t(M1),col.w = pop,graph = FALSE)
PCA2= PCA(t(M2),col.w = pop,graph = FALSE)

Pr   =as.matrix(Prf(PCA1))
pr   =as.matrix(Prf(PCA2))
pr   =pr[,c(5:1),drop=FALSE]

nPr  =matrix(nrow=6,ncol=5)
npr  =matrix(nrow=6,ncol=5)

vopt =matrix(nrow=6,ncol=5)

for(i in 1:5){
  for (j in 1:6){
    nPr[j,i]=Pr[j,i]/colSums(Pr)[i]
    npr[j,i]=pr[j,i]/colSums(pr)[i]
    vopt[j,i]=(pr[j,i]-Pr[j,i])/colSums(Pr)[i]
  }
}

# Agent Behavioural model:

A=as.matrix(AgentParam[,seq(from=2,to=11,by=2)])
S=as.matrix(AgentParam[,seq(from=3,to=11,by=2)])

A1= A[,1]
A2= A[,2]
A3= A[,3]
A4= A[,4]
A5= A[,5]

v1= vopt[,1]
v2= vopt[,2]
v3= vopt[,3]
v4= vopt[,4]
v5= vopt[,5]


#Param
pi= 1+c(0.13,0.15,0.21,0.24,0.29,0.46)
w= c(0.54,0.21,0.09,0.07,0.09)
#Plot Agent Demands:

C = data.frame(colSums(A1%*%t(1+v1)),colSums(A2%*%t(1+v2)),colSums(A3%*%t(1+v3)),colSums(A4%*%t(1+v4)),colSums(A5%*%t(1+v5)))
aC= 0.54*C[,1]+0.21*C[,2]+0.09*C[,3]+0.07*C[,4]+0.09*C[,5]
aS= sqrt(.54*S[,1]^2+0.21*S[,2]^2+0.09*S[,3]^2+0.07*S[,4]^2+0.09*S[,5]^2)/sqrt(5)
a2S= c(0.5709081,  0.6681407, -0.1578458,  0.8609849, -0.2461324,  0.1144746)
a3S= c(1.05846322,  0.52417040,  0.83675425,  1.27626030, -0.08881348, -0.67953931)
#Plot1 Rez 2

dC= c(c(NA,NA,100,C[,1]),c(NA,NA,100,C[,2]),c(NA,NA,100,C[,3]),c(NA,NA,100,C[,4]),c(NA,NA,100,C[,5]),c(98,101,100,aC))
dS= c(c(NA,NA,0,S[,1]),c(NA,NA,0,S[,2]),c(NA,NA,0,S[,3]),c(NA,NA,0,S[,4]),c(NA,NA,0,S[,5]),c(0.13,0.44,0,aS))
x = rep(seq(from=0,to=8,by=1),times=6,each=1)
Profile= rep(c("Agent 1","Agent 2","Agent 3","Agent 4","Agent 5","Aggregated"),times=1,each=9)

data= data.frame(dC,dS,x,Profile)


#tikz("Deskstop/MRes_paper/Tikz/PlotRez2.tex",width=3.5,height=3.5)
ggplot(data=data,aes(x=x,y=dC,colour = Profile))+
  geom_rect(aes(xmin = 1.31,xmax=2.59,ymin = -Inf,ymax=Inf),fill="lavenderblush1",alpha=0.1)+
  geom_line()+
  #geom_hline(yintercept=100,linetype="twodash",colour="red")+
  #geom_vline(xintercept = 1.59)+
  geom_ribbon(aes(ymin = dC-dS,ymax=dC+dS,fill =Profile),alpha=0.2)+
  geom_pointrange(aes(ymin=dC-1.96*dS,ymax=dC+1.96*dS))+
  geom_point(aes(x=2,y=100),colour="darkred")+
  geom_vline(xintercept=c(1.3,2.6),linetype="dashed")+
  geom_text(aes(x=2,y=35),label= "Inflation-Driven \nShock Window",legend=FALSE,inherit.aes = FALSE,colour="darkred")+
  xlab('Time Period')+
  scale_x_continuous(breaks = rep(seq(from=0,to=8,by=1)),limits = c(0,8))+
  ylab("Spending for Consumption (100= 2017 Aggregated Demand)")+
  theme_bw()
#dev.off()


#Plot 2: Resultat 1

aCt= c(96.5791,  99.4448,  101.4891, 108.4230,  108.1102,  114.3651)
aCf= c(102.33210,  106.7837,  108.9012, 112.92310,  113.8025,  119.1331)
dc = c(c(95,98.5,100,rep(NA,6)),c(NA,NA,100,aCf),c(NA,NA,100,aC),c(NA,NA,100,aCt))
#dcf= c(100,)
ds = c(c(0.13,0.44,0,rep(NA,6)),c(NA,NA,0,a2S),c(NA,NA,0,aS),c(NA,NA,0,a3S))
xx = rep(seq(from=0,to=8,by=1),4)
dt = data.frame(dc,ds,xx)
dt$Scenario= rep(c("Historical","No Shock","Actual","Counterfactual"),1,each=9)


#tikz("Deskstop/PlotRez1.tex",width=3.5,height=3.5)
ggplot(data=dt,aes(x=xx,y=dc,colour = Scenario))+
#geom_rect(aes(xmin = 0.81,xmax=2.0,ymin = -Inf,ymax=Inf),fill="lavenderblush2",alpha=0.1,color="darkred")+
  geom_polygon(aes(x=c(2,2,8,8,rep(NA,length(xx)-4)),y=c(95,100,115,106,rep(NA,length(xx)-4))),colour=NA,fill="lavenderblush3",alpha=0.3,show.legend = TRUE)+
  geom_rect(aes(xmin = 1.50,xmax=2.5,ymin = 90,ymax=105),fill="lavenderblush1",alpha=0.2,colour="lightcoral")+
  geom_abline(slope = 2.5,intercept = 95,color='darkred')+
  geom_abline(slope = 1.8334,intercept = 91.334,color='darkred')+
  geom_line()+
  geom_pointrange(aes(ymin=dc-1.96*ds,ymax=dc+1.96*ds))+
  geom_ribbon(aes(ymin = dc-ds,ymax=dc+ds,fill=Scenario),alpha=0.5)+
  geom_point(aes(x=2,y=100),colour="darkred",size=3)+
  geom_point(aes(x=2,y=95),colour="darkred",size=3)+
  geom_point(aes(x=8,y=106),colour="darkred",size=3)+
  geom_point(aes(x=8,y=115),colour="darkred",size=3)+
  geom_vline(xintercept = c(1.5,4.5),linetype= "dashed",colour="darkred")+
  geom_text(aes(x=2,y=91),label= "Intervention \nWindown",legend=FALSE,inherit.aes = FALSE,colour="darkred")+
  geom_text(aes(x=7.5,y=107),label= "Disequilibrium State",legend=FALSE,inherit.aes = FALSE,colour="peachpuff4")+
  xlab('Time Period')+
  scale_x_continuous(breaks = rep(seq(from=0,to=8,by=1)),limits = c(0,8))+
  scale_y_continuous(limits = c(85,125))+
  geom_segment(aes( x= 2.75,y=85,xend=1.55,yend=85),arrow=arrow(leng=unit(0.3,"cm")),legend=FALSE,inherit.aes = FALSE,colour="darkred")+
  geom_segment(aes( x= 3.25,y=85,xend=4.45,yend=85),arrow=arrow(leng=unit(0.3,"cm")),legend=FALSE,inherit.aes = FALSE,colour="darkred")+
  geom_text(aes(x=3,y=85),label= "Shock \nPeriod",legend=FALSE,inherit.aes = FALSE,colour="darkred")+
  ylab("Spending for Consumption (100= 2017 Aggregated Demand)")+
  theme_bw()

#dev.off() 
#Export Agent Behaviour Model data 

write.csv(vopt,file = "Desktop/vopt.csv")


