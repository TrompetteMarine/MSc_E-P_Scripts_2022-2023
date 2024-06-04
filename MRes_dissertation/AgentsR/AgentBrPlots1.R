library(readr)
library(ggplot2)
library(devtools)
library(tidyverse)

dtP <- read_csv("Desktop/main/MRes_paper/Data/dtP.csv")
dtC <- read_csv("Desktop/main/MRes_paper/Data/dtC.csv")
devtools::install_github("kassambara/ggpubr")

#functions 
Svar=function(X){
  sv= sqrt(sum(X^2))
  return(sv)
}

tot= function(X){
  v0=v1=v2=v3=v4=v5=vector("numeric",5)
  for(i in 1:5){
    v0[i]= sum(X$Ag[dtC$x==i])
    v1[i]= sum(X$Pr1[dtC$x==i])
    v2[i]= sum(X$Pr2[dtC$x==i])
    v3[i]= sum(X$Pr3[dtC$x==i])
    v4[i]= sum(X$Pr4[dtC$x==i])
    v5[i]= sum(X$Pr5[dtC$x==i])
  }
  V= data.frame(v0,v1,v2,v3,v4,v5)
  return(V)
}

dtot=function(X){
  s0=s1=s2=s3=s4=s5=vector("numeric",5)
  for(i in 1:5){
    s0[i]= sqrt(Svar(X$dAg[dtC$x==i]))/sqrt(5)
    s1[i]= sqrt(Svar(X$dPr1[dtC$x==i]))/sqrt(5) 
    s2[i]= sqrt(Svar(X$dPr2[dtC$x==i]))/sqrt(5) 
    s3[i]= sqrt(Svar(X$dPr3[dtC$x==i]))/sqrt(5) 
    s4[i]= sqrt(Svar(X$dPr4[dtC$x==i]))/sqrt(5) 
    s5[i]= sqrt(Svar(X$dPr5[dtC$x==i]))/sqrt(5) 
  }
  S= data.frame(s0,s1,s2,s3,s4,s5)
  return(S)
}

stk= function(X){
  Y = c(X[1,],X[2,],X[3,],X[4,],X[5,])
  return(Y)
}


# Total estimates and se estimates
dtT= dtot(dtC)
dTT= tot(dtC)
tT = as.numeric(stk(dTT))
dsetT= as.numeric(stk(dtT))

dTotal= data.frame(tT,dsetT)
dTotal$Profile = rep(c("Aggregated","Profile 1", "Profile 2", "Profile 3", "Profile 4","Profile 5"),times=5,each=1)
dTotal$x= rep(seq(from=1,to=5,by=1),times=6,each=1)
#Aggregated vs the world in 2017 consumption level
ggplot(dTotal,aes(x=x,y=tT, colour=Profile))+
  geom_line()+
  geom_pointrange(aes(ymin=tT-dsetT,ymax=tT+dsetT))+
  geom_ribbon(aes(ymin=tT-dsetT,ymax=tT+dsetT,fill=Profile),alpha=0.2)+
  scale_y_continuous(limits = c(0,3))+
  theme_bw()
#Profile reqaction by categorie of consumption

ggplot(dtP,aes(x=x,y=hP,colour = Profile))+
  geom_rect(aes(xmin = 1.50,xmax=2.5,ymin = 0,ymax=0.8),fill="lavenderblush1",alpha=0.2,colour= NA)+
  geom_line()+
  geom_pointrange(aes(ymin=hP-dhP,ymax=hP+dhP))+
  geom_ribbon(aes(ymin = hP-dhP,ymax=hP+dhP,fill=Profile),alpha=0.2)+
  geom_text(aes(x=2,y=0.75),label= "Shock \nPeriod",legend=FALSE,inherit.aes = FALSE,colour="darkred")+
  ylab("Weight of non-Energetic Domestic Expenses in Houshold's budget ")+
  xlab("Time steps")+
  theme_bw()

ggplot(dtP,aes(x=x,y=fP,colour = Profile))+
  geom_rect(aes(xmin = 1.50,xmax=2.5,ymin = 0,ymax=0.8),fill="lavenderblush1",alpha=0.2,colour= NA)+
  geom_line()+
  geom_pointrange(aes(ymin=fP-dfP,ymax=fP+dfP))+
  geom_ribbon(aes(ymin = fP-dfP,ymax=fP+dfP,fill=Profile),alpha=0.2)+
  geom_text(aes(x=2,y=0.75),label= "Shock \nPeriod",legend=FALSE,inherit.aes = FALSE,colour="darkred")+
  ylab("Weight of Food Expenses in Houshold's budget ")+
  xlab("Time steps")+
  theme_bw()

ggplot(dtP,aes(x=x,y=tP,colour = Profile))+
  geom_rect(aes(xmin = 1.50,xmax=2.5,ymin = 0,ymax=0.8),fill="lavenderblush1",alpha=0.2,colour= NA)+
  geom_line()+
  geom_pointrange(aes(ymin=tP-dtP,ymax=tP+dtP))+
  geom_ribbon(aes(ymin = tP-dtP,ymax=tP+dtP,fill=Profile),alpha=0.2)+
  geom_text(aes(x=2,y=0.75),label= "Shock \nPeriod",legend=FALSE,inherit.aes = FALSE,colour="darkred")+
  ylab("Weight of Transport Expenses in Houshold's budget ")+
  xlab("Time steps")+
  theme_bw()

ggplot(dtP,aes(x=x,y=dP,colour = Profile))+
  geom_rect(aes(xmin = 1.50,xmax=2.5,ymin = 0,ymax=0.8),fill="lavenderblush1",alpha=0.2,colour= NA)+
  geom_line()+
  geom_pointrange(aes(ymin=dP-ddP,ymax=dP+ddP))+
  geom_ribbon(aes(ymin = dP-ddP,ymax=dP+ddP,fill=Profile),alpha=0.2)+
  geom_text(aes(x=2,y=0.75),label= "Shock \nPeriod",legend=FALSE,inherit.aes = FALSE,colour="darkred")+
  ylab("Weight of Energetic Domestic Expenses in Houshold's budget")+
  xlab("Time steps")+
  theme_bw()

ggplot(dtP,aes(x=x,y=hlP,colour = Profile))+
  geom_rect(aes(xmin = 1.50,xmax=2.5,ymin = 0,ymax=0.8),fill="lavenderblush1",alpha=0.2,colour= NA)+
  geom_line()+
  geom_pointrange(aes(ymin=hlP-dhlP,ymax=hlP+dhlP))+
  geom_ribbon(aes(ymin = hlP-dhlP,ymax=hlP+dhlP,fill=Profile),alpha=0.2)+
  geom_text(aes(x=2,y=0.75),label= "Shock \nPeriod",legend=FALSE,inherit.aes = FALSE,colour="darkred")+
  ylab("Weight of Health Expenses in Houshold's budget ")+
  xlab("Time steps")+
  theme_bw()

#Profiles adjustment consumption 
dtC$Consumption = rep(c("E-Domestic","Food","Health","nE- Domestic","Transporation"),times=1,each=5)


ggplot(dtC,aes(x=x,y=Ag,colour=Consumption))+
  geom_line()+
  geom_pointrange(aes(ymin=Ag-dAg,ymax = Ag+dAg))+
  geom_ribbon(aes(ymin=Ag-dAg,ymax = Ag+dAg,fill= Consumption),alpha=0.3)+
  scale_y_continuous(limits = c(-.15,0.7))+
  theme_bw()

ggplot(dtC,aes(x=x,y=Pr1,colour=Consumption))+
  geom_rect(aes(xmin = 1.50,xmax=2.5,ymin = -0.15,ymax=0.7),fill="lavenderblush1",alpha=0.2,colour= NA)+
  geom_line()+
  geom_pointrange(aes(ymin=Pr1-dPr1,ymax = Pr1+dPr1))+
  geom_ribbon(aes(ymin=Pr1-dPr1,ymax = Pr1+dPr1,fill= Consumption),alpha=0.3)+
  scale_y_continuous(limits = c(-.15,0.7))+
  geom_text(aes(x=2,y=0.65),label= "Shock \nPeriod",legend=FALSE,inherit.aes = FALSE,colour="darkred")+
  ylab("Profile 1 Impulse Response Functions")+
  xlab("Time steps")+
  theme_bw()

ggplot(dtC,aes(x=x,y=Pr2,colour=Consumption))+
  geom_rect(aes(xmin = 1.50,xmax=2.5,ymin = -0.15,ymax=0.7),fill="lavenderblush1",alpha=0.2,colour= NA)+
  geom_line()+
  geom_pointrange(aes(ymin=Pr2-dPr2,ymax = Pr2+dPr2))+
  geom_ribbon(aes(ymin=Pr2-dPr2,ymax = Pr2+dPr2,fill= Consumption),alpha=0.3)+
  scale_y_continuous(limits = c(-.15,0.7))+
  geom_text(aes(x=2,y=0.65),label= "Shock \nPeriod",legend=FALSE,inherit.aes = FALSE,colour="darkred")+
  ylab("Profile 2 Impulse Response Functions")+
  xlab("Time steps")+
  theme_bw()

ggplot(dtC,aes(x=x,y=Pr3,colour=Consumption))+
  geom_rect(aes(xmin = 1.50,xmax=2.5,ymin = -0.15,ymax=0.7),fill="lavenderblush1",alpha=0.2,colour= NA)+
  geom_line()+
  geom_pointrange(aes(ymin=Pr3-dPr3,ymax = Pr3+dPr3))+
  geom_ribbon(aes(ymin=Pr3-dPr3,ymax = Pr3+dPr3,fill= Consumption),alpha=0.3)+
  scale_y_continuous(limits = c(-.15,0.7))+
  geom_text(aes(x=2,y=0.65),label= "Shock \nPeriod",legend=FALSE,inherit.aes = FALSE,colour="darkred")+
  ylab("Profile 3 Impulse Response Functions")+
  xlab("Time steps")+
  theme_bw()

ggplot(dtC,aes(x=x,y=Pr4,colour=Consumption))+
  geom_rect(aes(xmin = 1.50,xmax=2.5,ymin = -0.15,ymax=0.7),fill="lavenderblush1",alpha=0.2,colour= NA)+
  geom_line()+
  geom_pointrange(aes(ymin=Pr4-dPr4,ymax = Pr4+dPr4))+
  geom_ribbon(aes(ymin=Pr4-dPr4,ymax = Pr4+dPr4,fill= Consumption),alpha=0.3)+
  scale_y_continuous(limits = c(-.15,0.7))+
  geom_text(aes(x=2,y=0.65),label= "Shock \nPeriod",legend=FALSE,inherit.aes = FALSE,colour="darkred")+
  ylab("Profile 4 Impulse Response Functions")+
  xlab("Time steps")+
  theme_bw()

ggplot(dtC,aes(x=x,y=Pr5,colour=Consumption))+
  geom_rect(aes(xmin = 1.50,xmax=2.5,ymin = -0.15,ymax=0.7),fill="lavenderblush1",alpha=0.2,colour= NA)+
  geom_line()+
  geom_pointrange(aes(ymin=Pr5-dPr5,ymax = Pr5+dPr5))+
  geom_ribbon(aes(ymin=Pr5-dPr5,ymax = Pr5+dPr5,fill= Consumption),alpha=0.3)+
  scale_y_continuous(limits = c(-.15,0.7))+
  geom_text(aes(x=2,y=0.65),label= "Shock \nPeriod",legend=FALSE,inherit.aes = FALSE,colour="darkred")+
  #scale_colour_discrete(name="Consumption",labels=c("E-Domestic","Food","Health","nE-Domestic","Transportation"))+
  ylab("Profile 5 Impulse Response Functions")+
  xlab("Time steps")+
  theme_bw()

# Animation stuff 

ggplot(dtC,aes(x=x,y=Ag,colour=Consumption))+
  geom_line()+
  geom_pointrange(aes(ymin=Ag-dAg,ymax = Ag+dAg))+
  geom_ribbon(aes(ymin=Ag-dAg,ymax = Ag+dAg,fill= Consumption),alpha=0.3)+
  scale_y_continuous(limits = c(-.15,0.7))+
  ylab("")+
  xlab("Time steps")+
  theme_bw()

ggplot(dtC,aes(x=x,y=Ag,colour=Consumption))+
  geom_rect(aes(xmin = 1.50,xmax=2.5,ymin = -0.15,ymax=0.7),fill="lavenderblush1",alpha=0.2,colour= NA)+
  geom_line()+
  geom_pointrange(aes(ymin=Ag-dAg,ymax = Ag+dAg))+
  geom_ribbon(aes(ymin=Ag-dAg,ymax = Ag+dAg,fill= Consumption),alpha=0.3)+
  scale_y_continuous(limits = c(-.15,0.7))+
  geom_text(aes(x=2,y=0.65),label= "Shock \nPeriod",legend=FALSE,inherit.aes = FALSE,colour="darkred")+
  ylab("")+
  xlab("Time steps")+
  theme_bw()

ggplot(dtC,aes(x=x,y=Pr1,colour=Consumption))+
  geom_rect(aes(xmin = 1.50,xmax=2.5,ymin = -0.15,ymax=0.7),fill="lavenderblush1",alpha=0.2,colour= NA)+
  geom_line()+
  geom_pointrange(aes(ymin=Pr1-dPr1,ymax = Pr1+dPr1))+
  geom_ribbon(aes(ymin=Pr1-dPr1,ymax = Pr1+dPr1,fill= Consumption),alpha=0.3)+
  scale_y_continuous(limits = c(-.15,0.7))+
  geom_text(aes(x=2,y=0.65),label= "Shock \nPeriod",legend=FALSE,inherit.aes = FALSE,colour="darkred")+
  ylab("")+
  xlab("Time steps")+
  theme_bw()



ggplot(dtC,aes(x=x,y=Pr2,colour=Consumption))+
  geom_rect(aes(xmin = 1.50,xmax=2.5,ymin = -0.15,ymax=0.7),fill="lavenderblush1",alpha=0.2,colour= NA)+
  geom_line()+
  geom_pointrange(aes(ymin=Pr2-dPr2,ymax = Pr2+dPr2))+
  geom_ribbon(aes(ymin=Pr2-dPr2,ymax = Pr2+dPr2,fill= Consumption),alpha=0.3)+
  scale_y_continuous(limits = c(-.15,0.7))+
  geom_text(aes(x=2,y=0.65),label= "Shock \nPeriod",legend=FALSE,inherit.aes = FALSE,colour="darkred")+
  ylab("")+
  xlab("Time steps")+
  theme_bw()

ggplot(dtC,aes(x=x,y=Pr3,colour=Consumption))+
  geom_rect(aes(xmin = 1.50,xmax=2.5,ymin = -0.15,ymax=0.7),fill="lavenderblush1",alpha=0.2,colour= NA)+
  geom_line()+
  geom_pointrange(aes(ymin=Pr3-dPr3,ymax = Pr3+dPr3))+
  geom_ribbon(aes(ymin=Pr3-dPr3,ymax = Pr3+dPr3,fill= Consumption),alpha=0.3)+
  scale_y_continuous(limits = c(-.15,0.7))+
  geom_text(aes(x=2,y=0.65),label= "Shock \nPeriod",legend=FALSE,inherit.aes = FALSE,colour="darkred")+
  ylab("")+
  xlab("Time steps")+
  theme_bw()

ggplot(dtC,aes(x=x,y=Pr4,colour=Consumption))+
  geom_rect(aes(xmin = 1.50,xmax=2.5,ymin = -0.15,ymax=0.7),fill="lavenderblush1",alpha=0.2,colour= NA)+
  geom_line()+
  geom_pointrange(aes(ymin=Pr4-dPr4,ymax = Pr4+dPr4))+
  geom_ribbon(aes(ymin=Pr4-dPr4,ymax = Pr4+dPr4,fill= Consumption),alpha=0.3)+
  scale_y_continuous(limits = c(-.15,0.7))+
  geom_text(aes(x=2,y=0.65),label= "Shock \nPeriod",legend=FALSE,inherit.aes = FALSE,colour="darkred")+
  ylab("")+
  xlab("Time steps")+
  theme_bw()


dtC$Pr6= dtC$Pr5
dtC$Pr6[c(3,8,13,18,23)]=dtC$Pr5[c(3,8,13,18,23)]
dtC$Pr6[c(4,9,14,19,24)]=dtC$Pr5[c(3,8,13,18,23)]-0.05
dtC$Pr6[c(5,10,15,20,25)]=dtC$Pr5[c(5,10,15,20,25)] -0.25

ggplot(dtC,aes(x=x,y=Pr6,colour=Consumption))+
  geom_rect(aes(xmin = 1.50,xmax=2.5,ymin = -0.15,ymax=0.7),fill="lavenderblush1",alpha=0.2,colour= NA)+
  geom_line()+
  geom_pointrange(aes(ymin=Pr6-dPr5,ymax = Pr6+dPr5))+
  geom_ribbon(aes(ymin=Pr6-dPr5,ymax = Pr6+dPr5,fill= Consumption),alpha=0.3)+
  scale_y_continuous(limits = c(-.15,0.7))+
  geom_text(aes(x=2,y=0.65),label= "Shock \nPeriod",legend=FALSE,inherit.aes = FALSE,colour="darkred")+
  #scale_colour_discrete(name="Consumption",labels=c("E-Domestic","Food","Health","nE-Domestic","Transportation"))+
  ylab("")+
  xlab("Time steps")+
  theme_bw()



dtC$Pr7= dtC$Pr2
dtC$Pr7[c(1,6,11,16,21)]=dtC$Pr3[c(1,6,11,16,21)]

ggplot(dtC,aes(x=x,y=Pr7,colour=Consumption))+
  geom_rect(aes(xmin = 1.50,xmax=2.5,ymin = -0.15,ymax=0.7),fill="lavenderblush1",alpha=0.2,colour= NA)+
  geom_line()+
  geom_pointrange(aes(ymin=Pr7-dPr3,ymax = Pr7+dPr3))+
  geom_ribbon(aes(ymin=Pr7-dPr3,ymax = Pr7+dPr3,fill= Consumption),alpha=0.3)+
  scale_y_continuous(limits = c(-.15,0.7))+
  geom_text(aes(x=2,y=0.65),label= "Shock \nPeriod",legend=FALSE,inherit.aes = FALSE,colour="darkred")+
  #scale_colour_discrete(name="Consumption",labels=c("E-Domestic","Food","Health","nE-Domestic","Transportation"))+
  ylab("")+
  xlab("Time steps")+
  theme_bw()

ggplot(dtC,aes(x=x,y=Pr7,colour=Consumption))+
  geom_rect(aes(xmin = 1.50,xmax=2.5,ymin = -0.15,ymax=0.7),fill="lavenderblush1",alpha=0.2,colour= NA)+
  geom_line()+
  geom_pointrange(aes(ymin=Pr7-dPr2,ymax = Pr7+dPr2))+
  geom_ribbon(aes(ymin=Pr7-dPr2,ymax = Pr7+dPr2,fill= Consumption),alpha=0.3)+
  scale_y_continuous(limits = c(-.15,0.7))+
  geom_text(aes(x=2,y=0.65),label= "Shock \nPeriod",legend=FALSE,inherit.aes = FALSE,colour="darkred")+
  #scale_colour_discrete(name="Consumption",labels=c("E-Domestic","Food","Health","nE-Domestic","Transportation"))+
  ylab("")+
  xlab("Time steps")+
  theme_bw()
