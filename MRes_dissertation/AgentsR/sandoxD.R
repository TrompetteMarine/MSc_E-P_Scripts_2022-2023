



M2= function(X,v,s,i){return((1+(1/pi[i]*X-X)%*%t(v)-(X%*%t(s))))}
#M2= function(X,v,s,i){return((1+(1/pi[i]*X[,i]-X[,ifelse(i>1,i-1,1)])%*%t(v[,i])-(X[,1]%*%t(s[,i]))))}

Profile2= function(X,v,S){
  alpha=sqrt(cVar(M2(X,v,S,1),6))-1
  B1=alpha*X+(1-alpha)*colSums(M2(X,v,S,1))
  
  alpha2= sqrt(cVar(M2(B1,v,S,2),6))-1
  B2=alpha2*B1+(1-alpha2)*colSums(M2(B1,v,S,2))
  
  alpha3=sqrt(cVar(M2(B2,v,S,3),6))-1
  B3=alpha3*B2+(1-alpha3)*colSums(M2(B2,v,S,3))
  
  alpha4 = sqrt(cVar(M2(B3,v,S,4),6))-1
  B4=alpha4*B3+(1-alpha4)*colSums(M2(B3,v,S,4))
  
  alpha5 = sqrt(cVar(M2(B4,v,S,5),6))-1
  B5=alpha5*B3+(1-alpha5)*colSums(M2(B4,v,S,5))
  
  alpha6= sqrt(cVar(M2(B5,v,S,6),6))-1
  
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

for(i in 1:6){p0[[i]]=Profile2(A,V,S)}


Ag  = c(as.numeric(stk(Ctn)),as.numeric(colSums(Ctn)))
Pr1 = c(as.numeric(stk(Br1)),as.numeric(colSums(Br1)))
Pr2 = c(as.numeric(stk(Br2)),as.numeric(colSums(Br2)))
Pr3 = c(as.numeric(stk(Br3)),as.numeric(colSums(Br3)))
Pr4 = c(as.numeric(stk(Br4)),as.numeric(colSums(Br4)))
Pr5 = c(as.numeric(stk(Br5)),as.numeric(colSums(Br5)))

dAg  = c(as.numeric((stk(dCtn))),as.numeric(colSums(dCtn))/sqrt(25))
dPr1 = c(as.numeric((stk(Br1))),as.numeric(colSums(Br1))/sqrt(25))
dPr2 = c(as.numeric((stk(Br2))),as.numeric(colSums(Br2))/sqrt(25))
dPr3 = c(as.numeric((stk(Br3))),as.numeric(colSums(Br3))/sqrt(25))
dPr4 = c(as.numeric((stk(Br4))),as.numeric(colSums(Br4))/sqrt(25))
dPr5 = c(as.numeric((stk(Br5))),as.numeric(colSums(Br5))/sqrt(25))


#Management paris 
dx = c( 0.001634364,  0.134547091, -0.070861077, -0.054303401, -0.003012789,-0.008004187)
dy = c(0.000770099,  0.239076344, -0.122185625, -0.101683414, -0.011081753,-0.004895651)

# Solve the first one = last one problem "manually"
dz = c(-0.02013663,  0.00129489,  1.48835354, -0.74707091, -0.64626246, -0.07617843,0.903213667 )
dx = c( 0.001634364,  0.134547091, -0.070861077, -0.054303401, -0.003012789,0.008004187,0.081534687)
dy = c(0.000770099,  0.239076344, -0.122185625, -0.101683414, -0.011081753,0.004895651, 0.002634589)
x= 1:7
y= seq(-1,2,0.5)
ggp= data.frame(dz,dy,dx,dz-dy,dz-dx,x,y)

ggplot(ggp,aes(x=x,y=y))+
  geom_polygon(aes(x=c(1,1,6,6,rep(NA,3)),y=c(-0.75,-0.33,1.67,0.45,rep(NA,3))),colour=NA,fill="lavenderblush3",alpha=0.3,show.legend = TRUE)+
  geom_rect(aes(xmin = 1.50,xmax=2.5,ymin = -0.75,ymax= 0.5),fill="lavenderblush1",alpha=0.2,colour= "darkred")+
  geom_abline(slope = 0.4,intercept = -0.73,color='darkred')+
  geom_abline(slope = 0.24,intercept = -0.99,color='darkred')+
  geom_vline(xintercept = c(1.5,4.5),linetype= "dashed",colour="darkred")+
  #geom_ribbon(aes(ymin=dx,ymax = dy),colour="darkred",fill="pink",alpha=0.4)+
  geom_smooth(aes(y=(dx+dy)/2),colour="red",se=FALSE)+
  geom_pointrange(aes(y=(dx+dy)/2,ymin = dx,ymax=dy),colour="darkred")+
  #geom_ribbon(aes(ymin=V1.1,ymax = V1),colour="darkgreen",fill="lightblue",alpha=0.4)+
  geom_smooth(aes(y=dz...dx),colour="blue",se=FALSE)+
  geom_pointrange(aes(y=dz...dx,ymin=dz...dy,ymax = dz),colour="darkblue")+
  #geom_errorbar(aes(x=x,ymin = V1.1, ymax = V1),inherit.aes = FALSE)+
  geom_text(aes(x=2,y=-0.65),label= "Intervention \nWindow",legend=FALSE,inherit.aes = FALSE,colour="darkred")+
  geom_segment(aes( x= 2.75,y=-0.95,xend=1.55,yend=-0.95),arrow=arrow(leng=unit(0.3,"cm")),legend=FALSE,inherit.aes = FALSE,colour="darkred")+
  geom_segment(aes( x= 3.25,y=-0.95,xend=4.45,yend=-0.95),arrow=arrow(leng=unit(0.3,"cm")),legend=FALSE,inherit.aes = FALSE,colour="darkred")+
  geom_text(aes(x=3,y=-0.95),label= "Shock \nPeriod",legend=FALSE,inherit.aes = FALSE,colour="darkred")+
  geom_text(aes(x=5,y=.97),label= "Disequilibrium State",legend=FALSE,inherit.aes = FALSE,colour="peachpuff4")+
  scale_x_continuous(breaks = rep(seq(from=0,to=8,by=1)),limits = c(0,8))+
  ylab("Impusle response functions")+
  xlab("Time steps")+
  theme_bw()