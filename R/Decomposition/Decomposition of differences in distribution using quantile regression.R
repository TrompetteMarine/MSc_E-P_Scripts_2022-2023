
#This document contains codes that allow to estimate the decomposition proposed in the
#paper "Decomposition of differences in distribution using quantile regression"

#The main command is 
#rqdeco3(depref,covref,dep,cov,probs=seq(0.01,0.99,0.01),weightsref=rep(1,length(depref)),weights=rep(1,length(dep)),nrq=0,mn=10000000,method=1)
#Arguments: 	depref	  dependent variable from the reference sector/year/gender...
#		covref	  covariates from the reference sector/year/gender... (matrix with a line by observation)
#		dep	  dependent variable from the other sector/year/gender...
#		cov	  covariates from the other sector/year/gender... (matrix with a line by observation)
#		probs	  quantiles at which the decomposition will be estimated
#		weightsref  observation weights for the reference sector/year/gender...
#		weights	  observation weights for the other reference sector/year/gender...
#		nrq	  number of regression quantile in the first step. If nrq=0 the whole conditional quantile process is estimated
#		mn	  technical number, should not be changed (see details below)
#		method	  method used in the second step, possible values: 1 and 2, (see details below)
#Values: a matrix of size length(prob) X 9
#		column 1 gives the quantiles at which the decomposition will be estimated
#		column 2 gives the estimated unconditional quantile function for the other sector using quantile regression in the first step
#		column 3 gives the raw unconditional quantile for the other sector
#		column 4 gives the estimated unconditional quantile function for the reference sector using quantile regression in the first step
#		column 5 gives the raw unconditional quantile for the reference sector sector
#		column 6 gives the total difference between both sector (column 2 - column 4)
#		column 7 gives the difference that is explained by residuals
#		column 8 gives the difference that is explained by coefficients
#		column 9 gives the difference that is explained by characteristics
#Details: we must distinguish between the first and the second step
#	first step: for small samples (number of observations < 2000 or 3000 depending on the 
#	power of the computer) it is preferable to estimate the whole process and to set nrq=0. If
#	it is not possible, you should estimate an important number of quantile regression (100, 200 or more).
#	second step: the second step is the estimation of weighted quantiles which is principally simple
#	but the number of "observations" is very high: number of observations*number of quantile
#	regression. If the computer cannot perform the second step because of limited memory, we
# 	can first try to decrease mn. If it is not sufficient, we can set method=2 and decrease mn 
#	but this increase the computation time
#
#Internal functions called by rqdeco3:
#	rqp(y,x,w): estimate the whole quantile regression process
#	rql(y,x,w,nrq): estimate nrq quantile regression on a regular grid between 0 and 1
#	wq(y,w,prob): estimate the prob weighted quantile of y using wheights w
#	evalu(x,beta,wq,w=rep(1,dim(x)[1]),prob=seq(0.01,0.99,0.01),ns=1): function called by 
#		rqdeco3 in the second step if method=1 that estimates the prob quantiles using 
#		characteristics x and coefficients beta.
#	evalu1(y,x,beta,wq,w=rep(1,length(y)),prob=seq(0.01,0.99,0.01),ns): function called by 
#		rqdeco3 in the second step if method=2 that estimates the prob quantiles using 
#		characteristics x and coefficients beta. 

library(quantreg)
library(MASS)  		#needed only for the examples below

rqp<-function(y,x,w){
  rq<-rq(y~x,weights=w,tau=-1)$sol
  rq[1,1]<-0
  beta<-rq[4:dim(rq)[1],1:(dim(rq)[2]-1),drop=FALSE]
  wq<-rq[1,2:dim(rq)[2]]-rq[1,1:(dim(rq)[2]-1)]
  return(list(beta=beta,wq=wq))
}

rql<-function(y,x,w,nrq){
  beta<-rq(y~x,tau=0.5/nrq,weights=w,method="fn")$coef
  for(i in seq(2,nrq,1)) beta<-cbind(beta,rq(y~x,tau=(i/nrq-0.5/nrq),weights=w,method="fn")$coef)
  wq<-rep(1/nrq,nrq)
  return(liste(beta=beta,wq=wq))
}

wq<-function(y,w,prob){
  o<-order(y)
  y<-y[o]; w<-w[o]
  a<-0; i<-0; s<-sum(w)
  res<-c()
  for(q in prob){
    while(a<q){
      i<-i+1
      a<-a+w[i]/s
    }
    res<-c(res,y[i])
  }
  return(res)
}

evalu<-function(x,beta,wq,w=rep(1,dim(x)[1]),prob=seq(0.01,0.99,0.01),ns=1){
  beta<-as.matrix(beta)
  x<-cbind(1,x)
  pred<-c(t(t(beta[1:floor(dim(beta)[1]/ns),,drop=FALSE])%*%t(x[,1:floor(dim(beta)[1]/ns)])))
  if(ns>1) for(k in 2:ns){
    p<-t(t(beta[floor((k-1)*dim(beta)[1]/ns+1):floor(k*dim(beta)[1]/ns),,drop=FALSE])%*%t(x[,floor((k-1)*dim(beta)[1]/ns+1):floor(k*dim(beta)[1]/ns)]))
    pred<-pred+p
  }
  nw<-c(c(w)%*%t(c(wq)))
  o<-order(pred)
  pred<-pred[o]; nw<-nw[o]
  a<-0; i<-0; s<-sum(nw)
  res<-c()
  for(q in prob){
    while(a<q){
      i<-i+1
      a<-a+nw[i]/s
    }
    res<-c(res,pred[i])
  }
  return(res)
}

evalu2<-function(x,beta,w=rep(1,dim(x)[1]),prob=seq(0.01,0.99,0.01),ns=1){
  beta<-as.matrix(beta)
  x<-cbind(1,x)
  pred<-c(t(t(beta[1:floor(dim(beta)[1]/ns),,drop=FALSE])%*%t(x[,1:floor(dim(beta)[1]/ns)])))
  if(ns>1) for(k in 2:ns){
    p<-t(t(beta[floor((k-1)*dim(beta)[1]/ns+1):floor(k*dim(beta)[1]/ns),,drop=FALSE])%*%t(x[,floor((k-1)*dim(beta)[1]/ns+1):floor(k*dim(beta)[1]/ns)]))
    pred<-pred+p
  }
  o<-order(pred)
  pred1<-pred[o]
  a<-0; i<-0
  res<-c()
  for(q in prob){
    while(a<q){
      i<-i+1
      a<-a+1/s
    }
    res<-c(res,pred1[i])
  }
  quant<-matrix(,length(prob),dim(x)[1])
  for(q in 1:length(prob)){
    for(i in 1: dim(x)[1]) quant[q,i]<-sum(pred[seq(i,(dim(x)[1]*(dim(beta)[2]-1)+i),dim(x)[1])]<=res[q])
  }
  den<-matrix(,dim(beta)[2],dim(x)[1])
  return(res)
}

evalu1<-function(y,x,beta,wq,w=rep(1,length(y)),prob=seq(0.01,0.99,0.01),ns){
  if(ns<2) ns=2
  beta<-as.matrix(beta)
  x<-cbind(1,x)
  prej<-rep(list(matrix(,0,2)),ns)
  q<-c(-Inf,quantile(y,probs=seq(1/ns,(ns-1)/ns,1/ns)),Inf)
  for(k in 1:ns){
    pre<-cbind(c(t(t(beta[,floor((k-1)*dim(beta)[2]/ns+1):floor(k*dim(beta)[2]/ns)])%*%t(x))),c(c(w)%*%t(c(wq[floor((k-1)*dim(beta)[2]/ns+1):floor(k*dim(beta)[2]/ns)]))))
    for(j in 1:ns){
      prej[[j]]<-rbind(prej[[j]],pre[pre[,1]>q[j] & pre[,1]<=q[j+1],])
    }
  }
  sum<-c(0)
  for(j in 1:ns){
    o<-order(prej[[j]][,1])
    prej[[j]]<-prej[[j]][o,]
    sum<-c(sum,sum[j]+sum(prej[[j]][,2]))
  }
  s<-sum[ns+1]; sum<-sum/s
  res<-c(); a<-0; j<-1; i<-0; t<-prej[[j]]; a<-sum[j]
  for(q in prob){
    while(q>sum[j+1]){
      j<-j+1
      i<-0
      t<-prej[[j]]
      a<-sum[j]
    }
    while(a<q){
      i<-i+1
      a<-a+t[i,2]/s
    }
    res<-c(res,t[i,1])
  }
  return(res)
}

rqdeco3<-function(depref,covref,dep,cov,probs=seq(0.01,0.99,0.01),weightsref=rep(1,length(depref)),weights=rep(1,length(dep)),nrq=0,mn=10000000,method=1){
  probs<-sort(probs)
  cov<-as.matrix(cov)
  covref<-as.matrix(covref)
  if(nrq==0){
    rref<-rqp(depref,covref,weightsref)
    r<-rqp(dep,cov,weights)
  } else{
    rref<-rql(depref,covref,weightsref,nrq)
    r<-rql(dep,cov,weights,nrq)
  }
  mref<-rq(depref~covref,tau=0.5,weights=weightsref,method="fn")$coef
  m<-rq(dep~cov,tau=0.5,weights=weights,method="fn")$coef
  if(method==1){
    pred<-evalu(cov,r$beta,r$wq,weights,probs,min(ceiling(dim(r$beta)[1]*dim(r$beta)[2]*length(dep)/mn),dim(r$beta)[1]))
    predcounter1<-evalu(cov,m+rref$beta-mref,rref$wq,weights,probs,min(ceiling(dim(rref$beta)[1]*dim(rref$beta)[2]*length(dep)/mn),dim(rref$beta)[1]))
    predcounter2<-evalu(cov,rref$beta,rref$wq,weights,probs,min(ceiling(dim(rref$beta)[1]*dim(rref$beta)[2]*length(dep)/mn),dim(rref$beta)[1]))
    predref<-evalu(covref,rref$beta,rref$wq,weightsref,probs,min(ceiling(dim(rref$beta)[1]*dim(rref$beta)[2]*length(depref)/mn),dim(rref$beta)[1]))
  }	
  if(method==2){
    pred<-evalu1(dep,cov,r$beta,r$wq,weights,probs,ceiling(dim(r$beta)[1]*dim(r$beta)[2]*length(dep)/mn))
    predcounter1<-evalu1(c(dep,depref),cov,m+rref$beta-mref,rref$wq,weights,probs,ceiling(dim(rref$beta)[1]*dim(rref$beta)[2]*length(dep)/mn))
    predcounter2<-evalu1(c(dep,depref),cov,rref$beta,rref$wq,weights,probs,ceiling(dim(rref$beta)[1]*dim(rref$beta)[2]*length(dep)/mn))
    predref<-evalu1(depref,covref,rref$beta,rref$wq,weightsref,probs,ceiling(dim(rref$beta)[1]*dim(rref$beta)[2]*length(depref)/mn))
  }	
  predmcounter<-evalu(cov,mref,1,weights,probs,1)
  predm<-evalu(cov,m,1,weights,probs,1)
  qdep<-wq(dep,weights,probs)
  qdepref<-wq(depref,weightsref,probs)
  results<-cbind(probs,pred,qdep,predref,qdepref,pred-predref,pred-predcounter1,predcounter1-predcounter2,predcounter2-predref)
  return(results)
}

#artificial example 1
#500 observations, 5 regressors, whole quantile regression process is estimated, computation time 
#using a Pentium 4 CPU 2.53 GHz with 1 Go of RAM: 10 seconds

x<-mvrnorm(500,rep(1,5),diag(rep(1,5)))	#generate the variables
y<-1+x%*%rep(1,5)+rnorm(500,sd=2)		
xref<-mvrnorm(500,rep(1,5),diag(rep(1,5)))		
yref<-1+xref%*%rep(1,5)+rnorm(500,sd=4)		

res1<-rqdeco3(yref,xref,y,x)			#estimation of the decomposition

plot(res1[,1],res1[,2],type="l")		#plot the estimated quantiles using quantile regression
lines(res1[,1],res1[,3],col="red")		#and the raw quantiles to check the linear model
lines(res1[,1],res1[,4],col="green")
lines(res1[,1],res1[,5],col="blue")

plot(res1[,1],res1[,6],type="l")		#plot the total differences  
lines(res1[,1],res1[,7],col="red")		#plot the part explained by residuals
lines(res1[,1],res1[,8],col="blue")		#plot the part explained by coefficients
lines(res1[,1],res1[,9],col="green")		#plot the part explained by characteristics

#conclusion: all the differences are explained by the residuals

#artifcial example 2
#50000 observations, 1 regressor, 200 quantile regression are estimated, computation time: 17 minutes

x<-runif(50000)					#generate the variables
y<-1+x+rnorm(50000,sd=0.5)
xref<-runif(50000)+0.1		
yref<-1+xref*1.1+rnorm(50000,sd=0.5)

res2<-rqdeco3(yref,xref,y,x,nrq=200)

plot(res2[,1],res2[,2],type="l")		#plot the estimated quantiles using quantile regression
lines(res2[,1],res2[,3],col="red")		#and the raw quantiles to check the linear model
lines(res2[,1],res2[,4],col="green")
lines(res2[,1],res2[,5],col="blue")

plot(res2[,1],res2[,6],type="l",ylim=c(-0.35,0.1)) #plot the total differences  
lines(res2[,1],res2[,7],col="red")		  #plot the part explained by residuals
lines(res2[,1],res2[,8],col="blue")		  #plot the part explained by coefficients
lines(res2[,1],res2[,9],col="green")		  #plot the part explained by characteristics

#conclusion: characteristics explain why y is lower than yref, the effect is the same over the distribution
#the effect of the coeficient is 0 at the lowest quantiles but increase lineary with the quantile
#residuals have no effects

#artifcial example 3
#20000 observations, 20 regressors, 200 quantile regression are estimated, computation time: 14 minutes

x<-mvrnorm(20000,rep(1,20),diag(rep(1.1,20)))	#generate the variables
y<-1+x%*%rep(1,20)+rnorm(20000)		
xref<-mvrnorm(20000,rep(1,20),diag(rep(1,20)))		
yref<-1+xref%*%rep(1,20)+rnorm(20000)		

res3<-rqdeco3(yref,xref,y,x,nrq=200)

plot(res3[,1],res3[,2],type="l")		#plot the estimated quantiles using quantile regression
lines(res3[,1],res3[,3],col="red")		#and the raw quantiles to check the linear model
lines(res3[,1],res3[,4],col="green")
lines(res3[,1],res3[,5],col="blue")

plot(res3[,1],res3[,6],type="l")   		  #plot the total differences  
lines(res3[,1],res3[,7],col="red")		  #plot the part explained by residuals
lines(res3[,1],res3[,8],col="blue")		  #plot the part explained by coefficients
lines(res3[,1],res3[,9],col="green")		  #plot the part explained by characteristics

#conclusion: the lower variance in the reference sector is explained by a lower variance of the
#distribution of the characteristics. Coefficients and residuals play no role.
#rqdeco3.txt
#Affichage de rqdeco3.txt