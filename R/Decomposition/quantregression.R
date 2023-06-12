
#revised: 23.05.2006
#The main functions are rqdeco for the parametric estimator and qte and qtet for the nonparametric estimator
#
#rqdeco(y0,y1,reg0,reg1,w0,w1,nrq,prob,est.cov,se0,se1,sec,hs,ns)
#arguments: 	y0: dependent variable for the control units
#		y1: dependent veriable for the treated units
#		reg0: regressors for the control units (without a constant)
#		reg1: regressors for the treated units (without a constant)
#		w0: a vector of weights for the control units. (default: 1)
#		w1: a vector of weights for the treated units. (default: 1)
#		nrq: number of quantile regression to be estimated in the first step (default: -1, the whole process is estimated)
#		prob: quantiles of interest (default: seq(0.01,0.99,0.01))
#		est.cov: if TRUE then the variances are estimated
#		se0,se1: method1 used to estimate the asymptotic covariance of the first step quantile regression. It must be
#			 of the form list(c("m1","m2","m3"),c(n0,n1)). The method "m1" will be used for the lowest n0 quantiles.
#			 The method "m3" will be used for the n1 highest quantiles. The method "m2" will be used for all other
#			 quantiles. Possible methods are: "iid", "nid1", "nid2", "ker". "iid" presumes that the errors are iid
#			 as in Koenker and Bassett (1978). "nid1" and "nid2" use the method of Hendrick and Koenker (1992);
#			 "nid1" recompute 2 quantile regression while "nid2" use the already estimated regression. "ker" is an
#			 implementation of the idea of Powell (1984). The default is list(c("iid","nid2","iid"),c(5,5)) which
#			 estimate the variance of the extrem quantiles assuming iid sampling and use the "nid2" method for the
#			 rest. se0 is for the control units and se1 for the trated units.
#		sec: method used to estimate the conditional density function for the counterfactual distribution. Basically
#		     the same than for se0 but "ker" is not allowed because out-of-sample prediction may be necessary.
#		hs: Use Hall Sheather bandwidth for sparsity estimation If false revert to Bofinger bandwidth.
#		ns, method2: should not be changed
#value:		res: quantiles of the unconditional distributions. First column is for the control units, second column for
#		     the treated units and the third for the counterfactual distribution
#		cov: list of length(prob) covariance matrix for each of the quantile
#		deco: first column: total differential, second column: s.e. of the total differential, third column: effects
#		      of coefficients, fourth column: s.e. of the effects of coefficients, fifth column: effects of characteristics,
#		      sixth column: s.e. of the effects of characteristics.
#
#qte(y0,x0,y1,x1,hmed0,hmed1,nq0,nq1,prob,method0,method1,q0,q1,step0,step1)
#qtet(y0,x0,y1,x1,hmed0,hmed1,nq0,nq1,prob,method0,method1,q0,q1,step0,step1)
#arguments:	y0: dependent variable for the control units
#		y1: dependent veriable for the treated units
#		reg0: regressor for the control units (only 1-dimensional)
#		reg1: regressor for the treated units (only 1-dimensional)
#		hmed0: bandwidth for the median regression for the control units
#		hmed0: bandwidth for the median regression for the treated units
#		nq0: number of quantile regression to be estimated in the first step for the control units (default 100)
#		nq1: number of quantile regression to be estimated in the first step for the treated units (default 100)
#		prob: quantiles of interest (default: seq(0.01,0.99,0.01))
#		method0, method1, q0, q1, step0 and step1: 
#			if method==1: the local linear quantile regressions are estimated at all observations,
#			if method==2: the local linear quantile regression are estimated at a smaller number of points and 
#			the predicted quantiles are extrapolated. The local linear regression are estimated at 
#			seq(q[1]+step/2,q[2]-step/2,step). Additionaly, quantile regression are estimated at all observations
#			below q[1] aor above q[2]. Note that method==2 lead to potentially worse results than method==1 but 
#			can be computed much faster. If step is small enough, the differences between both methods are very small.
#			(q[2]-q[1])/step must be an integer number.
#value:		res: quantiles of the unconditional distributions. First column: control outcome, second column: treated outcome.
#		cov: first column: s.e. of the first column of res, second column: s.e. of the second column of res, third column:
#		     covariance between the first and second column of res.
#		qte, qtet: qte and qtet with estimated s.e.

library(quantreg)

psum<-function(x) c(rep(1,dim(x)[1])%*%x)

rqp<-function(y,x,w=rep(1,length(y))){
  rq<-rq(y~x,weights=w,tau=-1)$sol
  rq[1,1]<-0
  beta<-rq[4:dim(rq)[1],1:(dim(rq)[2]-1),drop=FALSE]
  wq<-rq[1,2:dim(rq)[2]]-rq[1,1:(dim(rq)[2]-1)]
  q<-(rq[1,2:dim(rq)[2]]+rq[1,1:(dim(rq)[2]-1)])/2
  list(beta=beta,wq=wq,q=q)
}

rql<-function(y,x,nrq,w=rep(1,length(y)),method="fn"){
  try(beta<-rq.wfit(cbind(1,x),y,tau=0.5/nrq,weights=w,method=method)$coef,TRUE)
  if(exists("beta")==0) beta<-rq.wfit(cbind(1,x),y,tau=0.5/nrq,weights=w,method="br")$coef
  if(max(is.na(beta)[1])==1) beta<-rq.wfit(cbind(1,x),y,tau=0.5/nrq,weights=w,method="br")$coef
  for(i in seq(2,nrq,1)){
    try(tempn<-rq.wfit(cbind(1,x),y,tau=(i/nrq-0.5/nrq),weights=w,method=method)$coef,TRUE)
    if(exists("tempn")==0) tempn<-rq.wfit(cbind(1,x),y,tau=(i/nrq-0.5/nrq),weights=w,method="br")$coef
    if(max(is.na(tempn)[1])==1) tempn<-rq.wfit(cbind(1,x),y,tau=(i/nrq-0.5/nrq),weights=w,method="br")$coef
    beta<-cbind(beta,tempn)
    rm(tempn)
  }
  wq<-rep(1/nrq,nrq)
  q<-seq(0.5/nrq,1-0.5/nrq,1/nrq)
  list(beta=beta,wq=wq,q=q)
}

rqpl<-function(y,x,nrq,w=rep(1,length(y))){
  rq<-rq(y~x,weights=w,tau=-1)$sol
  rq[1,1]<-0
  beta<-rq[4:dim(rq)[1],sum(rq[1,]<0.5/nrq),drop=FALSE]
  for(i in 2:nrq) beta<-cbind(beta,rq[4:dim(rq)[1],sum(rq[1,]<(i/nrq-0.5/nrq)),drop=FALSE])
  wq<-rep(1/nrq,nrq)
  q<-seq(0.5/nrq,1-0.5/nrq,1/nrq)
  list(beta=beta,wq=wq,q=q)
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
  res
}

evalu<-function(x,beta,wq=rep(1,dim(beta)[2]),w=rep(1,dim(x)[1]),prob=seq(0.01,0.99,0.01),ns=1){
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
  res
}

rqcov<-function(dep,reg,beta,tau,sel,se=rep("ker",dim(beta)[2]),method="br",weights=rep(1,length(dep)),hs=TRUE){
  reg<-as.matrix(reg); beta<-as.matrix(beta)
  fxxinv<-matrix(,dim(reg)[2],dim(reg)[2]*length(sel))
  den<-matrix(,dim(reg)[1],length(sel))
  i<-1; j<-1
  temp0<-diag(dim(reg)[2])
  while(j<=length(sel)){
    q<-sel[j]
    uhat<-c(dep-reg%*%beta[,q])
    if(se[q]=="nid1"){
      h<-bandwidth.rq(tau[q],length(dep),hs=hs)
      if(tau[q]-h<0.001) h<-tau[q]-0.001
      if(tau[q]+h>0.999) h<-0.999-tau[q]
      betahi<-rq.wfit(reg,dep,tau=(tau[q]+h),weights=w,method=method)$coef
      betalo<-rq.wfit(reg,dep,tau=(tau[q]-h),weights=w,method=method)$coef
      dyhat<-reg%*%(betahi-betalo)
      dyhat[dyhat>=0 & dyhat<0.1*median(dyhat)]<-(-1)
      den[,i]<-pmax(0,2*h/dyhat)
    }
    if(se[q]=="iid"){
      pz<-sum(abs(uhat) < .Machine$double.eps^(2/3))
      h <- max(dim(reg)[2],ceiling(length(dep)*bandwidth.rq(tau[q],length(dep),hs=hs)))
      ir <-(pz + 1):(h + pz + 1)
      ord.resid <- sort(uhat[order(abs(uhat))][ir])
      xt <- ir/(length(dep) - dim(reg)[2])
      den[,i] <- 1/rq(ord.resid ~ xt)$coef[2]
    }
    if(se[q]=="ker"){
      h<-bandwidth.rq(tau[q],dim(reg)[1],hs=hs)
      if(tau[q]-h<0.001) h<-tau[q]-0.001
      if(tau[q]+h>0.999) h<-0.999-tau[q]
      h <- (qnorm(tau[q]+h)-qnorm(tau[q]-h))*min(sqrt(var(uhat)),(quantile(uhat,0.75)-quantile(uhat,0.25))/1.34)
      den[,i]<-dnorm(uhat/h)/h
    }
    if(se[q]=="nid2"){
      if(q==1){
        h<-bandwidth.rq(tau[q],length(dep),hs=hs)
        hhi<-sum(tau<=tau[-q][abs(tau[-q]-(tau[q]+h))==min(abs(tau[-q]-(tau[q]+h)))])
        dyhat<-reg%*%(beta[,hhi]-beta[,1])
        dyhat[dyhat>=0 & dyhat<0.1*median(dyhat)]<-(-1)
        den[,i]<-pmax(0,(tau[hhi]-tau[1])/dyhat)
      } else if(q<length(tau)){
        h<-bandwidth.rq(tau[q],length(dep),hs=hs)
        hlo<-sum(tau<=tau[-q][abs(tau[-q]-(tau[q]-h))==min(abs(tau[-q]-(tau[q]-h)))])
        hhi<-sum(tau<=tau[-q][abs(tau[-q]-(tau[q]+h))==min(abs(tau[-q]-(tau[q]+h)))])
        dyhat<-reg%*%(beta[,hhi]-beta[,hlo])
        dyhat[dyhat>=0 & dyhat<0.1*median(dyhat)]<-(-1)
        den[,i]<-pmax(0,(tau[hhi]-tau[hlo])/dyhat)
      } else if(q==length(tau)){
        h<-bandwidth.rq(tau[q],length(dep),hs=hs)
        hlo<-sum(tau<=tau[-q][abs(tau[-q]-(tau[q]-h))==min(abs(tau[-q]-(tau[q]-h)))])
        dyhat<-reg%*%(beta[,length(tau)]-beta[,hlo])
        dyhat[dyhat>=0 & dyhat<0.1*median(dyhat)]<-(-1)
        den[,i]<-pmax(0,(tau[q]-tau[hlo])/dyhat)
      }
    }
    if(min(abs(eigen(qr(sqrt(den[,i]) * reg)$qr[1:dim(reg)[2],1:dim(reg)[2]],only.values=TRUE)$values))>0.00005){
      temp<-backsolve(qr(sqrt(den[,i]) * reg)$qr[1:dim(reg)[2],1:dim(reg)[2]],temp0)
      fxxinv[,((i-1)*dim(reg)[2]+1):(i*dim(reg)[2])]<-temp%*%t(temp)
      i<-i+1; j<-j+1} else se[q]<-"ker"
  }
  den<-t(den)
  cov<-matrix(,length(sel)*dim(reg)[2],length(sel)*dim(reg)[2])
  for(q in 1:length(sel)){
    for( i in 1:q){
      cov[((q-1)*dim(reg)[2]+1):(q*dim(reg)[2]),((i-1)*dim(reg)[2]+1):(i*dim(reg)[2])]<-(min(tau[sel[q]],tau[sel[i]])-tau[sel[q]]*tau[sel[i]])*fxxinv[,((q-1)*dim(reg)[2]+1):(q*dim(reg)[2])]%*%crossprod(reg)%*%fxxinv[,((i-1)*dim(reg)[2]+1):(i*dim(reg)[2])]
      cov[((i-1)*dim(reg)[2]+1):(i*dim(reg)[2]),((q-1)*dim(reg)[2]+1):(q*dim(reg)[2])]<-t(cov[((q-1)*dim(reg)[2]+1):(q*dim(reg)[2]),((i-1)*dim(reg)[2]+1):(i*dim(reg)[2])])
    }
  }
  list(den=den,cov=cov)
}

rqcov1<-function(dep,reg,beta,tau,sel,se=rep("ker",dim(beta)[2]),method="br",weights=rep(1,length(dep)),hs=TRUE){
  reg<-as.matrix(reg); beta<-as.matrix(beta)
  fxxinv<-matrix(,dim(reg)[2],dim(reg)[2]*length(sel))
  den<-matrix(,dim(reg)[1],length(sel))
  i<-1
  temp0<-diag(dim(reg)[2])
  for(q in sel){
    uhat<-c(dep-reg%*%beta[,q])
    if(se[q]=="nid1"){
      h<-bandwidth.rq(tau[q],length(dep),hs=hs)
      if(tau[q]-h<0.001) h<-tau[q]-0.001
      if(tau[q]+h>0.999) h<-0.999-tau[q]
      betahi<-rq.wfit(reg,dep,tau=(tau[q]+h),weights=w,method=method)$coef
      betalo<-rq.wfit(reg,dep,tau=(tau[q]-h),weights=w,method=method)$coef
      dyhat<-reg%*%(betahi-betalo)
      dyhat[dyhat>=0 & dyhat<0.1*median(dyhat)]<-(-1)
      den[,i]<-pmax(0,2*h/dyhat)
    }
    if(se[q]=="iid"){
      pz<-sum(abs(uhat) < .Machine$double.eps^(2/3))
      h <- max(dim(reg)[2],ceiling(length(dep)*bandwidth.rq(tau[q],length(dep),hs=hs)))
      ir <-(pz + 1):(h + pz + 1)
      ord.resid <- sort(uhat[order(abs(uhat))][ir])
      xt <- ir/(length(dep) - dim(reg)[2])
      den[,i] <- 1/rq(ord.resid ~ xt)$coef[2]
    }
    if(se[q]=="nid2"){
      if(q==1){
        h<-bandwidth.rq(tau[q],length(dep),hs=hs)
        hhi<-sum(tau<=tau[-q][abs(tau[-q]-(tau[q]+h))==min(abs(tau[-q]-(tau[q]+h)))])
        dyhat<-reg%*%(beta[,hhi]-beta[,1])
        dyhat[dyhat>=0 & dyhat<0.1*median(dyhat)]<-(-1)
        den[,i]<-pmax(0,(tau[hhi]-tau[1])/dyhat)
      } else if(q<length(tau)){
        h<-bandwidth.rq(tau[q],length(dep),hs=hs)
        hlo<-sum(tau<=tau[-q][abs(tau[-q]-(tau[q]-h))==min(abs(tau[-q]-(tau[q]-h)))])
        hhi<-sum(tau<=tau[-q][abs(tau[-q]-(tau[q]+h))==min(abs(tau[-q]-(tau[q]+h)))])
        dyhat<-reg%*%(beta[,hhi]-beta[,hlo])
        dyhat[dyhat>=0 & dyhat<0.1*median(dyhat)]<-(-1)
        den[,i]<-pmax(0,(tau[hhi]-tau[hlo])/dyhat)
      } else if(q==length(tau)){
        h<-bandwidth.rq(tau[q],length(dep),hs=hs)
        hlo<-sum(tau<=tau[-q][abs(tau[-q]-(tau[q]-h))==min(abs(tau[-q]-(tau[q]-h)))])
        dyhat<-reg%*%(beta[,length(tau)]-beta[,hlo])
        dyhat[dyhat>=0 & dyhat<0.1*median(dyhat)]<-(-1)
        den[,i]<-pmax(0,(tau[q]-tau[hlo])/dyhat)
      }
    }
    i<-i+1
  }
  den<-t(den)
  den
}

rqdeco<-function(y0,y1,reg0,reg1,w0=rep(1,length(y0)),w1=rep(1,length(y1)),nrq=-1,prob=seq(0.01,0.99,0.01),est.cov=TRUE,se0=list(c("iid","nid2","iid"),c(5,5)),se1=list(c("iid","nid2","iid"),c(5,5)),sec=list(c("iid","nid2","iid"),c(5,5)),hs=TRUE,ns=1,method2=1){
  if(nrq==(-1)){
    temp0<-rqp(y0,reg0,w0)
    temp1<-rqp(y1,reg1,w1)
  } else if(method2==1){
    temp0<-rql(y0,reg0,nrq,w0)
    temp1<-rql(y1,reg1,nrq,w1)
  } else{
    temp0<-rqpl(y0,reg0,nrq,w0)
    temp1<-rqpl(y1,reg1,nrq,w1)
  }
  beta0<-temp0$beta;beta1<-temp1$beta;tau0<-temp0$q;tau1<-temp1$q;wq0<-temp0$wq;wq1<-temp1$wq
  reg0<-cbind(1,reg0);reg1<-cbind(1,reg1);beta0<-as.matrix(beta0);beta1<-as.matrix(beta1)
  pred0<-c(t(t(beta0[1:floor(dim(beta0)[1]/ns),,drop=FALSE])%*%t(reg0[,1:floor(dim(beta0)[1]/ns)])))
  if(ns>1) for(k in 2:ns){
    p<-t(t(beta0[floor((k-1)*dim(beta0)[1]/ns+1):floor(k*dim(beta0)[1]/ns),,drop=FALSE])%*%t(reg0[,floor((k-1)*dim(beta0)[1]/ns+1):floor(k*dim(beta0)[1]/ns)]))
    pred0<-pred0+p
  }
  nw<-c(c(w0)%*%t(c(wq0)))
  o<-order(pred0)
  predo<-pred0[o]; nw<-nw[o]
  a<-0; i<-0; s<-sum(nw)
  res0<-c()
  prob10<-c(prob,seq(0.00005,0.99995,0.0001))
  rp0<-rank(prob10); o0<-order(prob10)
  for(q in prob10[o0]){
    while(a<q){
      i<-i+1
      a<-a+nw[i]/s
    }
    res0<-c(res0,predo[i])
  }
  res01<-res0[rp0][(length(prob)+1):length(prob10)]
  res0<-res0[rp0][1:length(prob)]
  predc<-c(t(t(beta0[1:floor(dim(beta0)[1]/ns),,drop=FALSE])%*%t(reg1[,1:floor(dim(beta0)[1]/ns)])))
  if(ns>1) for(k in 2:ns){
    p<-t(t(beta0[floor((k-1)*dim(beta0)[1]/ns+1):floor(k*dim(beta0)[1]/ns),,drop=FALSE])%*%t(reg1[,floor((k-1)*dim(beta0)[1]/ns+1):floor(k*dim(beta0)[1]/ns)]))
    predc<-predc+p
  }
  nw<-c(c(w1)%*%t(c(wq0)))
  o<-order(predc)
  predo<-predc[o]; nw<-nw[o]
  a<-0; i<-0; s<-sum(nw)
  resc<-c()
  for(q in prob10[o0]){
    while(a<q){
      i<-i+1
      a<-a+nw[i]/s
    }
    resc<-c(resc,predo[i])
  }
  resc1<-resc[rp0][(length(prob)+1):length(prob10)]
  resc<-resc[rp0][1:length(prob)]
  pred1<-c(t(t(beta1[1:floor(dim(beta1)[1]/ns),,drop=FALSE])%*%t(reg1[,1:floor(dim(beta1)[1]/ns)])))
  if(ns>1) for(k in 2:ns){
    p<-t(t(beta1[floor((k-1)*dim(beta1)[1]/ns+1):floor(k*dim(beta1)[1]/ns),,drop=FALSE])%*%t(reg1[,floor((k-1)*dim(beta1)[1]/ns+1):floor(k*dim(beta1)[1]/ns)]))
    pred1<-pred1+p
  }
  nw<-c(c(w1)%*%t(c(wq1)))
  o<-order(pred1)
  predo<-pred1[o]; nw<-nw[o]
  a<-0; i<-0; s<-sum(nw)
  res1<-c()
  for(q in prob10[o0]){
    while(a<q){
      i<-i+1
      a<-a+nw[i]/s
    }
    res1<-c(res1,predo[i])
  }
  res11<-res1[rp0][(length(prob)+1):length(prob10)]
  res1<-res1[rp0][1:length(prob)]
  res<-cbind(res0,res1,resc)
  if(est.cov==TRUE){
    quant0<-quant01<-matrix(,length(prob),dim(reg0)[1])
    for(q in 1:length(prob)) for(i in 1: dim(reg0)[1]) quant0[q,i]<-sum(wq0[reg0[i,]%*%beta0<=res0[q]])
    for(q in 1:length(prob)) for(i in 1: dim(reg0)[1]) quant01[q,i]<-max(sum(tau0<=quant0[q,i]),1)
    quant1<-quant11<-matrix(,length(prob),dim(reg1)[1])
    for(q in 1:length(prob)) for(i in 1: dim(reg1)[1]) quant1[q,i]<-sum(wq1[reg1[i,]%*%beta1<=res1[q]])
    for(q in 1:length(prob)) for(i in 1: dim(reg1)[1]) quant11[q,i]<-max(sum(tau1<=quant1[q,i]),1)
    quantc<-quantc1<-matrix(,length(prob),dim(reg1)[1])
    for(q in 1:length(prob)) for(i in 1: dim(reg1)[1]) quantc[q,i]<-sum(wq0[reg1[i,]%*%beta0<=resc[q]])
    for(q in 1:length(prob)) for(i in 1: dim(reg1)[1]) quantc1[q,i]<-max(sum(tau0<=quantc[q,i]),1)
    quant2<-sort(c(quant01,quantc1))
    sel0<-quant2[1]
    for(i in 2:length(quant2)) if(quant2[i]!=quant2[i-1]) sel0<-c(sel0,quant2[i])
    temp<-rqcov(y0,reg0,beta0,tau0,sel0,c(rep(se0[[1]][1],se0[[2]][1]),rep(se0[[1]][2],length(tau0)-sum(se0[[2]])),rep(se0[[1]][3],se0[[2]][2])),method,w0,hs)
    cov0<-temp$cov; den0<-temp$den
    temp<-rqcov1(y1,reg1,beta0,tau0,sel0,c(rep(sec[[1]][1],sec[[2]][1]),rep(sec[[1]][2],length(tau0)-sum(sec[[2]])),rep(sec[[1]][3],sec[[2]][2])),method,w0,hs)
    denc<-temp
    quant2<-sort(c(quant11))
    sel1<-quant2[1]
    for(i in 2:length(quant2)) if(quant2[i]!=quant2[i-1]) sel1<-c(sel1,quant2[i])
    temp<-rqcov(y1,reg1,beta1,tau1,sel1,c(rep(se1[[1]][1],se1[[2]][1]),rep(se1[[1]][2],length(tau1)-sum(se1[[2]])),rep(se1[[1]][3],se1[[2]][2])),method,w1,hs)
    cov1<-temp$cov; den1<-temp$den
    r10<-matrix(,length(prob),length(sel0)*dim(beta0)[1])
    r1c<-matrix(,length(prob),length(sel0)*dim(beta0)[1])
    r11<-matrix(,length(prob),length(sel1)*dim(beta1)[1])
    for(q in 1:length(prob)){
      for( i in 1:length(sel0)){
        r10[q,((i-1)*dim(beta0)[1]+1):(i*dim(beta0)[1])]<-mean(as.data.frame(den0[i,]*(quant01[q,]==sel0[i])*reg0))
        r1c[q,((i-1)*dim(beta0)[1]+1):(i*dim(beta0)[1])]<-mean(as.data.frame(denc[i,]*(quantc1[q,]==sel0[i])*reg1))
      }
      for( i in 1:length(sel1)) r11[q,((i-1)*dim(beta1)[1]+1):(i*dim(beta1)[1])]<-mean(as.data.frame(den1[i,]*(quant11[q,]==sel1[i])*reg1))
    }
    r20<-c(); r21<-c(); r2c<-c()
    for(q in 1:length(prob)){
      r20<-c(r20,density(res01,n=1,from=res0[q],to=res0[q],bw=1.06*min(sd(res01),(quantile(res01,0.75)-quantile(res01,0.25))/1.34)*(length(y0)*dim(beta0)[2])^(-0.2))$y)
      r21<-c(r21,density(res11,n=1,from=res1[q],to=res1[q],bw=1.06*min(sd(res01),(quantile(res01,0.75)-quantile(res01,0.25))/1.34)*(length(y1)*dim(beta1)[2])^(-0.2))$y)
      r2c<-c(r2c,density(resc1,n=1,from=resc[q],to=resc[q],bw=1.06*min(sd(res01),(quantile(res01,0.75)-quantile(res01,0.25))/1.34)*(length(y1)*dim(beta0)[2])^(-0.2))$y)
    }
    cov<-rep(list(matrix(,3,3)),length(prob))
    for(q in 1:length(prob)){
      cov[[q]][1,1]<-mean((prob[q]-quant0[q,])^2)/(r20[q]^2*dim(reg0)[1])+t(r10[q,])%*%cov0%*%r10[q,]/r20[q]^2
      cov[[q]][2,2]<-mean((prob[q]-quant1[q,])^2)/(r21[q]^2*dim(reg1)[1])+t(r11[q,])%*%cov1%*%r11[q,]/r21[q]^2
      cov[[q]][3,3]<-mean((prob[q]-quantc[q,])^2)/(r2c[q]^2*dim(reg1)[1])+t(r1c[q,])%*%cov0%*%r1c[q,]/r2c[q]^2
      cov[[q]][1,2]<-cov[[q]][2,1]<-0
      cov[[q]][1,3]<-cov[[q]][3,1]<-t(r1c[q,])%*%cov0%*%r10[q,]/(r2c[q]*r20[q])
      cov[[q]][2,3]<-cov[[q]][3,2]<-mean((prob[q]-quantc[q,])*(prob[q]-quant1[q,]))/(r2c[q]*r21[q]*dim(reg1)[1])
    }
    deco<-matrix(,length(prob),6)
    for(q in 1:length(prob)) deco[q,]<-c(res1[q]-res0[q],(cov[[q]][1,1]+cov[[q]][2,2])^0.5,res1[q]-resc[q],(cov[[q]][3,3]+cov[[q]][2,2]-2*cov[[q]][2,3])^0.5,resc[q]-res0[q],(cov[[q]][3,3]+cov[[q]][1,1]-2*cov[[q]][1,3])^0.5)
    colnames(deco)<-c("total differential","s.d. total","coefficients","s.d. coef","characteristics","s.d. char")
  } else{
    deco<-matrix(,length(prob),3)
    for(q in 1:length(prob)) deco[q,]<-c(res1[q]-res0[q],res1[q]-resc[q],resc[q]-res0[q])
    colnames(deco)<-c("total differential","coefficients","characteristics")
    cov<-NA}
  list(res=res,cov=cov,deco=deco)
}

llrq<-function(ev,y,x,hmed,tau=seq(0.005,0.995,0.01)){
  z<-x-ev
  ypred<-c()
  for(q in 1:length(tau)){
    h<-hmed*(tau[q]*(1-tau[q])*dnorm(qnorm(0.5))^2/(0.25*dnorm(qnorm(tau[q]))^2))^0.2
    wx<-(3/4)*(1-(z/h)^2)*(abs(z)<h)
    y1<-y[wx>0]; z1<-z[wx>0]; wx<-wx[wx>0]
    ypred<-c(ypred,rq.wfit(cbind(1,z1),y1,weights=wx,tau=tau[q])$coef[1])
  }
  ypred
}

npirq<-function(x1,y,x,nq,hmed){
  ypred<-matrix(,nq,length(x1)); x1<-sort(x1)
  for(i in 1:length(x1)){
    hmed1<-hmed
    while(sum(abs(x-x1[i])<hmed1)==0) hmed1<-hmed1*1.1
    if(sum(abs(x-x1[i])<hmed1)==1){
      ypred[,i]<-rep(y[abs(x-x1[i])<hmed1],nq)
    } else ypred[,i]<-llrq(x1[i],y,x,hmed1,tau=seq(0.5/nq,1-0.5/nq,1/nq))
  }
  ypred
}

llrq2<-function(evc,ev,y,x,hmed,tau=seq(0.005,0.995,0.01)){
  z<-x-evc
  ypred<-matrix(,length(tau),length(ev))
  for(q in 1:length(tau)){
    h<-hmed*(tau[q]*(1-tau[q])*dnorm(qnorm(0.5))^2/(0.25*dnorm(qnorm(tau[q]))^2))^0.2
    wx <- (3/4)*(1-(z/h)^2)*(abs(z)<h)
    y1<-y[wx>0]; z1<-z[wx>0]; wx<-wx[wx>0]
    temp<-rq.wfit(cbind(1,z1),y1,weights=wx,tau=tau[q])$coef
    ypred[q,]<-temp[1]+ev*temp[2]
  }
  ypred
}

npirq2<-function(x1,y,x,nq,hmed,q,step){
  ypred<-matrix(,nq,0); x1<-sort(x1)
  if(sum(x1<q[1])>0) for(i in 1:sum(x1<q[1])){
    hmed1<-hmed
    while(sum(abs(x-x1[i])<hmed1)==0) hmed1<-hmed1*1.1
    if(sum(abs(x-x1[i])<hmed1)==1){
      ypred<-cbind(ypred,rep(y[abs(x-x1[i])<hmed1],nq))
    } else ypred<-cbind(ypred,llrq(x1[i],y,x,hmed1,tau=seq(0.5/nq,1-0.5/nq,1/nq)))
  }
  for(i in seq(q[1]+step/2,q[2]-step/2,step)){
    hmed1<-hmed
    while(sum(abs(i-x)<hmed1)==0) hmed1<-hmed1*1.1
    if(sum(abs(i-x)<hmed1)==1){
      ypred<-cbind(ypred,matrix(y[abs(i-x)<hmed1],nq,length(x1[x1>(i-step/2) & x1<(i+step/2)])))
    } else ypred<-cbind(ypred,llrq2(i,x1[x1>(i-step/2) & x1<(i+step/2)]-i,y,x,hmed1,tau=seq(0.5/nq,1-0.5/nq,1/nq)))
  }
  if(sum(x1>q[2])>0) for(i in (sum(x1<=q[2])+1):length(x1)){
    hmed1<-hmed
    while(sum(abs(x-x1[i])<hmed1)==0) hmed1<-hmed1*1.1
    if(sum(abs(x-x1[i])<hmed1)==1){
      ypred<-cbind(ypred,rep(y[abs(x-x1[i])<hmed1],nq))
    } else ypred<-cbind(ypred,llrq(x1[i],y,x,hmed1,tau=seq(0.5/nq,1-0.5/nq,1/nq)))
  }
  ypred
}

qte<-function(y0,x0,y1,x1,hmed0,hmed1,nq0=100,nq1=100,prob=seq(0.01,0.99,0.01),method0=1,method1=1,q0=quantile(x1,c(0.05,0.95)),q1=quantile(x1,c(0.05,0.95)),step0=(q0[2]-q0[1])/round((q0[2]-q0[1])/(hmed0/2)),step1=(q1[2]-q1[1])/round((q1[2]-q1[1])/(hmed1/2))){
  if(method0==1) dep0<-npirq(c(x0,x1),y0,x0,nq0,hmed0) else dep0<-npirq2(x1,y0,x0,nq0,hmed0,q0,step0)
  if(method1==1) dep1<-npirq(c(x0,x1),y1,x1,nq1,hmed1) else dep1<-npirq2(x1,y1,x1,nq1,hmed1,q1,step1)
  res<-matrix(,0,2); cov<-matrix(,0,3); qte<-matrix(,0,2)
  for(q in 1:length(prob)){
    res<-rbind(res,c(quantile(dep0,prob[q]),quantile(dep1,prob[q])))
    den0<-density(dep0,n=1,from=res[(q-1)*2+1],to=res[(q-1)*2+1])$y
    den1<-density(dep1,n=1,from=res[(q-1)*2+2],to=res[(q-1)*2+2])$y
    cq0<-psum(dep0<=res[(q-1)*2+1])/nq0
    cq1<-psum(dep1<=res[(q-1)*2+2])/nq1
    cov<-rbind(cov,c(var(cq0)/den0^2/(length(y0)+length(y1))+mean(cq0*(1-cq0))/den0^2/length(y0),var(cq1)/den1^2/(length(y0)+length(y1))+mean(cq1*(1-cq1))/den1^2/length(y1),mean((cq0-prob[q])*(cq1-prob[q]))/(den0*den1*(length(y0)+length(y1)))))
    qte<-rbind(qte,c(res[q,2]-res[q,1],(cov[q,1]+cov[q,2]-2*cov[q,3])^0.5))
  }
  list(res=res,cov=cov,qte=qte)
}

qtet<-function(y0,x0,y1,x1,hmed0,hmed1,nq0=100,nq1=100,prob=seq(0.01,0.99,0.01),method0=1,method1=1,q0=quantile(x1,c(0.05,0.95)),q1=quantile(x1,c(0.05,0.95)),step0=(q0[2]-q0[1])/round((q0[2]-q0[1])/(hmed0/2)),step1=(q1[2]-q1[1])/round((q1[2]-q1[1])/(hmed1/2))){
  if(method0==1) dep0<-npirq(x1,y0,x0,nq0,hmed0) else dep0<-npirq2(x1,y0,x0,nq0,hmed0,q0,step0)
  if(method1==1) dep1<-npirq(x1,y1,x1,nq1,hmed1) else dep1<-npirq2(x1,y1,x1,nq1,hmed1,q1,step1)
  denx<-c()
  for(i in 1:length(x1)) denx<-c(denx,density(x1,n=1,from=x1[i],to=x1[i])$y/max(density(x0,n=1,from=x1[i],to=x1[i])$y,0.001))
  res<-matrix(,0,2); cov<-matrix(,0,3); qtet<-matrix(,0,2)
  for(q in 1:length(prob)){
    res<-rbind(res,c(quantile(dep0,prob[q]),quantile(dep1,prob[q])))
    den0<-density(dep0,n=1,from=res[(q-1)*2+1],to=res[(q-1)*2+1])$y
    den1<-density(dep1,n=1,from=res[(q-1)*2+2],to=res[(q-1)*2+2])$y
    cq0<-psum(dep0<=res[(q-1)*2+1])/nq0
    cq1<-psum(dep1<=res[(q-1)*2+2])/nq1
    cov<-rbind(cov,c(var(cq0)/den0^2/length(y1)+mean(cq0*(1-cq0)*denx)/den0^2/length(y0),prob[q]*(1-prob[q])/den1^2/length(y1),mean((cq0-prob[q])*(cq1-prob[q]))/(den0*den1*length(y1))))
    qtet<-rbind(qtet,c(res[q,2]-res[q,1],(cov[q,1]+cov[q,2]-2*cov[q,3])^0.5))
  }
  list(res=res,cov=cov,qtet=qtet)
}
