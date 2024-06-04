s2=as.matrix(st2)
s3=as.matrixe(st3)
s4=as.matrix(st4)
s5=as.matrix(st5)


s2$y= rep(seq(1:5),times=5,each=1)
s3$y= rep(seq(1:5),times=5,each=1)
s4$y= rep(seq(1:5),times=5,each=1)
s5$y= rep(seq(1:5),times=5,each=1)

s2$G= rep(seq(1:5),times=1,each=5)
s3$G= rep(seq(1:5),times=1,each=5)
s4$G= rep(seq(1:5),times=1,each=5)
s5$G= rep(seq(1:5),times=1,each=5)

s2$Meta= 2
s3$Meta= 3
s4$Meta= 4
s5$Meta= 5

# Distributions of agent behavior

p1.1=P1[P1$G==1,]
p1.2=P1[P1$G==2,]
p1.3=P1[P1$G==5,]
p1.4=P1[P1$G==4,]
p1.5=P1[P1$G==5,]

p2.1=P2[P2$G==1,]
p2.2=P2[P2$G==2,]
p2.3=P2[P2$G==5,]
p2.4=P2[P2$G==4,]
p2.5=P2[P2$G==5,]

p3.1=P3[P3$G==1,]
p3.2=P3[P3$G==2,]
p3.3=P3[P3$G==5,]
p3.4=P3[P3$G==4,]
p3.5=P3[P3$G==5,]

p4.1=P4[P4$G==1,]
p4.2=P4[P4$G==2,]
p4.3=P4[P4$G==5,]
p4.4=P4[P4$G==4,]
p4.5=P4[P4$G==5,]

p5.1=P5[P5$G==1,]
p5.2=P5[P5$G==2,]
p5.3=P5[P5$G==5,]
p5.4=P5[P5$G==4,]
p5.5=P5[P5$G==5,]

a1=a2=a3=a4=a5=vector("numeric",length = 6)