library(readr)
library(readxl)


FinalDataMap8 <- read_csv("Desktop/main/MRes_paper/Data/data/FinalDataMap8.csv")
res2022com <- read_excel("Desktop/resultats-par-niveau-subcom-t1-france-entiere.xlsx")


# Cleaning stuff

df = FinalDataMap8
dr = res2022com

dr$keep = ifelse(dr$`Libellé de la commune`%in% df$nom,1,0)
df$keep = ifelse(df$nom %in% dr$`Libellé de la commune`,1,0)

df1 = df[df$keep ==1,]
dr1 = dr[dr$keep ==1,]

code = df$code
# Functions 
LoL = function(X,i){return((X^i)/floor(1+max(X^i)))}
oui = function(X,Y){
  urbi = arima(X,c(0,2,2))
  orbi = arima(Y,c(0,2,2))
  Qxy = urbi$coef[1]*(X)+orbi$coef[2]*(Y) + (urbi$coef[1]*(X)+orbi$coef[2]*(Y)- orbi$coef[1]*(X)-urbi$coef[2]*(Y))
  return(as.numeric(Qxy))
} 
non = function(X,Y,u){
  urbi = arima(X,c(0,2,2))
  orbi = arima(Y,c(0,2,2))
  Qxy = urbi$coef[1]*(X)+orbi$coef[2]*(Y) + (urbi$coef[1]*(X)+orbi$coef[2]*(Y)- orbi$coef[1]*(X)-urbi$coef[2]*(Y))*(1+det(u))
  return(as.numeric(Qxy))
} 
# Set up

#Opinion stuff

abs    = dr1$Abstentions
votant = dr1$Votants

macron  =dr1$...38

eta = 1- macron/(abs+votant)
eta = c(eta, rep(NA,3))

theta = abs/(abs+votant) 
theta =c(theta, rep(NA,3))

rm(abs,votant,macron)

f8 = as.matrix(df1[,c(seq(from=25,to=33,by=2))])
fa = data.frame(eta,theta)


doxa = function(X){
  m = lm(X ~ fa[,1]+fa[,2])
  c = as.numeric(m$coef[1])
  a = as.numeric(m$coef[2])
  b = as.numeric(m$coef[3])
  
  A   = matrix(c(b,-c,c,a),nrow=2,ncol=2)
  B   = c(a,b)
  Phi = matrix(c(a,b-c,a-c,b),nrow = 2,ncol=2)
  
  lambda = 0.01
  
  y = A + exp(-lambda*Phi)*B
  
  return(y)
}
  
lm(fa[,1]~f8+fa[,2])
lm(fa[,2]~f8+fa[,1])



# Agent Opinion 

y1 = doxa(f8[,1])
y2 = doxa(f8[,2])
y3 = doxa(f8[,3])
y4 = doxa(f8[,4])
y5 = doxa(f8[,5])


#data stuff 
Fx = df$fx 
Fy = df$fy

a = (Fy-Fx)/Fx
q = (Fx-Fy)/Fy

A = (a-min(a))/(max(a)-min(a))
B = (a-min(a))/(mean(a)-min(a))

Q = (q-min(q))/(max(q)-min(q))
R = (q-min(q))/(mean(q)-min(q))

X0 = (A+Q)/(B+R)
X1 = (A*B)/(1+A*B)
X2 = 0.5*(A+R)/(B+Q)
X3 = (A+R)/(B+R)
X4 = (A+Q)/(B+Q)
X5 = (X0/X1 - X2/X3)/(X0/X1 + X2/X3)*X4



p = (1-X5)/X5

Fz = p*Fy + (1-p)*Fx

# Projection vectors
Hxy = tanh(A*B)
Hy  = abs(1-sinh(A*B))
Hx  = abs(1-cosh(A*B))

Mz  = Hxy*Fz 
Mxy = Hx*Fx + Hy*Fy

dMzxy = Mz - Mxy
dMzxy[is.na(dMzxy)]=0


# Drift

dFz1 = LoL(dMzxy,1) 
dFz2 = LoL(dMzxy,2)
dFz3 = LoL(dMzxy,3)
dFz4 = LoL(dMzxy,4)
dFz5 = LoL(dMzxy,5)


# drifts, assuming Gaussian errors 

set.seed(123)

DZ1 = LoL(dMzxy,1) + rnorm(length(dMzxy),0,sd(dMzxy)/sqrt(length(dMzxy)))
DZ2 = LoL(DZ1,2) + rnorm(length(dMzxy),0,sd(DZ1)/sqrt(length(dMzxy)))
DZ3 = LoL(DZ2,2) + rnorm(length(dMzxy),0,sd(DZ2)/sqrt(length(dMzxy)))
DZ4 = LoL(DZ3,2) + rnorm(length(dMzxy),0,sd(DZ3)/sqrt(length(dMzxy)))
DZ5 = LoL(DZ4,2) + rnorm(length(dMzxy),0,sd(DZ4)/sqrt(length(dMzxy)))

# Impulse Response Functions

v0 = oui(R-Q,B-A)
Z0 = Fz 

Z1 = Z0 + v0 + dFz1

v1 = oui(Z0,R-Q)
Z2 = (Z1 +v1 +dFz2)/2

v2 = oui(Z2-Z1,Z1-Z0)
Z3 = (Z2 +v2+ dFz3)/3

v3 = oui(Z3-Z2,Z2-Z1)
Z4 = (Z3 +v3+ dFz4)/4

v4 = oui(Z4-Z3,Z3-Z2)
Z5 =  (Z4 + v4+ dFz5)/5

# Impulse response functions with Gaussian drifts

vg0 = oui(R-Q,B-A)
Zg0 = Fz 

Zg1 = Zg0 + vg0 + DZ1

vg1 = oui(Zg1-Zg0,R-Q)
Zg2 = (Zg1 +vg1 +DZ2)/2

vg2 = oui(Zg2-Zg1,Zg1-Zg0)
Zg3 = (Zg2 +vg2+ DZ3)/3

vg3 = oui(Zg3-Zg2,Zg2-Zg1)
Zg4 = (Zg3 +vg3+ DZ4)/4

vg4 = oui(Zg4-Zg3,Zg3-Zg2)
Zg5 =  (Zg4 + vg4+ DZ5)/5


hist(Z0)
hist(Z1)
hist(Z2)
hist(Z3)
hist(Z4)
hist(Z5)


hist(Zg0)
hist(Zg1)
hist(Zg2)
hist(Zg3)
hist(Zg4)
hist(Zg5)

# Impulse response functions with Gaussian drifts per profile :

#Profile 1

vg0P1 = non(R-Q,B-A,y1)
Zg0P1 = Fz*df$DomDens1

Zg1P1 = Zg0 + vg0P1 + DZ1

vg1P1 = non(Zg1-Zg0,R-Q,y1)
Zg2P1 = (Zg1 +vg1P1 +DZ2)/2

vg2P1 = non(Zg2-Zg1,Zg1-Zg0,y1)
Zg3P1 = (Zg2 +vg2P1+ DZ3)/3

vg3P1 = non(Zg3-Zg2,Zg2-Zg1,y1)
Zg4P1 = (Zg3 +vg3P1+ DZ4)/4

vg4P1 = non(Zg4-Zg3,Zg3-Zg2,y1)
Zg5P1 =  (Zg4 + vg4P1+ DZ5)/5


# Profile 2

vg0P2 = non(R-Q,B-A,y2)
Zg0P2 = Fz*df$DomDens2

Zg1P2 = Zg0 + vg0P2 + DZ1

vg1P2 = non(Zg1-Zg0,R-Q,y2)
Zg2P2 = (Zg1 +vg1P2 +DZ2)/2

vg2P2 = non(Zg2-Zg1,Zg1-Zg0,y2)
Zg3P2 = (Zg2 +vg2P2+ DZ3)/3

vg3P2 = non(Zg3-Zg2,Zg2-Zg1,y2)
Zg4P2 = (Zg3 +vg3P2+ DZ4)/4

vg4P2 = non(Zg4-Zg3,Zg3-Zg2,y2)
Zg5P2 =  (Zg4 + vg4P2+ DZ5)/5

# Profile 3
vg0P3 = non(R-Q,B-A,y3)
Zg0P3 = Fz*df$DomDens3 

Zg1P3 = Zg0 + vg0P3 + DZ1

vg1P3 = non(Zg1-Zg0,R-Q,y3)
Zg2P3 = (Zg1 +vg1P3 +DZ2)/2

vg2P3 = non(Zg2-Zg1,Zg1-Zg0,y3)
Zg3P3 = (Zg2 +vg2P3+ DZ3)/3

vg3P3 = non(Zg3-Zg2,Zg2-Zg1,y3)
Zg4P3 = (Zg3 +vg3P3+ DZ4)/4

vg4P3 = non(Zg4-Zg3,Zg3-Zg2,y3)
Zg5P3 =  (Zg4 + vg4P3+ DZ5)/5


# Profile 4
vg0P4 = non(R-Q,B-A,y4)
Zg0P4 = Fz*df$DomDens4 

Zg1P4 = Zg0 + vg0P4 + DZ1

vg1P4 = non(Zg1-Zg0,R-Q,y4)
Zg2P4 = (Zg1 +vg1P4 +DZ2)/2

vg2P4 = non(Zg2-Zg1,Zg1-Zg0,y4)
Zg3P4 = (Zg2 +vg2P4+ DZ3)/3

vg3P4 = non(Zg3-Zg2,Zg2-Zg1,y4)
Zg4P4 = (Zg3 +vg3P4+ DZ4)/4

vg4P4 = non(Zg4-Zg3,Zg3-Zg2,y4)
Zg5P4 =  (Zg4 + vg4P4+ DZ5)/5


# Profile 5
vg0P5 = non(R-Q,B-A,y5)
Zg0P5 = Fz*df$DomDens5 

Zg1P5 = Zg0 + vg0P5 + DZ1

vg1P5 = non(Zg1-Zg0,R-Q,y5)
Zg2P5 = (Zg1 +vg1P5 +DZ2)/2

vg2P5 = non(Zg2-Zg1,Zg1-Zg0,y5)
Zg3P5 = (Zg2 +vg2P5+ DZ3)/3

vg3P5 = non(Zg3-Zg2,Zg2-Zg1,y5)
Zg4P5 = (Zg3 +vg3P5+ DZ4)/4

vg4P5 = non(Zg4-Zg3,Zg3-Zg2,y5)
Zg5P5 =  (Zg4 + vg4P5+ DZ5)/5

#Opinion Data 

OP1 = data.frame(code,Zg0P1,Zg1P1,Zg2P1,Zg3P1,Zg4P1,Zg5P1)
OP2 = data.frame(code,Zg0P2,Zg1P2,Zg2P2,Zg3P2,Zg4P2,Zg5P2)
OP3 = data.frame(code,Zg0P3,Zg1P3,Zg2P3,Zg3P3,Zg4P3,Zg5P3)
OP4 = data.frame(code,Zg0P4,Zg1P4,Zg2P4,Zg3P4,Zg4P4,Zg5P4)
OP5 = data.frame(code,Zg0P5,Zg1P5,Zg2P5,Zg3P5,Zg4P5,Zg5P5)

OP1[is.na(OP1)]=0
OP2[is.na(OP2)]=0
OP3[is.na(OP3)]=0
OP4[is.na(OP4)]=0
OP5[is.na(OP5)]=0

write.csv(OP1, file = "Desktop/OP1.csv")
write.csv(OP2, file = "Desktop/OP4.csv")
write.csv(OP3, file = "Desktop/OP3.csv")
write.csv(OP4, file = "Desktop/OP4.csv")
write.csv(OP5, file = "Desktop/OP5.csv")

