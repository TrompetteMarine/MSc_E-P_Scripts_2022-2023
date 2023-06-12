#Libraries + Data load for individual
#---------------------------------
library(finalfit)
library(dplyr)
library(readr)
library(mice)
INDIVIDU <- read_delim("Library/CloudStorage/OneDrive-Personal/Université/LMD/Master E&P/M1/Mémoire/Dissertation/Donnees/Budget fam.csv/data/INDIVIDU.csv", 
                       +delim= ";", escape_double = FALSE, trim_ws = TRUE)
DEPINDIV <- read_delim("Library/CloudStorage/OneDrive-Personal/Université/LMD/Master E&P/M1/Mémoire/Dissertation/Donnees/Budget fam.csv/data/DEPINDIV.csv", 
                       +     delim = ";", escape_double = FALSE, trim_ws = TRUE)


#Data cleaning 
#---------------------------------


df=data.frame(INDIVIDU$ACTOCCUP,INDIVIDU$AG,INDIVIDU$BSMERE,INDIVIDU$BSPERE,INDIVIDU$CHOMAGE,
              INDIVIDU$COUPLE,INDIVIDU$CS24,INDIVIDU$dip14,INDIVIDU$ENFANT,
              INDIVIDU$EXIACT8,INDIVIDU$RETRAITES,INDIVIDU$SALAIRES,INDIVIDU$IPROPLOC) #trunkate data set

INDIVIDU$BSPERE[is.na(INDIVIDU$BSPERE)]=0
INDIVIDU$BSMERE[is.na(INDIVIDU$BSMERE)]=0
sp=ifelse(INDIVIDU$BSPERE==3, 1, 0)
sm=ifelse(INDIVIDU$BSMERE==3, 1, 0)
id= sm*sp

#income for working age population
adulte= ifelse(INDIVIDU$AGE>16,1,0)
active=ifelse(INDIVIDU$CS24!=c(00,82,"ho"),1,0)
active[is.na(active)]=0

AA=adulte*active #Active adults

income = ifelse(AA==1,INDIVIDU$RETRAITES+INDIVIDU$CHOMAGE+INDIVIDU$SALAIRES,0)
income=income[income!=0]

#Savings
PF= data.frame(INDIVIDU$PATF1,INDIVIDU$PATF2,INDIVIDU$PATF3,
               INDIVIDU$PATF4,INDIVIDU$PATF5,INDIVIDU$PATF6,INDIVIDU$PATF7) # Patrimoine financier

PF[is.na(PF)]=0

aPF= vector(mode="numeric",length=42900)
aPF=ifelse(AA==1,PF[,1]+PF[,2]+PF[,3]+PF[,4]+PF[,5]+PF[,6]+PF[,7],0)
aPF=aPF[aPF!=0]
# aggregate Financial capital/savings

W=aPF+income
W=W[W!=0]

summary(W)
hist(W)
plot(density(W))

summary(INDIVIDU$AGE)

#Individual spending 
Leisure=data.frame(DEPINDIV$SMSPORT_D,DEPINDIV$SMBIBLI_D,
                   DEPINDIV$SMLOIS_D,DEPINDIV$SMMUSEE_D,
                   DEPINDIV$SMTHEAT_D,DEPINDIV$SMAUTRE_D,
                   DEPINDIV$SMVCLUB_D,DEPINDIV$SMJOURN_D,
                   DEPINDIV$SMCOURS_D,DEPINDIV$SMMUSIQ_D)
FirstNecessities=data.frame(DEPINDIV$)

Leisure[is.na(Leisure)]=0
L=vector(mode="numeric",length=42900)
L= Leisure[,1]+Leisure[,2]+Leisure[,3]+Leisure[,4]+Leisure[,5]+Leisure[,6]+Leisure[,7]+Leisure[,8]+Leisure[,9]+Leisure[,10]
summary(L)


cor(W,L)
plot(L,W)
cor(income,L)
hist(income-L)
plot(density(income-L))
summary(income-L)
#Utility functions set-up
#---------------------------------


