library(readr)
MENAGE <- read_csv("Library/CloudStorage/OneDrive-Personal/Université/LMD/Master E&P/M1/Mémoire/Dissertation/Donnees/Budget fam.csv/data/MENAGE.csv", 
                       +     delim = ";", escape_double = FALSE, trim_ws = TRUE)

#Data Cleaning and organisation script -Check Item dictionary for var names details
#---------------------------------
ID=data.frame(MENAGE$IDENT_MEN)
Aise=data.frame(MENAGE$Aise)
Apart=data.frame(MENAGE$APART)
AgePR=data.frame(MENAGE$AGAGR)
AgeCJ=data.frame(MENAGE$AGECJ)
#---------------------------------
BIENIM1=data.frame(MENAGE$BIENIM1) 
BIENIM4=data.frame(MENAGE$BIENIM4) 
BIENIM5=data.frame(MENAGE$BIENIM5) 
#---------------------------------
CATAEU= data.frame(MENAGE$CATAEU) 
CHOMAGE=data.frame(MENAGE$CHOMAGE)
CODCSPR=data.frame(MENAGE$CODCSPR)
CODCSCJ=data.frame(MENAGE$CODCSCJ)
#---------------------------------
Chomagea=data.frame(MENAGE$Chomagea)
Chomageb=data.frame(MENAGE$Chomageb)
Chomagec=data.frame(MENAGE$Chomagec)
#---------------------------------
COEFFUC=data.frame(MENAGE$COEFFUC) 
DNIVIE2=data.frame(MENAGE$DNIVIE2)
DNIVIE1=data.frame(MENAGE$DNIVIE1)


#Endowment & income
#---------------------------------

ESPOIRA=data.frame(MENAGE$Espoira) 
ESPOIRB=data.frame(MENAGE$Espoirb) 
#---------------------------------

PATRIMOINE=data.frame(MENAGE$Patrib)
PPA=data.frame(MENAGE$PPA)
REVSOC=data.frame(MENAGE$REVSOC)
REVTOT=data.frame(MENAGE$REVTOT)
RMINI=data.frame(MENAGE$Rmini)
REVDISP=data.frame(MENAGE$REVDISP) 
Wages=data.frame(MENAGE$SALAIRES)

#Description of the households
#---------------------------------
TYPMEN15=data.frame(MENAGE$TYPMEN15) 
TUU=data.frame(MENAGE$TUU) 
TYPLOG=data.frame(MENAGE$TYPLOG)  
ZEAT=data.frame(MENAGE$ZEAT) 

# Living standard Trends & perspectives
#---------------------------------
VARIB=data.frame(MENAGE$Varib)
VARIC=data.frame(MENAGE$Varic)


# Working Dataset
#---------------------------------

HouseholdSocioEco= data.frame(ID,Aise,Apart,AgePR,AgeCJ,BIENIM1,BIENIM4,BIENIM5,
                              CATAEU,CHOMAGE,CODCSPR,CODCSCJ,Chomagea,Chomageb,
                              Chomageb,COEFFUC,DNIVIE1,DNIVIE2,ESPOIRA,ESPOIRB,
                              PATRIMOINE,PPA,RMINI,REVSOC,REVTOT,REVDISP,Wages,TYPMEN15,
                              TUU,TYPLOG,ZEAT,VARIB,VARIC)

rm(ID,Aise,Apart,AgePR,AgeCJ,BIENIM1,BIENIM4,BIENIM5,
   CATAEU,CHOMAGE,CODCSPR,CODCSCJ,Chomagea,Chomageb,
   Chomagec,COEFFUC,DNIVIE1,DNIVIE2,ESPOIRA,ESPOIRB,
   PATRIMOINE,PPA,RMINI,REVSOC,REVTOT,REVDISP,Wages,TYPMEN15,
   TUU,TYPLOG,ZEAT,VARIB,VARIC)

# Saving working dataset
#---------------------------------
write.csv(HouseholdSocioEco,file="/Users/gabrielbontemps/Library/CloudStorage/OneDrive-Personal/Université/LMD/Master E&P/M1/Mémoire/Dissertation/Donnees/HouseholdSocioEco.csv")