library(dplyr)




HouseholdSocioEco <- read_csv("Library/CloudStorage/OneDrive-Personal/Université/LMD/Master E&P/M1/Mémoire/Dissertation/Donnees/HouseholdSocioEco.csv", 
                              +     col_types = cols(...1 = col_skip()))

#---------------------------------
#Dummy & matrices set up
#---------------------------------

#Geo-Zone (z_) Dummies 
#---------------------------------
HouseholdSocioEco$z_RParis=ifelse(HouseholdSocioEco$MENAGE.ZEAT==1,1,0)
HouseholdSocioEco$z_BParis=ifelse(HouseholdSocioEco$MENAGE.ZEAT==2,1,0)
HouseholdSocioEco$z_Nord=ifelse(HouseholdSocioEco$MENAGE.ZEAT==3,1,0)
HouseholdSocioEco$z_Est=ifelse(HouseholdSocioEco$MENAGE.ZEAT==4,1,0)
HouseholdSocioEco$z_Ouest=ifelse(HouseholdSocioEco$MENAGE.ZEAT==5,1,0)
HouseholdSocioEco$z_SudOuest=ifelse(HouseholdSocioEco$MENAGE.ZEAT==7,1,0)
HouseholdSocioEco$z_CentreEst=ifelse(HouseholdSocioEco$MENAGE.ZEAT==8,1,0)
HouseholdSocioEco$z_Mediterranee=ifelse(HouseholdSocioEco$MENAGE.ZEAT==9,1,0)


#Standard of livings evolution over 1 year (_B) and 5 years (_C)
#---------------------------------
HouseholdSocioEco$upalot_B=ifelse(HouseholdSocioEco$MENAGE.Varib==1,1,0)
HouseholdSocioEco$up_B=ifelse(HouseholdSocioEco$MENAGE.Varib==2,1,0)
HouseholdSocioEco$stagn_B=ifelse(HouseholdSocioEco$MENAGE.Varib==3,1,0)
HouseholdSocioEco$down_B=ifelse(HouseholdSocioEco$MENAGE.Varib==4,1,0)
HouseholdSocioEco$downalot_B=ifelse(HouseholdSocioEco$MENAGE.Varib==5,1,0)
HouseholdSocioEco$new_B=ifelse(HouseholdSocioEco$MENAGE.Varib==6,1,0)

HouseholdSocioEco$upalot_C=ifelse(HouseholdSocioEco$MENAGE.Varic==1,1,0)
HouseholdSocioEco$up_C=ifelse(HouseholdSocioEco$MENAGE.Varic==2,1,0)
HouseholdSocioEco$stagn_C=ifelse(HouseholdSocioEco$MENAGE.Varic==3,1,0)
HouseholdSocioEco$down_C=ifelse(HouseholdSocioEco$MENAGE.Varic==4,1,0)
HouseholdSocioEco$downalot_C=ifelse(HouseholdSocioEco$MENAGE.Varic==5,1,0)
HouseholdSocioEco$new_C=ifelse(HouseholdSocioEco$MENAGE.Varic==6,1,0)


#Geo-Urbanism (u_) Dummies -size of urban area
#---------------------------------
HouseholdSocioEco$u_rural=ifelse(HouseholdSocioEco$MENAGE.TYPLOG==0,1,0)
HouseholdSocioEco$u_less5k=ifelse(HouseholdSocioEco$MENAGE.TYPLOG==1,1,0)
HouseholdSocioEco$u_less10k=ifelse(HouseholdSocioEco$MENAGE.TYPLOG==2,1,0)
HouseholdSocioEco$u_less20k=ifelse(HouseholdSocioEco$MENAGE.TYPLOG==3,1,0)
HouseholdSocioEco$u_less50k=ifelse(HouseholdSocioEco$MENAGE.TYPLOG==4,1,0)
HouseholdSocioEco$u_less100k=ifelse(HouseholdSocioEco$MENAGE.TYPLOG==5,1,0)
HouseholdSocioEco$u_less200k=ifelse(HouseholdSocioEco$MENAGE.TYPLOG==6,1,0)
HouseholdSocioEco$u_more200k=ifelse(HouseholdSocioEco$MENAGE.TYPLOG==7,1,0)
HouseholdSocioEco$u_paris=ifelse(HouseholdSocioEco$MENAGE.TYPLOG==8,1,0)


# categories :Value of endowment (w_)
#---------------------------------
HouseholdSocioEco$w_less5k=ifelse(HouseholdSocioEco$MENAGE.Patrib==01,1,0)
HouseholdSocioEco$w_less10k=ifelse(HouseholdSocioEco$MENAGE.Patrib==02,1,0)
HouseholdSocioEco$w_less15k=ifelse(HouseholdSocioEco$MENAGE.Patrib==03,1,0)
HouseholdSocioEco$w_less30k=ifelse(HouseholdSocioEco$MENAGE.Patrib==04,1,0)
HouseholdSocioEco$w_less50k=ifelse(HouseholdSocioEco$MENAGE.Patrib==05,1,0)
HouseholdSocioEco$w_less100k=ifelse(HouseholdSocioEco$MENAGE.Patrib==06,1,0)
HouseholdSocioEco$w_less150k=ifelse(HouseholdSocioEco$MENAGE.Patrib==07,1,0)
HouseholdSocioEco$w_less200k=ifelse(HouseholdSocioEco$MENAGE.Patrib==08,1,0)
HouseholdSocioEco$w_less250k=ifelse(HouseholdSocioEco$MENAGE.Patrib==09,1,0)
HouseholdSocioEco$w_less300k=ifelse(HouseholdSocioEco$MENAGE.Patrib==10,1,0)
HouseholdSocioEco$w_less350k=ifelse(HouseholdSocioEco$MENAGE.Patrib==11,1,0)
HouseholdSocioEco$w_less400k=ifelse(HouseholdSocioEco$MENAGE.Patrib==12,1,0)
HouseholdSocioEco$w_less500k=ifelse(HouseholdSocioEco$MENAGE.Patrib==13,1,0)
HouseholdSocioEco$w_more500k=ifelse(HouseholdSocioEco$MENAGE.Patrib==14,1,0)
HouseholdSocioEco$w_more1mil=ifelse(HouseholdSocioEco$MENAGE.Patrib==15,1,0)


#Geo-Neighbor (n_) Dummies from MENAGE.TYPVOIS

#Employment pool categories in the urban area (Epc_) from CATAEU
#---------------------------------
HouseholdSocioEco$Epc_1= ifelse(HouseholdSocioEco$MENAGE.CATAEU==111,1,0)
HouseholdSocioEco$Epc_2= ifelse(HouseholdSocioEco$MENAGE.CATAEU==112,1,0)
HouseholdSocioEco$Epc_3= ifelse(HouseholdSocioEco$MENAGE.CATAEU==120,1,0)
HouseholdSocioEco$Epc_4= ifelse(HouseholdSocioEco$MENAGE.CATAEU==211,1,0)
HouseholdSocioEco$Epc_5= ifelse(HouseholdSocioEco$MENAGE.CATAEU==212,1,0)
HouseholdSocioEco$Epc_6= ifelse(HouseholdSocioEco$MENAGE.CATAEU==221,1,0)
HouseholdSocioEco$Epc_7= ifelse(HouseholdSocioEco$MENAGE.CATAEU==300,1,0)
HouseholdSocioEco$Epc_8= ifelse(HouseholdSocioEco$MENAGE.CATAEU==400,1,0)



# Detailed Households categories (hc_) Dummies)- from TYPMEN15
#---------------------------------
HouseholdSocioEco$hc_10=ifelse(HouseholdSocioEco$MENAGE.TYPMEN15==10,1,0)
HouseholdSocioEco$hc_11=ifelse(HouseholdSocioEco$MENAGE.TYPMEN15==11,1,0)

HouseholdSocioEco$hc_21=ifelse(HouseholdSocioEco$MENAGE.TYPMEN15==21,1,0)
HouseholdSocioEco$hc_22=ifelse(HouseholdSocioEco$MENAGE.TYPMEN15==22,1,0)

HouseholdSocioEco$hc_31=ifelse(HouseholdSocioEco$MENAGE.TYPMEN15==31,1,0)
HouseholdSocioEco$hc_32=ifelse(HouseholdSocioEco$MENAGE.TYPMEN15==32,1,0)
HouseholdSocioEco$hc_33=ifelse(HouseholdSocioEco$MENAGE.TYPMEN15==33,1,0)

HouseholdSocioEco$hc_41=ifelse(HouseholdSocioEco$MENAGE.TYPMEN15==41,1,0)
HouseholdSocioEco$hc_42=ifelse(HouseholdSocioEco$MENAGE.TYPMEN15==42,1,0)
HouseholdSocioEco$hc_43=ifelse(HouseholdSocioEco$MENAGE.TYPMEN15==43,1,0)
HouseholdSocioEco$hc_44=ifelse(HouseholdSocioEco$MENAGE.TYPMEN15==44,1,0)

HouseholdSocioEco$hc_51=ifelse(HouseholdSocioEco$MENAGE.TYPMEN15==51,1,0)
HouseholdSocioEco$hc_52=ifelse(HouseholdSocioEco$MENAGE.TYPMEN15==52,1,0)
HouseholdSocioEco$hc_53=ifelse(HouseholdSocioEco$MENAGE.TYPMEN15==53,1,0)




# Expectation for allocations of extra resources / assets
#---------------------------------

HouseholdSocioEco$E_a_food=ifelse(HouseholdSocioEco$MENAGE.Espoira==01,1,0)
HouseholdSocioEco$E_a_clothe=ifelse(HouseholdSocioEco$MENAGE.Espoira==02,1,0)
HouseholdSocioEco$E_a_accomodation=ifelse(HouseholdSocioEco$MENAGE.Espoira==03,1,0)
HouseholdSocioEco$E_a_furnitures=ifelse(HouseholdSocioEco$MENAGE.Espoira==04,1,0)
HouseholdSocioEco$E_a_health=ifelse(HouseholdSocioEco$MENAGE.Espoira==05,1,0)
HouseholdSocioEco$E_a_car=ifelse(HouseholdSocioEco$MENAGE.Espoira==06,1,0)
HouseholdSocioEco$E_a_leisure=ifelse(HouseholdSocioEco$MENAGE.Espoira==07,1,0)
HouseholdSocioEco$E_a_culture=ifelse(HouseholdSocioEco$MENAGE.Espoira==08,1,0)
HouseholdSocioEco$E_a_family=ifelse(HouseholdSocioEco$MENAGE.Espoira==09,1,0)
HouseholdSocioEco$E_a_loan=ifelse(HouseholdSocioEco$MENAGE.Espoira==10,1,0)
HouseholdSocioEco$E_a_savings=ifelse(HouseholdSocioEco$MENAGE.Espoira==11,1,0)
HouseholdSocioEco$E_a_other=ifelse(HouseholdSocioEco$MENAGE.Espoira==12,1,0)

HouseholdSocioEco$E_b_food=ifelse(HouseholdSocioEco$MENAGE.Espoirb==01,1,0)
HouseholdSocioEco$E_b_clothe=ifelse(HouseholdSocioEco$MENAGE.Espoirb==02,1,0)
HouseholdSocioEco$E_b_accomodation=ifelse(HouseholdSocioEco$MENAGE.Espoirb==03,1,0)
HouseholdSocioEco$E_b_furnitures=ifelse(HouseholdSocioEco$MENAGE.Espoirb==04,1,0)
HouseholdSocioEco$E_b_health=ifelse(HouseholdSocioEco$MENAGE.Espoirb==05,1,0)
HouseholdSocioEco$E_b_car=ifelse(HouseholdSocioEco$MENAGE.Espoirb==06,1,0)
HouseholdSocioEco$E_b_leisure=ifelse(HouseholdSocioEco$MENAGE.Espoirb==07,1,0)
HouseholdSocioEco$E_b_culture=ifelse(HouseholdSocioEco$MENAGE.Espoirb==08,1,0)
HouseholdSocioEco$E_b_family=ifelse(HouseholdSocioEco$MENAGE.Espoirb==09,1,0)
HouseholdSocioEco$E_b_loan=ifelse(HouseholdSocioEco$MENAGE.Espoirb==10,1,0)
HouseholdSocioEco$E_b_savings=ifelse(HouseholdSocioEco$MENAGE.Espoirb==11,1,0)
HouseholdSocioEco$E_b_other=ifelse(HouseholdSocioEco$MENAGE.Espoirb==12,1,0)

# Decile of Standard of livings
#---------------------------------

HouseholdSocioEco$D1= ifelse(HouseholdSocioEco$MENAGE.DNIVIE1==1,1,0)
HouseholdSocioEco$D2= ifelse(HouseholdSocioEco$MENAGE.DNIVIE1==2,1,0)
HouseholdSocioEco$D3= ifelse(HouseholdSocioEco$MENAGE.DNIVIE1==3,1,0)
HouseholdSocioEco$D4= ifelse(HouseholdSocioEco$MENAGE.DNIVIE1==4,1,0)
HouseholdSocioEco$D5= ifelse(HouseholdSocioEco$MENAGE.DNIVIE1==5,1,0)
HouseholdSocioEco$D6= ifelse(HouseholdSocioEco$MENAGE.DNIVIE1==6,1,0)
HouseholdSocioEco$D7= ifelse(HouseholdSocioEco$MENAGE.DNIVIE1==7,1,0)
HouseholdSocioEco$D8= ifelse(HouseholdSocioEco$MENAGE.DNIVIE1==8,1,0)
HouseholdSocioEco$D9= ifelse(HouseholdSocioEco$MENAGE.DNIVIE1==9,1,0)
HouseholdSocioEco$D10= ifelse(HouseholdSocioEco$MENAGE.DNIVIE1==10,1,0)


# Unemployment categories & expectations (U) from the interviewee & spouse (a-b) and children (c)
#---------------------------------

HouseholdSocioEco$U_a_norisk=ifelse(HouseholdSocioEco$MENAGE.Chomagea==1,1,0)
HouseholdSocioEco$U_a_lowrisk=ifelse(HouseholdSocioEco$MENAGE.Chomagea==2,1,0)
HouseholdSocioEco$U_a_risk=ifelse(HouseholdSocioEco$MENAGE.Chomagea==3,1,0)
HouseholdSocioEco$U_a_highrisk=ifelse(HouseholdSocioEco$MENAGE.Chomagea==4,1,0)
HouseholdSocioEco$U_a_certain=ifelse(HouseholdSocioEco$MENAGE.Chomagea==5,1,0)

HouseholdSocioEco$U_b_norisk=ifelse(HouseholdSocioEco$MENAGE.Chomageb==1,1,0)
HouseholdSocioEco$U_b_lowrisk=ifelse(HouseholdSocioEco$MENAGE.Chomageb==2,1,0)
HouseholdSocioEco$U_b_risk=ifelse(HouseholdSocioEco$MENAGE.Chomageb==3,1,0)
HouseholdSocioEco$U_b_highrisk=ifelse(HouseholdSocioEco$MENAGE.Chomageb==4,1,0)
HouseholdSocioEco$U_b_certain=ifelse(HouseholdSocioEco$MENAGE.Chomageb==5,1,0)

HouseholdSocioEco$U_c_norisk=ifelse(HouseholdSocioEco$MENAGE.Chomagec==1,1,0)
HouseholdSocioEco$U_c_lowrisk=ifelse(HouseholdSocioEco$MENAGE.Chomagec==2,1,0)
HouseholdSocioEco$U_c_risk=ifelse(HouseholdSocioEco$MENAGE.Chomagec==3,1,0)
HouseholdSocioEco$U_c_highrisk=ifelse(HouseholdSocioEco$MENAGE.Chomagec==4,1,0)
HouseholdSocioEco$U_c_certain=ifelse(HouseholdSocioEco$MENAGE.Chomagec==5,1,0)


#Budget categories : _S- separated, _C- common 
#---------------------------------

HouseholdSocioEco$budget_S=ifelse(HouseholdSocioEco$MENAGE.APART==1,1,0)
HouseholdSocioEco$budget_C=ifelse(HouseholdSocioEco$MENAGE.APART==2,1,0)



# New Data set
#--------------------------------

nKeep=c("MENAGE.ZEAT","MENAGE.Varib","MENAGE.Varic",
        "MENAGE.TYPLOG","MENAGE.CATAEU","MENAGE.Espoira",
        "MENAGE.Espoirb","MENAGE.DNIVIE1","MENAGE.Chomagea",
        "MENAGE.Chomageb","MENAGE.APART","MENAGE.Chomageb.1","MENAGE.Chomagec")



HSE = HouseholdSocioEco[,!(names(HouseholdSocioEco) %in% nKeep)]

# Save New Data set
#--------------------------------
write.csv(HSE,file="/Users/gabrielbontemps/Library/CloudStorage/OneDrive-Personal/Université/LMD/Master E&P/M1/Mémoire/Dissertation/Donnees/HSE.csv")

