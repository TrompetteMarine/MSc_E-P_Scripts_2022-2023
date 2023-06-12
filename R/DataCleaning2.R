#---------------------------------
#Data Cleaning and organisation script 
#Check Item dictionary for var names details
#---------------------------------

C05$C_ID=C05$IDENT_MEN
C05$C_TOT=C05$CTOT

#Aggregation of food products 
#---------------------------------

C05$C_f_CerealAgg= rowSums(C05[grepl("^C0111",names(C05))])
C05$C_f_MeatAgg= rowSums(C05[grepl("^C0112",names(C05))])
C05$C_FishSeafoodAgg=rowSums(C05[grepl("^C0113",names(C05))])
C05$C_f_MilkCheeseEggAgg=rowSums(C05[grepl("^C0114",names(C05))])
C05$C_f_OilNButterAgg=rowSums(C05[grepl("^C0115",names(C05))])
C05$C_f_FruitsAgg=rowSums(C05[grepl("^C0116",names(C05))])
C05$C_f_VeggNPotatoesAgg=rowSums(C05[grepl("^C0117",names(C05))])
C05$C_f_SuggarNCoAgg=rowSums(C05[grepl("^C0118",names(C05))])
C05$C_f_SpicesNCoAgg=rowSums(C05[grepl("^C0119",names(C05))])
C05$C_f_NonAlcoolBeveragesAgg=rowSums(C05[grepl("^C012",names(C05))])
C05$C_f_GiftNOthersAgg=rowSums(C05[grepl("^C013",names(C05))])

C05$C_all_agg_AZ=rowSums(C05[grepl("^C_f",names(C05))])
#Aggregation of Alcoholic, drug and tobacco products 
#---------------------------------

C05$C_AlcoolAgg=rowSums(C05[grepl("^C021",names(C05))])
C05$C_TobaccoAgg=rowSums(C05[grepl("^C022",names(C05))])
C05$C_StupAgg=rowSums(C05[grepl("^C023",names(C05))])

C05$C_all_agg_CA=C05$C_AlcoolAgg+C05$C_TobaccoAgg+C05$C_StupAgg

# Aggregated clothes consumption
#---------------------------------

C05$C_cl_ClothingAgg=rowSums(C05[grepl("^C031",names(C05))])
C05$C_cl_ShoesAgg=rowSums(C05[grepl("^C032",names(C05))])
C05$C_cl_OtherClothingAgg=rowSums(C05[grepl("^C033",names(C05))])

C05$C_all_agg_CB=rowSums(C05[grepl("^C_c",names(C05))])

# Relative to accommodation, water, gaz,electricity
#---------------------------------

C05$C_e_RentAgg=rowSums(C05[grepl("^C041",names(C05))])
C05$C_e_MaintenanceAgg=rowSums(C05[grepl("^C043",names(C05))])
C05$C_e_OtherMaintenanceAgg=rowSums(C05[grepl("^C044",names(C05))])
C05$C_e_EnergyAgg=rowSums(C05[grepl("^C045",names(C05))])
C05$C_e_OtherAccomodationAgg=rowSums(C05[grepl("^C046",names(C05))])

C05$C_all_agg_DZ=rowSums(C05[grepl("^C_e",names(C05))])

# Relative to furniture and maintenance (FM)
#---------------------------------

C05$C_FM_furnitureAgg=rowSums(C05[grepl("^C051",names(C05))])
C05$C_FM_beddingAgg=rowSums(C05[grepl("^C052",names(C05))])
C05$C_FM_HomeApplianceAgg=rowSums(C05[grepl("^C053",names(C05))])
C05$C_FM_DishesAgg=rowSums(C05[grepl("^C054",names(C05))])
C05$C_FM_GardenApplianceAgg=rowSums(C05[grepl("^C055",names(C05))])
C05$C_FM_DomesticProductNservicesAgg=rowSums(C05[grepl("^C056",names(C05))])
C05$C_FM_OtherHomeApplianceAgg=rowSums(C05[grepl("^C057",names(C05))])

C05$C_all_agg_CM=rowSums(C05[grepl("^C_FM",names(C05))])

# Health related expenses (Health)
#---------------------------------

C05$C_HealthProductExpensesAgg=rowSums(C05[grepl("^C061",names(C05))])
C05$C_HealthServiceExpensesAgg=rowSums(C05[grepl("^C062",names(C05))])
C05$C_HealthHospitalExpensesAgg=rowSums(C05[grepl("^C063",names(C05))])
C05$C_HealthOtherExpensesAgg=rowSums(C05[grepl("^C064",names(C05))])

C05$C_all_agg_QA=rowSums(C05[grepl("^C_Health",names(C05))])

# Commuting  expenses (T)
#---------------------------------

C05$C_T_VehiculePurshasesAgg=rowSums(C05[grepl("^C071",names(C05))])
C05$C_T_VehiculeCostAgg=rowSums(C05[grepl("^C072",names(C05))])
C05$C_T_VehiculeServisesAgg=rowSums(C05[grepl("^C073",names(C05))])
C05$C_T_VehiculeOtherExpensesAgg=rowSums(C05[grepl("^C074",names(C05))])

C05$C_all_agg_HZ=rowSums(C05[grepl("^C_T",names(C05))])
# Communication expenses
#---------------------------------
C05$C_all_agg_JB=rowSums(C05[grepl("^C081",names(C05))])

# Leisure & Cultural expenses (L)
#---------------------------------

C05$C_L_DigitalExpensesAgg=rowSums(C05[grepl("^C091",names(C05))])
C05$C_L_BigLeisureExpensesAgg=rowSums(C05[grepl("^C092",names(C05))])
C05$C_L_OtherCultrailNLeisureExpensesAgg=rowSums(C05[grepl("^C093",names(C05))])
C05$C_L_SportCulturalNRecreationalAgg=rowSums(C05[grepl("^C094",names(C05))])
C05$C_L_BooksNCoAgg=rowSums(C05[grepl("^C095",names(C05))])

C05$C_agg_all_RZ=rowSums(C05[grepl("^C_L",names(C05))])

# Education expenses
#---------------------------------

C05$C_all_agg_PZ=rowSums(C05[grepl("^C101",names(C05))])

# Restoration and hotel services (rhs)
#---------------------------------

C05$C_rhs_RestorationAgg=rowSums(C05[grepl("^C111",names(C05))])
C05$C_rhs_HotellingAgg=rowSums(C05[grepl("^C112",names(C05))])

C05$C_all_agg_IZ=rowSums(C05[grepl("^C_rhs",names(C05))])

# Other Good and services (gs)
#---------------------------------

C05$C_gs_BeautyProductsAgg=rowSums(C05[grepl("^C121",names(C05))])
C05$C_gs_LuxoryProductsAgg=rowSums(C05[grepl("^C123",names(C05))])
C05$C_gs_SocialProtectionAgg=rowSums(C05[grepl("^C124",names(C05))])
C05$C_gs_InsurranceAgg=rowSums(C05[grepl("^C125",names(C05))])
C05$C_gs_FinancialServicesAgg=rowSums(C05[grepl("^C126",names(C05))])
C05$C_gs_OtherServicesAgg=rowSums(C05[grepl("^C127",names(C05))])
C05$C_gs_OtherLuxExpensesAgg=rowSums(C05[grepl("^C128",names(C05))])
C05$C_gs_OtherSAIExpensesAgg=rowSums(C05[grepl("^C129",names(C05))])

C05$C_all_agg_SZ=rowSums(C05[grepl("^C_gs",names(C05))])

# Tax and savings
#---------------------------------

C05$C_TaxesAgg=rowSums(C05[grepl("^C131",names(C05))])
C05$C_MorgagesAgg=rowSums(C05[grepl("^C132",names(C05))])
C05$C_GiftsAgg=rowSums(C05[grepl("^C133",names(C05))])
C05$C_RenovationExpensesAgg=rowSums(C05[grepl("^C134",names(C05))])
C05$C_ConsomationLoansAgg=rowSums(C05[grepl("^C135",names(C05))])
C05$C_EmployerPrelevmentAgg=rowSums(C05[grepl("^C136",names(C05))])

C05$C_all_agg_Savings=rowSums(C05[grepl("^C137",names(C05))])

# Households' Allocations
#---------------------------------

C05$C_all_agg_TZ=rowSums(C05[grepl("^C141",names(C05))])

# New Data set
#---------------------------------

HC=C05[grep("^C_",names(C05))]
HCagg=C05[grep("^C_all_agg",names(C05))]

#  Save New Data set
#---------------------------------

write.csv(HC,file="/Users/gabrielbontemps/Library/CloudStorage/OneDrive-Personal/Université/LMD/Master E&P/M1/Mémoire/Dissertation/Donnees/HC.csv")
write.csv(HCagg,file="/Users/gabrielbontemps/Library/CloudStorage/OneDrive-Personal/Université/LMD/Master E&P/M1/Mémoire/Dissertation/Donnees/HCagg.csv")








