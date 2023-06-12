*gen geo variables

gen rural =1 if tuu==0
replace rural = 0 if rural ==.

gen hameaux =1 if tuu==1
replace hameaux=0 if hameaux==.

gen sVillage = 1 if tuu==2
replace sVillage = 0 if sVillage==.

gen medVillage = 1 if tuu==3
replace medVillage = 0 if medVillage==.

gen gVillage= 1 if tuu==4
replace gVillage =0 if gVillage==.

gen sVille=1 if tuu ==5
replace sVille = 0 if sVille==.

gen medVille =1 if tuu==6
replace medVille = 0 if medVille==.

gen gVille= 1 if tuu==7
replace gVille =0 if gVille==.

gen Paris = 1 if tuu==8
replace Paris = 0 if Paris==.


*gen macro categories + geo-macro variables

 

//Total

gen Total = c_tot

gen total_rural      = Total*rural
gen total_hameaux    = Total*hameaux
gen total_sVillage   = Total*sVillage
gen total_medVillage = Total*medVillage
gen total_gVillage   = Total*gVillage
gen total_sVille     = Total*sVille
gen total_medVille   = Total*medVille
gen total_gVille     = Total*gVille
gen total_Paris      = Total*Paris

//Food
gen f1 = c_f_cerealagg  + c_fishseafoodagg +c_f_milkcheeseeggagg + c_f_fruitsagg + c_f_veggnpotatoesagg + c_f_oilnbutteragg + c_f_suggarncoagg + c_f_spices+ c_f_nonalcoolbeveragesagg
gen food= f1

gen Food_rural      = food*rural
gen Food_hameaux    = food*hameaux
gen Food_sVillage   = food*sVillage
gen Food_medVillage = food*medVillage
gen Food_gVillage   = food*gVillage
gen Food_sVille     = food*sVille
gen Food_medVille   = food*medVille
gen Food_gVille     = food*gVille
gen Food_Paris      = food*Paris

//Housing & Housing equipements

gen h1 = c_e_rentagg + c_e_energyagg +c_e_maintenanceagg + c_e_othermaintenanceagg+ c_e_otheraccomodationagg
gen Housing= h1

gen housing_rural      = Housing*rural
gen housing_hameaux    = Housing*hameaux
gen housing_sVillage   = Housing*sVillage
gen housing_medVillage = Housing*medVillage
gen housing_gVillage   = Housing*gVillage
gen housing_sVille     = Housing*sVille
gen housing_medVille   = Housing*medVille
gen housing_gVille     = Housing*gVille
gen housing_Paris      = Housing*Paris


gen Housingeq = c_all_agg_cm

gen housingeq_rural      = Housingeq*rural
gen housingeq_hameaux    = Housingeq*hameaux
gen housingeq_sVillage   = Housingeq*sVillage
gen housingeq_medVillage = Housingeq*medVillage
gen housingeq_gVillage   = Housingeq*gVillage
gen housingeq_sVille     = Housingeq*sVille
gen housingeq_medVille   = Housingeq*medVille
gen housingeq_gVille     = Housingeq*gVille
gen housingeq_Paris      = Housingeq*Paris

// Health 
gen Health = c_all_agg_qa

gen health_rural      = Health*rural
gen health_hameaux    = Health*hameaux
gen health_sVillage   = Health*sVillage
gen health_medVillage = Health*medVillage
gen health_gVillage   = Health*gVillage
gen health_sVille     = Health*sVille
gen health_medVille   = Health*medVille
gen health_gVille     = Health*gVille
gen health_Paris      = Health*Paris



// Transport 
gen t1 = c_t_vehiculepurshasesagg + c_t_vehiculecostagg + c_t_vehiculeservisesagg + c_t_vehiculeotherexpensesagg

gen Transport            =t1
gen transport_rural      = Transport*rural
gen transport_hameaux    = Transport*hameaux
gen transport_sVillage   = Transport*sVillage
gen transport_medVillage = Transport*medVillage
gen transport_gVillage   = Transport*gVillage
gen transport_sVille     = Transport*sVille
gen transport_medVille   = Transport*medVille
gen transport_gVille     = Transport*gVille
gen transport_Paris      = Transport*Paris


*gen survey data for estimated CPI (subjective values), functional form of cdf derrived from mdn models plus survey datas (see python code)

set seed 1 
set obs 16978 


*food survey 

gen     surveyCPIfood= food+ 4*rbinomial(3,0.2)*food                            if c_tot < 12908
replace surveyCPIfood= food+ rchi2(5)-rnormal(2)*food                           if surveyCPIfood==. & c_tot<21106
replace surveyCPIfood= food+ rchi2(5)*food                                      if surveyCPIfood==. & c_tot<33227 
replace surveyCPIfood= food+ 2*rnormal(10)*food                                 if surveyCPIfood==. & c_tot< 61146 
replace surveyCPIfood= food+ 2*rnormal(10) -rnormal(2)*food                     if surveyCPIfood==. & c_tot< 70000
replace surveyCPIfood= food+ 2.4*rnormal(10) -rnormal(20)*food                  if surveyCPIfood==. & c_tot< 85000
replace surveyCPIfood= food+ 2*rnormal(10) -rnormal(20)*food                    if surveyCPIfood==. & c_tot< 95000
replace surveyCPIfood= food+ 2*rnormal(10) -rnormal(20)*food                    if surveyCPIfood==. & c_tot< 105000
replace surveyCPIfood= food- rnormal(3)*food                                    if surveyCPIfood==. & c_tot< 120000
replace surveyCPIfood= food+ rnormal(4)-rchi2(2)                                if surveyCPIfood==. & c_tot<=322099

gen diffFood= surveyCPIfood-food


*Transport survey 

gen     surveyCPITransport = Transport - (0.4*rnormal(1)-rchi2(1))*Transport    if c_tot <= 12908
replace surveyCPITransport = Transport + (normal(1)+rchi2(1))*Transport         if surveyCPITransport==. & c_tot<21106
replace surveyCPITransport = Transport + (rnormal(1)+rchi2(1))*Transport        if surveyCPITransport==. & c_tot<33227 
replace surveyCPITransport = Transport - (0.6*rnormal(1)- rchi2(1))*Transport   if surveyCPITransport==. & c_tot< 61146 
replace surveyCPITransport = Transport - (rnormal(1)-rchi2(1))*Transport        if surveyCPITransport==. & c_tot< 70000
replace surveyCPITransport = Transport - (rchi2(1))*Transport                   if surveyCPITransport==. & c_tot< 85000
replace surveyCPITransport = Transport - rnormal(2)*Transport                   if surveyCPITransport==. & c_tot< 95000
replace surveyCPITransport = Transport - (rchi2(1))*Transport                   if surveyCPITransport==. & c_tot< 105000
replace surveyCPITransport = Transport - rnormal(1)*Transport                   if surveyCPITransport==. & c_tot< 120000
replace surveyCPITransport = Transport - (rchi2(6))*Transport                   if surveyCPITransport==. & c_tot<=322099

gen  diffTransport= surveyCPITransport-Transport

*Health survey

gen     surveyCPIHealth= Health + 2*rnormal(1.5)*Health                         if c_tot <= 12908
replace surveyCPIHealth= Health + 0.7*rchi2(1)*rnormal(0.5)*Health              if surveyCPIHealth==. & c_tot<21106
replace surveyCPIHealth= Health + 0.5*rchi2(1)*rnormal(0.5)*Health              if surveyCPIHealth==. & c_tot<33227 
replace surveyCPIHealth= Health + rchi2(0.75)*rnormal(0.5)*Health               if surveyCPIHealth==. & c_tot< 61146 
replace surveyCPIHealth= Health - rchi2(2)*rnormal(0.5)*Health                  if surveyCPIHealth==. & c_tot< 70000
replace surveyCPIHealth= Health - rchi2(2)-rnormal(0.3)*Health                  if surveyCPIHealth==. & c_tot< 85000
replace surveyCPIHealth= Health - rnormal(0.5)*Health                           if surveyCPIHealth==. & c_tot< 95000
replace surveyCPIHealth= Health - (2*rnormal(0.5)+rnormal(0.1))*Health          if surveyCPIHealth==. & c_tot< 105000
replace surveyCPIHealth= Health - 2.5*rnormal(0.5)+rnormal(0.1)*Health          if surveyCPIHealth==. & c_tot< 120000
replace surveyCPIHealth= Health + rnormal(1)*Health+rchi2(1)                    if surveyCPIHealth==. & c_tot<=322099 

gen diffHealth= surveyCPIHealth-Health

*Housing survey

gen     surveyCPIhousing= Housing + rnormal(1)*Housing                           if c_tot <= 12908
replace surveyCPIhousing= Housing + rchi2(1.2)*rnormal(0.5)*Housing              if surveyCPIhousing==. & c_tot<21106
replace surveyCPIhousing= Housing + rchi2(1)*rnormal(0.5)*Housing                if surveyCPIhousing==. & c_tot<33227 
replace surveyCPIhousing= Housing + rchi2(0.75)*rnormal(0.5)*Housing             if surveyCPIhousing==. & c_tot< 61146 
replace surveyCPIhousing= Housing - rchi2(2)*rnormal(0.5)*Housing                if surveyCPIhousing==. & c_tot< 70000
replace surveyCPIhousing= Housing - rchi2(2)+rnormal(0.3)*Housing                if surveyCPIhousing==. & c_tot< 85000
replace surveyCPIhousing= Housing - rnormal(0.5)+rnormal(0.1)*Housing            if surveyCPIhousing==. & c_tot< 95000
replace surveyCPIhousing= Housing - 2*rnormal(0.5)+rnormal(0.1)*Housing          if surveyCPIhousing==. & c_tot< 105000
replace surveyCPIhousing= Housing - 2.5*rnormal(0.5)+rnormal(0.1)*Housing        if surveyCPIhousing==. & c_tot< 120000
replace surveyCPIhousing= Housing + rnormal(1)                                   if surveyCPIhousing==. & c_tot<=322099 

gen diffHousing = surveyCPIhousing - Housing


*Housingeq survey

gen     surveyCPIhousingeq= Housingeq + 0.3*rnormal(1)*Housingeq                if c_tot <= 12908
replace surveyCPIhousingeq= Housingeq + rchi2(1.2)*rnormal(0.5)*Housingeq       if surveyCPIhousingeq==. & c_tot<21106
replace surveyCPIhousingeq= Housingeq + rchi2(1)*rnormal(0.5)*Housingeq         if surveyCPIhousingeq==. & c_tot<33227 
replace surveyCPIhousingeq= Housingeq + rchi2(0.75)*rnormal(0.5)*Housingeq      if surveyCPIhousingeq==. & c_tot< 61146 
replace surveyCPIhousingeq= Housingeq - rchi2(2)*rnormal(0.5)*Housingeq         if surveyCPIhousing==. & c_tot< 70000
replace surveyCPIhousingeq= Housingeq - rchi2(2)+rnormal(0.3)*Housingeq         if surveyCPIhousingeq==. & c_tot< 85000
replace surveyCPIhousingeq= Housingeq - rnormal(0.5)+rnormal(0.1)*Housingeq     if surveyCPIhousingeq==. & c_tot< 95000
replace surveyCPIhousingeq= Housingeq - 2*rnormal(0.5)+rnormal(0.1)*Housingeq   if surveyCPIhousingeq==. & c_tot< 105000
replace surveyCPIhousingeq= Housingeq - 2.5*rnormal(0.5)+rnormal(0.1)*Housingeq if surveyCPIhousingeq==. & c_tot< 120000
replace surveyCPIhousingeq= Housingeq + rnormal(1)                              if surveyCPIhousingeq==. & c_tot<=322099 

gen diffHousingeq = surveyCPIhousingeq - Housing



*c_tot survey 

gen     surveyCPIctot= c_tot+ (rchi2(1)-2*rbinomial(1,.3)-.25*rnormal(1))*c_tot if c_tot < 21106
replace surveyCPIctot= c_tot+ rchi2(5)*c_tot                                    if surveyCPIctot==. & c_tot<33227 
replace surveyCPIctot= c_tot+ 2*rnormal(10) -rnormal(4)*c_tot                   if surveyCPIctot==. & c_tot< 21106.5
replace surveyCPIctot= c_tot+ 2*rnormal(10) -rnormal(20)*c_tot                  if surveyCPIctot==. & c_tot< 33227
replace surveyCPIctot= c_tot+ rnormal(4)-rchi2(2)*c_tot                         if surveyCPIctot==. & c_tot<322099

gen diffCtot= surveyCPIctot-c_tot

*Spatial distribution 
gen v = f1*pondmen
gen H = Health*pondmen 
gen T = t1*pondmen
gen O = Housing*pondmen
gen E = Housingeq*pondmen

*qrprocess v Food*        [pweight= pondmen], q(.1 .2 .3 .4 .5 .6 .7 .8 .9)
*qrprocess H health*      [pweight= pondmen], q(.1 .2 .3 .4 .5 .6 .7 .8 .9)
*qrprocess T transport*   [pweight= pondmen], q(.1 .2 .3 .4 .5 .6 .7 .8 .9)
*qrprocess O housing*     [pweight= pondmen], q(.1 .2 .3 .4 .5 .6 .7 .8 .9)
*qrprocess E housingeq*   [pweight= pondmen], q(.1 .2 .3 .4 .5 .6 .7 .8 .9)

* percentages form : (var / sum var )*100
gen diff100      = diffCtot/(1.47e+06)
gen diffHz100    = diffHousing/(3.41e+05)
gen diffHzEq100  = diffHousingeq/(-3.10e+05)
gen diffHl100    = diffHealth/35987.5
gen diffT100     = diffTransport/( 3.32e+05)
gen diffF100     = diffFood/(3.59e+06)

* Contrefactual micro-Inflation adjusted data : var/ (1+pi_cf)
gen Housing_cf      = Housing/1.23536307
gen Food_cf         = food/1.43561924
gen HousingEq_cf    = Housingeq/1.36681301
gen Transport_cf    = Transport/1.31789472
gen Health_cf       = Health/1.23536307

gen ToTInfl_cf      = Total/1.294

* Actual micro-Inflation adjusted data : var/ (1+pi_act)
gen Housing_act   = Housing/1.25808952
gen Food_act      = food/1.53113298
gen HousingEq_act = Housingeq/1.62714504
gen Transport_act = Transport/1.54704505
gen Health_act    = Health/1.32317858

gen ToTInfl_act   = Total/1.465

* Perceived micro-Inflation adjusted data:var/ (1+pi_survey)

gen Housing_per   = Housing/1.73142071
gen Food_per      = food/1.51895263
gen HousingEq_per = Housingeq/1.73142071
gen Transport_per = Transport/1.59489492
gen Health_per    = Health/1.33598019

* Total according to their  specific 2023 weights:
 
gen o_act = ToTInfl_act-Food_act-Transport_act-Housing_act-HousingEq_act-Health_act
gen Total_act = .225*Food_act + 0.16*Transport_act + 0.06*Housing_act +0.183*HousingEq_act + 0.062*Health_act + (1-.225-0.160-0.060-0.183-0.062)*o_act

gen o_cf = ToTInfl_cf-Food_cf-Transport_cf-Housing_cf-HousingEq_cf-Health_cf
gen Total_cf  = .187*Food_cf + 0.099*Transport_cf + 0.106*Housing_cf +0.091*HousingEq_cf + 0.058*Health_cf + (1-.187-0.099-0.106-0.1091-0.058)*o_cf

gen o_per = ToTInfl_act-Food_cf-Transport_per-Housing_per-HousingEq_cf-Health_per
gen Total_per = .222*Food_per + 0.179*Transport_per + 0.112*Housing_per +0.250*HousingEq_per + 0.069*Health_per + (1-.222-0.179-0.112-0.250-0.069)*o_per



*Counterfactual Effect  scenarii according to V. CHERNOZHUKOV, I. FERNÁNDEZ-VAL, AND B. MELLY( 2013)

gen delta_act_cf  = Total_act - Total_cf
gen delta_per_act = Total_per - Total_act
gen delta_per_cf  = Total_per - Total_cf 


*Generating Conditional distributions according to each scenario



foreach c in  rural sVillage medVillage gVillage sVille medVille gVille Paris{
	foreach j in act per cf {
		gen Housing_`j'_`c'   = Housing_`j'*`c'
	    gen Food_`j'_`c'      = Food_`j'*`c'
	    gen Housingeq_`j'_`c' = HousingEq_`j'*`c'
	    gen Transport_`j'_`c' = Transport_`j'*`c'
		gen Health_`j'_`c'    = Health_`j'*`c'
		gen Other_`j'_`c'     = o_`j'*`c'
	}
	
}


*gen Hz   = Housing 
*gen Fd   = Food
*gen HzEq = HousingEq
*gen Hl   = Health
*gen Te   = transport

*drop Housing Food HousingEq Health transport

*Decomposition according to B. MELLY (2005)
*Actual vs Perceived: AvsP / Counterfactual vs Perceived : CvsP/ Actual vs Counterfactual: AvsC

*Pimp the thing for decompositions
*foreach c in Housing Food Housingeq Transport Health Other{foreach j in rural sVillage medVillage gVillage sVille medVille gVille Paris{gen q_`c'_`j'= }}

*foreach c in Housing Food Housingeq Transport Health Other{foreach j in rural sVillage medVillage gVillage sVille medVille gVille Paris{foreach v in AvsP CvsP AvsC{gen Dif_`v'_`c'_`j' = (`c'_per_`j' - `c'_act_`j')}}}
foreach c in Housing Food Housingeq Transport Health Other{
	foreach j in rural sVillage medVillage gVillage sVille medVille gVille Paris{
		 gen Dif_AvsP_`c'_`j' = (`c'_per_`j' - `c'_act_`j')
		 gen Dif_CvsP_`c'_`j' = (`c'_per_`j' - `c'_cf_`j')
		 gen Dif_AvsC_`c'_`j' = (`c'_act_`j' - `c'_cf_`j')
	}
}

* Distribution reg 

quiet drprocess delta_act_cf Dif_AvsC_* ,functional method(lpm, onestep) vce(multiplier, reps(10))
plotprocess Dif_AvsC_Food_medVille
quiet drprocess delta_per_act Dif_AvsP_* ,functional method(lpm, onestep) vce(multiplier, reps(10))
plotprocess

quiet qrprocess delta_act_cf Dif_AvsC_*
plotprocess

*quiet drprocess delta_per_act Dif_AvsP_*,functional method(lpm, onestep) vce(multiplier, reps(10))


*quiet drprocess delta_per_cf Dif_CvcP_*

*Quantile reg
*qrprocess delta_act_cf Dif_AvsC_* [pweight=pondmen]
*qrprocess delta_per_act Dif_AvsP_* [pweight=pondmen]



