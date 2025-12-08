


* =======================================
* BRING COUPLES DATASET INTO STATA
* =======================================

cd"...\Replication Files"

* =======================================
* BRING COUPLES DATASET INTO STATA
* =======================================
use "tables.dta",clear



su wage pglabgro ijob1 plc0013_h plc0016

sort parid syear

su wage pglabgro ijob1 plc0013_h plc0016

* ======================================================================================
* FOCUS ON DUAL EARNERS COUPLES
* ======================================================================================


* MALES-FEMALES

g diff_wage=(wage - wage_female) if !missing(wage) & !missing(wage_female)


* FEMALES-MALES
g diff_wage_OR=(wage_female - wage) if !missing(wage) & !missing(wage_female)
g diff_pglabgro_OR=(pglabgro_female - pglabgro) if !missing(pglabgro) & !missing(pglabgro_female)
g diff_ijob1_OR=(ijob1_female - ijob1) if !missing(ijob1) & !missing(ijob1_female)
g diff_plc0013_h_OR=(plc0013_h_female - plc0013_h) if !missing(plc0013_h) & !missing(plc0013_h_female)


su diff_wage diff_ijob1_OR

gen d50_wage=diff_wage<0 if diff_wage!=.
gen d50_pglabgro=diff_pglabgro<0 if diff_wage!=.
gen d50_ijob1=diff_ijob1<0 if diff_ijob1!=.
gen d50_plc0013_h=diff_plc0013_h<0 if diff_plc0013_h!=.

xtile med_mcs=mcs, nq(2)
recode med_mcs 1=0 2=1

gen bins_diff_wage=int(diff_wage/1000)

gen DINT=diff_wage*d50_wage 

bysort pid: egen wm=mean(phrf) 

gen share=wage/(wage_female+wage) if !missing(wage) & !missing(wage_female)
su share

* =================================================
* KEY RUNNING VARIABLE CENTERED AT THE CUTOFF
* =================================================
gen recode_diff_wage_OR=diff_wage_OR/1000

gen recode_diff_wage_OR_int=int(diff_wage_OR/1000)

su recode_diff_wage_OR recode_diff_wage_OR_int

unique recode_diff_wage_OR
unique recode_diff_wage_OR_int


global running recode_diff_wage_OR

global running_or recode_diff_wage_OR_int



gen recode_diff_wage=diff_wage/1000
gen recode_female_wage=wage_female/1000
gen recode_male_wage_OR=wage/1000



* =======================================
* AGE CONSTRAINT ON MALES	
* =======================================
keep if age>17&age<65


keep if partner==1

keep if female==0	
	

* ========================================
* DROP MISSING ON THE COVARIATES
* ========================================
drop if missing(edu_isced)
drop if missing(edu_isced_female)

drop if missing(nchildren)
drop if missing(nchildren_female)


center mcs, standardize prefix(std_)
center pcs, standardize prefix(std_)

center mcs_female, standardize prefix(std_)
center pcs_female, standardize prefix(std_)


su std_mcs std_pcs 	
	
global outcomes  overalllifesat worksat m11125 std_mcs std_pcs 

global outcomes_female  overalllifesat_female worksat_female m11125_female std_mcs_female std_pcs_female 


* ====================================================
* CLEAN VARIABLES FROM INDIVIDUAL FIXED EFFECTS
* ====================================================
foreach outcome in $outcomes {

areg  `outcome', abs(pid)

predict `outcome'_res, res


}


global outcomes_res  overalllifesat_res worksat_res m11125_res std_mcs_res std_pcs_res 


foreach outcome in $outcomes_female {

areg  `outcome', abs(pid)

predict `outcome'_res, res


}

rename overalllifesat_female_res overalllifesat_res_female
rename worksat_female_res worksat_res_female
rename m11125_female_res m11125_res_female
rename std_mcs_female_res std_mcs_res_female
rename std_pcs_female_res std_pcs_res_female



global outcomes_res_female  overalllifesat_res_female worksat_res_female m11125_res_female std_mcs_res_female std_pcs_res_female 





global threshold 0.00001

g dummy=(diff_wage_OR>0) if !missing(diff_wage_OR)
g running_int=recode_diff_wage_OR*west
global running_int running_int 



capture erase ${table1}table2PanelA.xls
capture erase ${table1}table2PanelA.txt		


foreach outcomeres in $outcomes_res {
	
	
	rdrobust `outcomeres' $running_int , c($threshold) vce(cluster $running_or) covs($running west dummy)
	
			scalar define ysample=r(mean)
			estadd scalar ysample= ysample
			sum `outcomeres'  if e(sample)==1
			scalar define ymsample=r(mean)
			estadd scalar ymsample= ysample
			scalar define ysdsample=r(sd)
			estadd scalar ysdsample= ysample
			
			local ymean "ymsample"
			local ysd "ysdsample"

	
	outreg2 using ${table1}table2PanelA.xls, excel se bdec(3) sdec(3) rdec(3) nocons addtext(state fe) addstat(Mean of Dep. Var., `ymean', Std.Err. of Dep. Var., `ysd') ct(`outcome') ti("RDD ROBUST") append
	
	
	
}


capture erase ${table1}table2PanelA.txt














* =======================================
* BRING COUPLES DATASET INTO STATA
* =======================================

use tables, replace


su wage pglabgro ijob1 plc0013_h plc0016

sort parid syear

su wage pglabgro ijob1 plc0013_h plc0016


* ======================================================================================
* FOCUS ON DUAL EARNERS COUPLES
* ======================================================================================

* MALES-FEMALES
g diff_wage=(wage - wage_female) if !missing(wage) & !missing(wage_female)

* FEMALES-MALES
g diff_wage_OR=(wage_female - wage) if !missing(wage) & !missing(wage_female)
g diff_pglabgro_OR=(pglabgro_female - pglabgro) if !missing(pglabgro) & !missing(pglabgro_female)
g diff_ijob1_OR=(ijob1_female - ijob1) if !missing(ijob1) & !missing(ijob1_female)
g diff_plc0013_h_OR=(plc0013_h_female - plc0013_h) if !missing(plc0013_h) & !missing(plc0013_h_female)



su diff_wage diff_ijob1_OR
gen d50_wage=diff_wage<0 if diff_wage!=.
gen d50_pglabgro=diff_pglabgro<0 if diff_wage!=.
gen d50_ijob1=diff_ijob1<0 if diff_ijob1!=.
gen d50_plc0013_h=diff_plc0013_h<0 if diff_plc0013_h!=.

xtile med_mcs=mcs, nq(2)
recode med_mcs 1=0 2=1

gen bins_diff_wage=int(diff_wage/1000)


gen DINT=diff_wage*d50_wage 

bysort pid: egen wm=mean(phrf) 

gen share=wage/(wage_female+wage) if !missing(wage) & !missing(wage_female)
su share


gen recode_diff_wage=diff_wage/1000

* =================================================
* KEY RUNNING VARIABLE CENTERED AT THE CUTOFF
* =================================================

gen recode_diff_wage_OR=diff_wage_OR/1000
global running recode_diff_wage_OR

gen recode_diff_wage_OR_int=int(diff_wage_OR/1000)
global running_or recode_diff_wage_OR_int




* =======================================
* AGE CONSTRAINT ON MALES	
* =======================================
keep if age>17&age<65


keep if partner==1
keep if female==0	


* ========================================
* DROP MISSING ON THE COVARIATES
* ========================================
drop if missing(edu_isced)
drop if missing(edu_isced_female)

drop if missing(nchildren)
drop if missing(nchildren_female)

	
	
center mcs_female, standardize prefix(std_)
center pcs_female, standardize prefix(std_)

center mcs, standardize prefix(std_)
center pcs, standardize prefix(std_)



global outcomes_female  overalllifesat_female worksat_female m11125_female std_mcs_female std_pcs_female 

global outcomes  overalllifesat worksat m11125 std_mcs std_pcs 





* ====================================================
* CLEAN VARIABLES FROM INDIVIDUAL FIXED EFFECTS
* ====================================================
foreach outcome in $outcomes_female {

areg  `outcome', abs(pid)

predict `outcome'_res, res


}

foreach outcome in $outcomes {

areg  `outcome', abs(pid)

predict `outcome'_res, res


}



global outcomes_res  overalllifesat_female_res worksat_female_res m11125_female_res  std_mcs_female_res std_pcs_female_res 


rename overalllifesat overalllifesat_female_male
rename worksat worksat_female_male
rename m11125 m11125_female_male
rename std_mcs std_mcs_female_male
rename std_pcs std_pcs_female_male



rename overalllifesat_res overalllifesat_female_res_m
rename worksat_res worksat_female_res_m
rename m11125_res m11125_female_res_m
rename std_mcs_res std_mcs_female_res_m
rename std_pcs_res std_pcs_female_res_m




global threshold 0.00001


g running_int=recode_diff_wage_OR*west
global running_int running_int 
g dummy=(diff_wage_OR>0) if !missing(diff_wage_OR)






capture erase ${table1}table2PanelB.xls
capture erase ${table1}table2PanelB.txt		


foreach outcomeres in $outcomes_res {
	
	
	rdrobust `outcomeres' $running_int, c($threshold) vce(cluster $running_or) covs($running west dummy)
	
			scalar define ysample=r(mean)
			estadd scalar ysample= ysample
			sum `outcomeres'  if e(sample)==1
			scalar define ymsample=r(mean)
			estadd scalar ymsample= ysample
			scalar define ysdsample=r(sd)
			estadd scalar ysdsample= ysample
			
			local ymean "ymsample"
			local ysd "ysdsample"

	
	outreg2 using ${table1}table2PanelB.xls, excel se bdec(3) sdec(3) rdec(3) nocons addtext(state fe) addstat(Mean of Dep. Var., `ymean', Std.Err. of Dep. Var., `ysd') ct(`outcome') ti("RDD ROBUST") append
	
	
	
}


capture erase ${table1}table2PanelB.txt















