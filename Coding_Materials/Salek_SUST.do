





*=====================================================================
* BDHS 2022 — SWPER EMPOWERMENT & ANC UTILIZATION ANALYSIS
* Stata 17-compatible full pipeline
* Includes: main model + WHO-8 sensitivity + husband-occ sensitivity
*=====================================================================

clear all
set maxvar 30000
set more off

*---------------------------------------------------------------------
* 0. LOAD DATA
*---------------------------------------------------------------------
use "D:/Research/BDHS Research/Cross country analysis/Data/BDIR81FL.DTA", clear

*=====================================================================
* 1. OUTCOME — ANC UTILIZATION (main + WHO-8)
*=====================================================================
replace m14_1 = . if m14_1 == 98

* Main 3-level outcome
gen anc_util = .
replace anc_util = 0 if m14_1 == 0
replace anc_util = 1 if inrange(m14_1, 1, 4)
replace anc_util = 2 if m14_1 > 4 & m14_1 < .

label define anc_util_lbl 0 "No ANC visits" ///
                          1 "1–4 ANC visits" ///
                          2 "More than 4 ANC visits", replace
label values anc_util anc_util_lbl
label variable anc_util "ANC utilization (3 ordered categories)"

* WHO-8 aligned outcome
gen anc_who = .
replace anc_who = 1 if inrange(m14_1, 0, 3)
replace anc_who = 2 if inrange(m14_1, 4, 7)
replace anc_who = 3 if m14_1 >= 8 & m14_1 < .

label define anc_who_lbl 1 "<4 visits" 2 "4–7 visits" ///
                         3 "8+ visits (WHO)", replace
label values anc_who anc_who_lbl
label variable anc_who "ANC utilization (WHO-8 aligned)"

tab anc_util, missing
tab anc_who, missing

keep if !missing(anc_util)

*=====================================================================
* 2. SWPER INDEX CONSTRUCTION
*=====================================================================
rename v744a ipv_goes_out
rename v744b ipv_neglects_children
rename v744c ipv_argues
rename v744d ipv_refuses_sex
rename v744e ipv_burns_food

foreach var in ipv_goes_out ipv_neglects_children ipv_argues ///
               ipv_refuses_sex ipv_burns_food {
    replace `var' = . if `var' == 8
}

rename v743a decision_health
rename v743b decision_purchases
rename v743d decision_visits

foreach var in decision_health decision_purchases decision_visits {
    replace `var' = . if inlist(`var',5,6,8)
}

rename v133 edu_years
rename v157 news_freq
rename v158 radio_freq
rename v159 tv_freq
rename v511 age_marriage
rename v212 age_birth

foreach var in ipv_goes_out ipv_neglects_children ipv_argues ///
               ipv_refuses_sex ipv_burns_food {
    recode `var' (1=1) (0=0) (else=.), generate(`var'_pca)
}

foreach var in decision_health decision_purchases decision_visits {
    gen `var'_pca = .
    replace `var'_pca = 2 if `var' == 1
    replace `var'_pca = 1 if `var' == 2
    replace `var'_pca = 0 if `var' == 3
    replace `var'_pca = . if `var' == 4
}

pca ipv_goes_out_pca ipv_neglects_children_pca ipv_argues_pca ///
    ipv_refuses_sex_pca ipv_burns_food_pca ///
    edu_years news_freq radio_freq tv_freq ///
    age_marriage age_birth ///
    decision_health_pca decision_purchases_pca decision_visits_pca

predict swper1 swper2 swper3
rename swper1 attitude_violence
rename swper2 social_independence
rename swper3 decision_autonomy

xtile attitude_violence_tertile = attitude_violence, nq(3)
xtile social_independence_tertile = social_independence, nq(3)
xtile decision_autonomy_tertile = decision_autonomy, nq(3)

label define tertile 1 "Low" 2 "Medium" 3 "High", replace
label values attitude_violence_tertile tertile
label values social_independence_tertile tertile
label values decision_autonomy_tertile tertile

*=====================================================================
* 3. COMPLETE-CASE FLAG & SEQUENTIAL EXCLUSION REPORT
*=====================================================================
gen complete_case = 1
replace complete_case = 0 if missing(anc_util)
replace complete_case = 0 if missing(attitude_violence)
replace complete_case = 0 if missing(social_independence)
replace complete_case = 0 if missing(decision_autonomy)
replace complete_case = 0 if missing(v012)
replace complete_case = 0 if missing(v106)
replace complete_case = 0 if missing(v190)
replace complete_case = 0 if missing(v025)
replace complete_case = 0 if missing(v024)

display as result _n "=============== SEQUENTIAL EXCLUSIONS ==============="
count
display "Total women identified: " r(N)
count if missing(anc_util)
display "Excluded — missing ANC outcome: " r(N)
count if !missing(anc_util) & (missing(attitude_violence) ///
        | missing(social_independence) | missing(decision_autonomy))
display "Excluded — missing SWPER exposure: " r(N)
count if !missing(anc_util) ///
        & !missing(attitude_violence) ///
        & !missing(social_independence) ///
        & !missing(decision_autonomy) ///
        & (missing(v012) | missing(v106) | missing(v190) ///
           | missing(v025) | missing(v024))
display "Excluded — missing covariates: " r(N)
count if complete_case == 1
display "Final complete-case analytical sample (unweighted): " r(N)

keep if complete_case == 1

*=====================================================================
* 4. COVARIATE CLEANING — 10 VARIABLES
*=====================================================================
gen women_age = .
replace women_age = 1 if inrange(v012, 15, 19)
replace women_age = 2 if inrange(v012, 20, 29)
replace women_age = 3 if inrange(v012, 30, 39)
replace women_age = 4 if inrange(v012, 40, 49)
label define women_age_lbl 1 "15-19" 2 "20-29" 3 "30-39" 4 "40-49", replace
label values women_age women_age_lbl

gen women_edu = .
replace women_edu = 0 if v106 == 0
replace women_edu = 1 if v106 == 1
replace women_edu = 2 if v106 == 2
replace women_edu = 3 if v106 == 3
replace women_edu = . if inlist(v106, 8, 9) | missing(v106)
label define women_edu_lbl 0 "No education" 1 "Primary" 2 "Secondary" 3 "Higher", replace
label values women_edu women_edu_lbl

rename v025 residence
label define residence_lbl 1 "Urban" 2 "Rural", replace
label values residence residence_lbl

gen husb_edu = .
replace husb_edu = 0 if v701 == 0
replace husb_edu = 1 if v701 == 1
replace husb_edu = 2 if v701 == 2
replace husb_edu = 3 if v701 == 3
replace husb_edu = . if inlist(v701, 8, 9) | missing(v701)
label define husb_edu_lbl 0 "No education" 1 "Primary" 2 "Secondary" 3 "Higher", replace
label values husb_edu husb_edu_lbl

gen husb_occ = .
replace husb_occ = 0 if v705 == 0
replace husb_occ = 1 if inlist(v705, 1, 2, 3)
replace husb_occ = 2 if inlist(v705, 4, 5)
replace husb_occ = 3 if inlist(v705, 6, 7)
replace husb_occ = . if inlist(v705, 8, 9, 96, 98) | missing(v705)
label define husb_occ_lbl 0 "Not working" 1 "Professional/technical" ///
                          2 "Sales/service" 3 "Agricultural/domestic", replace
label values husb_occ husb_occ_lbl

gen wealth = v190
label define wealth_lbl 1 "Poorest" 2 "Poorer" 3 "Middle" 4 "Richer" 5 "Richest", replace
label values wealth wealth_lbl

gen birth_order = .
replace birth_order = 1 if v218 == 1
replace birth_order = 2 if inrange(v218, 2, 4)
replace birth_order = 3 if v218 > 4 & !missing(v218)
label define birth_order_lbl 1 "1" 2 "2-4" 3 ">4", replace
label values birth_order birth_order_lbl

gen edu_gap = .
replace edu_gap = 0 if edu_years == v715
replace edu_gap = 1 if edu_years <  v715
replace edu_gap = 2 if edu_years >  v715
replace edu_gap = . if missing(edu_years) | missing(v715)
label define edu_gap_lbl 0 "No gap" 1 "Wife lower" 2 "Wife higher", replace
label values edu_gap edu_gap_lbl

gen women_occ = .
replace women_occ = 0 if v714 == 0
replace women_occ = 1 if v714 == 1
label define women_occ_lbl 0 "Unemployed" 1 "Employed", replace
label values women_occ women_occ_lbl

capture confirm variable v238
if _rc == 0 {
    gen births_3yr = .
    replace births_3yr = 0 if v238 == 0
    replace births_3yr = 1 if v238 == 1
    replace births_3yr = 2 if v238 > 1 & !missing(v238)
}
else {
    gen births_3yr = .
    replace births_3yr = 0 if v208 == 0
    replace births_3yr = 1 if v208 == 1
    replace births_3yr = 2 if v208 > 1 & !missing(v208)
}
label define births3_lbl 0 "No birth" 1 "One birth" 2 "More than one", replace
label values births_3yr births3_lbl

*=====================================================================
* 5. SURVEY DESIGN
*=====================================================================
svyset v021 [pw = v005], strata(v022)
svydescribe

count
display "Final analytical sample (unweighted): " r(N)

*=====================================================================
* 6. TABLE 1 — WEIGHTED
*=====================================================================
svy: tab attitude_violence_tertile anc_util, row percent
svy: tab social_independence_tertile anc_util, row percent
svy: tab decision_autonomy_tertile anc_util, row percent
svy: tab women_age anc_util, row percent
svy: tab women_edu anc_util, row percent
svy: tab residence anc_util, row percent
svy: tab husb_edu anc_util, row percent
svy: tab husb_occ anc_util, row percent
svy: tab wealth anc_util, row percent
svy: tab birth_order anc_util, row percent
svy: tab edu_gap anc_util, row percent
svy: tab women_occ anc_util, row percent
svy: tab births_3yr anc_util, row percent

*=====================================================================
* 7. SUPPLEMENTARY TABLE S1 — UNWEIGHTED
*=====================================================================
tab attitude_violence_tertile anc_util, row chi2
tab social_independence_tertile anc_util, row chi2
tab decision_autonomy_tertile anc_util, row chi2
tab women_age anc_util, row chi2
tab women_edu anc_util, row chi2
tab residence anc_util, row chi2
tab husb_edu anc_util, row chi2
tab husb_occ anc_util, row chi2
tab wealth anc_util, row chi2
tab birth_order anc_util, row chi2
tab edu_gap anc_util, row chi2
tab women_occ anc_util, row chi2
tab births_3yr anc_util, row chi2

*=====================================================================
* 8. MODEL 1 — MAIN ANALYSIS (0 / 1–4 / >4) — cOR and aOR
*=====================================================================
display as result _n "=========== MODEL 1 UNADJUSTED (cOR) ==========="
svy: ologit anc_util ///
    i.attitude_violence_tertile ///
    i.social_independence_tertile ///
    i.decision_autonomy_tertile, or
estimates store unadj

display as result _n "=========== MODEL 1 ADJUSTED (aOR) ==========="
svy: ologit anc_util ///
    i.attitude_violence_tertile ///
    i.social_independence_tertile ///
    i.decision_autonomy_tertile ///
    i.women_age i.women_edu i.residence ///
    i.husb_edu i.husb_occ i.wealth ///
    i.birth_order i.edu_gap i.women_occ i.births_3yr, or
estimates store adj

*=====================================================================
* 9. MODEL 2 — WHO-8 SENSITIVITY ANALYSIS
*=====================================================================
display as result _n "=========== MODEL 2 WHO-8 SENSITIVITY ==========="
svy: ologit anc_who ///
    i.attitude_violence_tertile ///
    i.social_independence_tertile ///
    i.decision_autonomy_tertile ///
    i.women_age i.women_edu i.residence ///
    i.husb_edu i.husb_occ i.wealth ///
    i.birth_order i.edu_gap i.women_occ i.births_3yr, or

*=====================================================================
* 9b. MODEL 3 — SENSITIVITY WITHOUT HUSBAND OCCUPATION
*     (Retains the full 2,664 SWPER-complete sample)
*=====================================================================
display as result _n "=========== MODEL 3: SENSITIVITY (NO HUSB OCC) ==========="

* Complete-case flag for reduced covariate set (no husband occupation)
gen cc_reduced = 1
replace cc_reduced = 0 if missing(anc_util)
replace cc_reduced = 0 if missing(attitude_violence)
replace cc_reduced = 0 if missing(social_independence)
replace cc_reduced = 0 if missing(decision_autonomy)
replace cc_reduced = 0 if missing(women_age)
replace cc_reduced = 0 if missing(women_edu)
replace cc_reduced = 0 if missing(residence)
replace cc_reduced = 0 if missing(husb_edu)
replace cc_reduced = 0 if missing(wealth)
replace cc_reduced = 0 if missing(birth_order)
replace cc_reduced = 0 if missing(edu_gap)
replace cc_reduced = 0 if missing(women_occ)
replace cc_reduced = 0 if missing(births_3yr)

count if cc_reduced == 1
display "Sensitivity sample N (without husband occupation): " r(N)

preserve
    keep if cc_reduced == 1
    svy: ologit anc_util ///
        i.attitude_violence_tertile ///
        i.social_independence_tertile ///
        i.decision_autonomy_tertile ///
        i.women_age i.women_edu i.residence ///
        i.husb_edu i.wealth ///
        i.birth_order i.edu_gap i.women_occ i.births_3yr, or
    estimates store sens_no_husbocc
restore

*=====================================================================
* 10. PROPORTIONAL ODDS TEST — oparallel (Stata 17)
*=====================================================================
display as result _n "=========== PROPORTIONAL ODDS TEST (oparallel) ==========="

capture which oparallel
if _rc != 0 {
    ssc install oparallel, replace
}

ologit anc_util ///
    i.attitude_violence_tertile ///
    i.social_independence_tertile ///
    i.decision_autonomy_tertile ///
    i.women_age i.women_edu i.residence ///
    i.husb_edu i.husb_occ i.wealth ///
    i.birth_order i.edu_gap i.women_occ i.births_3yr

oparallel, ic

estimates store parallel_test

*=====================================================================
* 11. GOODNESS-OF-FIT — WITHOUT svy: prefix
*=====================================================================
display as result _n "=========== GOODNESS-OF-FIT ==========="

estat ic

display "McFadden's Pseudo R² = " %6.4f e(r2_p)

*=====================================================================
* 12. AUC / ROC FOR ORDINAL MODEL (one-vs-rest)
*=====================================================================
display as result _n "=========== AUC / ROC ==========="

svy: ologit anc_util ///
    i.attitude_violence_tertile ///
    i.social_independence_tertile ///
    i.decision_autonomy_tertile ///
    i.women_age i.women_edu i.residence ///
    i.husb_edu i.husb_occ i.wealth ///
    i.birth_order i.edu_gap i.women_occ i.births_3yr

predict double pr1 pr2 pr3, pr

gen byte anc_bin0 = (anc_util == 0)
gen byte anc_bin1 = (anc_util == 1)
gen byte anc_bin2 = (anc_util == 2)

display _n "---- AUC: No visits vs (1–4 or >4) ----"
roctab anc_bin0 pr1

display _n "---- AUC: 1–4 visits vs (No or >4) ----"
roctab anc_bin1 pr2

display _n "---- AUC: >4 visits vs (No or 1–4) ----"
roctab anc_bin2 pr3

*=====================================================================
* 13. SAVE FINAL DATASET
*=====================================================================
compress
save "D:/Research/BDHS Research/Cross country analysis/Data/BDIR81FL_final.dta", replace

display as result _n "============= ANALYSIS COMPLETE ============="
*=====================================================================
* END OF DO-FILE
*=====================================================================

