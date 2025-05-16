********************************************************************************
*MULTILEVEL MODELLING OF NIGHT AWAKENINGS TRAJECTORIES IN ALSPAC
********************************************************************************
* This is the syntax for multilevel growth curve modelling trajectories of night-time awakeningstrajectories in autistic and non-autistic participants of ALSPAC 

* This syntax assumes that you have already cleaned your data. The data should be in long format, with columns for subject ID, occasion, age, and night-time sleep duration, and covariates 

********************************************************************************
*DATA PREPARATION*
********************************************************************************
*Make a new variable which is age in years
gen age_years = age/12

* Format the new age variable to have less decimal places 
format %9.2f age age_years

* Sort so that your data are in the right order
sort id time_point

* Create a new variable which is the number of occasions a person has completed 
* You can then choose to include or exclude people based on the number of assessments they have completed. 
* drop if numocc == 1 // eg. you might choose this if you wanted to drop people with only one assessment 
by id: egen numocc = count(intdiff)

* Generate a new numeric variable occ which corresponds to the timepoint
egen occ = group(time_point)

replace occ = 1 if time_point == "age_t1"
replace occ = 2 if time_point == "age_t2"
replace occ = 3 if time_point == "age_t3"
replace occ = 4 if time_point == "age_t4"
replace occ = 5 if time_point == "age_t5"
replace occ = 6 if time_point == "age_t6"
replace occ = 7 if time_point == "age_t7"


* Order the key variables needed 
order id time_point occ age_years intdiff 


********************************************************************************
*DESCRIPTIVE INFORMATION & GROWTH CURVE PREP*
********************************************************************************
* Declare the data to be panel for use in later analysis 
xtset id occ

* Describe the missing wake data patterns 
* In the output, 1st row shows that 3709 people have completed every measure, etc 
xtdes if intdiff~=. 

* To get the sleep and age stats by occasion 
bysort occ: sum intdiff age_years

* Check whether the data is linear to guide how to model growth curves
* Assumption check 1: Is the data linear? 
lowess intdiff age_years, nograph gen(yhatlowess)
line yhatlowess age_years, sort // it looks, weird? doesnt matter, we're not necessarily expecting linear changes anyway 

* Create a graph showing mean intdiff over time by age to guide how to model trajectories 
*first create a new variable which is the mean age of all assessments
egen mean_age = mean(age_years), by(occ)
egen tag = tag(occ)
egen internalising = mean(intdiff), by(occ)

* install software for editing graphs on stata
ssc install grstyle, replace
ssc install palettes, replace

* initialise the software 
set scheme s2color 
grstyle init

* Specify the settings for the graph you're about to plot
grstyle set nogrid 

* if you want to clear the settings above: grstyle clear

twoway (connected internalising mean_age if tag, sort msymbol(circle) msize(small) lcolor(dknavy)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(3(2)18, format(%5.0f)) ///
ylabel(0(1)4) ///
xtitle("Age (Years)") ///
ytitle("Internalising Difficulties (SDQ)") 


* We can also plot group-specific changes 
egen intdiff_no_asd = mean(intdiff) if asd==0, by(occ)
egen intdiff_asd = mean(intdiff) if asd==1, by(occ)

twoway (connected intdiff_no_asd mean_age, sort msymbol(circle) msize(small) lcolor(dknavy)) ///
(connected intdiff_asd mean_age, sort msymbol(circle) msize(small) lcolor(dkorange)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(3(2)18, format(%5.0f)) ///
ylabel(0(1)8) ///
xtitle("Age (Years)") ///
ytitle("Internalising Difficulties (SDQ)") ///
legend(order(1 "Non-autistic" 2 "Autistic"))



********************************************************************************
*EXPLORING DIFFERENT SHAPES OF GROWTH CURVES*
********************************************************************************
* First we will center the age variable to allow for improved convergence and interpretability 
* The new age variable (agemc) is just the age_years variable subtracted by the mean age_years
sum age_years // mean is 9.518781 

gen agemc = age_years-9.518781  //

* LINEAR MODEL 
mixed intdiff agemc || id: agemc, cov(uns)

* You can locally store the model estimates as m1 (for comparing against later models)
estimates store m1

* Get information criteria for model comparison 
estat ic

* And you can get predictions from that model
predict intdiff_lin

* You can then plot the trajectory from the linear model
twoway (line intdiff_lin age_years, sort lcolor(dknavy)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(3(2)18, format(%5.0f)) ///
ylabel(0(1)4) ///
xtitle("Age (Years)") ///
ytitle("Internalising Difficulties (SDQ)") 

*Plot model against observed 
twoway ///
(line intdiff_lin age_years, sort lcolor(dknavy) lpattern(solid)) ///
(connected internalising mean_age if tag, sort lcolor(midblue) lpattern(dash) msize(small) mcolor(navy)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(3(2)18, format(%5.0f)) ///
ylabel(0(1)4) ///
xtitle("Age (Years)") ///
ytitle("Internalising Difficulties (SDQ)") ///
legend(order(1 "Linear Prediction" 2 "Observed Sample Mean") size (small))
* You can see that it doesn't really fit well 



* NON-LINEAR MODEL - QUADRATIC (two age terms)

* Create a quadratic agemc term to allow for the non-linearity 
gen agemc2 = agemc^2

*now let's explore a non-linear, quadratic change
mixed intdiff agemc agemc2 || id: agemc, cov(uns)

* Locally store the model estimates as m2 
estimates store m2

* Get information criteria for model comparison 
estat ic

* Like above, you can then make predictions from this model to plot figures 
predict intdiff_quad

*plot that model
twoway (line intdiff_quad age_years, sort lcolor(dknavy)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(3(2)18, format(%5.0f)) ///
ylabel(0(1)4) ///
xtitle("Age (Years)") ///
ytitle("Internalising Difficulties (SDQ)") 

*compare the predictions with the observed
twoway ///
(line intdiff_quad age_years, sort lcolor(dknavy) lpattern(solid)) ///
(connected internalising mean_age if tag, sort lcolor(midblue) lpattern(dash) msymbol(circle) msize(small) mcolor(navy)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(3(2)18, format(%5.0f)) ///
ylabel(0(1)4) ///
xtitle("Age (Years)") ///
ytitle("Internalising Difficulties (SDQ)") ///
legend(order(1 "Quadratic Prediction" 2 "Observed Sample Mean") size (small))

* We can also statistically compare the linear and quadratic model using a likelihood ratio test 
lrtest m1 m2 // 
* p-value < 0.0001 indicating that the quadratic model explains the data better than the linear one 




* NON-LINEAR MODEL - CUBIC (three age terms)
* Create a cubic agemc term to allow for the non-linearity 
gen agemc3 = agemc^3

*now let's explore a non-linear, cubic change
mixed intdiff agemc agemc2 agemc3 || id: agemc, cov(uns)

* Locally store the model estimates 
estimates store m3

* Get information criteria for model comparison 
estat ic

* Like above, you can then make predictions from this model to plot figures 
predict intdiff_cubic

*plot that model
twoway (line intdiff_cubic age_years, sort lcolor(dknavy)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(3(2)18, format(%5.0f)) ///
ylabel(0(1)4) ///
xtitle("Age (Years)") ///
ytitle("Internalising Difficulties (SDQ)") 

*compare the predictions with the observed
twoway ///
(line intdiff_cubic age_years, sort lcolor(dknavy) lpattern(solid)) ///
(connected internalising mean_age if tag, sort lcolor(midblue) lpattern(dash) msymbol(circle) msize(small) mcolor(navy)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(3(2)18, format(%5.0f)) ///
ylabel(0(1)4) ///
xtitle("Age (Years)") ///
ytitle("Internalising Difficulties (SDQ)") ///
legend(order(1 "Cubic Prediction" 2 "Observed Sample Mean") size (small))

* We can also statistically compareusing a likelihood ratio test 
lrtest m2 m3 // There is evidence that cubic is still better - also visually examining it, it looks like cubic might be better too




* NON-LINEAR MODEL - QUARTIC(four age terms)
* Create a cubic agemc term to allow for the non-linearity 
gen agemc4 = agemc^4

*now let's explore a non-linear, cubic change
mixed intdiff agemc agemc2 agemc3 agemc4 || id: agemc, cov(uns)

* Locally store the model estimates 
estimates store m4

* Get information criteria for model comparison 
estat ic

* Like above, you can then make predictions from this model to plot figures 
predict intdiff_quart

*plot that model
twoway (line intdiff_quart age_years, sort lcolor(dknavy)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(3(2)18, format(%5.0f)) ///
ylabel(0(1)4) ///
xtitle("Age (Years)") ///
ytitle("Internalising Difficulties (SDQ)") 

*compare the predictions with the observed
twoway ///
(line intdiff_quart age_years, sort lcolor(dknavy) lpattern(solid)) ///
(connected internalising mean_age if tag, sort lcolor(midblue) lpattern(dash) msymbol(circle) msize(small) mcolor(navy)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(3(2)18, format(%5.0f)) ///
ylabel(0(1)4) ///
xtitle("Age (Years)") ///
ytitle("Internalising Difficulties (SDQ)") ///
legend(order(1 "Quartic Prediction" 2 "Observed Sample Mean") size (small))

* We can also statistically compare using a likelihood ratio test 
lrtest m3 m4 



********************************************************************************
*FITTING MODEL TO ASD AND NON ASD SUBGROUPS*
********************************************************************************
* Fit the best model to the subgroups and plot against observed sample mean to determine whether it fits the subgroups well 

***** CUBIC *********************************
* without interaction terms
mixed intdiff agemc agemc2 agemc3 asd || id: agemc, cov(uns)

predictnl cubicnonasd_nonadj = ///
_b[intdiff:_cons] ///
+ _b[intdiff: agemc]*agemc ///
+ _b[intdiff: agemc2]*agemc2 ///
+ _b[intdiff: agemc3]*agemc3 ///
+ _b[intdiff: asd]*0 ///
, se(cubicnonasd_nonadj_se)

predictnl cubicasd_nonadj = ///
_b[intdiff:_cons] ///
+ _b[intdiff: agemc]*agemc ///
+ _b[intdiff: agemc2]*agemc2 ///
+ _b[intdiff: agemc3]*agemc3 ///
+ _b[intdiff: asd]*1 ///
, se(cubicasd_nonadj_se)

* add the confidence intervals high and low 95% conf intervals 
gen cubicnonasd_nonadj_lo = cubicnonasd_nonadj - 1.96*cubicnonasd_nonadj_se
gen cubicnonasd_nonadj_hi = cubicnonasd_nonadj + 1.96*cubicnonasd_nonadj_se  

gen cubicasd_nonadj_lo = cubicasd_nonadj - 1.96*cubicasd_nonadj_se
gen cubicasd_nonadj_hi = cubicasd_nonadj + 1.96*cubicasd_nonadj_se  

* plot the trajectories for subgroups first
* install software for editing graphs on stata
ssc install grstyle, replace
ssc install palettes, replace

* initialise the software 
set scheme s2color 
grstyle init

* Specify the settings for the graph you're about to plot
grstyle set nogrid 

* Autistic group
twoway ///
(line cubicasd_nonadj age_years, sort lcolor(maroon)) ///
(connected intdiff_asd mean_age, sort lcolor(dkorange) lpattern(dash) msize(small)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(3(2)18, format(%5.0f)) ///
ylabel(0(1)8) ///
xtitle("Age (Years)") ///
ytitle("Internalising Difficulties (SDQ)") ///
legend(order(1 "Cubic Prediction" 2 "Observed Sample Mean") size (small))

* Non-autistic
twoway ///
(line cubicnonasd_nonadj age_years, sort lcolor(dknavy)) ///
(connected intdiff_no_asd mean_age, sort lcolor(midblue) lpattern(dash) msize(small) mcolor(dknavy)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(3(2)18, format(%5.0f)) ///
ylabel(0(1)8) ///
xtitle("Age (Years)") ///
ytitle("Internalising Difficulties (SDQ)") ///
legend(order(1 "Cubic Prediction" 2 "Observed Sample Mean") size (small))


* both groups with observed 
twoway (connected intdiff_no_asd mean_age, sort lcolor(midblue) lpattern(dash) msize(small) mcolor(dknavy)) ///
(connected intdiff_asd mean_age, sort lcolor(dkorange) lpattern(dash) msize(small)) ///
(line cubicnonasd_nonadj age_years if inrange(mean_age, 3, 18), sort lcolor(dknavy) lpattern(solid)) ///
(line cubicasd_nonadj age_years if inrange(mean_age, 3, 18), sort lcolor(maroon) lpattern(solid)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(3(2)18, format(%5.0f)) ///
ylabel(0(1)8) ///
xtitle("Age (Years)") ///
ytitle("Internalising Difficulties (SDQ)") ///
legend(order(1 "Observed Non-autistic" 2 "Observed Autistic" 3 "Predicted Non-autistic" 4 "Predicted Autistic") size (small))



* Repeat with interaction terms included 
mixed intdiff agemc agemc2 agemc3 asdxagemc asdxagemc2 asdxagemc3  asd || id: agemc, cov(uns)

predictnl cubicnonasd_nonadj2 = ///
_b[intdiff:_cons] ///
+ _b[intdiff: agemc]*agemc ///
+ _b[intdiff: agemc2]*agemc2 ///
+ _b[intdiff: agemc3]*agemc3 ///
+ _b[intdiff: asd]*0 ///
+ _b[intdiff: asdxagemc]*0*agemc ///
+ _b[intdiff: asdxagemc2]*0*agemc2 ///
+ _b[intdiff: asdxagemc3]*0*agemc3 ///
, se(cubicnonasd_nonadj_se2)

predictnl cubicasd_nonadj2 = ///
_b[intdiff:_cons] ///
+ _b[intdiff: agemc]*agemc ///
+ _b[intdiff: agemc2]*agemc2 ///
+ _b[intdiff: agemc3]*agemc3 ///
+ _b[intdiff: asd]*1 ///
+ _b[intdiff: asdxagemc]*1*agemc ///
+ _b[intdiff: asdxagemc2]*1*agemc2 ///
+ _b[intdiff: asdxagemc3]*1*agemc3 ///
, se(cubicasd_nonadj_se2)

* Autistic group
twoway ///
(line cubicasd_nonadj2 age_years, sort lcolor(maroon)) ///
(connected intdiff_asd mean_age, sort lcolor(dkorange) lpattern(dash) msize(small)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(3(2)18, format(%5.0f)) ///
ylabel(0(1)8) ///
xtitle("Age (Years)") ///
ytitle("Internalising Difficulties (SDQ)") ///
legend(order(1 "Cubic Prediction" 2 "Observed Sample Mean") size (small))

* Non-autistic
twoway ///
(line cubicnonasd_nonadj2 age_years, sort lcolor(dknavy)) ///
(connected intdiff_no_asd mean_age, sort lcolor(midblue) lpattern(dash) msize(small) mcolor(dknavy)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(3(2)18, format(%5.0f)) ///
ylabel(0(1)8) ///
xtitle("Age (Years)") ///
ytitle("Internalising Difficulties (SDQ)") ///
legend(order(1 "Cubic Prediction" 2 "Observed Sample Mean") size (small))



* both groups with observed 
twoway (connected intdiff_no_asd mean_age, sort lcolor(midblue) lpattern(dash) msize(small) mcolor(dknavy)) ///
(connected intdiff_asd mean_age, sort lcolor(dkorange) lpattern(dash) msize(small)) ///
(line cubicnonasd_nonadj2 age_years if inrange(mean_age, 3, 18), sort lcolor(dknavy) lpattern(solid)) ///
(line cubicasd_nonadj2 age_years if inrange(mean_age, 3, 18), sort lcolor(maroon) lpattern(solid)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(3(2)18, format(%5.0f)) ///
ylabel(0(1)8) ///
xtitle("Age (Years)") ///
ytitle("Internalising Difficulties (SDQ)") ///
legend(order(1 "Observed Non-autistic" 2 "Observed Autistic" 3 "Predicted Non-autistic" 4 "Predicted Autistic") size (small))



********************************************************************************
*ADDING COVARIATES*
********************************************************************************
* Run cubic model with asd and interaction terms, and add covariates 
* First, generate variables to model asd group interactions 
gen asdxagemc = asd*agemc
gen asdxagemc2 = asd*agemc2
gen asdxagemc3 = asd*agemc3

mixed intdiff agemc agemc2 agemc3 asd asdxagemc asdxagemc2 asdxagemc3 postndep cceianx2 i.epilepsy i.mat_class i.mat_edu i.sex fin_probs || id: agemc, cov(uns)


* Save the estimates for later analysis. "save" exports it to a file, whereas "store" from earlier stores it locally. When saving, you can revist the estimates without re-running the model 
estimates save "int_cubic_adj"
estimates store int_cubic_adj

* Get information criteria for model comparison 
estat ic


* You can also run linear and quadratic models with asd and covariates added - and compare them again to make sure cubic is still best after adjusting 
* linear 

mixed intdiff agemc asd asdxagemc postndep cceianx2 i.epilepsy i.mat_class i.mat_edu i.sex fin_probs || id: agemc, cov(uns)

estimates store int_lin_adj

estat ic


* quadratic
mixed intdiff agemc agemc2 asd asdxagemc asdxagemc2 postndep cceianx2 i.epilepsy i.mat_class i.mat_edu i.sex fin_probs || id: agemc, cov(uns)

estimates store int_quad_adj

estat ic


* quartic
gen asdxagemc4 = asd*agemc4

mixed intdiff agemc agemc2 agemc3 agemc4 asd asdxagemc asdxagemc2 asdxagemc3 asdxagemc4 postndep cceianx2 i.epilepsy i.mat_class i.mat_edu i.sex fin_probs || id: agemc, cov(uns)

estimates store int_quart_adj
estimates save "int_quart_adj"

estat ic

* LRT tests comparing models 
lrtest int_lin_adj int_quad_adj
lrtest int_quad_adj int_cubic_adj 
lrtest int_cubic_adj int_quart_adj 


********************************************************************************
*CREATING FINAL GROWTH CURVES FOR NON ASD AND ASD GROUPS*
********************************************************************************

mixed intdiff agemc agemc2 agemc3 asd asdxagemc asdxagemc2 asdxagemc3 postndep cceianx2 i.epilepsy i.mat_class i.mat_edu i.sex fin_probs || id: agemc, cov(uns)

*make the trajectories for each group
predictnl nonasd_adj3 = ///
_b[intdiff:_cons] ///
+ _b[intdiff: agemc]*agemc ///
+ _b[intdiff: agemc2]*agemc2 ///
+ _b[intdiff: agemc3]*agemc3 ///
+ _b[intdiff: asd]*0 ///
+ _b[intdiff: asdxagemc]*0*agemc ///
+ _b[intdiff: asdxagemc2]*0*agemc2 ///
+ _b[intdiff: asdxagemc3]*0*agemc3 ///
, se(nonasd_adj_se3)

predictnl asd_adj3 = ///
_b[intdiff:_cons] ///
+ _b[intdiff: agemc]*agemc ///
+ _b[intdiff: agemc2]*agemc2 ///
+ _b[intdiff: agemc3]*agemc3 ///
+ _b[intdiff: asd]*1 ///
+ _b[intdiff: asdxagemc]*1*agemc ///
+ _b[intdiff: asdxagemc2]*1*agemc2 ///
+ _b[intdiff: asdxagemc3]*1*agemc3 ///
, se(asd_adj_se3)


* add the confidence intervals high and low 95% conf intervals 
gen nonasd_adj_lo3 = nonasd_adj3 - 1.96*nonasd_adj_se3
gen nonasd_adj_hi3 = nonasd_adj3 + 1.96*nonasd_adj_se3 

gen asd_adj_lo3 = asd_adj3 - 1.96*asd_adj_se3
gen asd_adj_hi3 = asd_adj3 + 1.96*asd_adj_se3  

* plot the final trajectories
* install software for editing graphs on stata
ssc install grstyle, replace
ssc install palettes, replace

* initialise the software 
set scheme s2color 
grstyle init

* Specify the settings for the graph you're about to plot
grstyle set nogrid  
* to clear settings: grstyle clear

twoway ///
(rarea nonasd_adj_lo3 nonasd_adj_hi age_years, sort color(dimgray)) ///
(rarea asd_adj_lo3 asd_adj_hi age_years, sort color(dimgray)) ///
(line nonasd_adj3 age_years, sort lcolor(ebblue) lpattern(solid)) ///
(line asd_adj3 age_years, sort lcolor(orange) lpattern(solid)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(3(2)18, format(%5.0f)) ///
ylabel(0(1)8) ///
xtitle("Age (Years)") ///
ytitle("Internalising Difficulties (SDQ)") ///
legend(order(3 "Non-autistic" 4 "Autistic") size (small) row(1) position (bottom))


*Plot those differences with the observed values 
sum mean_age, detail

twoway (connected intdiff_no_asd mean_age, sort lcolor(midblue) lpattern(dash) mcolor(dknavy) msize(small)) ///
(connected intdiff_asd mean_age, sort  lcolor(dkorange) lpattern(dash) msymbol(circle) msize(small)) ///
(line nonasd_adj3 age_years if inrange(mean_age, 3, 18), sort lcolor(dknavy) lpattern(solid)) ///
(line asd_adj3 age_years if inrange(mean_age, 3, 18), sort lcolor(maroon) lpattern(solid)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(3(2)18, format(%5.0f)) ///
ylabel(0(1)8) ///
xtitle("Age (Years)") ///
ytitle("Internalising Difficulties (SDQ)") ///
legend(order(1 "Observed Non-autistic" 2 "Observed Autistic" 3 "Predicted Non-autistic" 4 "Predicted Autistic") size (small))



********************************************************************************
*PREDICTING SLEEP FOR ASD AND NON-ASD GROUPS AT SPECIFIC TIMEPOINTS*
********************************************************************************
* We can also compare what sleep looks like at different time points
* and how these vary by the autistic and non-autistic groups 

* For example if we want to compare internalising difficulties between the two groups

* AT AGE 10 YEARS 
***************************** 
* at age 10 years
* First we get the mean of age 
sum age_years
*answer is =   9.518781

* Then we subtract the mean age from the age at which you want to predict 
di 10 - 9.518781
*answer is 0.481219

* Call the saved estimates from the mixed model 
estimate use "int_cubic_adj" // from the adjusted model 

* Then use nlcom to compare the two groups
nlcom ///
(nonasd_age10y: _b[intdiff:_cons] ///
+ _b[intdiff:agemc]*0.481219 ///
+ _b[intdiff:agemc2]*0.481219^2 ///
+ _b[intdiff:agemc3]*0.481219^3 ///
) ///
(asd_age10y: _b[intdiff:_cons] ///
+ _b[intdiff:asd]*1 ///
+ _b[intdiff:agemc]*0.481219 ///
+ _b[intdiff:agemc2]*0.481219^2 ///
+ _b[intdiff:agemc3]*0.481219^3 ///
+ _b[intdiff:asdxagemc]*1*0.481219 ///
+ _b[intdiff:asdxagemc2]*1*0.481219^2 ///
+ _b[intdiff:asdxagemc3]*1*0.481219^3 ///
), post // creates the scores for each group 

nlcom (difference: _b[asd_age10y] - _b[nonasd_age10y]), cformat(%9.2f) post // calculates the difference between them
est tab, p(%12.10g) // gets you exact p values for multiple comparisons testing etc 



* You can plot the model and add reference lines to double check that the predictions are correct 
twoway ///
(line nonasd_adj3 age_years, sort lcolor(dknavy) lpattern(solid)) ///
(line asd_adj3 age_years, sort lcolor(dkorange) lpattern(dash)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(3(2)18, format(%5.0f)) ///
ylabel(0(1)8) ///
xtitle("Age (Years)") ///
ytitle("Internalising Difficulties (SDQ)") ///
legend(order(3 "Non-autistic" 4 "Autistic") size (small) row(1) position (bottom))



* AT AGE 4YEARS 
***************************** 
di 4 - 9.518781
*answer is -5.518781

estimate use "int_cubic_adj" 

nlcom ///
(nonasd_age4y: _b[intdiff:_cons] ///
+ _b[intdiff:agemc]*(-5.518781) ///
+ _b[intdiff:agemc2]*(-5.518781)^2 ///
+ _b[intdiff:agemc3]*(-5.518781)^3 ///
) ///
(asd_age4y: _b[intdiff:_cons] ///
+ _b[intdiff:asd]*1 ///
+ _b[intdiff:agemc]*(-5.518781) ///
+ _b[intdiff:agemc2]*(-5.518781)^2 ///
+ _b[intdiff:agemc3]*(-5.518781)^3 ///
+ _b[intdiff:asdxagemc]*1*(-5.518781) ///
+ _b[intdiff:asdxagemc2]*1*(-5.518781)^2 ///
+ _b[intdiff:asdxagemc3]*1*(-5.518781)^3 ///
), post // creates the scores for each group 

nlcom (difference: _b[asd_age4y] - _b[nonasd_age4y]), cformat(%9.2f) post
est tab, p(%12.10g)



* AT AGE 5 YEARS 
***************************** 
di 5 - 9.518781
*answer is -4.518781


estimate use "int_cubic_adj" 

nlcom ///
(nonasd_age5y: _b[intdiff:_cons] ///
+ _b[intdiff:agemc]*(-4.518781) ///
+ _b[intdiff:agemc2]*(-4.518781)^2 ///
+ _b[intdiff:agemc3]*(-4.518781)^3 ///
) ///
(asd_age5y: _b[intdiff:_cons] ///
+ _b[intdiff:asd]*1 ///
+ _b[intdiff:agemc]*(-4.518781) ///
+ _b[intdiff:agemc2]*(-4.518781)^2 ///
+ _b[intdiff:agemc3]*(-4.518781)^3 ///
+ _b[intdiff:asdxagemc]*1*(-4.518781) ///
+ _b[intdiff:asdxagemc2]*1*(-4.518781)^2 ///
+ _b[intdiff:asdxagemc3]*1*(-4.518781)^3 ///
), post 

nlcom (difference: _b[asd_age5y] - _b[nonasd_age5y]), cformat(%9.2f) post
est tab, p(%12.10g)



* AT AGE 6 YEARS 
***************************** 
di 6 - 9.518781
*answer is -3.518781


estimate use "int_cubic_adj" 

nlcom ///
(nonasd_age6y: _b[intdiff:_cons] ///
+ _b[intdiff:agemc]*(-3.518781) ///
+ _b[intdiff:agemc2]*(-3.518781)^2 ///
+ _b[intdiff:agemc3]*(-3.518781)^3 ///
) ///
(asd_age6y: _b[intdiff:_cons] ///
+ _b[intdiff:asd]*1 ///
+ _b[intdiff:agemc]*(-3.518781) ///
+ _b[intdiff:agemc2]*(-3.518781)^2 ///
+ _b[intdiff:agemc3]*(-3.518781)^3 ///
+ _b[intdiff:asdxagemc]*1*(-3.518781) ///
+ _b[intdiff:asdxagemc2]*1*(-3.518781)^2 ///
+ _b[intdiff:asdxagemc3]*1*(-3.518781)^3 ///
), post 

nlcom (difference: _b[asd_age6y] - _b[nonasd_age6y]), cformat(%9.2f) post
est tab, p(%12.10g)



* AT AGE 7 YEARS 
***************************** 
di 7 - 9.518781
*answer is -2.518781

estimate use "int_cubic_adj" 

nlcom ///
(nonasd_age7y: _b[intdiff:_cons] ///
+ _b[intdiff:agemc]*(-2.518781) ///
+ _b[intdiff:agemc2]*(-2.518781)^2 ///
+ _b[intdiff:agemc3]*(-2.518781)^3 ///
) ///
(asd_age7y: _b[intdiff:_cons] ///
+ _b[intdiff:asd]*1 ///
+ _b[intdiff:agemc]*(-2.518781) ///
+ _b[intdiff:agemc2]*(-2.518781)^2 ///
+ _b[intdiff:agemc3]*(-2.518781)^3 ///
+ _b[intdiff:asdxagemc]*1*(-2.518781) ///
+ _b[intdiff:asdxagemc2]*1*(-2.518781)^2 ///
+ _b[intdiff:asdxagemc3]*1*(-2.518781)^3 ///
), post 

nlcom (difference: _b[asd_age7y] - _b[nonasd_age7y]), cformat(%9.2f) post
est tab, p(%12.10g)




* AT AGE 8 YEARS 
***************************** 
di 8 - 9.518781
*answer is -1.518781

estimate use "int_cubic_adj" 

nlcom ///
(nonasd_age8y: _b[intdiff:_cons] ///
+ _b[intdiff:agemc]*(-1.518781) ///
+ _b[intdiff:agemc2]*(-1.518781)^2 ///
+ _b[intdiff:agemc3]*(-1.518781)^3 ///
) ///
(asd_age8y: _b[intdiff:_cons] ///
+ _b[intdiff:asd]*1 ///
+ _b[intdiff:agemc]*(-1.518781) ///
+ _b[intdiff:agemc2]*(-1.518781)^2 ///
+ _b[intdiff:agemc3]*(-1.518781)^3 ///
+ _b[intdiff:asdxagemc]*1*(-1.518781) ///
+ _b[intdiff:asdxagemc2]*1*(-1.518781)^2 ///
+ _b[intdiff:asdxagemc3]*1*(-1.518781)^3 ///
), post 

nlcom (difference: _b[asd_age8y] - _b[nonasd_age8y]), cformat(%9.2f) post
est tab, p(%12.10g)



* AT AGE 9 YEARS 
***************************** 
di 9 - 9.518781
*answer is -0.518781

estimate use "int_cubic_adj" 

nlcom ///
(nonasd_age9y: _b[intdiff:_cons] ///
+ _b[intdiff:agemc]*(-0.518781) ///
+ _b[intdiff:agemc2]*(-0.518781)^2 ///
+ _b[intdiff:agemc3]*(-0.518781)^3 ///
) ///
(asd_age9y: _b[intdiff:_cons] ///
+ _b[intdiff:asd]*1 ///
+ _b[intdiff:agemc]*(-0.518781) ///
+ _b[intdiff:agemc2]*(-0.518781)^2 ///
+ _b[intdiff:agemc3]*(-0.518781)^3 ///
+ _b[intdiff:asdxagemc]*1*(-0.518781) ///
+ _b[intdiff:asdxagemc2]*1*(-0.518781)^2 ///
+ _b[intdiff:asdxagemc3]*1*(-0.518781)^3 ///
), post 

nlcom (difference: _b[asd_age9y] - _b[nonasd_age9y]), cformat(%9.2f) post
est tab, p(%12.10g)




* AT AGE 11 YEARS 
***************************** 
di 11 - 9.518781

*answer is 1.481219

estimate use "int_cubic_adj" 

nlcom ///
(nonasd_age11y: _b[intdiff:_cons] ///
+ _b[intdiff:agemc]*1.481219 ///
+ _b[intdiff:agemc2]*1.481219^2 ///
+ _b[intdiff:agemc3]*1.481219^3 ///
) ///
(asd_age11y: _b[intdiff:_cons] ///
+ _b[intdiff:asd]*1 ///
+ _b[intdiff:agemc]*1.481219 ///
+ _b[intdiff:agemc2]*1.481219^2 ///
+ _b[intdiff:agemc3]*1.481219^3 ///
+ _b[intdiff:asdxagemc]*1*1.481219 ///
+ _b[intdiff:asdxagemc2]*1*1.481219^2 ///
+ _b[intdiff:asdxagemc3]*1*1.481219^3 ///
), post 

nlcom (difference: _b[asd_age11y] - _b[nonasd_age11y]), cformat(%9.2f) post
est tab, p(%12.10g)




* AT AGE 12 YEARS 
***************************** 
di 12 - 9.518781

*answer is 2.481219

estimate use "int_cubic_adj" 

nlcom ///
(nonasd_age12y: _b[intdiff:_cons] ///
+ _b[intdiff:agemc]*2.481219 ///
+ _b[intdiff:agemc2]*2.481219^2 ///
+ _b[intdiff:agemc3]*2.481219^3 ///
) ///
(asd_age12y: _b[intdiff:_cons] ///
+ _b[intdiff:asd]*1 ///
+ _b[intdiff:agemc]*2.481219 ///
+ _b[intdiff:agemc2]*2.481219^2 ///
+ _b[intdiff:agemc3]*2.481219^3 ///
+ _b[intdiff:asdxagemc]*1*2.481219 ///
+ _b[intdiff:asdxagemc2]*1*2.481219^2 ///
+ _b[intdiff:asdxagemc3]*1*2.481219^3 ///
), post 

nlcom (difference: _b[asd_age12y] - _b[nonasd_age12y]), cformat(%9.2f) post
est tab, p(%12.10g)



* AT AGE 13 YEARS 
***************************** 
di 13 - 9.518781

*answer is 3.481219

estimate use "int_cubic_adj" 

nlcom ///
(nonasd_age13y: _b[intdiff:_cons] ///
+ _b[intdiff:agemc]*3.481219 ///
+ _b[intdiff:agemc2]*3.481219^2 ///
+ _b[intdiff:agemc3]*3.481219^3 ///
) ///
(asd_age13y: _b[intdiff:_cons] ///
+ _b[intdiff:asd]*1 ///
+ _b[intdiff:agemc]*3.481219 ///
+ _b[intdiff:agemc2]*3.481219^2 ///
+ _b[intdiff:agemc3]*3.481219^3 ///
+ _b[intdiff:asdxagemc]*1*3.481219 ///
+ _b[intdiff:asdxagemc2]*1*3.481219^2 ///
+ _b[intdiff:asdxagemc3]*1*3.481219^3 ///
), post 

nlcom (difference: _b[asd_age13y] - _b[nonasd_age13y]), cformat(%9.2f) post
est tab, p(%12.10g)




* AT AGE 14 YEARS 
***************************** 
di 14 - 9.518781

*answer is 4.481219

estimate use "int_cubic_adj" 

nlcom ///
(nonasd_age14y: _b[intdiff:_cons] ///
+ _b[intdiff:agemc]*4.481219 ///
+ _b[intdiff:agemc2]*4.481219^2 ///
+ _b[intdiff:agemc3]*4.481219^3 ///
) ///
(asd_age14y: _b[intdiff:_cons] ///
+ _b[intdiff:asd]*1 ///
+ _b[intdiff:agemc]*4.481219 ///
+ _b[intdiff:agemc2]*4.481219^2 ///
+ _b[intdiff:agemc3]*4.481219^3 ///
+ _b[intdiff:asdxagemc]*1*4.481219 ///
+ _b[intdiff:asdxagemc2]*1*4.481219^2 ///
+ _b[intdiff:asdxagemc3]*1*4.481219^3 ///
), post 

nlcom (difference: _b[asd_age14y] - _b[nonasd_age14y]), cformat(%9.2f) post
est tab, p(%12.10g)





* AT AGE 15 YEARS 
***************************** 
di 15 - 9.518781

*answer is 5.481219

estimate use "int_cubic_adj" 

nlcom ///
(nonasd_age15y: _b[intdiff:_cons] ///
+ _b[intdiff:agemc]*5.481219 ///
+ _b[intdiff:agemc2]*5.481219^2 ///
+ _b[intdiff:agemc3]*5.481219^3 ///
) ///
(asd_age15y: _b[intdiff:_cons] ///
+ _b[intdiff:asd]*1 ///
+ _b[intdiff:agemc]*5.481219 ///
+ _b[intdiff:agemc2]*5.481219^2 ///
+ _b[intdiff:agemc3]*5.481219^3 ///
+ _b[intdiff:asdxagemc]*1*5.481219 ///
+ _b[intdiff:asdxagemc2]*1*5.481219^2 ///
+ _b[intdiff:asdxagemc3]*1*5.481219^3 ///
), post 

nlcom (difference: _b[asd_age15y] - _b[nonasd_age15y]), cformat(%9.2f) post
est tab, p(%12.10g)






* AT AGE 16 YEARS 
***************************** 
di 16 - 9.518781

*answer is 6.481219

estimate use "int_cubic_adj" 

nlcom ///
(nonasd_age16y: _b[intdiff:_cons] ///
+ _b[intdiff:agemc]*6.481219 ///
+ _b[intdiff:agemc2]*6.481219^2 ///
+ _b[intdiff:agemc3]*6.481219^3 ///
) ///
(asd_age16y: _b[intdiff:_cons] ///
+ _b[intdiff:asd]*1 ///
+ _b[intdiff:agemc]*6.481219 ///
+ _b[intdiff:agemc2]*6.481219^2 ///
+ _b[intdiff:agemc3]*6.481219^3 ///
+ _b[intdiff:asdxagemc]*1*6.481219 ///
+ _b[intdiff:asdxagemc2]*1*6.481219^2 ///
+ _b[intdiff:asdxagemc3]*1*6.481219^3 ///
), post 

nlcom (difference: _b[asd_age16y] - _b[nonasd_age16y]), cformat(%9.2f) post
est tab, p(%12.10g)





* AT AGE 17 YEARS 
***************************** 
di 17 - 9.518781

*answer is 7.481219

estimate use "int_cubic_adj" 

nlcom ///
(nonasd_age17y: _b[intdiff:_cons] ///
+ _b[intdiff:agemc]*7.481219 ///
+ _b[intdiff:agemc2]*7.481219^2 ///
+ _b[intdiff:agemc3]*7.481219^3 ///
) ///
(asd_age17y: _b[intdiff:_cons] ///
+ _b[intdiff:asd]*1 ///
+ _b[intdiff:agemc]*7.481219 ///
+ _b[intdiff:agemc2]*7.481219^2 ///
+ _b[intdiff:agemc3]*7.481219^3 ///
+ _b[intdiff:asdxagemc]*1*7.481219 ///
+ _b[intdiff:asdxagemc2]*1*7.481219^2 ///
+ _b[intdiff:asdxagemc3]*1*7.481219^3 ///
), post 

nlcom (difference: _b[asd_age17y] - _b[nonasd_age17y]), cformat(%9.2f) post
est tab, p(%12.10g)





* AT AGE 18 YEARS 
***************************** 
di 18 - 9.518781

*answer is 8.481219

estimate use "int_cubic_adj" 

nlcom ///
(nonasd_age18y: _b[intdiff:_cons] ///
+ _b[intdiff:agemc]*8.481219 ///
+ _b[intdiff:agemc2]*8.481219^2 ///
+ _b[intdiff:agemc3]*8.481219^3 ///
) ///
(asd_age18y: _b[intdiff:_cons] ///
+ _b[intdiff:asd]*1 ///
+ _b[intdiff:agemc]*8.481219 ///
+ _b[intdiff:agemc2]*8.481219^2 ///
+ _b[intdiff:agemc3]*8.481219^3 ///
+ _b[intdiff:asdxagemc]*1*8.481219 ///
+ _b[intdiff:asdxagemc2]*1*8.481219^2 ///
+ _b[intdiff:asdxagemc3]*1*8.481219^3 ///
), post 

nlcom (difference: _b[asd_age18y] - _b[nonasd_age18y]), cformat(%9.2f) post
est tab, p(%12.10g)



















