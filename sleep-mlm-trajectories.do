********************************************************************************
*MULTILEVEL MODELLING OF SLEEP DURATION TRAJECTORIES IN ALSPAC
********************************************************************************
* This is the syntax for multilevel growth curve modelling trajectories of night-time sleep duration trajectories in autistic and non-autistic participants of ALSPAC 

* This syntax assumes that you have already cleaned your data. The data should be in long format, with columns for subject ID, occasion, age, and night-time sleep duration, and covariates 


********************************************************************************
*DESCRIPTIVE INFORMATION & GROWTH CURVE PREP*
********************************************************************************
* Declare the data to be panel for use in later analysis 
xtset id occ

* Describe the missing sleep data patterns 
xtdes if sleep~=. 

* To get the sleep and age stats by occasion 
bysort occ: sum sleep age_years

* Check whether the data is linear to guide how to model growth curves
* Assumption check 1: Is the data linear? 
lowess sleep age_years, nograph gen(yhatlowess)
line yhatlowess age_years, sort 
* The graph shows a concave curvilinear relationship between age and sleep 
* We would likely need to go with a quadratic model

* You can also create a graph showing mean sleep over time by age to guide how to model trajectories 
*first create a new variable which is the mean age of all assessments
egen mean_age = mean(age_years), by(occ)
egen tag = tag(occ)
egen time_asleep = mean(sleep), by(occ)

twoway (connected time_asleep mean_age if tag, sort msymbol(circle) msize(small) lcolor(dknavy)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(0(2)18, format(%5.0f)) ///
ylabel(8(1)12) ///
xtitle("Age (Years)") ///
ytitle("Night-time Sleep Duration (Hours)") 


* We can also plot group-specific changes 
egen sleep_no_asd = mean(sleep) if asd==0, by(occ)
egen sleep_asd = mean(sleep) if asd==1, by(occ)

twoway (connected sleep_no_asd mean_age, sort msymbol(circle) msize(small) lcolor(dknavy)) ///
(connected sleep_asd mean_age, sort msymbol(circle) msize(small) lcolor(dkorange)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(0(2)16) ///
ylabel(8(1)12) ///
xtitle("Age (Years)") ///
ytitle("Night-time Sleep Duration (Hours)") ///
legend(order(1 "Non-autistic" 2 "Autistic"))


********************************************************************************
*EXPLORING DIFFERENT SHAPES OF GROWTH CURVES*
********************************************************************************
* LINEAR MODEL 
mixed sleep age_years || id: age_years, cov(uns)  

* You can locally store the model estimates as m1 (for comparing against later models)
estimates store m1

* Information criteria for comparing models 
estat ic

* And you can get predictions from that model
predict sleep_lin

* You can then plot the trajectory from the linear model
twoway (line sleep_lin age_years, sort lcolor(dknavy)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(0(2)18, format(%5.0f)) ///
ylabel(8(1)12) ///
xtitle("Age (Years)") ///
ytitle("Night-time Sleep Duration (Hours)") 

* You can visually examine whether your model fits the data by plotting the prediction with the mean plotted data from earlier 
twoway ///
(line sleep_lin age_years, sort lcolor(dknavy) lpattern(solid)) ///
(connected time_asleep mean_age if tag, sort lcolor(midblue) lpattern(dash) msize(small) mcolor(navy)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(0(2)18, format(%5.0f)) ///
ylabel(8(1)12) ///
xtitle("Age (Years)") ///
ytitle("Night-time Sleep Duration (Hours)") ///
legend(order(1 "Linear Prediction" 2 "Observed Sample Mean") size (small))
* You can see that it doesn't really fit well 


* NON-LINEAR MODEL - QUADRATIC (two age terms)

* Create a quadratic agemc term to allow for the non-linearity 
gen age_years2 = age_years^2

*now let's explore a non-linear, quadratic change
mixed sleep age_years age_years2 || id: age_years, cov(uns)

* Locally store the model estimates as m2 
estimates store m2

* Information criteria for comparing models 
estat ic

* Like above, you can then make predictions from this model to plot figures 
predict sleep_quad

*plot that model
twoway (line sleep_quad age_years, sort lcolor(dknavy)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(0(2)18, format(%5.0f)) ///
ylabel(8(1)12) ///
xtitle("Age (Years)") ///
ytitle("Night-time Sleep Duration (Hours)")

*compare the predictions with the observed
twoway ///
(line sleep_quad age_years, sort lcolor(dknavy) lpattern(solid)) ///
(connected time_asleep mean_age if tag, sort lcolor(midblue) lpattern(dash) msymbol(circle) msize(small) mcolor(navy)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(0(2)18, format(%5.0f)) ///
ylabel(8(1)12) ///
xtitle("Age (Years)") ///
ytitle("Night-time Sleep Duration (Hours)") ///
legend(order(1 "Quadratic Prediction" 2 "Observed Sample Mean") size (small))

* We can also statistically compare the linear and quadratic model using a likelihood ratio test 
lrtest m1 m2 // 
* p-value < 0.0001 indicating that the quadratic model explains the data better than the linear one 



* NON-LINEAR MODEL - CUBIC (three age terms)

* Create a cubic age term to allow for the non-linearity 
gen age_years3 = age_years^3

*now let's explore a non-linear, cubic change
mixed sleep age_years age_years2 age_years3 || id: age_years, cov(uns)

* Locally store the model estimates as m3 
estimates store m3

* Information criteria for comparing models 
estat ic

* Like above, you can then make predictions from this model to plot figures 
predict sleep_cubic

*plot that model
twoway (line sleep_cubic age_years, sort lcolor(dknavy)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(0(2)18, format(%5.0f)) ///
ylabel(8(1)12) ///
xtitle("Age (Years)") ///
ytitle("Night-time Sleep Duration (Hours)")

*compare the predictions with the observed
twoway ///
(line sleep_cubic age_years, sort lcolor(dknavy) lpattern(solid)) ///
(connected time_asleep mean_age if tag, sort lcolor(midblue) lpattern(dash) mcolor(navy) msize(small)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(0(2)18, format(%5.0f)) ///
ylabel(8(1)12) ///
xtitle("Age (Years)") ///
ytitle("Night-time Sleep Duration (Hours)") ///
legend(order(1 "Cubic Prediction" 2 "Observed Sample Mean") size (small))

* We can also statistically compare the quadratic and cubic model using a likelihood ratio test 
lrtest m2 m3 // 




* NON-LINEAR MODEL - QUARTIC (four age terms)

* Create a quartic agemc term to allow for the non-linearity 
gen age_years4 = age_years^4

*now let's explore a non-linear, quartic change
mixed sleep age_years age_years2 age_years3 age_years4 || id: age_years, cov(uns)

* Locally store the model estimates as m4
estimates store m4

* Information criteria for comparing models 
estat ic

* Like above, you can then make predictions from this model to plot figures 
predict sleep_quart

*plot that model
twoway (line sleep_quart age_years, sort lcolor(dknavy)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(0(2)18, format(%5.0f)) ///
ylabel(8(1)12) ///
xtitle("Age (Years)") ///
ytitle("Night-time Sleep Duration (Hours)")


*compare the predictions with the observed
twoway ///
(line sleep_quart age_years, sort lcolor(dknavy) lpattern(solid)) ///
(connected time_asleep mean_age if tag, sort lcolor(midblue) lpattern(dash) mcolor(navy) msize(small)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(0(2)18, format(%5.0f)) ///
ylabel(8(1)12) ///
xtitle("Age (Years)") ///
ytitle("Night-time Sleep Duration (Hours)") ///
legend(order(1 "Quartic Prediction" 2 "Observed Sample Mean") size (small))

* We can also statistically compare the cubic and quartic model using a likelihood ratio test 
lrtest m3 m4 // 
* Test is not significant, suggesting that cubic is the best option 

* From above, we can conclude that the cubic model fits our data best. Now we can proceed to add covariates and finalise the model 



********************************************************************************
*FITTING MODEL TO ASD AND NON ASD SUBGROUPS*
********************************************************************************
* Fit the best model to the subgroups (without interaction terms at first) and plot against observed sample mean to determine whether it fits the subgroups well 

***** CUBIC *********************************
* without interaction terms
mixed sleep age_years age_years2 age_years3 asd || id: age_years, cov(uns)

predict fitted_full if asd == 1
predict fitted_full_noasd if asd == 0 

twoway ///
(line fitted_full age_years if asd == 1, sort lcolor(maroon)) ///
(connected sleep_asd mean_age, sort lcolor(dkorange) lpattern(dash) msize(small)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(0(2)18, format(%5.0f)) ///
ylabel(8(1)12) ///
xtitle("Age (Years)") ///
ytitle("Night-time Sleep Duration (Hours)") ///
legend(order(1 "Cubic Prediction" 2 "Observed Sample Mean") size (small))


twoway ///
(line fitted_full_noasd age_years if asd == 0, sort lcolor(dknavy)) ///
(connected sleep_no_asd mean_age, sort lcolor(midblue) lpattern(dash) msize(small) mcolor(dknavy)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(0(2)18, format(%5.0f)) ///
ylabel(8(1)12) ///
xtitle("Age (Years)") ///
ytitle("Night-time Sleep Duration (Hours)") ///
legend(order(1 "Cubic Prediction" 2 "Observed Sample Mean") size (small))



* Now add interaction terms

mixed sleep age_years age_years2 age_years3 asd asdxage_years asdxage_years2 asdxage_years3 || id: age_years, cov(uns)

predict fitted_full2 if asd == 1
predict fitted_full_noasd2 if asd == 0 

twoway ///
(line fitted_full2 age_years if asd == 1, sort lcolor(maroon)) ///
(connected sleep_asd mean_age, sort lcolor(dkorange) lpattern(dash) msize(small)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(0(2)18, format(%5.0f)) ///
ylabel(8(1)12) ///
xtitle("Age (Years)") ///
ytitle("Night-time Sleep Duration (Hours)") ///
legend(order(1 "Cubic Prediction" 2 "Observed Sample Mean") size (small))


twoway ///
(line fitted_full_noasd2 age_years if asd == 0, sort lcolor(dknavy)) ///
(connected sleep_no_asd mean_age, sort lcolor(midblue) lpattern(dash) msize(small) mcolor(dknavy)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(0(2)18, format(%5.0f)) ///
ylabel(8(1)12) ///
xtitle("Age (Years)") ///
ytitle("Night-time Sleep Duration (Hours)") ///
legend(order(1 "Cubic Prediction" 2 "Observed Sample Mean") size (small))



* Plot best-fitting model for both groups 
mixed sleep age_years age_years2 age_years3 asd asdxage_years asdxage_years2 asdxage_years3 || id: age_years, cov(uns)

predictnl nonasd_nonadj = ///
_b[sleep:_cons] ///
+ _b[sleep: age_years]*age_years ///
+ _b[sleep: age_years2]*age_years2 ///
+ _b[sleep: age_years3]*age_years3 ///
+ _b[sleep: asd]*0 ///
+ _b[sleep: asdxage_years]*0*age_years ///
+ _b[sleep: asdxage_years2]*0*age_years2 ///
+ _b[sleep: asdxage_years2]*0*age_years3 ///
, se(nonasd_nonadj_se)

predictnl asd_nonadj = ///
_b[sleep:_cons] ///
+ _b[sleep: age_years]*age_years ///
+ _b[sleep: age_years2]*age_years2 ///
+ _b[sleep: age_years3]*age_years3 ///
+ _b[sleep: asd]*1 ///
+ _b[sleep: asdxage_years]*1*age_years ///
+ _b[sleep: asdxage_years2]*1*age_years2 ///
+ _b[sleep: asdxage_years3]*1*age_years3 ///
, se(asd_nonadj_se)

* add the confidence intervals high and low 95% conf intervals 
gen nonasd_nonadj_lo = nonasd_nonadj - 1.96*nonasd_nonadj_se
gen nonasd_nonadj_hi = nonasd_nonadj + 1.96*nonasd_nonadj_se  

gen asd_nonadj_lo = asd_nonadj - 1.96*asd_nonadj_se
gen asd_nonadj_hi = asd_nonadj + 1.96*asd_nonadj_se  

* plot the final trajectories
* install software for editing graphs on stata
ssc install grstyle, replace
ssc install palettes, replace

* initialise the software 
set scheme s2color 
grstyle init

* Specify the settings for the graph you're about to plot
grstyle set nogrid 

*to clear settings: grstyle clear

twoway ///
(rarea nonasd_nonadj_lo nonasd_nonadj_hi age_years, sort color(gs12)) ///
(rarea asd_nonadj_lo asd_nonadj_hi age_years, sort color(gs12)) ///
(line nonasd_nonadj age_years, sort lcolor(dknavy) lpattern(solid)) ///
(line asd_nonadj age_years, sort lcolor(dkorange) lpattern(dash)) ///
, ///
graphregion(fcolor(white)) ///
xtitle("Age (Years)") ///
ytitle("Night-time Sleep Duration (Hours)") ///
xlabel(0(2)18, format(%5.0f)) ///
ylabel(8(1)12) ///
graphregion(fcolor(white)) ///
legend(order(3 "Non-autistic" 4 "Autistic") size (small) row(1) position (bottom))


*Plot those differences with the observed values 
sum mean_age, detail

twoway (connected sleep_no_asd mean_age, sort lcolor(midblue) lpattern(dash) mcolor(dknavy) msize(small)) ///
(connected sleep_asd mean_age, sort lcolor(dkorange) lpattern(dash) mcolor(maroon) msize(small)) ///
(line nonasd_nonadj age_years if inrange(mean_age, 0.12, 15.48), sort lcolor(dknavy) lpattern(solid)) ///
(line asd_nonadj age_years if inrange(mean_age, 0.12, 15.48), sort lcolor(maroon) lpattern(solid)) ///
 , ///
graphregion(fcolor(white)) ///
xlabel(0(2)18, format(%5.0f)) ///
ylabel(8(1)12) ///
xtitle("Age (Years)") ///
ytitle("Night-time Sleep Duration (Hours)") ///
legend(order(1 "Observed Non-autistic" 2 "Observed Autistic" 3 "Predicted Non-autistic" 4 "Predicted Autistic") size (small))




********************************************************************************
*ADDING COVARIATES*
********************************************************************************
* Run cubic model with asd and covariates added 
* First, generate variables to model asd group interactions 
gen asdxage_years = asd*age_years
gen asdxage_years2 = asd*age_years2
gen asdxage_years3 = asd*age_years3

mixed sleep age_years age_years2 age_years3 asd asdxage_years asdxage_years2 asdxage_years3 postndep cceianx2 i.epilepsy i.mat_class i.mat_edu i.sex fin_probs || id: age_years, cov(uns)

* save the estimates for later analysis. "save" exports it to a file, whereas "store" from earlier stores it locally. When saving, you can revist the estimates without re-running the model 
estimates save "sleep_cubic_adj"
estimates store sleep_cubic_adj

* Get information criteria for model comparison 
estat ic

* You can also run linear and quadratic models with asd and covariates added - and compare them again to the cubic model to make sure cubic is still best after adjusting 
* linear 
mixed sleep age_years asd asdxage_years postndep cceianx2 i.epilepsy i.mat_class i.mat_edu i.sex fin_probs || id: age_years, cov(uns)

estimates store sleep_lin_adj

* Get information criteria for model comparison 
estat ic

* quadratic
mixed sleep age_years age_years2 asd asdxage_years asdxage_years2 postndep cceianx2 i.epilepsy i.mat_class i.mat_edu i.sex fin_probs || id: age_years, cov(uns)

estimates store sleep_quad_adj

* Get information criteria for model comparison 
estat ic


lrtest sleep_lin_adj sleep_quad_adj 
lrtest sleep_quad_adj sleep_cubic_adj 


* quartic
gen asdxage_years4 = asd*age_years4

mixed sleep age_years age_years2 age_years3 age_years4 asd asdxage_years asdxage_years2 asdxage_years3 asdxage_years4 postndep cceianx2 i.epilepsy i.mat_class i.mat_edu i.sex fin_probs || id: age_years, cov(uns)

estimates store sleep_quart_adj

* Get information criteria for model comparison 
estat ic

lrtest sleep_cubic_adj sleep_quart_adj

* Stick with adjusted cubic model 



********************************************************************************
*CREATING GROWTH CURVES FOR NON ASD AND ASD GROUPS*
********************************************************************************
mixed sleep age_years age_years2 age_years3 asd asdxage_years asdxage_years2 asdxage_years3 postndep cceianx2 i.epilepsy i.mat_class i.mat_edu i.sex fin_probs || id: age_years, cov(uns)

*make the trajectories for each group
predictnl nonasd_adj3 = ///
_b[sleep:_cons] ///
+ _b[sleep: age_years]*age_years ///
+ _b[sleep: age_years2]*age_years2 ///
+ _b[sleep: age_years3]*age_years3 ///
+ _b[sleep: asd]*0 ///
+ _b[sleep: asdxage_years]*0*age_years ///
+ _b[sleep: asdxage_years2]*0*age_years2 ///
+ _b[sleep: asdxage_years2]*0*age_years3 ///
, se(nonasd_adj_se3)

predictnl asd_adj3 = ///
_b[sleep:_cons] ///
+ _b[sleep: age_years]*age_years ///
+ _b[sleep: age_years2]*age_years2 ///
+ _b[sleep: age_years3]*age_years3 ///
+ _b[sleep: asd]*1 ///
+ _b[sleep: asdxage_years]*1*age_years ///
+ _b[sleep: asdxage_years2]*1*age_years2 ///
+ _b[sleep: asdxage_years3]*1*age_years3 ///
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
(rarea nonasd_adj_lo3 nonasd_adj_hi3 age_years, sort color(dimgray)) ///
(rarea asd_adj_lo3 asd_adj_hi3 age_years, sort color(dimgray)) ///
(line nonasd_adj3 age_years, sort lcolor(ebblue) lpattern(solid)) ///
(line asd_adj3 age_years, sort lcolor(orange) lpattern(solid)) ///
, ///
graphregion(fcolor(white)) ///
xtitle("Age (Years)") ///
ytitle("Night-time Sleep Duration (Hours)") ///
xlabel(0(2)18, format(%5.0f)) ///
ylabel(8(1)12) ///
graphregion(fcolor(white)) ///
legend(order(3 "Non-autistic" 4 "Autistic") size (small) row(1) position (bottom))

*gs12 was old grey colour 

*Plot those differences with the observed values 
sum mean_age, detail

twoway (connected sleep_no_asd mean_age, sort lcolor(midblue) lpattern(dash) mcolor(dknavy) msize(small)) ///
(connected sleep_asd mean_age, sort lcolor(dkorange) lpattern(dash) mcolor(maroon) msize(small)) ///
(line nonasd_adj3 age_years if inrange(mean_age, 0.12, 15.48), sort lcolor(dknavy) lpattern(solid)) ///
(line asd_adj3 age_years if inrange(mean_age, 0.12, 15.48), sort lcolor(maroon) lpattern(solid)) ///
 , ///
graphregion(fcolor(white)) ///
xlabel(0(2)18, format(%5.0f)) ///
ylabel(8(1)12) ///
xtitle("Age (Years)") ///
ytitle("Night-time Sleep Duration (Hours)") ///
legend(order(1 "Observed Non-autistic" 2 "Observed Autistic" 3 "Predicted Non-autistic" 4 "Predicted Autistic") size (small))

* We can edit the model above to show differences in within individual variability between groups 
* In this case is there more variability between those in the autistic group compared to those in the non-autistic group
mixed sleep age_years age_years2 age_years3 asd asdxage_years asdxage_years2 asdxage_years3 postndep cceianx2 i.epilepsy i.mat_class i.mat_edu i.sex fin_probs || id: age_years, cov(uns) residuals(ind, by(asd)) 

* the result shows greater within individual variabilty in total sleep time for autistic participants 



********************************************************************************
*PREDICTING SLEEP FOR ASD AND NON-ASD GROUPS AT SPECIFIC TIMEPOINTS*
********************************************************************************
* We can also compare what sleep looks like at different time points
* and how these vary by the autistic and non-autistic groups 


* AT AGE 2 YEARS 
***************************** 

* Call the saved estimates from the mixed model 
estimate use "sleep_cubic_adj" // from the adjusted model 

* Create the scores for the two groups at 2 years 
nlcom ///
(nonasd_age2y: _b[sleep:_cons] ///
+ _b[sleep:asd]*0 ///
+ _b[sleep:age_years]*2 ///
+ _b[sleep:age_years2]*2^2 ///
+ _b[sleep:age_years3]*2^3 ///
+ _b[sleep:asdxage_years]*0*2 ///
+ _b[sleep:asdxage_years2]*0*2^2 ///
+ _b[sleep:asdxage_years3]*0*2^3 ///
) ///
(asd_age2y: _b[sleep:_cons] ///
+ _b[sleep:asd]*1 ///
+ _b[sleep:age_years]*2 ///
+ _b[sleep:age_years2]*2^2 ///
+ _b[sleep:age_years3]*2^3 ///
+ _b[sleep:asdxage_years]*1*2 ///
+ _b[sleep:asdxage_years2]*1*2^2 ///
+ _b[sleep:asdxage_years3]*1*2^3 ///
), post // creates the scores for each group 

* Calculates the difference between groups (estimates formatted to two s.f.)
* It also gives you the results for the Wald test which evaluates whether the difference in predicted values between the groups is statistically significant (z and p-value)
nlcom (difference: _b[nonasd_age2y] - _b[asd_age2y]), cformat(%9.2f) post

* This gives you the calculated difference between groups, and exact p-value from the Wald's test - both up to 10 s.f.
est tab, p(%12.10g) // gets you exact p values for multiple comparisons testing etc 


* You can plot the model and add reference lines to double check that the predictions are correct 
twoway ///
(line nonasd_adj age_years, sort lcolor(dknavy) lpattern(solid)) ///
(line asd_adj age_years, sort lcolor(dkorange) lpattern(dash)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(0(2)18, format(%5.0f)) ///
ylabel(8(1)12) ///
ytitle ("Total sleep duration (Hours)") ///
xtitle ("Age (Years)") ///
graphregion(fcolor(white)) ///
legend(order(3 "Non-autistic" 4 "Autistic") size (small) row(1) position (bottom))


* AT AGE 1 YEARS 
***************************** 
* Call the saved estimates from the mixed model 
estimate use "sleep_cubic_adj" // from the adjusted model

nlcom ///
(nonasd_age1y: _b[sleep:_cons] ///
+ _b[sleep:age_years]*1 ///
+ _b[sleep:age_years2]*1^2 ///
+ _b[sleep:age_years3]*1^3 ///
) ///
(asd_age1y: _b[sleep:_cons] ///
+ _b[sleep:asd]*1 ///
+ _b[sleep:age_years]*1 ///
+ _b[sleep:age_years2]*1^2 ///
+ _b[sleep:age_years3]*1^3 ///
+ _b[sleep:asdxage_years]*1*1 ///
+ _b[sleep:asdxage_years2]*1*1^2 ///
+ _b[sleep:asdxage_years3]*1*1^3 ///
), post // creates the scores for each group 

* Calculates the difference between groups (estimates formatted to two s.f.)
* It also gives you the results for the Wald test which evaluates whether the difference in predicted values between the groups is statistically significant (z and p-value)
nlcom (difference: _b[nonasd_age1y] - _b[asd_age1y]), cformat(%9.2f) post

* This gives you the calculated difference between groups, and exact p-value from the Wald's test - both up to 10 s.f.
est tab, p(%12.10g) // gets you exact p values for multiple comparisons testing etc 


* AT AGE 4 WEEKS  
***************************** 
estimate use "sleep_cubic_adj" // from the adjusted model

nlcom ///
(nonasd_age4w: _b[sleep:_cons] ///
+ _b[sleep:age_years]*0.0192 ///
+ _b[sleep:age_years2]*0.0192^2 ///
+ _b[sleep:age_years3]*0.0192^3 ///
) ///
(asd_age4w: _b[sleep:_cons] ///
+ _b[sleep:asd]*1 ///
+ _b[sleep:age_years]*0.0192 ///
+ _b[sleep:age_years2]*0.0192^2 ///
+ _b[sleep:age_years3]*0.0192^3 ///
+ _b[sleep:asdxage_years]*1*0.0192 ///
+ _b[sleep:asdxage_years2]*1*0.0192^2 ///
+ _b[sleep:asdxage_years3]*1*0.0192^3 ///
), post // creates the scores for each group 

nlcom (difference: _b[nonasd_age4w] - _b[asd_age4w]), cformat(%9.2f) post

est tab, p(%12.10g)


* AT AGE 3 YEARS  
***************************** 
estimate use "sleep_cubic_adj" // from the adjusted model

nlcom ///
(nonasd_age3y: _b[sleep:_cons] ///
+ _b[sleep:age_years]*3 ///
+ _b[sleep:age_years2]*3^2 ///
+ _b[sleep:age_years3]*3^3 ///
) ///
(asd_age3y: _b[sleep:_cons] ///
+ _b[sleep:asd]*1 ///
+ _b[sleep:age_years]*3 ///
+ _b[sleep:age_years2]*3^2 ///
+ _b[sleep:age_years3]*3^3 ///
+ _b[sleep:asdxage_years]*1*3 ///
+ _b[sleep:asdxage_years2]*1*3^2 ///
+ _b[sleep:asdxage_years3]*1*3^3 ///
), post // creates the scores for each group 

nlcom (difference: _b[nonasd_age3y] - _b[asd_age3y]), cformat(%9.2f) post

est tab, p(%12.10g)


* AT AGE 4 YEARS  
***************************** 
estimate use "sleep_cubic_adj" // from the adjusted model

nlcom ///
(nonasd_age4y: _b[sleep:_cons] ///
+ _b[sleep:age_years]*4 ///
+ _b[sleep:age_years2]*4^2 ///
+ _b[sleep:age_years3]*4^3 ///
) ///
(asd_age4y: _b[sleep:_cons] ///
+ _b[sleep:asd]*1 ///
+ _b[sleep:age_years]*4 ///
+ _b[sleep:age_years2]*4^2 ///
+ _b[sleep:age_years3]*4^3 ///
+ _b[sleep:asdxage_years]*1*4 ///
+ _b[sleep:asdxage_years2]*1*4^2 ///
+ _b[sleep:asdxage_years3]*1*4^3 ///
), post // creates the scores for each group 

nlcom (difference: _b[nonasd_age4y] - _b[asd_age4y]), cformat(%9.2f) post

est tab, p(%12.10g)


* AT AGE 5 YEARS  
***************************** 
estimate use "sleep_cubic_adj" // from the adjusted model

nlcom ///
(nonasd_age5y: _b[sleep:_cons] ///
+ _b[sleep:age_years]*5 ///
+ _b[sleep:age_years2]*5^2 ///
+ _b[sleep:age_years3]*5^3 ///
) ///
(asd_age5y: _b[sleep:_cons] ///
+ _b[sleep:asd]*1 ///
+ _b[sleep:age_years]*5 ///
+ _b[sleep:age_years2]*5^2 ///
+ _b[sleep:age_years3]*5^3 ///
+ _b[sleep:asdxage_years]*1*5 ///
+ _b[sleep:asdxage_years2]*1*5^2 ///
+ _b[sleep:asdxage_years3]*1*5^3 ///
), post // creates the scores for each group 

nlcom (difference: _b[nonasd_age5y] - _b[asd_age5y]), cformat(%9.2f) post

est tab, p(%12.10g)


* AT AGE 6 YEARS  
***************************** 
estimate use "sleep_cubic_adj" // from the adjusted model

nlcom ///
(nonasd_age6y: _b[sleep:_cons] ///
+ _b[sleep:age_years]*6 ///
+ _b[sleep:age_years2]*6^2 ///
+ _b[sleep:age_years3]*6^3 ///
) ///
(asd_age6y: _b[sleep:_cons] ///
+ _b[sleep:asd]*1 ///
+ _b[sleep:age_years]*6 ///
+ _b[sleep:age_years2]*6^2 ///
+ _b[sleep:age_years3]*6^3 ///
+ _b[sleep:asdxage_years]*1*6 ///
+ _b[sleep:asdxage_years2]*1*6^2 ///
+ _b[sleep:asdxage_years3]*1*6^3 ///
), post // creates the scores for each group 

nlcom (difference: _b[nonasd_age6y] - _b[asd_age6y]), cformat(%9.2f) post

est tab, p(%12.10g)


* AT AGE 7 YEARS  
***************************** 
estimate use "sleep_cubic_adj" // from the adjusted model

nlcom ///
(nonasd_age7y: _b[sleep:_cons] ///
+ _b[sleep:age_years]*7 ///
+ _b[sleep:age_years2]*7^2 ///
+ _b[sleep:age_years3]*7^3 ///
) ///
(asd_age7y: _b[sleep:_cons] ///
+ _b[sleep:asd]*1 ///
+ _b[sleep:age_years]*7 ///
+ _b[sleep:age_years2]*7^2 ///
+ _b[sleep:age_years3]*7^3 ///
+ _b[sleep:asdxage_years]*1*7 ///
+ _b[sleep:asdxage_years2]*1*7^2 ///
+ _b[sleep:asdxage_years3]*1*7^3 ///
), post // creates the scores for each group 

nlcom (difference: _b[nonasd_age7y] - _b[asd_age7y]), cformat(%9.2f) post

est tab, p(%12.10g)


* AT AGE 8 YEARS  
***************************** 
estimate use "sleep_cubic_adj" // from the adjusted model

nlcom ///
(nonasd_age8y: _b[sleep:_cons] ///
+ _b[sleep:age_years]*8 ///
+ _b[sleep:age_years2]*8^2 ///
+ _b[sleep:age_years3]*8^3 ///
) ///
(asd_age8y: _b[sleep:_cons] ///
+ _b[sleep:asd]*1 ///
+ _b[sleep:age_years]*8 ///
+ _b[sleep:age_years2]*8^2 ///
+ _b[sleep:age_years3]*8^3 ///
+ _b[sleep:asdxage_years]*1*8 ///
+ _b[sleep:asdxage_years2]*1*8^2 ///
+ _b[sleep:asdxage_years3]*1*8^3 ///
), post // creates the scores for each group 

nlcom (difference: _b[nonasd_age8y] - _b[asd_age8y]), cformat(%9.2f) post

est tab, p(%12.10g)


* AT AGE 9 YEARS  
***************************** 
estimate use "sleep_cubic_adj" // from the adjusted model

nlcom ///
(nonasd_age9y: _b[sleep:_cons] ///
+ _b[sleep:age_years]*9 ///
+ _b[sleep:age_years2]*9^2 ///
+ _b[sleep:age_years3]*9^3 ///
) ///
(asd_age9y: _b[sleep:_cons] ///
+ _b[sleep:asd]*1 ///
+ _b[sleep:age_years]*9 ///
+ _b[sleep:age_years2]*9^2 ///
+ _b[sleep:age_years3]*9^3 ///
+ _b[sleep:asdxage_years]*1*9 ///
+ _b[sleep:asdxage_years2]*1*9^2 ///
+ _b[sleep:asdxage_years3]*1*9^3 ///
), post // creates the scores for each group 

nlcom (difference: _b[nonasd_age9y] - _b[asd_age9y]), cformat(%9.2f) post

est tab, p(%12.10g)


* AT AGE 10 YEARS  
***************************** 
estimate use "sleep_cubic_adj" // from the adjusted model

nlcom ///
(nonasd_age10y: _b[sleep:_cons] ///
+ _b[sleep:age_years]*10 ///
+ _b[sleep:age_years2]*10^2 ///
+ _b[sleep:age_years3]*10^3 ///
) ///
(asd_age10y: _b[sleep:_cons] ///
+ _b[sleep:asd]*1 ///
+ _b[sleep:age_years]*10 ///
+ _b[sleep:age_years2]*10^2 ///
+ _b[sleep:age_years3]*10^3 ///
+ _b[sleep:asdxage_years]*1*10 ///
+ _b[sleep:asdxage_years2]*1*10^2 ///
+ _b[sleep:asdxage_years3]*1*10^3 ///
), post // creates the scores for each group 

nlcom (difference: _b[nonasd_age10y] - _b[asd_age10y]), cformat(%9.2f) post

est tab, p(%12.10g)


* AT AGE 11 YEARS  
***************************** 
estimate use "sleep_cubic_adj" // from the adjusted model

nlcom ///
(nonasd_age11y: _b[sleep:_cons] ///
+ _b[sleep:age_years]*11 ///
+ _b[sleep:age_years2]*11^2 ///
+ _b[sleep:age_years3]*11^3 ///
) ///
(asd_age11y: _b[sleep:_cons] ///
+ _b[sleep:asd]*1 ///
+ _b[sleep:age_years]*11 ///
+ _b[sleep:age_years2]*11^2 ///
+ _b[sleep:age_years3]*11^3 ///
+ _b[sleep:asdxage_years]*1*11 ///
+ _b[sleep:asdxage_years2]*1*11^2 ///
+ _b[sleep:asdxage_years3]*1*11^3 ///
), post // creates the scores for each group 

nlcom (difference: _b[nonasd_age11y] - _b[asd_age11y]), cformat(%9.2f) post

est tab, p(%12.10g)


* AT AGE 12 YEARS  
***************************** 
estimate use "sleep_cubic_adj" // from the adjusted model

nlcom ///
(nonasd_age12y: _b[sleep:_cons] ///
+ _b[sleep:age_years]*12 ///
+ _b[sleep:age_years2]*12^2 ///
+ _b[sleep:age_years3]*12^3 ///
) ///
(asd_age12y: _b[sleep:_cons] ///
+ _b[sleep:asd]*1 ///
+ _b[sleep:age_years]*12 ///
+ _b[sleep:age_years2]*12^2 ///
+ _b[sleep:age_years3]*12^3 ///
+ _b[sleep:asdxage_years]*1*12 ///
+ _b[sleep:asdxage_years2]*1*12^2 ///
+ _b[sleep:asdxage_years3]*1*12^3 ///
), post // creates the scores for each group 

nlcom (difference: _b[nonasd_age12y] - _b[asd_age12y]), cformat(%9.2f) post

est tab, p(%12.10g)


* AT AGE 13 YEARS  
***************************** 
estimate use "sleep_cubic_adj" // from the adjusted model

nlcom ///
(nonasd_age13y: _b[sleep:_cons] ///
+ _b[sleep:age_years]*13 ///
+ _b[sleep:age_years2]*13^2 ///
+ _b[sleep:age_years3]*13^3 ///
) ///
(asd_age13y: _b[sleep:_cons] ///
+ _b[sleep:asd]*1 ///
+ _b[sleep:age_years]*13 ///
+ _b[sleep:age_years2]*13^2 ///
+ _b[sleep:age_years3]*13^3 ///
+ _b[sleep:asdxage_years]*1*13 ///
+ _b[sleep:asdxage_years2]*1*13^2 ///
+ _b[sleep:asdxage_years3]*1*13^3 ///
), post // creates the scores for each group 

nlcom (difference: _b[nonasd_age13y] - _b[asd_age13y]), cformat(%9.2f) post

est tab, p(%12.10g)



* AT AGE 14 YEARS  
***************************** 
estimate use "sleep_cubic_adj" // from the adjusted model

nlcom ///
(nonasd_age14y: _b[sleep:_cons] ///
+ _b[sleep:age_years]*14 ///
+ _b[sleep:age_years2]*14^2 ///
+ _b[sleep:age_years3]*14^3 ///
) ///
(asd_age14y: _b[sleep:_cons] ///
+ _b[sleep:asd]*1 ///
+ _b[sleep:age_years]*14 ///
+ _b[sleep:age_years2]*14^2 ///
+ _b[sleep:age_years3]*14^3 ///
+ _b[sleep:asdxage_years]*1*14 ///
+ _b[sleep:asdxage_years2]*1*14^2 ///
+ _b[sleep:asdxage_years3]*1*14^3 ///
), post // creates the scores for each group 

nlcom (difference: _b[nonasd_age14y] - _b[asd_age14y]), cformat(%9.2f) post

est tab, p(%12.10g)



* AT AGE 15 YEARS  
***************************** 
estimate use "sleep_cubic_adj" // from the adjusted model

nlcom ///
(nonasd_age15y: _b[sleep:_cons] ///
+ _b[sleep:age_years]*15 ///
+ _b[sleep:age_years2]*15^2 ///
+ _b[sleep:age_years3]*15^3 ///
) ///
(asd_age15y: _b[sleep:_cons] ///
+ _b[sleep:asd]*1 ///
+ _b[sleep:age_years]*15 ///
+ _b[sleep:age_years2]*15^2 ///
+ _b[sleep:age_years3]*15^3 ///
+ _b[sleep:asdxage_years]*1*15 ///
+ _b[sleep:asdxage_years2]*1*15^2 ///
+ _b[sleep:asdxage_years3]*1*15^3 ///
), post // creates the scores for each group 

nlcom (difference: _b[asd_age15y] - _b[nonasd_age15y]), cformat(%9.2f) post

est tab, p(%12.10g)



* AT AGE 16 YEARS  
***************************** 
estimate use "sleep_cubic_adj" // from the adjusted model

nlcom ///
(nonasd_age16y: _b[sleep:_cons] ///
+ _b[sleep:age_years]*16 ///
+ _b[sleep:age_years2]*16^2 ///
+ _b[sleep:age_years3]*16^3 ///
) ///
(asd_age16y: _b[sleep:_cons] ///
+ _b[sleep:asd]*1 ///
+ _b[sleep:age_years]*16 ///
+ _b[sleep:age_years2]*16^2 ///
+ _b[sleep:age_years3]*16^3 ///
+ _b[sleep:asdxage_years]*1*16 ///
+ _b[sleep:asdxage_years2]*1*16^2 ///
+ _b[sleep:asdxage_years3]*1*16^3 ///
), post // creates the scores for each group 

nlcom (difference: _b[asd_age16y] - _b[nonasd_age16y]), cformat(%9.2f) post

est tab, p(%12.10g)



* AT AGE 17 YEARS  
***************************** 
estimate use "sleep_cubic_adj" // from the adjusted model

nlcom ///
(nonasd_age17y: _b[sleep:_cons] ///
+ _b[sleep:age_years]*17 ///
+ _b[sleep:age_years2]*17^2 ///
+ _b[sleep:age_years3]*17^3 ///
) ///
(asd_age17y: _b[sleep:_cons] ///
+ _b[sleep:asd]*1 ///
+ _b[sleep:age_years]*17 ///
+ _b[sleep:age_years2]*17^2 ///
+ _b[sleep:age_years3]*17^3 ///
+ _b[sleep:asdxage_years]*1*17 ///
+ _b[sleep:asdxage_years2]*1*17^2 ///
+ _b[sleep:asdxage_years3]*1*17^3 ///
), post // creates the scores for each group 

nlcom (difference: _b[asd_age17y] - _b[nonasd_age17y]), cformat(%9.2f) post

est tab, p(%12.10g)




* AT AGE 18 YEARS  
***************************** 
estimate use "sleep_cubic_adj" // from the adjusted model

nlcom ///
(nonasd_age18y: _b[sleep:_cons] ///
+ _b[sleep:age_years]*18 ///
+ _b[sleep:age_years2]*18^2 ///
+ _b[sleep:age_years3]*18^3 ///
) ///
(asd_age18y: _b[sleep:_cons] ///
+ _b[sleep:asd]*1 ///
+ _b[sleep:age_years]*18 ///
+ _b[sleep:age_years2]*18^2 ///
+ _b[sleep:age_years3]*18^3 ///
+ _b[sleep:asdxage_years]*1*18 ///
+ _b[sleep:asdxage_years2]*1*18^2 ///
+ _b[sleep:asdxage_years3]*1*18^3 ///
), post // creates the scores for each group 

nlcom (difference: _b[asd_age18y] - _b[nonasd_age18y]), cformat(%9.2f) post

est tab, p(%12.10g)




