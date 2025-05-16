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
by id: egen numocc = count(wake)

* Generate a new numeric variable occ which corresponds to the timepoint
egen occ = group(time_point)

replace occ = 1 if time_point == "age_t1"
replace occ = 2 if time_point == "age_t2"
replace occ = 3 if time_point == "age_t3"
replace occ = 4 if time_point == "age_t4"
replace occ = 5 if time_point == "age_t5"
replace occ = 6 if time_point == "age_t6"
replace occ = 7 if time_point == "age_t7"
replace occ = 8 if time_point == "age_t8"
replace occ = 9 if time_point == "age_t9"

* Order the key variables needed 
order id time_point occ age_years wake 

********************************************************************************
*DESCRIPTIVE INFORMATION & GROWTH CURVE PREP*
********************************************************************************
* Declare the data to be panel for use in later analysis 
xtset id occ

* Describe the missing wake data patterns 
* In the output, 1st row shows that 4776 people have completed every measure, etc 
xtdes if wake~=. 

* To get the wake and age stats by occasion 
bysort occ: sum wake age_years

* Check whether the data is linear to guide how to model growth curves
* Assumption check 1: Is the data linear? 
lowess wake age_years, nograph gen(yhatlowess)
line yhatlowess age_years, sort 
* The graph shows a convex curvilinear relationship between age and sleep 
* We would likely need to go with a quadratic model

* Create a graph showing mean waking over time by age to guide how to model trajectories 
*first create a new variable which is the mean age of all assessments
egen mean_age = mean(age_years), by(occ)
egen tag = tag(occ)
egen waking = mean(wake), by(occ)

twoway (connected waking mean_age if tag, sort msymbol(circle) msize(small) lcolor(dknavy)), ///
graphregion(fcolor(white)) ///
xlabel(0(2)10, format(%5.0f)) ///
ylabel(0(0.5)2) ///
xtitle("Age (Years)") ///
ytitle("Night-time Awakenings (Freq)") 

* We can also plot group-specific changes 
egen waking_no_asd = mean(wake) if asd==0, by(occ)
egen waking_asd = mean(wake) if asd==1, by(occ)

twoway (connected waking_no_asd mean_age, sort msymbol(circle) msize(small) lcolor(dknavy)) ///
(connected waking_asd mean_age, sort msymbol(circle) msize(small) lcolor(dkorange)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(0(2)10, format(%5.0f)) ///
ylabel(0(0.5)2) ///
xtitle("Age (Years)") ///
ytitle("Night-time Awakenings (Freq)") ///
legend(order(1 "Non-autistic" 2 "Autistic"))


********************************************************************************
*EXPLORING DIFFERENT SHAPES OF GROWTH CURVES*
********************************************************************************
* LINEAR MODEL 
mixed wake age_years || id: age_years, cov(uns)

* You can locally store the model estimates as m1 (for comparing against later models)
estimates store m1

* Get information criteria for model comparison 
estat ic 

* And you can get predictions from that model
predict wake_lin

* You can then plot the trajectory from the linear model
twoway (line wake_lin age_years, sort lcolor(dknavy)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(0(2)10, format(%5.0f)) ///
ylabel(0(0.5)1.5) ///
xtitle("Age (Years)") ///
ytitle("Night-time Awakenings (Freq)")

* You can visually examine whether your model fits the data by plotting the prediction with the mean plotted data from earlier 

twoway /// 
(line wake_lin age_years, sort lcolor(dknavy) lpattern(solid)) ///
(connected waking mean_age if tag, sort msymbol(circle) msize(small) mcolor (dknavy) lcolor(midblue) lpattern(dash)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(0(2)10, format(%5.0f)) ///
ylabel(0(0.5)2) ///
xtitle("Age (Years)") ///
ytitle("Night-time Awakenings (Freq)") ///
legend(order(1 "Linear Prediction" 2 "Observed Sample Mean") size (small))
* You can see that it doesn't really fit well 


* NON-LINEAR MODEL - QUADRATIC (two age terms)
gen age_years2 = age_years^2

*now let's explore a non-linear, quadratic change
mixed wake2 age_years age_years2 || id: age_years, cov(uns)

* Locally store the model estimates as m2 
estimates store m2

* Get information criteria for model comparison 
estat ic 

* Like above, you can then make predictions from this model to plot figures 
predict wake_quad

* You can then plot the trajectory from the quadratic model
twoway (line wake_quad age_years, sort lcolor(dknavy)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(0(2)10, format(%5.0f)) ///
ylabel(0(0.5)1.5) ///
xtitle("Age (Years)") ///
ytitle("Night-time Awakenings (Freq)")


* Compare predictions with observed
twoway /// 
(line wake_quad age_years, sort lcolor(dknavy) lpattern(solid)) ///
(connected waking mean_age if tag, sort msymbol(circle) msize(small) mcolor (dknavy) lcolor(midblue) lpattern(dash)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(0(2)10, format(%5.0f)) ///
ylabel(0(0.5)2) ///
xtitle("Age (Years)") ///
ytitle("Night-time Awakenings (Freq)") ///
legend(order(1 "Quadratic Prediction" 2 "Observed Sample Mean") size (small))
* You can see that it fits better

* We can also statistically compare the linear and quadratic model using a likelihood ratio test 
lrtest m1 m2 // p-value < 0.0001 indicating that the quadratic model explains the data better than the linear one 



* NON-LINEAR MODEL - CUBIC (three age terms)
*now let's explore a non-linear, cubic change
gen age_years3 = age_years^3
mixed wake2 age_years age_years2 age_years3 || id: age_years, cov(uns)

* Locally store the model estimates  
estimates store m3

* Get information criteria for model comparison 
estat ic 

* Like above, you can then make predictions from this model to plot figures 
predict wake_cubic

* You can then plot the trajectory from the quadratic model
twoway (line wake_cubic age_years, sort lcolor(dknavy)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(0(2)10, format(%5.0f)) ///
ylabel(0(0.5)1.5) ///
xtitle("Age (Years)") ///
ytitle("Night-time Awakenings (Freq)")

* Compare predictions with observed
twoway /// 
(line wake_cubic age_years, sort lcolor(dknavy) lpattern(solid)) ///
(connected waking mean_age if tag, sort msymbol(circle) msize(small) mcolor (dknavy) lcolor(midblue) lpattern(dash)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(0(2)10, format(%5.0f)) ///
ylabel(0(0.5)2) ///
xtitle("Age (Years)") ///
ytitle("Night-time Awakenings (Freq)") ///
legend(order(1 "Cubic Prediction" 2 "Observed Sample Mean") size (small))
* Doesnt look like it fits better 

* We can also statistically compare the quadratic and cubic model using a likelihood ratio test 
lrtest m2 m3 // p-value < 0.0001 indicating that the cubic model explains the data better than the quadratic one



* NON-LINEAR MODEL - QUARTIC (four age terms)
*now let's explore a non-linear, cubic change
gen age_years4 = age_years^4
mixed wake2 age_years age_years2 age_years3 age_years4 || id: age_years, cov(uns)

* Locally store the model estimates  
estimates store m4

* Get information criteria for model comparison 
estat ic 

* Like above, you can then make predictions from this model to plot figures 
predict wake_quart

* You can then plot the trajectory from the quadratic model
twoway (line wake_quart age_years, sort lcolor(dknavy)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(0(2)10, format(%5.0f)) ///
ylabel(0(0.5)1.5) ///
xtitle("Age (Years)") ///
ytitle("Night-time Awakenings (Freq)")

* Compare predictions with observed
twoway /// 
(line wake_quart age_years, sort lcolor(dknavy) lpattern(solid)) ///
(connected waking mean_age if tag, sort msymbol(circle) msize(small) mcolor (dknavy) lcolor(midblue) lpattern(dash)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(0(2)10, format(%5.0f)) ///
ylabel(0(0.5)2) ///
xtitle("Age (Years)") ///
ytitle("Night-time Awakenings (Freq)") ///
legend(order(1 "Quartic Prediction" 2 "Observed Sample Mean") size (small))
* This looks absolutely insane so no 

* We can also statistically compare the quadratic and cubic model using a likelihood ratio test 
lrtest m3 m4 // p-value < 0.0001 indicating that the quartic model explains the data better but from visually examining the model vs observed you can see that the model overfits the data 


* From above, we can conclude that the cubic model fits our data best. Now we can proceed to add covariates and finalise the model 



********************************************************************************
*FITTING MODEL TO ASD AND NON ASD SUBGROUPS*
********************************************************************************
* Fit the best model to the subgroups (without interaction terms at first) and plot against observed sample mean to determine whether it fits the subgroups well 

***** CUBIC *********************************
* without interaction terms
mixed wake2 age_years age_years2 age_years3 asd || id: age_years, cov(uns)

predictnl nonasd_adj = ///
_b[wake2:_cons] ///
+ _b[wake2: age_years]*age_years ///
+ _b[wake2: age_years2]*age_years2 ///
+ _b[wake2: age_years3]*age_years3 ///
+ _b[wake2: asd]*0 ///
, se(nonasd_adj_se)

predictnl asd_adj = ///
_b[wake2:_cons] ///
+ _b[wake2: age_years]*age_years ///
+ _b[wake2: age_years2]*age_years2 ///
+ _b[wake2: age_years3]*age_years3 ///
+ _b[wake2: asd]*1 ///
, se(asd_adj_se)

*plot the final trajectories
* install software for editing graphs on stata
ssc install grstyle, replace
ssc install palettes, replace

* initialise the software 
set scheme s2color 
grstyle init

* Specify the settings for the graph you're about to plot
grstyle set nogrid

twoway (connected waking_no_asd mean_age, sort lcolor(midblue) lpattern(dash) mcolor(dknavy) msize(small)) ///
(connected waking_asd mean_age, sort lcolor(dkorange) lpattern(dash) mcolor(maroon) msize(small)) ///
(line nonasd_adj age_years if inrange(mean_age, 0.10, 9.65), sort lcolor(dknavy) lpattern(solid)) ///
(line asd_adj age_years if inrange(mean_age, 0.10, 9.65), sort lcolor(maroon) lpattern(solid)) ///
 , ///
graphregion(fcolor(white)) ///
xlabel(0(2)10, format(%5.0f)) ///
ylabel(0(0.5)2) ///
xtitle("Age (Years)") ///
ytitle("Night-time Awakenings (Freq)") ///
legend(order(1 "Observed Non-autistic" 2 "Observed Autistic" 3 "Predicted Non-autistic" 4 "Predicted Autistic") size (small))


* WITH interaction terms
mixed wake2 age_years age_years2 age_years3 asd asdxage_years asdxage_years2 asdxage_years3 || id: age_years, cov(uns)


* Plot best fitting model against observed 
predictnl nonasd_adj2 = ///
_b[wake2:_cons] ///
+ _b[wake2: age_years]*age_years ///
+ _b[wake2: age_years2]*age_years2 ///
+ _b[wake2: age_years3]*age_years3 ///
+ _b[wake2: asd]*0 ///
+ _b[wake2: asdxage_years]*0*age_years ///
+ _b[wake2: asdxage_years2]*0*age_years2 ///
, se(nonasd_adj_se2)

predictnl asd_adj2 = ///
_b[wake2:_cons] ///
+ _b[wake2: age_years]*age_years ///
+ _b[wake2: age_years2]*age_years2 ///
+ _b[wake2: asd]*1 ///
+ _b[wake2: asdxage_years]*1*age_years ///
+ _b[wake2: asdxage_years2]*1*age_years2 ///
, se(asd_adj_se2)


twoway (connected waking_no_asd mean_age, sort lcolor(midblue) lpattern(dash) mcolor(dknavy) msize(small)) ///
(connected waking_asd mean_age, sort lcolor(dkorange) lpattern(dash) mcolor(maroon) msize(small)) ///
(line nonasd_adj2 age_years if inrange(mean_age, 0.10, 9.65), sort lcolor(dknavy) lpattern(solid)) ///
(line asd_adj2 age_years if inrange(mean_age, 0.10, 9.65), sort lcolor(maroon) lpattern(solid)) ///
 , ///
graphregion(fcolor(white)) ///
xlabel(0(2)10, format(%5.0f)) ///
ylabel(0(0.5)2) ///
xtitle("Age (Years)") ///
ytitle("Night-time Awakenings (Freq)") ///
legend(order(1 "Observed Non-autistic" 2 "Observed Autistic" 3 "Predicted Non-autistic" 4 "Predicted Autistic") size (small))








********************************************************************************
*ADDING COVARIATES*
********************************************************************************
* Run cubic model with asd and covariates added 
* First, generate variables to model asd group interactions 
gen asdxage_years = asd*age_years
gen asdxage_years2 = asd*age_years2
gen asdxage_years3 = asd*age_years3


mixed wake2 age_years age_years2 age_years3 asd asdxage_years asdxage_years2 asdxage_years3 postndep cceianx2 i.epilepsy i.mat_class i.mat_edu i.sex fin_probs || id: age_years, cov(uns)

* save the estimates for later analysis. "save" exports it to a file, whereas "store" from earlier stores it locally. When saving, you can revist the estimates without re-running the model 
estimates save "wake_quad_adj"
estimates store wake_quad_adj

estat ic

* You can also run the other models with asd and covariates added - and compare them again to the cubic model to make sure cubic is still best after adjusting 
* linear 
mixed wake2 age_years asd asdxage_years postndep cceianx2 i.epilepsy i.mat_class i.mat_edu i.sex fin_probs || id: age_years, cov(uns)

estimates store wake_lin_adj

estat ic

lrtest wake_lin_adj wake_quad_adj 
 

* cubic
gen asdxage_years3 = asd*age_years3

mixed wake2 age_years age_years2 age_years3 asd asdxage_years asdxage_years2 asdxage_years3 postndep cceianx2 i.epilepsy i.mat_class i.mat_edu i.sex fin_probs || id: age_years, cov(uns)

estimates store wake_cubic_adj

estat ic

lrtest wake_quad_adj wake_cubic_adj 

* quartic
gen asdxage_years4 = asd*age_years4

mixed wake2 age_years age_years2 age_years3 age_years4 asd asdxage_years asdxage_years2 asdxage_years3 asdxage_years4 postndep cceianx2 i.epilepsy i.mat_class i.mat_edu i.sex fin_probs || id: age_years, cov(uns)

estimates store wake_quart_adj

estat ic

lrtest wake_cubic_adj wake_quart_adj

* Going to stick with the cubic one 




********************************************************************************
*CREATING GROWTH CURVES FOR NON ASD AND ASD GROUPS*
********************************************************************************
mixed wake2 age_years age_years2 age_years3 asd asdxage_years asdxage_years2 asdxage_years3 postndep cceianx2 i.epilepsy i.mat_class i.mat_edu i.sex fin_probs || id: age_years, cov(uns)

*make the trajectories for each group
predictnl nonasd_adj3 = ///
_b[wake2:_cons] ///
+ _b[wake2: age_years]*age_years ///
+ _b[wake2: age_years2]*age_years2 ///
+ _b[wake2: age_years3]*age_years3 ///
+ _b[wake2: asd]*0 ///
+ _b[wake2: asdxage_years]*0*age_years ///
+ _b[wake2: asdxage_years2]*0*age_years2 ///
+ _b[wake2: asdxage_years3]*0*age_years3 ///
, se(nonasd_adj_se3)

predictnl asd_adj3 = ///
_b[wake2:_cons] ///
+ _b[wake2: age_years]*age_years ///
+ _b[wake2: age_years2]*age_years2 ///
+ _b[wake2: age_years3]*age_years3 ///
+ _b[wake2: asd]*1 ///
+ _b[wake2: asdxage_years]*1*age_years ///
+ _b[wake2: asdxage_years2]*1*age_years2 ///
+ _b[wake2: asdxage_years3]*1*age_years3 ///
, se(asd_adj_se3)

* add the confidence intervals high and low 95% conf intervals 
gen nonasd_adj_lo3 = nonasd_adj3 - 1.96*nonasd_adj_se3
gen nonasd_adj_hi3 = nonasd_adj3 + 1.96*nonasd_adj_se3  

gen asd_adj_lo3 = asd_adj3 - 1.96*asd_adj_se3
gen asd_adj_hi3 = asd_adj3 + 1.96*asd_adj_se3  


twoway ///
(rarea nonasd_adj_lo3 nonasd_adj_hi3 age_years, sort color(dimgray)) ///
(rarea asd_adj_lo3 asd_adj_hi3 age_years, sort color(dimgray)) ///
(line nonasd_adj3 age_years, sort lcolor(ebblue) lpattern(solid)) ///
(line asd_adj3 age_years, sort lcolor(orange) lpattern(solid)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(0(2)10, format(%5.0f)) ///
ylabel(0(0.5)2) ///
xtitle("Age (Years)") ///
ytitle("Night-time Awakenings (Freq)") ///
legend(order(3 "Non-autistic" 4 "Autistic") size (small) row(1) position (bottom))


*Plot those differences with the observed values
sum mean_age, detail

twoway (connected waking_no_asd mean_age, sort lcolor(midblue) lpattern(dash) mcolor(dknavy) msize(small)) ///
(connected waking_asd mean_age, sort lcolor(dkorange) lpattern(dash) mcolor(maroon) msize(small)) ///
(line nonasd_adj3 age_years if inrange(mean_age, 0.10, 10), sort lcolor(dknavy) lpattern(solid)) ///
(line asd_adj3 age_years if inrange(mean_age, 0.10, 10), sort lcolor(maroon) lpattern(solid)) ///
 , ///
graphregion(fcolor(white)) ///
xlabel(0(2)10, format(%5.0f)) ///
ylabel(0(0.5)2) ///
xtitle("Age (Years)") ///
ytitle("Night-time Awakenings (Freq)") ///
legend(order(1 "Observed Non-autistic" 2 "Observed Autistic" 3 "Predicted Non-autistic" 4 "Predicted Autistic") size (small))

* We can edit the model above to show differences in within individual variability between groups 
* In this case is there more variability between those in the autistic group compared to those in the non-autistic group
mixed wake2 age_years age_years2 age_years3 asd asdxage_years asdxage_years2 asdxage_years3 postndep cceianx2 i.epilepsy i.mat_class i.mat_edu i.sex fin_probs || id: age_years, cov(uns) residuals(ind, by(asd)) 

* the result shows greater within individual variabilty in total sleep time for autistic participants 




********************************************************************************
*PREDICTING WAKING FOR ASD AND NON-ASD GROUPS AT SPECIFIC TIMEPOINTS*
********************************************************************************
* We can also compare what waking looks like at different time points
* and how these vary by the autistic and non-autistic groups 


* AT AGE 1 YEAR 
***************************** 

* Call the saved estimates from the mixed model 
estimate use "wake_cubic_adj" // from the adjusted model 

* Create the scores for the two groups at 1 year 
nlcom ///
(nonasd_age1y: _b[wake2:_cons] ///
+ _b[wake2:age_years]*1 ///
+ _b[wake2:age_years2]*1^2 ///
+ _b[wake2:age_years3]*1^3 ///
) ///
(asd_age1y: _b[wake2:_cons] ///
+ _b[wake2:asd]*1 ///
+ _b[wake2:age_years]*1 ///
+ _b[wake2:age_years2]*1^2 ///
+ _b[wake2:age_years3]*1^3 ///
+ _b[wake2:asdxage_years]*1*1 ///
+ _b[wake2:asdxage_years2]*1*1^2 ///
+ _b[wake2:asdxage_years3]*1*1^3 ///
), post // creates the scores for each group 

* Calculates the difference between groups (estimates formatted to two s.f.)
* It also gives you the results for the Wald test which evaluates whether the difference in predicted values between the groups is statistically significant (z and p-value)
nlcom (difference: _b[asd_age1y] - _b[nonasd_age1y]), cformat(%9.2f) post

* This gives you the calculated difference between groups, and exact p-value from the Wald's test - both up to 10 s.f.
est tab, p(%12.10g) // gets you exact p values for multiple comparisons testing etc 


* You can plot the model and add reference lines to double check that the predictions are correct 
twoway ///
(line nonasd_adj3 age_years, sort lcolor(dknavy) lpattern(solid)) ///
(line asd_adj3 age_years, sort lcolor(dkorange) lpattern(dash)) ///
, ///
graphregion(fcolor(white)) ///
xlabel(0(2)10, format(%5.0f)) ///
ylabel(0(0.5)2) ///
xtitle("Age (Years)") ///
ytitle("Night-time Awakenings (Freq)") ///
legend(order(3 "Non-autistic" 4 "Autistic") size (small) row(1) position (bottom))


* AT AGE 4 WEEKS 
***************************** 
estimate use "wake_cubic_adj"

nlcom ///
(nonasd_age4w: _b[wake2:_cons] ///
+ _b[wake2:age_years]*0.0192 ///
+ _b[wake2:age_years2]*0.0192^2 ///
+ _b[wake2:age_years3]*0.0192^3 ///
) ///
(asd_age4w: _b[wake2:_cons] ///
+ _b[wake2:asd]*1 ///
+ _b[wake2:age_years]*0.0192 ///
+ _b[wake2:age_years2]*0.0192^2 ///
+ _b[wake2:age_years3]*0.0192^3 ///
+ _b[wake2:asdxage_years]*1*0.0192 ///
+ _b[wake2:asdxage_years2]*1*0.0192^2 ///
+ _b[wake2:asdxage_years3]*1*0.0192^3 ///
), post 

nlcom (difference: _b[asd_age4w] - _b[nonasd_age4w]), cformat(%9.2f) post

est tab, p(%12.10g) 


* AT AGE 2 YEARS
***************************** 
estimate use "wake_cubic_adj"

nlcom ///
(nonasd_age2y: _b[wake2:_cons] ///
+ _b[wake2:age_years]*2 ///
+ _b[wake2:age_years2]*2^2 ///
+ _b[wake2:age_years3]*2^3 ///
) ///
(asd_age2y: _b[wake2:_cons] ///
+ _b[wake2:asd]*1 ///
+ _b[wake2:age_years]*2 ///
+ _b[wake2:age_years2]*2^2 ///
+ _b[wake2:age_years3]*2^3 ///
+ _b[wake2:asdxage_years]*1*2 ///
+ _b[wake2:asdxage_years2]*1*2^2 ///
+ _b[wake2:asdxage_years3]*1*2^3 ///
), post 

nlcom (difference: _b[asd_age2y] - _b[nonasd_age2y]), cformat(%9.2f) post

est tab, p(%12.10g) 


* AT AGE 3 YEARS
***************************** 
estimate use "wake_cubic_adj"

nlcom ///
(nonasd_age3y: _b[wake2:_cons] ///
+ _b[wake2:age_years]*3 ///
+ _b[wake2:age_years2]*3^2 ///
+ _b[wake2:age_years3]*3^3 ///
) ///
(asd_age3y: _b[wake2:_cons] ///
+ _b[wake2:asd]*1 ///
+ _b[wake2:age_years]*3 ///
+ _b[wake2:age_years2]*3^2 ///
+ _b[wake2:age_years3]*3^3 ///
+ _b[wake2:asdxage_years]*1*3 ///
+ _b[wake2:asdxage_years2]*1*3^2 ///
+ _b[wake2:asdxage_years3]*1*3^3 ///
), post 

nlcom (difference: _b[asd_age3y] - _b[nonasd_age3y]), cformat(%9.2f) post

est tab, p(%12.10g) 


* AT AGE 4 YEARS
***************************** 
estimate use "wake_cubic_adj"

nlcom ///
(nonasd_age4y: _b[wake2:_cons] ///
+ _b[wake2:age_years]*4 ///
+ _b[wake2:age_years2]*4^2 ///
+ _b[wake2:age_years3]*4^3 ///
) ///
(asd_age4y: _b[wake2:_cons] ///
+ _b[wake2:asd]*1 ///
+ _b[wake2:age_years]*4 ///
+ _b[wake2:age_years2]*4^2 ///
+ _b[wake2:age_years3]*4^3 ///
+ _b[wake2:asdxage_years]*1*4 ///
+ _b[wake2:asdxage_years2]*1*4^2 ///
+ _b[wake2:asdxage_years3]*1*4^3 ///
), post  

nlcom (difference: _b[asd_age4y] - _b[nonasd_age4y]), cformat(%9.2f) post

est tab, p(%12.10g) 


* AT AGE 5 YEARS
***************************** 
estimate use "wake_cubic_adj"

nlcom ///
(nonasd_age5y: _b[wake2:_cons] ///
+ _b[wake2:age_years]*5 ///
+ _b[wake2:age_years2]*5^2 ///
+ _b[wake2:age_years3]*5^3 ///
) ///
(asd_age5y: _b[wake2:_cons] ///
+ _b[wake2:asd]*1 ///
+ _b[wake2:age_years]*5 ///
+ _b[wake2:age_years2]*5^2 ///
+ _b[wake2:age_years3]*5^3 ///
+ _b[wake2:asdxage_years]*1*5 ///
+ _b[wake2:asdxage_years2]*1*5^2 ///
+ _b[wake2:asdxage_years3]*1*5^3 ///
), post

nlcom (difference: _b[asd_age5y] - _b[nonasd_age5y]), cformat(%9.2f) post

est tab, p(%12.10g)


* AT AGE 6 YEARS
***************************** 
estimate use "wake_cubic_adj"

nlcom ///
(nonasd_age6y: _b[wake2:_cons] ///
+ _b[wake2:age_years]*6 ///
+ _b[wake2:age_years2]*6^2 ///
+ _b[wake2:age_years3]*6^3 ///
) ///
(asd_age6y: _b[wake2:_cons] ///
+ _b[wake2:asd]*1 ///
+ _b[wake2:age_years]*6 ///
+ _b[wake2:age_years2]*6^2 ///
+ _b[wake2:age_years3]*6^3 ///
+ _b[wake2:asdxage_years]*1*6 ///
+ _b[wake2:asdxage_years2]*1*6^2 ///
+ _b[wake2:asdxage_years3]*1*6^3 ///
), post

nlcom (difference: _b[asd_age6y] - _b[nonasd_age6y]), cformat(%9.2f) post

est tab, p(%12.10g)


* AT AGE 7 YEARS
***************************** 
estimate use "wake_cubic_adj"

nlcom ///
(nonasd_age7y: _b[wake2:_cons] ///
+ _b[wake2:age_years]*7 ///
+ _b[wake2:age_years2]*7^2 ///
+ _b[wake2:age_years3]*7^3 ///
) ///
(asd_age7y: _b[wake2:_cons] ///
+ _b[wake2:asd]*1 ///
+ _b[wake2:age_years]*7 ///
+ _b[wake2:age_years2]*7^2 ///
+ _b[wake2:age_years3]*7^3 ///
+ _b[wake2:asdxage_years]*1*7 ///
+ _b[wake2:asdxage_years2]*1*7^2 ///
+ _b[wake2:asdxage_years3]*1*7^3 ///
), post

nlcom (difference: _b[asd_age7y] - _b[nonasd_age7y]), cformat(%9.2f) post

est tab, p(%12.10g)


* AT AGE 8 YEARS
***************************** 
estimate use "wake_cubic_adj"

nlcom ///
(nonasd_age8y: _b[wake2:_cons] ///
+ _b[wake2:age_years]*8 ///
+ _b[wake2:age_years2]*8^2 ///
+ _b[wake2:age_years3]*8^3 ///
) ///
(asd_age8y: _b[wake2:_cons] ///
+ _b[wake2:asd]*1 ///
+ _b[wake2:age_years]*8 ///
+ _b[wake2:age_years2]*8^2 ///
+ _b[wake2:age_years3]*8^3 ///
+ _b[wake2:asdxage_years]*1*8 ///
+ _b[wake2:asdxage_years2]*1*8^2 ///
+ _b[wake2:asdxage_years3]*1*8^3 ///
), post

nlcom (difference: _b[asd_age8y] - _b[nonasd_age8y]), cformat(%9.2f) post

est tab, p(%12.10g)


* AT AGE 9 YEARS
***************************** 
estimate use "wake_cubic_adj"

nlcom ///
(nonasd_age9y: _b[wake2:_cons] ///
+ _b[wake2:age_years]*9 ///
+ _b[wake2:age_years2]*9^2 ///
+ _b[wake2:age_years3]*9^3 ///
) ///
(asd_age9y: _b[wake2:_cons] ///
+ _b[wake2:asd]*1 ///
+ _b[wake2:age_years]*9 ///
+ _b[wake2:age_years2]*9^2 ///
+ _b[wake2:age_years3]*9^3 ///
+ _b[wake2:asdxage_years]*1*9 ///
+ _b[wake2:asdxage_years2]*1*9^2 ///
+ _b[wake2:asdxage_years3]*1*9^3 ///
), post

nlcom (difference: _b[asd_age9y] - _b[nonasd_age9y]), cformat(%9.2f) post

est tab, p(%12.10g)


* AT AGE 10 YEARS
***************************** 
estimate use "wake_cubic_adj"

nlcom ///
(nonasd_age10y: _b[wake2:_cons] ///
+ _b[wake2:age_years]*10 ///
+ _b[wake2:age_years2]*10^2 ///
+ _b[wake2:age_years3]*10^3 ///
) ///
(asd_age10y: _b[wake2:_cons] ///
+ _b[wake2:asd]*1 ///
+ _b[wake2:age_years]*10 ///
+ _b[wake2:age_years2]*10^2 ///
+ _b[wake2:age_years3]*10^3 ///
+ _b[wake2:asdxage_years]*1*10 ///
+ _b[wake2:asdxage_years2]*1*10^2 ///
+ _b[wake2:asdxage_years3]*1*10^3 ///
), post

nlcom (difference: _b[asd_age10y] - _b[nonasd_age10y]), cformat(%9.2f) post

est tab, p(%12.10g)
