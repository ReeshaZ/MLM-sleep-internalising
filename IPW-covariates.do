********************************************************************************
*INVERSE PROBABILITY WEIGHTING 
********************************************************************************
* This is the syntax to apply inverse probability weighting to handle missing covariate data in analyses 

* This syntax assumes that you have already cleaned your data. The data should be in wide format


* Generate a new var for if they have a ANY sleep data acorss occasions 1-10) yes or no
egen any_sleep = rownonmiss(sleep_t1-sleep_t10)
tab any_sleep
recode any_sleep 0=0 1/10=1, gen(completecase)
tab complete case

* Now you have a variable called completecase which is yes or no any sleep data

* We will now estimate the probability of having any of the sleep data, conditional on a bunch of factors which we think affect missingness (postdep, female, finance...) using logistic regression
logistic completecase postndep cceianx2 i.mat_class i.mat_edu i.sex fin_probs

* Now we take the indivisual level predictions (of the probabability of having data) from that model and call it something like pr_miss
predict pr_miss, pr

* Now we want to generate the inverse probability weights, which is the inverse or opposite of having data, so we give more weight to people in the analysis who have dropped out of the study, so that the data should be more representative
gen ipw = 1/pr_miss

* Now we have ipws that can be used as weights for analysis.

* Make the dataset long, making the sleep and age vars in long format, veeyrthing else (weights, covars, asd will all be time invariant)

reshape long sleep_t age_t, i(id) j(occ)

* Now you can do the analysis, starting with the normal model with no weights

mixed sleep_t age_t asd postndep cceianx2 i.epilepsy i.mat_class i.mat_edu i.sex fin_probs  || id: age_t , cov(uns)

*and then with weights, at the fixed effects level (everything before the ||) and again with weights at the random level (everything aftert the ||)

mixed sleep_t age_t asd postndep cceianx2 i.epilepsy i.mat_class i.mat_edu i.sex fin_probs [pweight = ipw], || id: age_t, cov(uns) pweight(ipw)
