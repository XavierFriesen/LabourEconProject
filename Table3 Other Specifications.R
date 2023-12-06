##1.1 Sub-25
Y_sub_25_female <- coxph(Surv(time, laid_off) ~ dum_25_inp + d.high_edu + leeftijd + partner + d.children + job_type + sector + d.mental_health + d.heavy_alcohol + job_satisf + strata(year), data = df_no_na_fem, robust = TRUE)
summary(Y_sub_25_female)
cox.zph(Y_sub_25_female)

log_likelihood_Y_fem <- summary(Y_sub_25_female)$loglik
log_likelihood_Y_fem 
deviance_Y_fem <- -2 * (log_likelihood_Y_fem[1] - log_likelihood_Y_fem[2])
deviance_Y_fem
num_parameters_Y_fem <- length(coef(Y_sub_25_female)) #parameters
num_parameters_Y_fem
wald_test_Y_fem <- summary(Y_sub_25_female)$waldtest
wald_test_Y_fem

Y_sub_25_male <- coxph(Surv(time, laid_off) ~ dum_25_inp + d.high_edu + leeftijd + partner + d.children + job_type + sector + d.mental_health + d.heavy_alcohol + job_satisf + strata(year), data = df_no_na_male, robust = TRUE)
summary(Y_sub_25_male)

cox.zph(Y_sub_25_female)
log_likelihood_Y_male <- summary(Y_sub_25_male)$loglik
log_likelihood_Y_male 
deviance_Y_male <- -2 * (log_likelihood_Y_male[1] - log_likelihood_Y_male[2])
deviance_Y_male
num_parameters_Y_male <- length(coef(Y_sub_25_male)) #parameters
num_parameters_Y_male
wald_test_Y_male <- summary(Y_sub_25_male)$waldtest
wald_test_Y_male

remove('Y_sub_25_female', 'log_likelihood_Y_fem', 'deviance_Y_fem', 'num_parameters_Y_fem', 'wald_test_Y_fem',
       'Y_sub_25_male', 'log_likelihood_Y_male', 'deviance_Y_male', 'num_parameters_Y_male', 'wald_test_Y_male'
)


##1.2 Sub-35

Y_sub_35_female <- coxph(Surv(time, laid_off) ~ dum_35_inp + d.high_edu + leeftijd + partner + d.children + job_type + sector + d.mental_health + d.heavy_alcohol + job_satisf + strata(year), data = df_no_na_fem, robust = TRUE)
summary(Y_sub_35_female)
cox.zph(Y_sub_35_female)

log_likelihood_Y_fem <- summary(Y_sub_35_female)$loglik
log_likelihood_Y_fem 
deviance_Y_fem <- -2 * (log_likelihood_Y_fem[1] - log_likelihood_Y_fem[2])
deviance_Y_fem
num_parameters_Y_fem <- length(coef(Y_sub_35_female)) #parameters
num_parameters_Y_fem
wald_test_Y_fem <- summary(Y_sub_35_female)$waldtest
wald_test_Y_fem

Y_sub_35_male <- coxph(Surv(time, laid_off) ~ dum_35_inp + d.high_edu + leeftijd + partner + d.children + job_type + sector + d.mental_health + d.heavy_alcohol + job_satisf + strata(year), data = df_no_na_male, robust = TRUE)
summary(Y_sub_35_male)

cox.zph(Y_sub_35_female)
log_likelihood_Y_male <- summary(Y_sub_35_male)$loglik
log_likelihood_Y_male 
deviance_Y_male <- -2 * (log_likelihood_Y_male[1] - log_likelihood_Y_male[2])
deviance_Y_male
num_parameters_Y_male <- length(coef(Y_sub_35_male)) #parameters
num_parameters_Y_male
wald_test_Y_male <- summary(Y_sub_35_male)$waldtest
wald_test_Y_male

remove('Y_sub_35_female', 'log_likelihood_Y_fem', 'deviance_Y_fem', 'num_parameters_Y_fem', 'wald_test_Y_fem',
'Y_sub_35_male', 'log_likelihood_Y_male', 'deviance_Y_male', 'num_parameters_Y_male', 'wald_test_Y_male'
)

## 1.3. Sub_45
Y_sub_45_female <- coxph(Surv(time, laid_off) ~ dum_45_inp + d.high_edu + leeftijd + partner + d.children + job_type + sector + d.mental_health + d.heavy_alcohol + job_satisf + strata(year), data = df_no_na_fem, robust = TRUE)
summary(Y_sub_45_female)
cox.zph(Y_sub_45_female)

log_likelihood_Y_fem <- summary(Y_sub_45_female)$loglik
log_likelihood_Y_fem 
deviance_Y_fem <- -2 * (log_likelihood_Y_fem[1] - log_likelihood_Y_fem[2])
deviance_Y_fem
num_parameters_Y_fem <- length(coef(Y_sub_45_female)) #parameters
num_parameters_Y_fem
wald_test_Y_fem <- summary(Y_sub_45_female)$waldtest
wald_test_Y_fem

Y_sub_45_male <- coxph(Surv(time, laid_off) ~ dum_45_inp + d.high_edu + leeftijd + partner + d.children + job_type + sector + d.mental_health + d.heavy_alcohol + job_satisf + strata(year), data = df_no_na_male, robust = TRUE)
summary(Y_sub_45_male)

log_likelihood_Y_male <- summary(Y_sub_45_male)$loglik
log_likelihood_Y_male 
deviance_Y_male <- -2 * (log_likelihood_Y_male[1] - log_likelihood_Y_male[2])
deviance_Y_male
num_parameters_Y_male <- length(coef(Y_sub_45_male)) #parameters
num_parameters_Y_male
wald_test_Y_male <- summary(Y_sub_45_male)$waldtest
wald_test_Y_male

remove('Y_sub_45_female', 'log_likelihood_Y_fem', 'deviance_Y_fem', 'num_parameters_Y_fem', 'wald_test_Y_fem',
       'Y_sub_45_male', 'log_likelihood_Y_male', 'deviance_Y_male', 'num_parameters_Y_male', 'wald_test_Y_male'
)


## continuous linear
Y_cont_lin_female <- coxph(Surv(time, laid_off) ~ rosenberg_inp + d.high_edu + leeftijd + partner + d.children + job_type + sector + d.mental_health + d.heavy_alcohol + job_satisf + strata(year), data = df_no_na_fem, robust = TRUE)
summary(Y_cont_lin_female)

log_likelihood_Y_fem <- summary(Y_cont_lin_female)$loglik
log_likelihood_Y_fem 
deviance_Y_fem <- -2 * (log_likelihood_Y_fem[1] - log_likelihood_Y_fem[2])
deviance_Y_fem
num_parameters_Y_fem <- length(coef(Y_cont_lin_female)) #parameters
num_parameters_Y_fem
wald_test_Y_fem <- summary(Y_cont_lin_female)$waldtest
wald_test_Y_fem


test.ph <- cox.zph(Y_cont_lin_female) #testing assumptions using Schoenfeld Residuals
test.ph #valid

Y_cont_lin_male <- coxph(Surv(time, laid_off) ~ rosenberg_inp + d.high_edu + leeftijd + partner + d.children + job_type + sector + d.mental_health + d.heavy_alcohol + job_satisf + strata(year), data = df_no_na_male, robust = TRUE) 
summary(Y_cont_lin_male) 

log_likelihood_Y_male <- summary(Y_cont_lin_male)$loglik
log_likelihood_Y_male 
deviance_Y_male <- -2 * (log_likelihood_Y_male[1] - log_likelihood_Y_male[2])
deviance_Y_male
num_parameters_Y_male <- length(coef(Y_cont_lin_male)) #parameters
num_parameters_Y_male
wald_test_Y_male <- summary(Y_cont_lin_male)$waldtest
wald_test_Y_male

test.ph <- cox.zph(Y_cont_lin_male)  #testing assumptions using Schoenfeld Residuals
test.ph #valid

remove('Y_cont_lin_female', 'log_likelihood_Y_fem', 'deviance_Y_fem', 'num_parameters_Y_fem', 'wald_test_Y_fem',
       'Y_cont_lin_male', 'log_likelihood_Y_male', 'deviance_Y_male', 'num_parameters_Y_male', 'wald_test_Y_male'
)

