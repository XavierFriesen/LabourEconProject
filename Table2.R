A_sub_40_female <- coxph(Surv(time, laid_off) ~ dum_40_inp + strata(year), data = df_no_na_fem, robust = TRUE)
summary(A_sub_40_female)
cox.zph(A_sub_40_female)

log_likelihood_A_fem <- summary(A_sub_40_female)$loglik
log_likelihood_A_fem 
deviance_A_fem <- -2 * (log_likelihood_A_fem[1] - log_likelihood_A_fem[2])
deviance_A_fem
num_parameters_A_fem <- length(coef(A_sub_40_female)) #parameters
num_parameters_A_fem
wald_test_A_fem <- summary(A_sub_40_female)$waldtest
wald_test_A_fem

A_sub_40_male <- coxph(Surv(time, laid_off) ~ dum_40_inp + strata(year), data = df_no_na_male, robust = TRUE)
summary(A_sub_40_male)
cox.zph(A_sub_40_female)

log_likelihood_A_male <- summary(A_sub_40_male)$loglik
log_likelihood_A_male 
deviance_A_male <- -2 * (log_likelihood_A_male[1] - log_likelihood_A_male[2])
deviance_A_male
num_parameters_A_male <- length(coef(A_sub_40_male)) #parameters
num_parameters_A_male
wald_test_A_male <- summary(A_sub_40_male)$waldtest
wald_test_A_male

B_sub_40_female <- coxph(Surv(time, laid_off) ~ dum_40_inp + d.high_edu + leeftijd + strata(year), data = df_no_na_fem, robust = TRUE)
summary(B_sub_40_female)
cox.zph(B_sub_40_female)

log_likelihood_B_fem <- summary(B_sub_40_female)$loglik
log_likelihood_B_fem 
deviance_B_fem <- -2 * (log_likelihood_B_fem[1] - log_likelihood_B_fem[2])
deviance_B_fem
num_parameters_B_fem <- length(coef(B_sub_40_female)) #parameters
num_parameters_B_fem
wald_test_B_fem <- summary(B_sub_40_female)$waldtest
wald_test_B_fem

B_sub_40_male <- coxph(Surv(time, laid_off) ~ dum_40_inp + d.high_edu + leeftijd + strata(year), data = df_no_na_male, robust = TRUE)
summary(B_sub_40_male)
cox.zph(B_sub_40_male)

log_likelihood_B_male <- summary(B_sub_40_male)$loglik
log_likelihood_B_male 
deviance_B_male <- -2 * (log_likelihood_B_male[1] - log_likelihood_B_male[2])
deviance_B_male
num_parameters_B_male <- length(coef(B_sub_40_male)) #parameters
num_parameters_B_male
wald_test_B_male <- summary(B_sub_40_male)$waldtest
wald_test_B_male

C_sub_40_female <- coxph(Surv(time, laid_off) ~ dum_40_inp + d.high_edu + leeftijd + partner + d.children + strata(year), data = df_no_na_fem, robust = TRUE)
summary(C_sub_40_female)
cox.zph(C_sub_40_female) #partner significant

log_likelihood_C_fem <- summary(C_sub_40_female)$loglik
log_likelihood_C_fem 
deviance_C_fem <- -2 * (log_likelihood_C_fem[1] - log_likelihood_C_fem[2])
deviance_C_fem
num_parameters_C_fem <- length(coef(C_sub_40_female)) #parameters
num_parameters_C_fem
wald_test_C_fem <- summary(C_sub_40_female)$waldtest
wald_test_C_fem

C_sub_40_male <- coxph(Surv(time, laid_off) ~ dum_40_inp + d.high_edu + leeftijd + partner + d.children + strata(year), data = df_no_na_male, robust = TRUE)
summary(C_sub_40_male)
cox.zph(C_sub_40_male) 

log_likelihood_C_male <- summary(C_sub_40_male)$loglik
log_likelihood_C_male 
deviance_C_male <- -2 * (log_likelihood_C_male[1] - log_likelihood_C_male[2])
deviance_C_male
num_parameters_C_male <- length(coef(C_sub_40_male)) #parameters
num_parameters_C_male
wald_test_C_male <- summary(C_sub_40_male)$waldtest
wald_test_C_male

D_sub_40_female <- coxph(Surv(time, laid_off) ~ dum_40_inp + d.high_edu + leeftijd + partner + d.children + job_type + strata(year), data = df_no_na_fem, robust = TRUE)
summary(D_sub_40_female)
cox.zph(D_sub_40_female)

log_likelihood_D_fem <- summary(D_sub_40_female)$loglik
log_likelihood_D_fem 
deviance_D_fem <- -2 * (log_likelihood_D_fem[1] - log_likelihood_D_fem[2])
deviance_D_fem
num_parameters_D_fem <- length(coef(D_sub_40_female)) #parameters
num_parameters_D_fem
wald_test_D_fem <- summary(D_sub_40_female)$waldtest
wald_test_D_fem

D_sub_40_male <- coxph(Surv(time, laid_off) ~ dum_40_inp + d.high_edu + leeftijd + partner + d.children + job_type + strata(year), data = df_no_na_male, robust = TRUE)
summary(D_sub_40_male)
cox.zph(D_sub_40_male) #leeftijd almost significant

log_likelihood_D_male <- summary(D_sub_40_male)$loglik
log_likelihood_D_male 
deviance_D_male <- -2 * (log_likelihood_D_male[1] - log_likelihood_D_male[2])
deviance_D_male
num_parameters_D_male <- length(coef(D_sub_40_male)) #parameters
num_parameters_D_male
wald_test_D_male <- summary(D_sub_40_male)$waldtest
wald_test_D_male

E_sub_40_female <- coxph(Surv(time, laid_off) ~ dum_40_inp + d.high_edu + leeftijd + partner + d.children + job_type + sector + strata(year), data = df_no_na_fem, robust = TRUE)
summary(E_sub_40_female)
cox.zph(E_sub_40_female)

log_likelihood_E_fem <- summary(E_sub_40_female)$loglik
log_likelihood_E_fem 
deviance_E_fem <- -2 * (log_likelihood_E_fem[1] - log_likelihood_E_fem[2])
deviance_E_fem
num_parameters_E_fem <- length(coef(E_sub_40_female)) #parameters
num_parameters_E_fem
wald_test_E_fem <- summary(E_sub_40_female)$waldtest
wald_test_E_fem

E_sub_40_male <- coxph(Surv(time, laid_off) ~ dum_40_inp + d.high_edu + leeftijd + partner + d.children + job_type + sector + strata(year), data = df_no_na_male, robust = TRUE)
summary(E_sub_40_male)
cox.zph(E_sub_40_male)

log_likelihood_E_male <- summary(E_sub_40_male)$loglik
log_likelihood_E_male 
deviance_E_male <- -2 * (log_likelihood_E_male[1] - log_likelihood_E_male[2])
deviance_E_male
num_parameters_E_male <- length(coef(E_sub_40_male)) #parameters
num_parameters_E_male
wald_test_E_male <- summary(E_sub_40_male)$waldtest
wald_test_E_male

F_sub_40_female <- coxph(Surv(time, laid_off) ~ dum_40_inp + d.high_edu + leeftijd + partner + d.children + job_type + sector + d.mental_health + d.heavy_alcohol + strata(year), data = df_no_na_fem, robust = TRUE)
summary(F_sub_40_female)
cox.zph(F_sub_40_female)

log_likelihood_F_fem <- summary(F_sub_40_female)$loglik
log_likelihood_F_fem 
deviance_F_fem <- -2 * (log_likelihood_F_fem[1] - log_likelihood_F_fem[2])
deviance_F_fem
num_parameters_F_fem <- length(coef(F_sub_40_female)) #parameters
num_parameters_F_fem
wald_test_F_fem <- summary(F_sub_40_female)$waldtest
wald_test_F_fem

F_sub_40_male <- coxph(Surv(time, laid_off) ~ dum_40_inp + d.high_edu + leeftijd + partner + d.children + job_type + sector + d.mental_health + d.heavy_alcohol + strata(year), data = df_no_na_male, robust = TRUE)
summary(F_sub_40_male)
cox.zph(F_sub_40_male)

log_likelihood_F_male <- summary(F_sub_40_male)$loglik
log_likelihood_F_male 
deviance_F_male <- -2 * (log_likelihood_F_male[1] - log_likelihood_F_male[2])
deviance_F_male
num_parameters_F_male <- length(coef(F_sub_40_male)) #parameters
num_parameters_F_male
wald_test_F_male <- summary(F_sub_40_male)$waldtest
wald_test_F_male

Y_sub_40_female <- coxph(Surv(time, laid_off) ~ dum_40_inp + d.high_edu + leeftijd + partner + d.children + job_type + sector + d.mental_health + d.heavy_alcohol + job_satisf + strata(year), data = df_no_na_fem, robust = TRUE)
summary(Y_sub_40_female)
cox.zph(Y_sub_40_female)

log_likelihood_Y_fem <- summary(Y_sub_40_female)$loglik
log_likelihood_Y_fem 
deviance_Y_fem <- -2 * (log_likelihood_Y_fem[1] - log_likelihood_Y_fem[2])
deviance_Y_fem
num_parameters_Y_fem <- length(coef(Y_sub_40_female)) #parameters
num_parameters_Y_fem
wald_test_Y_fem <- summary(Y_sub_40_female)$waldtest
wald_test_Y_fem

Y_sub_40_male <- coxph(Surv(time, laid_off) ~ dum_40_inp + d.high_edu + leeftijd + partner + d.children + job_type + sector + d.mental_health + d.heavy_alcohol + job_satisf + strata(year), data = df_no_na_male, robust = TRUE)
summary(Y_sub_40_male)

cox.zph(Y_sub_40_female)
log_likelihood_Y_male <- summary(Y_sub_40_male)$loglik
log_likelihood_Y_male 
deviance_Y_male <- -2 * (log_likelihood_Y_male[1] - log_likelihood_Y_male[2])
deviance_Y_male
num_parameters_Y_male <- length(coef(Y_sub_40_male)) #parameters
num_parameters_Y_male
wald_test_Y_male <- summary(Y_sub_40_male)$waldtest
wald_test_Y_male

#Use AIC to calculate best model
# Calculate AIC for each model
aic_A <- AIC(A_sub_40_female)
aic_B <- AIC(B_sub_40_female)
aic_C <- AIC(C_sub_40_female)
aic_D <- AIC(D_sub_40_female)
aic_E <- AIC(E_sub_40_female)
aic_F <- AIC(F_sub_40_female)
aic_Y <- AIC(Y_sub_40_female)

# Create a named vector to store AIC values for easy comparison
aic_values <- c(A = aic_A, B = aic_B, C = aic_C, D = aic_D, E = aic_E, F = aic_F, Y = aic_Y)

# Find the model with the lowest AIC
best_model <- names(aic_values)[which.min(aic_values)]

# Print the AIC values and the best model
aic_values
best_model

#check with BIC
BIC_A <- BIC(A_sub_40_female)
BIC_B <- BIC(B_sub_40_female)
BIC_C <- BIC(C_sub_40_female)
BIC_D <- BIC(D_sub_40_female)
BIC_E <- BIC(E_sub_40_female)
BIC_F <- BIC(F_sub_40_female)
BIC_Y <- BIC(Y_sub_40_female)
bic_values <- c(A = BIC_A, B = BIC_B, C = BIC_C, D = BIC_D, E = BIC_E, F = BIC_F, Y = BIC_Y)
best_model <- names(bic_values)[which.min(bic_values)]
bic_values
best_model

#Use AIC to calculate best model (Male)
aic_A <- AIC(A_sub_40_male)
aic_B <- AIC(B_sub_40_male)
aic_C <- AIC(C_sub_40_male)
aic_D <- AIC(D_sub_40_male)
aic_E <- AIC(E_sub_40_male)
aic_F <- AIC(F_sub_40_male)
aic_Y <- AIC(Y_sub_40_male)

aic_values <- c(A = aic_A, B = aic_B, C = aic_C, D = aic_D, E = aic_E, F = aic_F, Y = aic_Y)

# Find the model with the lowest AIC
best_model <- names(aic_values)[which.min(aic_values)]

# Print the AIC values and the best model
aic_values
best_model

#sensitivity check with bic
BIC_A <- BIC(A_sub_40_male)
BIC_B <- BIC(B_sub_40_male)
BIC_C <- BIC(C_sub_40_male)
BIC_D <- BIC(D_sub_40_male)
BIC_E <- BIC(E_sub_40_male)
BIC_F <- BIC(F_sub_40_male)
BIC_Y <- BIC(Y_sub_40_male)
bic_values <- c(A = BIC_A, B = BIC_B, C = BIC_C, D = BIC_D, E = BIC_E, F = BIC_F, Y = BIC_Y)
best_model <- names(bic_values)[which.min(bic_values)]
bic_values
best_model

remove("aic_A", "aic_B", "aic_C", "aic_D", "aic_E", "aic_F", "aic_Y", "aic_values")
remove("BIC_A", "BIC_B", "BIC_C", "BIC_D", "BIC_E", "BIC_F", "BIC_Y", "BIC_values", best_model)
rm(list = c(
      'A_sub_40_female', 'log_likelihood_A_fem', 'deviance_A_fem', 'num_parameters_A_fem', 'wald_test_A_fem',
      'A_sub_40_male', 'log_likelihood_A_male', 'deviance_A_male', 'num_parameters_A_male', 'wald_test_A_male',
      'B_sub_40_female', 'log_likelihood_B_fem', 'deviance_B_fem', 'num_parameters_B_fem', 'wald_test_B_fem',
      'B_sub_40_male', 'log_likelihood_B_male', 'deviance_B_male', 'num_parameters_B_male', 'wald_test_B_male',
      'C_sub_40_female', 'log_likelihood_C_fem', 'deviance_C_fem', 'num_parameters_C_fem', 'wald_test_C_fem',
      'C_sub_40_male', 'log_likelihood_C_male', 'deviance_C_male', 'num_parameters_C_male', 'wald_test_C_male',
      'D_sub_40_female', 'log_likelihood_D_fem', 'deviance_D_fem', 'num_parameters_D_fem', 'wald_test_D_fem',
      'D_sub_40_male', 'log_likelihood_D_male', 'deviance_D_male', 'num_parameters_D_male', 'wald_test_D_male',
      'E_sub_40_female', 'log_likelihood_E_fem', 'deviance_E_fem', 'num_parameters_E_fem', 'wald_test_E_fem',
      'E_sub_40_male', 'log_likelihood_E_male', 'deviance_E_male', 'num_parameters_E_male', 'wald_test_E_male',
      'F_sub_40_female', 'log_likelihood_F_fem', 'deviance_F_fem', 'num_parameters_F_fem', 'wald_test_F_fem',
      'F_sub_40_male', 'log_likelihood_F_male', 'deviance_F_male', 'num_parameters_F_male', 'wald_test_F_male',
      'Y_sub_40_female', 'log_likelihood_Y_fem', 'deviance_Y_fem', 'num_parameters_Y_fem', 'wald_test_Y_fem',
      'Y_sub_40_male', 'log_likelihood_Y_male', 'deviance_Y_male', 'num_parameters_Y_male', 'wald_test_Y_male'
))
