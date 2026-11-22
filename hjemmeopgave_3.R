#### Opgave A 1 ####
# Indlæser data
data(UCBAdmissions)
ucb <- as.data.frame(UCBAdmissions)
head(ucb)

# Lav three way contengency table
tbl <- xtabs(Freq ~ Admit + Gender + Dept, data = ucb)

# Summer over institutterne 
(lambda_AG <- margin.table(tbl, margin = c(1,2)))

# Find andele for hver indgang
(pi_AG <- lambda_AG/sum(lambda_AG))

# Find odds for mænd
odds_M <- pi_AG[1,1]/pi_AG[2,1]
odds_F <- pi_AG[1,2]/pi_AG[2,2]
(odds_R <- odds_M/odds_F)

# Omdan tabel til dataframe
ucb_AG <- as.data.frame(lambda_AG)

# Fit poisson model med log link både additiv og med vekselvirkning
fit_additiv <- glm(Freq ~ Admit + Gender, data = ucb_AG, family = poisson)
fit_veksel  <- glm(Freq ~ Admit + Gender + Admit*Gender, data = ucb_AG, family = poisson)

# Likelihood rator for om der er en vekselvirkning
anova(fit_additiv, fit_veksel, test = "LRT")

#### Opgave A 2 ####

# Fit modelen
fit_ucb <- glm(Freq ~ Admit + Gender + Dept + Admit*Dept + Gender*Dept, data = ucb, family = poisson) 
fit_ucb <- glm(Freq ~ Admit + Gender + Dept + Admit:Dept + Gender:Dept, data = ucb, family = poisson) 
fit_ucb <- glm(Freq ~ Admit*Dept + Gender*Dept, data = ucb, family = poisson) 

#### Opgave A 4 ####
# Fitter model med alle tovejs vekselvirkninger og den mættede model
fit_all_veksel <- glm(Freq ~ Admit*Dept + Gender*Dept + Admit*Gender, data = ucb, family = poisson) 
fit_saturated  <- glm(Freq ~ Admit*Dept*Gender, data = ucb, family = poisson)

# Undersøger om trevejsvekselvirkningen er signifikant med LRT test
anova(fit_all_veksel, fit_saturated, test = "LRT")

# Vi beregner odds ratio mellem optagelse og køn for alle institutter
odds_R_temp <- numeric(6)
for (i in 1:6){
odds_M_temp <- tbl[1,1,i]/tbl[2,1,i]
odds_F_temp <- tbl[1,2,i]/tbl[2,2,i]
odds_R_temp[i] <- odds_M_temp/odds_F_temp
}
odds_R_temp

hat_lambda_temp_11 <- numeric(6)
hat_lambda_temp_21 <- numeric(6)
hat_lambda_temp_12 <- numeric(6)
hat_lambda_temp_22 <- numeric(6)

for (i in 1:6){
df_temp_11 <- data.frame(
  Admit  = "Admitted",
  Gender = "Male",
  Dept   = (levels(ucb$Dept))[i]
)

df_temp_21 <- data.frame(
  Admit  = "Rejected",
  Gender = "Male",
  Dept   = (levels(ucb$Dept))[i]
)

df_temp_12 <- data.frame(
  Admit  = "Admitted",
  Gender = "Female",
  Dept   = (levels(ucb$Dept))[i]
)

df_temp_22 <- data.frame(
  Admit  = "Rejected",
  Gender = "Female",
  Dept   = (levels(ucb$Dept))[i]
)

hat_lambda_temp_11[i] <- predict(fit_all_veksel, newdata = df_temp_11, type = "response")
hat_lambda_temp_21[i] <- predict(fit_all_veksel, newdata = df_temp_21, type = "response")
hat_lambda_temp_12[i] <- predict(fit_all_veksel, newdata = df_temp_12, type = "response")
hat_lambda_temp_22[i] <- predict(fit_all_veksel, newdata = df_temp_22, type = "response")
}
hat_lambda_11 <- sum(hat_lambda_temp_11)
hat_lambda_21 <- sum(hat_lambda_temp_21)
hat_lambda_12 <- sum(hat_lambda_temp_12)
hat_lambda_22 <- sum(hat_lambda_temp_22)

fitted_OR <- (hat_lambda_11/hat_lambda_21)/(hat_lambda_12/hat_lambda_22)





pchisq(20.204, df = 5, lower.tail = FALSE)














