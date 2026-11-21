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




















