# Indlæser data
data(UCBAdmissions)
ucb <- as.data.frame(UCBAdmissions)
head(ucb)


# Lav three way contengency table
tbl <- xtabs(Freq ~ Admit + Gender + Dept, data = ucb)

# Summer over institutterne 
lambda_AG <- margin.table(tbl, margin = c(1,2))

# Find andele for hver indgang
(pi_AG <- lambda_AG/sum(lambda_AG))

# Find odds for mænd
odds_M <- pi_AG[1,1]/pi_AG[2,1]
odds_F <- pi_AG[1,2]/pi_AG[2,2]
(odds_R <- odds_M/odds_F)

fit <- glm(Freq ~ )