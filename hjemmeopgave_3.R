# Indlæser data
data(UCBAdmissions)
ucb <- as.data.frame(UCBAdmissions)
head(ucb)


# Lav three way contengency table
tbl <- xtabs(Freq ~ Admit + Gender + Dept, data = ucb)

# Summer over institutterne 
lambda_AG <- margin.table(tbl, margin = c(1,2))

# Find andele for hver indgang
pi_AG <- lambda_AG/sum(lambda_AG)


