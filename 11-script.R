#################################
### Contingency tables


#############################
# 1D Table
ratcounts <- c(23, 36, 31)
door <- factor(1:3)
ratfit <- glm(ratcounts ~ door, poisson)
summary(ratfit)
## Check parameter estimates
log(ratcounts/ratcounts[1])
log(ratcounts[1])


## reduced model
ratfit0 <- glm(ratcounts ~ 1, poisson)
fitted(ratfit0)

## testing
anova(ratfit0, ratfit, test="LRT")
anova(ratfit0, ratfit, test="Rao")

## compare classical chi^2 test
chisq.test(ratcounts)

## (Direct compute of chi^2)
sum((ratcounts-30)^2/30)

#############################
### Twoway table

(flu <- cbind(
  expand.grid(
    Flu=c("y","n"), 
    Shots=c("0","1","2")), 
  Freq=c(24,289,9,100, 13,565)))
n <- sum(flu$Freq)

model.matrix(~Flu+Shots, flu)
flufit <- glm(Freq~Flu+Shots, flu, family=poisson)
summary(flufit)
fitted(flufit)
fitted(flufit)/n
(phat <- xtabs(fitted(flufit)/n~Flu+Shots, flu))
(prow <- margin.table(phat,1))
(pcol <- margin.table(phat,2))
outer(prow, pcol)

## test
flufitfull <- glm(Freq~Flu*Shots, flu, family=poisson)
predict(flufitfull, type = "response") # compare with flu
flu
anova(flufit, flufitfull, test="LRT")
anova(flufit, flufitfull, test="Rao")

## Compare chi^2
flutbl <- xtabs(Freq~Flu+Shots, flu)
chisq.test(flutbl)

###
model.matrix(flufit)
model.matrix(flufitfull)

#########################################
## homogenitet og uafhængighed

tbl <- xtabs(Freq ~ Flu + Shots, data = flu)
tbl
prop.table(tbl, margin = 1)  # Andel inden for hver gruppe

## 1. Poisson-regression
## H0: Ingen sammenhæng mellem Shots og Flu
# (når Poissonmodellen som en model for homogenitet
# i en kontingenstabel, så giver Shots-termen et frit
# rækkeintercept for hver af tre binomialfordeling over
# yes / no. Rollerne for Shots og Flu kan også fortolkes
# omvendt.)
fit0 <- glm(Freq ~ Shots + Flu,
            family = poisson, data = flu)

## Mættet model (alternativet)
fit1 <- glm(Freq ~ Shots * Flu,
            family = poisson, data = flu)

## Forventede antal under H0
fitted(fit0)
addmargins(tbl)

## LRT
anova(fit0, fit1, test = "Chisq")

## Scoretest
anova(fit0, fit1, test = "Rao")

## 2. Klassisk test for uafhængighed
chisq.test(tbl, correct = FALSE)

## 3. Homogenitet: Tre binomialfordelinger
## Antal influenza-tilfælde og antal uden influenza
yes <- tbl["y",]
no  <- tbl["n",]

## H0: Samme influenza-sandsynlighed i alle grupper
bin0 <- glm(cbind(yes, no) ~ 1,
            family = binomial)

## Alternativ: Forskellig sandsynlighed
bin1 <- glm(cbind(yes, no) ~ factor(colnames(tbl)),
            family = binomial)

## LRT for homogenitet (tester om hypotesen om, at de tre binomialfordelinger er ens)
anova(bin0, bin1, test = "Chisq")

## 4. Sammenlign teststørrelser
c(
  Poisson_LRT = deviance(fit0) - deviance(fit1),
  Binomial_LRT = deviance(bin0) - deviance(bin1),
  Pearson = unname(chisq.test(tbl, correct = FALSE)$statistic),
  Poisson_Score = sum(residuals(fit0, type = "pearson")^2)
)



##########################################
### Log-linear models: Three-way table
### Boliga data
##########################################

## Data: Three categorical variables
## Sold  = Sold within 90 days (TRUE/FALSE)
## Pcode = Postal code
## Type  = Type of house

boliga <- read.csv("forsale.csv")

boliga$Postal.Code <- factor(boliga$Postal.Code)

## Construct the three-way contingency table
tbl <- xtabs(
  ~ Sold.Within.90.Days + Postal.Code + Type.of.House,
  data = boliga
)

## Convert table to data frame for Poisson regression
boliga.aggr <- as.data.frame(tbl)
names(boliga.aggr) <- c("Sold", "Pcode", "Type", "Freq")

print(tbl)
print(boliga.aggr)


##########################################
### 1. From intensities to probabilities
##########################################

## Saturated log-linear model:
## All main effects and interactions are included.
##
## log(lambda_ijk) =
##   beta0 + effects of Sold, Pcode, Type
##   + all two-way interactions
##   + three-way interaction

boliga.full <- glm(
  Freq ~ Sold * Pcode * Type,
  family = poisson,
  data = boliga.aggr
)

## Fitted intensities lambda_ijk
lambda.full <- fitted(boliga.full)

## Store fitted intensities as a three-way table.
## Using array() is safe here because as.data.frame(tbl)
## preserves the cell ordering.

lambda.full.tbl <- tbl
lambda.full.tbl[] <- lambda.full

## Joint probabilities:
## pi_ijk = lambda_ijk / lambda_...
pi.full <- lambda.full.tbl / sum(lambda.full.tbl)

sum(pi.full)  # 1

## Conditional probabilities:
## P(Sold, Type | Pcode)
##
## Normalize separately within each postal code.

pi.conditional <- prop.table(lambda.full.tbl, margin = 2)

pi.conditional[, "2720", ]

## Verify that probabilities sum to one
## within each postal code.

apply(pi.conditional, 2, sum)


##########################################
### 2. Conditional independence
##########################################

## Hypothesis:
##
## Sold independent of Type given Pcode
##
## Sold _||_ Type | Pcode
##
## Graph:
##
## Sold --- Pcode --- Type
##
## Maximal cliques:
## {Sold, Pcode} and {Pcode, Type}
##
## Log-linear model:
##
## log(lambda_ijk) = f_SP(i,j) + f_PT(j,k)
##
## No Sold:Type interaction
## No Sold:Pcode:Type interaction

boliga.cond.indep <- glm(
  Freq ~ Sold * Pcode + Pcode * Type,
  family = poisson,
  data = boliga.aggr
)

summary(boliga.cond.indep)

## Compare conditional independence with saturated model.
## The LRT tests both missing interaction terms jointly.

anova(
  boliga.cond.indep,
  boliga.full,
  test = "LRT"
)

## Score test for the same hypothesis

anova(
  boliga.cond.indep,
  boliga.full,
  test = "Rao"
)

## Pearson chi-square goodness-of-fit statistic

X2 <- sum(residuals(boliga.cond.indep,
                    type = "pearson")^2)

df <- df.residual(boliga.cond.indep)

c(X2 = X2, df = df,
  p.value = pchisq(X2, df, lower.tail = FALSE))


##########################################
### 3. Factorization of intensities
##########################################

## Under conditional independence:
##
## lambda_ijk =
##   lambda_ij. * lambda_.jk / lambda_.j.
##
## We can verify this identity using fitted values.

lambda.ci <- tbl
lambda.ci[] <- fitted(boliga.cond.indep)

## Compute fitted marginal intensities

lambda.SP <- margin.table(lambda.ci, c(1, 2)) # sum over type
lambda.PT <- margin.table(lambda.ci, c(2, 3)) # sum over sold
lambda.P  <- margin.table(lambda.ci, 2)

## Reconstruct the intensities from the factorization

lambda.reconstructed <- lambda.ci

for (i in seq_len(dim(tbl)[1])) {
  for (j in seq_len(dim(tbl)[2])) {
    for (k in seq_len(dim(tbl)[3])) {
      
      lambda.reconstructed[i, j, k] <-
        lambda.SP[i, j] * lambda.PT[j, k] /
        lambda.P[j]
      
    }
  }
}

## The two arrays should be identical
## up to numerical precision.

all.equal(
  as.vector(lambda.ci),
  as.vector(lambda.reconstructed)
)


##########################################
### 4. Conditional independence
###    expressed using probabilities
##########################################

## Under conditional independence:
##
## P(Sold, Type | Pcode)
##   = P(Sold | Pcode) * P(Type | Pcode)
##
## We verify this for postal code 2720.

pcode <- "2720"

## Joint conditional distribution
p.ST <- prop.table(lambda.ci[, pcode, ])

## Conditional marginal distributions

p.S <- rowSums(p.ST)
p.T <- colSums(p.ST)

## Product of conditional marginals

p.product <- outer(p.S, p.T)

dimnames(p.product) <- dimnames(p.ST)

print(p.ST)
print(p.product)

max(abs(unname(p.ST) - unname(p.product)))

## The equality follows from the log-linear model


##########################################
### 5. Odds-ratios and interactions
##########################################

## For a 2x2 table:
##
##           Type=1   Type=2
## Sold=1       a        b
## Sold=2       c        d
##
## OR = (a*d)/(b*c)
##
## log(OR) = log(a) + log(d) - log(b) - log(c)
##
## This definition is invariant to normalization:
## We obtain the same OR using lambda or pi.

odds.ratio <- function(x) {
  stopifnot(all(dim(x) == c(2, 2)))
  (x[1, 1] * x[2, 2]) /
    (x[1, 2] * x[2, 1])
}

## Compute conditional odds-ratios for each postal code

conditional.OR <- function(lambda.table) {
  sapply(dimnames(lambda.table)[[2]], function(pc) {
    odds.ratio(lambda.table[, pc, ])
  })
}

## Saturated model:
## OR can differ between postal codes.

OR.full <- conditional.OR(lambda.full.tbl)
OR.full

## Conditional independence model:
## OR = 1 for every postal code.

OR.ci <- conditional.OR(lambda.ci)
OR.ci



##########################################
### 6. No three-way interaction
##########################################

## Model with all two-way interactions:
##
## log(lambda_ijk) =
##   main effects + all two-way interactions
##
## No Sold:Pcode:Type interaction
##
## R formula:
## ~ (Sold + Pcode + Type)^2
##
## This is a hierarchical log-linear model.
##
## It is NOT a graphical log-linear model:
## All three pairs are connected, so the graph
## is complete and has a three-variable clique.

boliga.2way <- glm(
  Freq ~ (Sold + Pcode + Type)^2,
  family = poisson,
  data = boliga.aggr
)

## Compare with saturated model:
## Test for absence of three-way interaction.

anova(
  boliga.2way,
  boliga.full,
  test = "LRT"
)

lambda.2way <- tbl
lambda.2way[] <- fitted(boliga.2way)

## Important interpretation:
##
## No three-way interaction implies that
## the conditional odds-ratio between Sold and Type
## is the same for every postal code.
##
## It does NOT imply that this odds-ratio is 1.

OR.2way <- conditional.OR(lambda.2way)
OR.2way

## equality of conditional odds-ratios

## Compare all three models

OR.comparison <- data.frame(
  Pcode = names(OR.full),
  Saturated = unname(OR.full),
  Conditional.independence = unname(OR.ci),
  No.threeway = unname(OR.2way)
)

print(OR.comparison)


##########################################
### 7. Graphical vs hierarchical models
##########################################

## Conditional independence model:
##
## Sold --- Pcode --- Type
##
## Maximal cliques:
## {Sold, Pcode}, {Pcode, Type}
##
## Formula:
## Sold*Pcode + Pcode*Type
##
## This model is both hierarchical and graphical.


## No three-way interaction model:
##
## Sold ----- Pcode
##   \         /
##    \       /
##       Type
##
## All three pairwise interactions are present.
##
## However, the three-way interaction is absent.
##
## This model is hierarchical but NOT graphical.
##
## A complete graph has {Sold, Pcode, Type}
## as a maximal clique, and therefore allows
## the three-way interaction.


##########################################
### 8. Connection to logistic regression
##########################################

## We now treat Sold as the binary response.
##
## The log-linear model with all two-way
## interactions implies:
##
## logit P(Sold=TRUE | Pcode, Type)
##   = gamma0 + gamma_Pcode + gamma_Type
##
## In particular, there is no Pcode:Type
## interaction in the logistic model.

logistic.fit <- glm(
  Sold.Within.90.Days ~ Postal.Code + Type.of.House,
  family = binomial,
  data = boliga
)

summary(logistic.fit)

## Obtain the conditional probabilities from the
## log-linear Poisson model:
##
## P(Sold=TRUE | Pcode, Type)
##   = lambda_TRUE,j,k /
##     (lambda_FALSE,j,k + lambda_TRUE,j,k)

p.poisson <- prop.table(lambda.2way, margin = c(2, 3))

## Extract the TRUE level of Sold

p.poisson.true <- p.poisson["TRUE", , ]

## Obtain fitted probabilities from logistic regression
## for each combination of Pcode and Type.

newdata <- expand.grid(
  Postal.Code = levels(boliga$Postal.Code),
  Type.of.House = unique(boliga$Type.of.House)
)

newdata$p.logistic <- predict(
  logistic.fit,
  newdata = newdata,
  type = "response"
)

## Match predictions to the same ordering
## as the Pcode x Type table.

p.logistic <- xtabs(
  p.logistic ~ Postal.Code + Type.of.House,
  data = newdata
)

## The two sets of probabilities should agree.

unname(as.matrix(p.poisson.true)) - unname(as.matrix(p.logistic))



## Why?
##
## The Pcode:Type interaction in the Poisson model
## gives a free total intensity for every combination
## of Pcode and Type.
##
## These total intensities are variation-independent
## of the logistic regression parameters.
##
## Consequently, maximizing the Poisson likelihood
## gives the same estimates of the conditional
## probabilities as binomial logistic regression.

