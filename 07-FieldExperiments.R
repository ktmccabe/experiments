## --------------------------------------------------------------------------------
library(foreign)
wom <- read.dta("data/karpetal.dta")


## --------------------------------------------------------------------------------
## Treatment indicator
table(wom$condition)
class(wom$condition)

## Dependent variable
summary(wom$prop_sd_fem2014)


## --------------------------------------------------------------------------------
## Regression approach
r1 <- lm(prop_sd_fem2014 ~ condition, wom)
round(summary(r1)$coefficients, digits=4)


## ---- eval=F---------------------------------------------------------------------
## install.packages("estimatr")


## ---- warning=F------------------------------------------------------------------
library(estimatr)
r1.cluster <- lm_robust(prop_sd_fem2014 ~ condition, wom,
                        se_type="stata", clusters = county)
round(summary(r1.cluster)$coefficients, digits=4)


## --------------------------------------------------------------------------------
1 - (1 - 0.05)^3


## --------------------------------------------------------------------------------
## extract p-values from the regression
pvals <- summary(r1.cluster)$coefficients[2:4, 4]
pvals

## bonferroni-- very conservative
p.adjust(pvals, method="bonferroni")


## --------------------------------------------------------------------------------
## manual (p-vals over 1 switch to 1)
m <- 3 # number of tests
pvals * 3


## --------------------------------------------------------------------------------
## holm
p.adjust(pvals, method="holm")


## --------------------------------------------------------------------------------
## manual-- requires pval sorting
m <- 3 # number of tests
i <- 1:3 # sequence of rankings
cummax((m + 1 - i) * sort(pvals))


## --------------------------------------------------------------------------------
## Replicating Chapter 5 analyses
## z = Assigned, d = Treated, y= Voted
library(foreign)
voters <- read.dta("data/ggch5.dta")

## 1. effect of experimental assignment on receipt of treatment
ITTd <- lm(treated ~ assigned, voters)

## 2. effect of experimental assignment on whether voted (outcome)
ITT <- lm(voted ~ assigned, voters)

## 3. ratio of treatment effect. compare with Box 5.6
ITT$coefficients[2]/ITTd$coefficients[2]


## ----eval=F----------------------------------------------------------------------
## install.packages("AER")


## ----warning=F, message=F--------------------------------------------------------
library(AER) 
## Function for conducting two-stage least squares regression
## Takes form ivreg(Y ~ d | z, data)
cace <- ivreg(voted ~ treated  | assigned, data = voters)
summary(cace)$coefficients[2,]

## Optional, more conservative standard errors
coeftest(cace, vcovHC(cace))[2,] 

