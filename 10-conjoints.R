## ---- include=F, eval=F----------------------------------------------------------
## gender <- c("Female", "Male")
## language <- c("Fluent English", "Broken English", "No English")
## country <- c("Mexico", "China", "Sudan")
## 
## conjointtoy <- data.frame(Respondent= c(1,1,1,1,2,2,2,2,3,3,3,3),
##                           Task = c(rep(c(1,1,2,2), 3)),
##                           Chosen= c(1,0,0,1,0,1, 0,1,1,0,1,0),
##                           Gender = sample(gender, size=12, replace=T),
##                           Country = sample(country, size=12, replace=T),
##                           Language = sample(language, size=12, replace=T))
## save(conjointtoy, file="data/conjointtoy.RData")


## ---- eval=T, include=F----------------------------------------------------------
load("data/conjointtoy.RData")
conjointtoy


## ---- eval=F---------------------------------------------------------------------
## load("conjointtoy.RData")


## --------------------------------------------------------------------------------
conjointtoy


## --------------------------------------------------------------------------------
prob.fluent <- mean(conjointtoy$Chosen[conjointtoy$Language == "Fluent English"])
prob.no <- mean(conjointtoy$Chosen[conjointtoy$Language == "No English"])

prob.fluent - prob.no


## --------------------------------------------------------------------------------
conjointtoy$Language <- as.factor(conjointtoy$Language)
conjointtoy$Language <- relevel(conjointtoy$Language, ref="No English")


## ---- warning=F, message =F------------------------------------------------------
library(estimatr)
fit <- lm_robust(Chosen ~ Language, data = conjointtoy,
                 se_type="stata", clusters=Respondent)
summary(fit)


## ---- include=F------------------------------------------------------------------
library(foreign)

conj <- read.dta("data/conjoint.dta")


## ---- eval=F---------------------------------------------------------------------
## library(foreign)
## 
## conj <- read.dta("conjoint.dta")


## --------------------------------------------------------------------------------
head(conj)


## --------------------------------------------------------------------------------
## How many observations?
nrow(conj)

## How many unique participants?
length(unique(conj$CaseID)) # 1396 

## How many contests? (tasks)
table(conj$contest_no) # 5

## Education features
table(conj$FeatEd)

## Job features
table(conj$FeatJob)

## Lang features
table(conj$FeatLang)

## Outcome: Choice Task 
table(conj$Chosen_Immigrant)


## --------------------------------------------------------------------------------
## Make outcome numeric
conj$Chosen_Immigrant <- ifelse(conj$Chosen_Immigrant == "Yes", 1, 0)
fit.lang <- lm_robust(Chosen_Immigrant ~ FeatLang, 
                      data = conj, se_type="stata",
                      clusters=CaseID)
summary(fit.lang)


## --------------------------------------------------------------------------------
table(conj$FeatJob,conj$FeatEd)


## ---- warning=F, message=F-------------------------------------------------------
library(cregg)
fit.cregg <- amce(Chosen_Immigrant ~ FeatLang,data= conj, id=~CaseID)
fit.cregg
plot(fit.cregg)

