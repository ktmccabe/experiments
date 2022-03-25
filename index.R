## ---- echo=FALSE, warning=FALSE, message=FALSE-----------------------------------
library("vembedr")
library(knitr)

embed_url("https://www.youtube.com/watch?v=ulIv0NiVTs4")


## --------------------------------------------------------------------------------
5 + 3

5 - 3

5^2

5 * 3

5/3

(5 + 3) * 2


## ---- eval=F---------------------------------------------------------------------
## ## Example of where my directory was
## getwd()


## ---- eval=FALSE-----------------------------------------------------------------
## ## Example of setting the working directory using setwd().
## ## Your computer will have your own file path.
## setwd("/Users/ktmccabe/Dropbox/Rutgers Teaching/")


## --------------------------------------------------------------------------------
5 + 3


## --------------------------------------------------------------------------------
## Example
sum53 <- 5 + 3 # example of assigning an addition calculation


## --------------------------------------------------------------------------------
sum53 <- 5 + 3


## --------------------------------------------------------------------------------
sum53


## --------------------------------------------------------------------------------
sum53 - 2


## --------------------------------------------------------------------------------
ten <- 5 + 5
two <- 1 + 1
ten / two


## --------------------------------------------------------------------------------
mccabe <- "professor for this course"
mccabe


## --------------------------------------------------------------------------------
somenumbers <- c(3, 6, 8, 9)
somenumbers


## --------------------------------------------------------------------------------
sum53 <- 5 + 3


## ---- error=TRUE-----------------------------------------------------------------
Sum53


## ---- error=TRUE-----------------------------------------------------------------
mccabe / 2


## ---- eval=F---------------------------------------------------------------------
## ## The turnout dataset is available in the week 2 Canvas module
## turnout <- read.csv("turnout.csv")


## ---- eval=F---------------------------------------------------------------------
## turnout <- read.csv("turnout.csv", stringsAsFactors = T)


## ---- eval=F---------------------------------------------------------------------
## install.packages("rio", dependencies = T)


## ---- eval=F---------------------------------------------------------------------
## library(rio)
## turnout <- import("turnout.csv")


## --------------------------------------------------------------------------------
turnout <- read.csv("https://raw.githubusercontent.com/ktmccabe/teachingdata/main/turnout.csv")


## --------------------------------------------------------------------------------
class(turnout)


## ---- eval=FALSE-----------------------------------------------------------------
## View(turnout)


## ---- eval=TRUE------------------------------------------------------------------
head(turnout)


## --------------------------------------------------------------------------------
turnout$year


## --------------------------------------------------------------------------------
median(turnout$year)


## ---- eval=F---------------------------------------------------------------------
## install.packages("rmarkdown")
## install.packages("knitr")


## ---- eval=T, echo=FALSE, include=FALSE------------------------------------------
load("data/status.RData")


## ----eval=F----------------------------------------------------------------------
## load("status.RData")


## --------------------------------------------------------------------------------
head(status)


## --------------------------------------------------------------------------------
library(foreign)
statusdta <- read.dta("status.dta")


## ---- eval=F---------------------------------------------------------------------
## install.packages("rio", dependencies=T)


## ---- warning=F, message=F-------------------------------------------------------
library(rio)
statusrio <- import("status.dta")


## --------------------------------------------------------------------------------
statuscsv <- read.csv("status.csv")


## ---- warning=F, message=F-------------------------------------------------------
library(tidyverse)
statuscsv2 <- read_csv("status.csv")


## --------------------------------------------------------------------------------
## Use import to load the csv file
## Note: remember to use the appropriate library command



## ---- eval=F---------------------------------------------------------------------
## View(status)


## --------------------------------------------------------------------------------
class(status)


## ---- eval=F---------------------------------------------------------------------
## status$condition


## --------------------------------------------------------------------------------
## Access the values in the econcon column



## --------------------------------------------------------------------------------
range(status$econcon)


## --------------------------------------------------------------------------------
## Find the mean of the econcon column



## --------------------------------------------------------------------------------
range(status$econcon, na.rm=T)


## ---- eval=F---------------------------------------------------------------------
## table(status$econcon, na.rm=T)


## --------------------------------------------------------------------------------
table(status$condition)


## --------------------------------------------------------------------------------
## Use the table command to indicate how many male and female respondents



## ---- eval=F---------------------------------------------------------------------
## ## for each observation, does the value of condition equal "Self-Esteem"?
## status$condition == "Self-Esteem"


## ---- eval=F---------------------------------------------------------------------
## status$male == 1


## --------------------------------------------------------------------------------
## Does condition equal Placebo?



## --------------------------------------------------------------------------------
mean(status$econcon, na.rm=T)


## --------------------------------------------------------------------------------
mean(status$econcon[status$condition == "Social Approval"], na.rm=T)


## --------------------------------------------------------------------------------
## Find the mean econcon for those in the Placebo condition



## ---- eval=F---------------------------------------------------------------------
## status$condition == "Social Approval" & status$male == 1


## ---- eval=F---------------------------------------------------------------------
## status$condition == "Social Approval" | status$condition == "Placebo"


## --------------------------------------------------------------------------------
mean(status$econcon[status$condition == "Social Approval" 
                    & status$male == 1], na.rm=T)


## --------------------------------------------------------------------------------
### Mean econcon for respondents in Social Approval or Placebo condition




## --------------------------------------------------------------------------------
meanSocApp <- mean(status$econcon[status$condition == "Social Approval"],
                   na.rm=T)
meanSocApp


## --------------------------------------------------------------------------------
meanPlacebo <- mean(status$econcon[status$condition == "Placebo"],
                   na.rm=T)
meanPlacebo


## --------------------------------------------------------------------------------
meanConcrete <- mean(status$econcon[status$condition == "Concrete"],
                   na.rm=T)
meanConcrete


## --------------------------------------------------------------------------------
maleonly <- subset(status, male ==1)


## ----eval=F----------------------------------------------------------------------
## View(maleonly)


## --------------------------------------------------------------------------------
## Using maleonly dataframe, find mean economic conservatism


## Note the value's equivalence to
mean(status$econcon[status$male == 1], na.rm=T)


## --------------------------------------------------------------------------------
meanConcrete
meanPlacebo
meanSocApp


## --------------------------------------------------------------------------------
conditionmeans <- c(meanConcrete, meanPlacebo, meanSocApp)
conditionmeans


## --------------------------------------------------------------------------------
plot(x = c(1,2,3),
     y = conditionmeans)


## --------------------------------------------------------------------------------
plot(x = c(1,2,3),
     y = conditionmeans,
     xlim = c(.5, 3.5),
     ylim=c(.6, .75),
     xlab= "Experimental Condition",
     ylab= "Mean Economic Conservatism",
     main = "Economic conservatism by condition",
     xaxt="n")
axis(1, c(1,2,3), c("Concrete", "Placebo", "Social\n Approval"),
     tick=F)

