## ---- eval=F---------------------------------------------------------------------
## fund <- read.csv("fundsub.csv")


## ---- echo=F---------------------------------------------------------------------
fund <- read.csv("data/fundsub.csv")


## --------------------------------------------------------------------------------
## X is the running/forcing variable
## X > 50 means a female candidate won
range(fund$female.margin[fund$male.winner ==0])

## X < 50 means a male candidate won
range(fund$female.margin[fund$male.winner ==1])


## --------------------------------------------------------------------------------
fund$forcing.variable <- -1*(fund$female.margin - 50)


## --------------------------------------------------------------------------------
rsub <- subset(fund, female.margin > 48 & 
                 female.margin < 52 &
                 female.margin !=50)


## --------------------------------------------------------------------------------
uniform <- lm(log.total.raised.candidate ~ male.winner, 
          data = rsub)

coef(uniform)["male.winner"]

## This is equivalent to
mean(rsub$log.total.raised.candidate[rsub$male.winner == 1]) -
  mean(rsub$log.total.raised.candidate[rsub$male.winner == 0])


## --------------------------------------------------------------------------------
linear <- lm(log.total.raised.candidate ~ male.winner*forcing.variable, 
          data = rsub)

coef(linear)["male.winner"]


## ---- eval=F---------------------------------------------------------------------
## install.packages("rdrobust", dependencies = T)


## ---- warning=F------------------------------------------------------------------
library(rdrobust)

## remove ties
rdprep <- subset(fund, female.margin !=50)
rdb <- rdrobust(y=rdprep$log.total.raised.candidate,
                x=rdprep$forcing.variable, 
                kernel = "uniform", 
                p=1, 
                bwselect = "mserd")
summary(rdb)

## rdd effect
rdb$coef[1]


## --------------------------------------------------------------------------------
rdbold <- rdrobust(y=rdprep$log.total.raised.candidate,
                x=rdprep$forcing.variable, 
                kernel = "uniform", 
                p=1, 
                h=2)
rdbold$coef[1]


## --------------------------------------------------------------------------------
## remove ties
rdb2 <- rdrobust(y=rdprep$log.total.raised.candidate,
                x=rdprep$forcing.variable, 
                kernel = "triangular", # implements a type of weighting 
                p=2, # shifts to quadratic 
                bwselect = "mserd")
summary(rdb2)

## rdd effect
rdb2$coef[1]


## ---- echo=F---------------------------------------------------------------------
plot(x=1:3,
     y= c(coef(linear)["male.winner"], 
          rdb$coef[1],
          rdb2$coef[1]), 
     ylim = c(-1, 2), 
     xlim = c(.8, 3.2),
     pch = 15, 
     xaxt= "n", xlab = "Bandwidths", main = "RDD Estimates",
     ylab = "Effect on Log Total Raised Candidate")
lines(c(1,1), confint(linear)[2,])
lines(c(2,2), rdb$ci[1,])
lines(c(3,3), rdb2$ci[1,])

abline(h=0, col = "red")
text(1:3, -.5, c("+/- 2",  "+/- 7 \n Optimal", "Triangular \n Quadratic"), cex = .6)


## ---- eval=F---------------------------------------------------------------------
## fit.sorting <- lm(log.total.dist.last ~ male.winner*forcing.variable,
##                   data=rsub)
## summary(fit.sorting)


## --------------------------------------------------------------------------------
rdplot(y= rdprep$log.total.raised.candidate,
               x=rdprep$female.margin,
               kernel="uniform",  p=1, c=50, 
               title="RD Plot: Candidate Fundraising",
      y.label="Fundraising at time t+1", 
      x.label="Female Vote Share in Election at time t")


## --------------------------------------------------------------------------------
rdplot(y= rdprep$log.total.raised.candidate,
               x=rdprep$female.margin,
               kernel="triangular",  p=2, c=50,
               title="RD Plot: Candidate Fundraising",
      y.label="Fundraising at time t+1", 
      x.label="Female Vote Share in Election at time t")

