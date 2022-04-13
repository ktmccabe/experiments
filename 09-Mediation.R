## Mediation

setwd()


## Gadarian and van der Voort example
library(rio)
gad <- import("gadvan.dta")


## Mediator as outcome, regressed on treatment
model.m <- lm(Disgust ~ disgust_treat + positive_treat + dss, data = gad)

## Y as outcome, controlling on mediator and treatment
model.y <- lm(adoption ~ disgust_treat + positive_treat 
                + Disgust + angry + PID + ideology, data=gad)


install.packages("mediation")


library(mediation)
medfit <- mediate(model.m=model.m, model.y=model.y,
                  treat="disgust_treat", mediator="Disgust")


## summary of results
summary(medfit)


## Check with Baron-Kenny
c.1 <- coef(model.m)["disgust_treat"] # Coef for treatment ("a" from before)
c.2 <- coef(model.y)["Disgust"] # Coef for mediator ("b" from before)
## Mediation Effect (indirect effect)
c.1 * c.2


## plotting mediation effects
plot(medfit)


## Extension (time permitting, available in course notes)

## Load data
tw <- read.csv("tw-replication-dvn.csv", stringsAsFactors = FALSE)


## Outcome: support for strike
tw$favor_binary <- ifelse(tw$favor_attack < 3, 1, 0)
table(tw$favor_binary)

## Treatment indicator
table(tw$dem_dummy)

## Mediator dummy- reverse code it 
## so threat=0 means presence of threat
tw$threat_dummy <- 1-tw$threat_dummy
table(tw$threat_dummy)


## Interaction model
fit <- lm(favor_binary ~ dem_dummy*threat_dummy, data = tw)


acde <- coef(fit)["dem_dummy"]
acde

ate <- coef(fit)["dem_dummy"] + coef(fit)["dem_dummy:threat_dummy"]
ate

predict(fit, data.frame(dem_dummy = 1, threat_dummy = 1)) -
  predict(fit, data.frame(dem_dummy = 0, threat_dummy = 1)) 

aee <- coef(fit)["dem_dummy:threat_dummy"]

par(mar = c(4.1, 0.1, 2.1, 0.1))
plot(x = c(ate, acde, aee), y = 1:3, 
     xlim = c(-0.25, 0.25), 
     ylim = c(0, 4), bty="n",
     yaxt = "n", pch=20,
     ylab = "", main = "Tomz and Weeks Extension", 
     xlab = "Effect on Supporting Preemptive Strike")
abline(v = 0, col = "grey50", lty = 2)
text(x = -0.01, y = 3, "Eliminated Effect = ATE - ACDE", pos=2)
text(x = 0.15, y = 2, "Effect of Democracy \n Fixing Threat (ACDE)")
text(x = 0.15, y = 1, "Effect of Democracy \n without Fixing Threat (ATE)")
lines(confint(fit)[2,], c(2,2))
lines(confint(fit)[4,], c(3,3))
se.sum <-sqrt(vcov(fit)[2,2] + vcov(fit)[4,4] + 2*vcov(fit)[2,4])
lines( c(ate - 1.96*se.sum, ate + 1.96*se.sum), c(1,1))

