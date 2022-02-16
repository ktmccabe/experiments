## --------------------------------------------------------------------------------------------
## install.packages("rio", dependencies=T)
library(rio)
banks <- import("data/banksstudy2.dta")


## --------------------------------------------------------------------------------------------
banks <- subset(banks, is.na(baddata)==T)


## --------------------------------------------------------------------------------------------
d.i.m <- mean(banks$blackdon[banks$angvcon == 1], na.rm=T) -
  mean(banks$blackdon[banks$angvcon == 0], na.rm=T)
t.results <- t.test(banks$blackdon[banks$angvcon == 1],
                    banks$blackdon[banks$angvcon == 0])
ci <- t.results$conf.int


## --------------------------------------------------------------------------------------------
d.i.m2 <- mean(banks$blackdon[banks$hopevcon == 1], na.rm=T) -
  mean(banks$blackdon[banks$hopevcon == 0], na.rm=T)
t.results2 <- t.test(banks$blackdon[banks$hopevcon == 1],
                    banks$blackdon[banks$hopevcon == 0])
ci2 <- t.results2$conf.int


## ---- warning=F, message=F-------------------------------------------------------------------
angercontrol <- subset(banks, angvcon == 0 | angvcon ==1)

## remove missing data
angercontrol <- subset(angercontrol, is.na(blackdon) ==F)

## install.packages("ri2", dependencies=T)
library(ri2)

## Declare randomization
declaration <- declare_ra(N=nrow(angercontrol), prob=.5)

## Estimate the average treatment effect
set.seed(1215)
ri2_out <- conduct_ri(
  formula = blackdon ~ angvcon,
  assignment = "angvcon",
  declaration = declaration,
  sharp_hypothesis = 0,
  data = angercontrol
)





## ---- warning=F, message=F-------------------------------------------------------------------
plot(ri2_out)


## --------------------------------------------------------------------------------------------
summary(ri2_out)


## --------------------------------------------------------------------------------------------
estimate <- tidy(ri2_out)$estimate
nsims <- length(ri2_out$sims_df$est_sim)
simstimates <- ri2_out$sims_df$est_sim
## Two-tailed p-value
length(simstimates[abs(simstimates) >= abs(estimate)])/nsims


## ---- echo=F---------------------------------------------------------------------------------

t <- seq(-3, 3, .1)
plot(t, dt(t, df=268.77), ylab="Density", type="l")
abline(v=c(t.results$statistic, -t.results$statistic), col="red", lty=2)
polygon(c(-3,seq(-3, -t.results$statistic, .05),-t.results$statistic),
        c(0,dt(seq(-3, -t.results$statistic, .05), df=268.77),0),
        col="pink")
polygon(c(t.results$statistic, seq(t.results$statistic, 3,.05),3),
        c(0,dt(seq(t.results$statistic,3, .05), df=268.77),0),
        col="pink")


## ---- echo=F---------------------------------------------------------------------------------
plot(0,
     xlim=c(1, 10), # x-axis limits
     ylim=c(1,100), # y-axis limits
     type = "n", # n means blank
     xlab = "x-axis label",
     ylab = "y-axis label",
     cex.main = .7, cex.lab = .7, cex.axis=.6, # size of title, label, axis text
     main = "title of plot")


## --------------------------------------------------------------------------------------------
## Plot 
plot(x = c(1, 2), 
     y = c(d.i.m, d.i.m2))


## --------------------------------------------------------------------------------------------
plot(x = c(1, 2), 
     y = c(d.i.m, d.i.m2),
     xlim=c(.5, 2.5),
     ylim = c(-1, 2),
     main="Average Treatment Effects on Donations to Black Organizations",
     cex.main=.8,
     ylab="Difference in Donation Amount",
     xlab= "Treatment Comparison",
     cex.lab=.8)



## --------------------------------------------------------------------------------------------
plot(x = c(1, 2), 
     y = c(d.i.m, d.i.m2),
     xlim=c(.5, 2.5),
     ylim = c(-1, 2),
     main="Average Treatment Effects on Donations to Black Organizations",
     cex.main=.8,
     ylab="Difference in Donation Amount",
     xlab= "Treatment Comparison",
     cex.lab=.8,
     xaxt="n")
axis(1, at=1:2, labels=c("Anger vs. \n Control","Hope vs. \n Control"),
     tick=F)


## --------------------------------------------------------------------------------------------
plot(x = c(1, 2), 
     y = c(d.i.m, d.i.m2),
     xlim=c(.5, 2.5),
     ylim = c(-1, 2),
     main="Average Treatment Effects on Donations to Black Organizations",
     cex.main=.8,
     ylab="Difference in Donation Amount",
     xlab= "Treatment Comparison",
     cex.lab=.8,
     xaxt="n")
axis(1, at=1:2, labels=c("Anger vs. \n Control","Hope vs. \n Control"),
     tick=F)
abline(h=0, col="red3", lty=2)
lines(c(1,1), ci)
lines(c(2,2), ci2)


## --------------------------------------------------------------------------------------------
## Put each result in a vector
angerresults <- c(d.i.m, ci)
hoperesults <- c(d.i.m2, ci2)

## Bind these together as rows, store as dataframe
comb <- data.frame(rbind(angerresults, hoperesults))

## Give columns informative labels
names(comb) <- c("ATE", "lower", "upper")

## Add group indicator
comb$Comparison <- c("Anger vs. \n Control","Hope vs. \n Control")



## ---- message=F, warning=F-------------------------------------------------------------------
library(ggplot2)
ggplot(comb, aes(x=Comparison, y=ATE))+
  geom_point()+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1)+
  theme_bw()


## --------------------------------------------------------------------------------------------
ggplot(comb, aes(x=Comparison, y=ATE))+
  geom_point()+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1)+
  theme_bw()+
  geom_hline(aes(yintercept=0), linetype="dashed", colour="red3")+
  ggtitle("Average Treatment Effects on Donations to Black Organizations")+
  theme(plot.title = element_text(hjust = 0.5))# centers title


## --------------------------------------------------------------------------------------------
angcontrol <- subset(banks, angvcon == 1 | angvcon == 0)


## --------------------------------------------------------------------------------------------
## option 1- categorical
fit <- lm(blackdon ~ angvcon*factor(blackauto3), data=angcontrol)
summary(fit)


## --------------------------------------------------------------------------------------------
## option 2- numeric
fit.numeric <- lm(blackdon ~ angvcon*blackauto3, data=angcontrol)


## ---- warning=F, message=F-------------------------------------------------------------------
library(margins)
outp <- margins(fit, at = list(blackauto3 = c(0, 1, 2)),
        variable = "angvcon", change = c(0, 1))
summary(outp)


## --------------------------------------------------------------------------------------------
outp.df <- summary(outp)

ggplot(outp.df, aes(x=blackauto3, y=AME))+
  geom_point()+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1)+
  theme_bw()+
  geom_hline(aes(yintercept=0), linetype="dashed", colour="red3")+
  ggtitle("Average Treatment Effects on Donations to Black Organizations \n by Community nationalism")+
  ylab("Average Treatment Effects on Black Org. Donations")+
  xlab("Community Nationalism")+
  scale_x_continuous(breaks = c(0, 1, 2), labels = c("Low", "Medium", "High"))+
  theme(plot.title = element_text(hjust = 0.5))# centers title


## --------------------------------------------------------------------------------------------
ggplot(outp.df, aes(x=blackauto3, y=AME))+
  geom_point()+
  geom_line()+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.4)+
  theme_bw()+
  geom_hline(aes(yintercept=0), linetype="dashed", colour="red3")+
  ggtitle("Average Treatment Effects on Donations to Black Organizations \n by Community nationalism")+
  ylab("Average Treatment Effects on Black Org. Donations")+
  xlab("Community Nationalism")+
  scale_x_continuous(breaks = c(0, 1, 2), labels = c("Low", "Medium", "High"))+
  theme(plot.title = element_text(hjust = 0.5))# centers title


## ---- message=F, warning=F-------------------------------------------------------------------
library(prediction)
outp2 <- prediction(fit, at = list(blackauto3 = c(0, 1, 2),
                                  angvcon = c(0,1)),
                   calculate_se = T)
summary(outp2)

outp2.df <- data.frame(summary(outp2))


## --------------------------------------------------------------------------------------------
ggplot(outp2.df, aes(x=at.blackauto3., y=Prediction, 
                     fill=as.factor(at.angvcon.)))+
  geom_point()+
  geom_line()+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.4)+
  theme_bw()+
  ggtitle("Average Donations to Black Organizations \n by Community nationalism")+
  ylab("Average Black Org. Donations")+
  xlab("Community Nationalism")+
  scale_fill_manual("Condition", labels=c("Control", "Anger"), values=c("orange", "dodgerblue"))+
  scale_x_continuous(breaks = c(0, 1, 2), 
                     labels = c("Low", "Medium", "High"))+
  theme(plot.title = element_text(hjust = 0.5))# centers title


## --------------------------------------------------------------------------------------------
banks$condition <- NA
banks$condition[banks$angvcon == 1] <- "Anger"
banks$condition[banks$hopevcon == 1] <- "Hope"
banks$condition[banks$angvcon ==  0 & banks$hopevcon == 0] <- "Control"
banks$condition <- factor(banks$condition, levels=c("Control", "Anger", "Hope"))


## ---- warning=F, message=FALSE---------------------------------------------------------------
library(tidyverse)
banks %>%
  filter(is.na(condition)==F) %>%
  ggplot(aes(x=blackdon, fill=condition))+
  geom_histogram(alpha=.4)+
  theme_bw()+
  ggtitle("Distribution of Donations to Black Organizations")+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") +
  facet_grid(~condition)+
  xlab("Amount Donation (dollars)")


## ---- warning=F, message=FALSE---------------------------------------------------------------
banks %>%
  filter(is.na(condition)==F) %>%
  ggplot(aes(y=blackdon, x=condition, color=condition))+
  geom_boxplot()+
  geom_jitter(alpha=.5)+
  theme_bw()+
  ggtitle("Distribution of Donations to Black Organizations")+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") +
  ylab("Amount Donation (dollars)")


## ---- warning=F, message=FALSE---------------------------------------------------------------
banks %>%
  filter(is.na(condition)==F) %>%
  ggplot(aes(x=blackdon, fill=condition))+
  geom_density(alpha=.5)+
  theme_bw()+
  ggtitle("Distribution of Donations to Black Organizations")+
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_grid(~condition)+
  xlab("Amount Donation (dollars)")


## ---- warning=F, message=FALSE---------------------------------------------------------------
## Find means and confidence intervals by condition
banks <- subset(banks, is.na(condition)==F)
m.cond <- tapply(banks$blackdon, banks$condition, mean, na.rm=T)
ci.hope <- t.test(banks$blackdon[banks$condition == "Hope"])$conf.int
ci.anger <- t.test(banks$blackdon[banks$condition == "Anger"])$conf.int
ci.control <- t.test(banks$blackdon[banks$condition == "Control"])$conf.int
combd <- data.frame(cbind(cbind(m.cond),rbind(ci.control, ci.anger, ci.hope)))
names(combd) <- c("Mean", "lower", "upper")
combd$condition <- c("Control", "Anger", "Hope")
combd$condition <- factor(combd$condition, levels=c("Control", "Anger", "Hope"))


## Note we draw from both data=combd and data=banks
ggplot(combd, aes(x=condition, y=Mean, color=condition)) +
  geom_point(data = banks, aes(y=blackdon),
    position = position_jitter(width = 0.2, height = 0.1),
    alpha = 0.4) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0) +
  theme_bw() +
  ggtitle("Donations to Black Organizations by Condition")+
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 10, length.out = 5)) +
  theme(axis.title.x = element_blank()) +
  ylab("Donations (dollars)")

