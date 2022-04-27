## --------------------------------------------------------------------------------
resume <- read.csv("https://raw.githubusercontent.com/ktmccabe/teachingdata/main/resume.csv")


## --------------------------------------------------------------------------------
table(resume$call)


## --------------------------------------------------------------------------------
t.example <- t.test(resume$call[resume$race == "black"],
       resume$call[resume$race == "white"])


## --------------------------------------------------------------------------------
## In this case setting var.equal to T/F won't matter. In other cases, it might
t.example.ve <- t.test(resume$call[resume$race == "black"],
       resume$call[resume$race == "white"], var.equal = T)

t.example.ve$estimate[2]-t.example.ve$estimate[1]
t.example.ve$stderr

reg.example <- lm(call ~ race, data=resume)
summary(reg.example)$coefficients


## --------------------------------------------------------------------------------
## Use prop.test: NOTE THE DIFFERENT SYNTAX FROM t.test
## x is the "number of successes" i.e., number of 1's for each group
## n is sample size for each group
table(resume$race, resume$call)
p.test <- prop.test(x = c(157,235), 
                n = c(157+2278, 235+2200),
                correct=F)

p.test


## --------------------------------------------------------------------------------
t.example$conf.int
p.test$conf.int


## --------------------------------------------------------------------------------
chi.test <- chisq.test(table(resume$race,resume$call),
                       correct = F)
chi.test$statistic
chi.test$p.value


## --------------------------------------------------------------------------------
p.n <- mean(resume$call)
n.black <- 157+2278
n.white <-2200+235

se.race <- sqrt(p.n*(1-p.n) * (1/n.black + 1/n.white))
p.stat <- (0.09650924 -0.06447639) /se.race 
X2 <- p.stat^2
X2


## --------------------------------------------------------------------------------
p.test.c <- prop.test(x = c(157,235), 
                n = c(157+2278, 235+2200),
                correct = F)

p.test.c$statistic


## --------------------------------------------------------------------------------
resume$racegender <- NA
resume$racegender[resume$race=="white" & resume$sex == "male"] <- "White Male"
resume$racegender[resume$race=="white" & resume$sex == "female"] <- "White Female"
resume$racegender[resume$race=="black" & resume$sex == "male"] <- "Black Male"
resume$racegender[resume$race=="black" & resume$sex == "female"] <- "Black Female"

## make it a factor variable
resume$racegender <- as.factor(resume$racegender)
resume$racegender <- relevel(resume$racegender, ref="White Male")


## --------------------------------------------------------------------------------
t.males <- t.test(resume$call[resume$racegender=="White Male"],
                 resume$call[resume$racegender=="Black Male"])


## --------------------------------------------------------------------------------
reg.all <- lm(call ~ racegender, data=resume)
summary(reg.all)$coefficients


## --------------------------------------------------------------------------------
aov.all <- aov(call~ racegender, data=resume)
summary(aov.all)


## --------------------------------------------------------------------------------
## get overall mean
overall.mean <- mean(resume$call)

## get means for each group
group.means <- tapply(resume$call, resume$racegender, mean)
group.means

## get sd for each group
group.sd <- tapply(resume$call, resume$racegender, sd)
group.sd

## get N for each group
group.n <- tapply(resume$call, resume$racegender, length)
group.n


## --------------------------------------------------------------------------------
## sum the deviations of y_i from the overall mean
sum.sqtot <- sum((resume$call - overall.mean)^2)

## look at the group standard deviations
error.ss <- sum(((group.n -1) * group.sd^2))

## subtract these group errors from the total squared errors
group.ss <- sum.sqtot - error.ss  
## alternative:
group.ss2 <- sum(group.n *(group.means - overall.mean)^2)

 
## degrees of freedom 
Total_df <- nrow(resume) - 1
Error_df <- nrow(resume) - length(group.n)
Group_df <- Total_df - Error_df

## find means
MS_Error <- error.ss/Error_df
MS_Group <- group.ss/Group_df

## calculate F
F_value <- MS_Group / MS_Error
F_value

## compare to ANOVA
summary(aov.all)


## --------------------------------------------------------------------------------
summary(reg.all)$fstatistic


## --------------------------------------------------------------------------------
TukeyHSD(aov.all)


## --------------------------------------------------------------------------------
chi.all <- chisq.test(table(resume$racegender, resume$call))
chi.all


## --------------------------------------------------------------------------------
wil.test  <- wilcox.test(resume$call[resume$race=="black"],
                         resume$call[resume$race=="white"])
wil.test

