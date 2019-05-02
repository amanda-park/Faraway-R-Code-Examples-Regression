#Extending the Linear Model Chapter 3
#Answers 3.2, 3.3, 3.5, 3.7

require(faraway)
require(MASS)
require(RcmdrMisc)

#3.2
mod1 <- glm(colonies~dose, salmonella, family="poisson")
summary(mod1)
pchisq(75.806, df=16, lower=FALSE)

par(mfrow=c(2,2))
plot(mod1)

#Plots indicate quadratic or cubic relationship

mod2 <- glm(colonies~dose+I(dose^2), salmonella, family="poisson")
summary(mod2)
mod3 <- glm(colonies~dose+I(dose^2)+I(dose^3), salmonella, family="poisson")
summary(mod3)
mod4 <- glm(colonies~dose+I(dose^2)+I(dose^3)+I(dose^4), salmonella, family="poisson")
summary(mod4)

plot(mod3)

#Cubic model is the best according to AIC and significance of predictors

sum(residuals(mod3, type="pearson")^2)/mod3$df.residual

#2.61 > 1, indicates possible overdispersion ^

mod5 <- glm.nb(colonies~dose+I(dose^2)+I(dose^3), salmonella)
summary(mod5)
pchisq(mod5$deviance, mod5$df.residual, lower=FALSE)

sum(residuals(mod5, type="pearson")^2)/mod5$df.residual

#3.3
mod6 <- glm(incidents ~ year + period + service + type, ships, family="poisson")
summary(mod6)
#Bad, need to consider modifications to data
ships$year <- ordered(ships$year)
ships$period <- ordered(ships$period)

mod7 <- glm(incidents ~ year + period + offset(log(service)) + type, ships, subset=(service>0),family="poisson")
summary(mod7)
pchisq(mod7$deviance, mod7$df.residual, lower=FALSE)
plot(mod7)
sum(residuals(mod7, type="pearson")^2)/mod7$df.residual

mod8 <- glm.nb(incidents ~ year + period + offset(log(service)) + type, ships, subset=(service>0))
summary(mod8)
pchisq(mod8$deviance, mod8$df.residual, lower=FALSE)

summary(mod7)

#3.5
###3.5a  Build a Poisson regression model Considering the deviance of this model, 
#        does this model fit the data?
mod35a <- glm(doctorco~ sex+age+agesq+income+levyplus+freepoor+freerepa
              +illness+actdays+hscore+chcond1+chcond2,data=dvisits,family=poisson)
summary(mod35a)                        # AIC 6737.1
pchisq(4379.5,df=5177,lower=FALSE)     # 1
# p-value close to 1 with small residual deviance in comparison to the degrees of freedom. 
# So the model is acceptable fit. 

### 3.5b Plot the residuals and the fitted values. why are there lines of observations on the plot?
par(mfrow=c(2,2))
plot(mod35a)
#lets look at how many unique responses there are
unique(dvisits$doctorco)  # 1 2 3 4 8 5 7 6 9 0 so 10 possible responses
# the lines in the residual plot are being caused by these 10 possible responses 

### 3.5c Use backward elimination with a critical p-value of 5much as possible. Report your model
require(RcmdrMisc)
mod35c <- stepwise(mod35a,direction="backward",criterion="AIC")
# doctorco ~ sex + age + income + levyplus + freepoor + illness + actdays + hscore + chcond1 +chcond2
# removed "agesq" and "freerepa", AIC barely changed at all. still acceptable by deviance standards. 
summary(mod35c)                     # 6734.5
pchisq(4381,df=5179,lower=FALSE)    # 1

### 3.5d What sort of person would be predicted to visit the doctor the most from your model?
#        You can make your statement according to the signs of regression coefficients.
summary(mod35c)
#

### 3.5e For the last person in the dataset, compute the predicted probability distribution for
#        their visits to the doctor, i.e., give the probability they visit 0,1,2, etc. times.
dim(dvisits)                            # 5190 by 19
predict(mod35c,newdata=dvisits[5190,],se.fit=TRUE)
#predicted mean (predicted number of visits)
pre<- exp(-1.883321)                          # .1520842
#fit -1.883321,  se.fit .08674103, residual scale 1
#point estimate, plug into a poisson distribution to ???nd the probability of this point estimate.
dpois(0:9,pre)
# 8.589160e-01 1.306275e-01 9.933192e-03 5.035605e-04 1.914590e-05 5.823577e-07 1.476123e-08
# 3.207072e-10 6.096812e-12 1.030254e-13

### 3.5f Fit a (Gaussian) linear model and graphically compare the fits. Describe how they differ.
#ols version of mod35a
mod35f <- lm(doctorco~ sex+age+agesq+income+levyplus+freepoor+freerepa
             +illness+actdays+hscore+chcond1+chcond2,data=dvisits)
stepwise(mod35f,direction="backward",criterion="AIC")
#AIC=-3491.7   doctorco ~ sex + age + income + freepoor + illness + actdays + hscore
mod35g <- lm(doctorco ~ sex + age + income + freepoor + illness + actdays + hscore, data=dvisits)
#To compare the fit of the ols and glm models, compare their predicted values on the entire data set.
pos <- predict(mod35c,newdata=dvisits,type="response")
lm <- predict(mod35g,newdata=dvisits,type="response")
##plots of predictions
plot(pos,lm,ylab="Linear Predictions",xlab="Poisson Predictions",
     ylim=c(0,5),xlim=c(0,5))
abline(0,1)
#Linear underestimates the number of visits
#insert plot 

#3.7

data(esdcomp)
head(esdcomp)
#want to model on complaints
#Poisson for rate of events
# offset is either hours or visits: offset(log())
#try each
mod1 <- glm(complaints~ offset(log(hours))+residency + gender + revenue+visits,
            data=esdcomp,family=poisson)
summary(mod1)                        # 184.96 AIC
pchisq(52.181,39,lower.tail=FALSE)   # .077092
mod2 <- glm(complaints~ offset(log(visits))+residency + gender + revenue+hours,
            data=esdcomp,family=poisson)
summary(mod2)                        # 187.3 AIC
pchisq(54.518,39,lower.tail=FALSE)   # .050509
#lower AIC and pchisq farther above .05, so use hours
# mod 1 is accaptable fit from a deviance point of view but not great
# now we perform model selection to reduce 
mod3 <- stepwise(mod1,direction="backward",criterion="AIC")
#Step:  AIC=181.16     complaints ~ visits + offset(log(hours))
summary(mod3)                        
pchisq(54.380,df=42,lower=FALSE)     # .095398
# mod 1 is accaptable fit from a deviance point of view
# diagnostics next
par(mfrow=c(2,2))
plot(mod3)
# compare with diagnostics of mod 1 to see if stepwise helped
par(mfrow=c(2,2))
plot(mod1)
# plots look about the same for each with issues: residual plot doesnt seem random
# possible quadratic trend looking at the curve of the residual plot
mod4 <- glm(complaints~ offset(log(hours))+residency + gender + I(revenue^2) + visits,
            data=esdcomp,family=poisson)
summary(mod4)                        # 184.91
pchisq(52.130,df=39,lower=FALSE)     # .07778
par(mfrow=c(2,2))
plot(mod4)

mod5 <- glm(complaints~ offset(log(hours))+residency + gender + revenue + I(visits^2),
            data=esdcomp,family=poisson)
summary(mod5)                        # 182.85
pchisq(50.067,df=39,lower=FALSE)     # .1103
par(mfrow=c(2,2))
plot(mod5)
#overdispersion does not seem to be an issue either
#mod 3 has lowest AIC and acceptable pchisq. we choose mod 3 over 4 or 5 
#since quadratic terms did not fix any issue and add complexity to it
### interpret mod 3 
#insert diagnostics for mod 3, maybe summary too
mod3 <- step(mod1,direction="backward",criterion="AIC")
# complaints ~ visits + offset(log(hours))
summary(mod3)                        # AIC=181.16                             
pchisq(54.380,df=42,lower=FALSE)     # .095398
# mod 1 is accaptable fit from a deviance point of view
# diagnostics next
par(mfrow=c(2,2))
plot(mod3)
#holding other factors constant, the number of visits from patients has 
#little effect on the number of complaints but is statistically significant. 
