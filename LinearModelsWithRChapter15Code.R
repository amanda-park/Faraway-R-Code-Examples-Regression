#Code for questions 15.5, 15.7 for Linear Models with R

require(faraway)
require(ggplot2)
require(MASS)

#Part 15.5a Boxplot
head(anaesthetic)
boxplot(breath~tgrp,data=anaesthetic, main="Boxplot of Anaesthetic Data", 
        ylab="Minutes to Breathe Unassisted", xlab="Treatment Groups")

#15.5b stripchart
stripchart(breath~tgrp, anaesthetic,
           main="Stripchart of Treatment Groups",
           xlab="Minutes to Breathe Unassisted",
           ylab="Treatment Group",
           method="jitter",
           col=c("orange","red", "green", "blue"),
           pch=16
)

#15.5c using ggplot to create boxplot and stripchart
ggplot(anaesthetic, aes(x = tgrp, y = breath)) +
  geom_boxplot() + ggtitle("Boxplot of Treatment Groups") 

ggplot(anaesthetic, aes(x = tgrp, y = breath, color= tgrp, shape = tgrp)) +
  geom_jitter(position=position_jitter(0.1)) + ggtitle("Stripchart of Treatment Groups")

ggplot(anaesthetic, aes(x = tgrp, y = breath, color=tgrp, shape = tgrp)) +
  geom_boxplot()+ 
  geom_jitter(position=position_jitter(0.1)) +ggtitle("Overlay of Boxplot and Stripchart")

#15.5d Fitting one factor model for recovery times
mod1 <- lm(breath~tgrp, anaesthetic)
summary(mod1)
anova(mod1)
TukeyHSD(aov(mod1))

#15.5e Box-cox transformation
boxcox(mod1, plotit=TRUE) #fails because response goes negative and log function cannot go negative

#15.5f square root transformation
mod2 <- lm(sqrt(breath)~tgrp, anaesthetic)
summary(mod2)
anova(mod2)
TukeyHSD(aov(mod2))
qqnorm(residuals(mod2))
qqline(residuals(mod2))
plot(jitter(fitted(mod2)),residuals(mod2),xlab="Fitted",ylab="
       Residuals", main="Residual Plot (Square Root Response)")
abline(h=0)

#Bartlett Test for New Model
bartlett.test(sqrt(breath)~tgrp,anaesthetic)

#Levene Test, Square Root Model
med2<-with(anaesthetic,tapply(sqrt(breath),tgrp,median))
ar2<-with(anaesthetic,abs(sqrt(breath) -med2[tgrp]))
anova(lm(ar2~tgrp,anaesthetic))

#15.7a
#Boxplot
boxplot(waste~supplier,data=denim, main="Waste by Supplier", xlab="Supplier", ylab="Waste")
#15.7b
mod1<-lm(waste ~supplier, denim)
anova(mod1)
TukeyHSD(aov(waste ~ supplier, denim))
confint(mod1)
#15.7c
##diagnostics
#QQ Plot
qqnorm(residuals(mod1),ylab="Residuals")
qqline(residuals(mod1))
QQ<- qqnorm(residuals(mod1),ylab="Residuals")
identify(QQ)
#Nonconstant Variance Assumption
fit<-fitted(mod1)
res<-residuals(mod1)
plot(fit,res,xlab="Fitted",ylab="Residuals",main="Residual Plot: ", lwd=2)
plot(fit,sqrt(abs(res)),xlab="Fitted",ylab="Residuals",main="Square Root Residual Plot: ", lwd=2)
#multicolinearity
vif(mod1)
#Leverage Points
studres <- rstudent(mod1)
studres[which.max(abs(studres))]
range(studres)
#Cook's Distance
D <- row.names(denim)
cook <- cooks.distance(mod1)
halfnorm(cook,5,labs=D,ylab="Cook's distances",main="Cook's Plot ", xlim=c(0,5))
#15.7d
#New linear model after removing 2 outliers 82,87
denim2<-denim[c(-82,-87),]
mod2<-lm(waste ~ supplier, denim2)
#rerun part b for mod2
#15.7e as well 
anova(mod2)
TukeyHSD(aov(waste ~ supplier, denim2))
confint(mod2)
#15.7f
boxplot(waste~supplier,data=denim2, main="Waste by Supplier Without Outliers", xlab="Supplier", ylab="Waste")
