require(faraway)
require(ggplot2)
require(MASS)
require(Rcmdr)
require(leaps)
#5.1
fullmod<-lm(gamble ~ sex + status + verbal + income, teengamb)
mod1<-lm(gamble ~ sex + status + verbal, teengamb)
mod2<-lm(gamble ~ sex + status + income, teengamb)
mod3<-lm(gamble ~ sex + verbal + income, teengamb)
mod4<-lm(gamble ~ sex + income, teengamb)
mod5<-lm(gamble ~ sex + verbal, teengamb)
mod6<-lm(gamble ~ sex + status, teengamb)
mod7<-lm(gamble ~ sex, teengamb)
summary(fullmod)
summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)
summary(mod5)
summary(mod6)
summary(mod7)
#sex is always significant, coefficents max=-21.634 and min=-35.7094
#9.1a
ggplot(aatemp, aes(year, temp)) + geom_point() + geom_smooth(method=lm)
#does not have a linear relatioship
#9.1b
lmod <- lm(temp~year, aatemp)
res<-residuals(lmod)#extract residuals
#the residual plot
plot(res,type="l", xlab="Year",ylab="Residuals")
points(res)
abline(h=0) #draw the reference line
##scatter plot of successive pair of residuals
plot(res[-length(res)],res[-1],xlab=expression(hat(epsilon)[t-1]),
     ylab=expression(hat(epsilon)[t]),lwd=2)
#9.1c
mod9.1c <- lm(temp ~ poly(year,degree=5),aatemp)
ggplot(aatemp, aes(year, temp)) + geom_point() + stat_smooth(method='lm', formula= y~poly(x,5))
newData <- data.frame(year=2020)
predict.lm(mod9.1c, newData, interval=c("prediction"), level=.95)

#9.1d
#design matrix to remove data before 1930
newaatemp <- aatemp[c(51:115),]
mod9.1d <- lm(temp ~ year, newaatemp)
summary(mod9.1d)
ggplot(newaatemp, aes(year, temp)) + geom_point() + stat_smooth(method='lm', formula= y~x)
#2
mod9.2 <- lm(yield~ nitrogen,cornnit)
boxcox(mod9.2, plotit=T)
##Zoom in the plot for lambda in [0.5,1.5]
boxcox(mod9.2, plotit=T, lambda=seq(0,5 ,by=0.1))
mod9.2b <- lm(yield^2 ~ polym(nitrogen,degree=4), cornnit)
boxcox(mod9.2b, plotit=T, lambda=seq(0,5 ,by=0.1))
#and we test for lack of ﬁt: still need to do this
#  proc rsreg data=cornnit; 
#  model y2= nitrogen n2 n3 n4/lackfit covar=4; 
#  run;
#9.3
mod9.3<-lm(O3~ temp + humidity + ibh, ozone)
boxcox(mod9.3, plotit=T, lambda=seq(-1,2 ,by=0.1))
mod9.3b<-lm(O3^(1/3)~ temp + humidity + ibh, ozone)
boxcox(mod9.3b, plotit=T, lambda=seq(-1,3 ,by=0.1))
summary(mod9.3)
summary(mod9.3b)
#9.5
mod9.5<- lm(Volume~ Girth + Height, trees)
summary(mod9.5)
mod9.5b<-lm(log(Volume)~I(Girth^2) + Girth+ Height, trees)
summary(mod9.5b)
#10.1a
mod10.1<-lm(lpsa~ gleason + pgg45 + lcavol + lweight + age + lbph + svi + lcp,prostate)
stepwise(mod10.1, direction="backward", criterion="AIC")
stepwise(mod10.1, direction="backward", criterion="BIC")
#10.1b
stepwise(mod10.1, direction="forward", criterion="AIC")
stepwise(mod10.1, direction="backward", criterion="AIC")
stepwise(mod10.1, direction="forward/backward", criterion="AIC")
#10.1c
regsubsets()
mod10.1c<-regsubsets(lpsa~.,data=prostate)
rs<-summary(mod10.1c)
rs$which
rs$which[which.max(rs$adjr),]
#10.1d
rs$which[which.min(rs$cp),]
#10.4
mod10.4 = lm(log(Volume)~Girth+Height+I(Girth^2)+I(Height^2)+Girth:Height,data=trees)
sumary(mod10.4)
stepwise(mod10.4, direction="backward", criterion="AIC")
mod10.4b = lm(formula = log(Volume) ~ Girth + Height + I(Girth^2), data = trees)
sumary(mod10.4b)
anova(mod10.4b, mod10.4)
#10.5
mod10.5<-lm(stack.loss~.,data=stackloss)
summary(mod10.5)
stepwise(mod10.5, direction="forward", criterion="AIC")

studres <- rstudent(mod10.5)
studres[which.max(abs(studres))]
range(studres)
x<- row.names(stackloss)
cook <- cooks.distance(mod10.5)
halfnorm(cook,4,labs=x,ylab="Cook's distances",main="Cook's Plot Stackloss", xlim=c(0,3))

mod10.5b<-lm(stack.loss~Air.Flow + Water.Temp,data=stackloss)
studresb <- rstudent(mod10.5b)
studres[which.max(abs(studres))]
range(studresb)
x<- row.names(stackloss)
cook <- cooks.distance(mod10.5b)
halfnorm(cook,4,labs=x,ylab="Cook's distances",main="Cook's Plot Stackloss", xlim=c(0,3))
mod10.5c<-lm(stack.loss~.,data=stackloss, subset=(!(rownames(stackloss)%in%c("21"))))
mod10.5d<-lm(stack.loss~Air.Flow + Water.Temp,data=stackloss, subset=(!(rownames(stackloss)%in%c("21"))))
stepwise(mod10.5c, direction="forward", criterion="AIC")
#10.6
mod10.6 = lm(hipcenter~.,seatpos)
summary(mod10.6)
x<- model.matrix(mod10.6)
x0<-apply(x,2,mean)
predict(mod10.6,new=data.frame(t(x0)),interval="prediction")
stepwise(mod10.6, direction="backward", criterion="AIC")
mod10.6b = lm(formula = hipcenter ~ Age + HtShoes + Leg, data = seatpos)
summary(mod10.6b)
xb = model.matrix(mod10.6b)
x0b = apply(xb,2,mean)
predict(mod10.6b,new=data.frame(t(x0b)),interval="prediction")
