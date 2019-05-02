#Extending the Linear Model in R
#Code for all chapter 2 problems

require(faraway)
require(ggplot2)
require(RcmdrMisc)

#2.1a
binmod <- glm(cbind(ncases, ncontrols) ~ agegp * alcgp * tobgp, family = binomial(),
              data = esoph)
summary(binmod)

#Consider 2-way model because 3-way model is disgusting
binmod2 <- glm(cbind(ncases, ncontrols) ~ (agegp*alcgp) + (agegp*tobgp) + (alcgp*tobgp),
               family = binomial(), data = esoph)
summary(binmod2)
drop1(binmod2, test="F")

#Remove agegp*tobgp
binmod3 <- glm(cbind(ncases, ncontrols) ~ agegp*alcgp + tobgp*alcgp,
    data = esoph, family = binomial())
summary(binmod3)
drop1(binmod3, test="F")

#Remove alcgp*tobgp, but keep tobacco in without any interaction term first
binmod4 <-glm(cbind(ncases, ncontrols) ~ agegp*alcgp + tobgp,
              data = esoph, family = binomial())
summary(binmod4)
drop1(binmod4, test="F")

#Everything is significant here according to drop1, but the model is gross still
#Consider a basic model here with no interactions for comparison
binmod5 <- glm(cbind(ncases, ncontrols) ~ agegp + alcgp + tobgp,
               data = esoph, family = binomial())
summary(binmod5)
drop1(binmod5, test="F")

pchisq(deviance(binmod5),76,lower=FALSE)

#Try removing tobgp from basic model
binmod6 <- glm(cbind(ncases, ncontrols) ~ agegp + alcgp,
               data = esoph, family = binomial())
summary(binmod6) 

#According to deviance and AIC, binmod5 is the best model, but if interactions are 
#necessary, binmod4 is tbe best model

#2.1b
#See distribution of predictors compared to response

par(mfrow=c(2,2))

plot(ncases/ncontrols ~ agegp,esoph)
plot(ncases/ncontrols ~ alcgp,esoph)
plot(ncases/ncontrols ~ tobgp,esoph)

#Unclass all predictors
esoph2 <- esoph
esoph2$agegp <- unclass(esoph$agegp)
esoph2$alcgp <- unclass(esoph$alcgp)
esoph2$tobgp <- unclass(esoph$tobgp)

#Consider 2-way interaction model with all unclassed predictors
binmod_unc <- glm(cbind(ncases, ncontrols) ~ (agegp*alcgp) + (agegp*tobgp) + (alcgp*tobgp),
                                data = esoph2, family = binomial())
summary(binmod_unc)
drop1(binmod_unc, test="F")

#Drop agegp:tobgp
binmod_unc2 <- glm(cbind(ncases, ncontrols) ~ (agegp*alcgp) + (alcgp*tobgp),
                  data = esoph2, family = binomial())
summary(binmod_unc2)

pchisq(deviance(binmod_unc2),82,lower.tail=F)

#Diagnostics 2.1d
plot(binmod_unc2)

esoph2[cooks.distance(binmod_unc2) > 0.17,]

#Computing prediction interval 2.1f
#First find variance covariance matrix of model
M <- vcov(binmod_unc2)
i <- c(3,5,6)
M2 <- M[i,i]
c(2,2,1)%*%M2%*%c(1,2,2)
vars <- c(.93 - sqrt(0.0765621)*1.96, .93 + sqrt(0.0765621)*1.96)
exp(vars)

#2.2a
#Fit binomial model for wbcd
binmod22 <- glm(Class ~ .,
                data = wbca, family = binomial())
summary(binmod22)
pchisq(deviance(binmod22),df.residual(binmod22),lower=FALSE)

#2.2b
stepwise(binmod22,direction="forward/backward",criterion="AIC")
stepwise(binmod22,direction="backward/forward",criterion="AIC")

binmod22reduced<-glm(Class ~ Adhes + BNucl + Chrom + Mitos + NNucl + Thick + UShap,
                     family="binomial", data=wbca)

#2.2c
x0<-c(1,1,1,3,1,1,4,1)
est<-sum(x0*coef(binmod22reduced))
ilogit(est)

#Extract Variance to compute confidence interval
binmod22reduced.summary<-summary(binmod22reduced)
cm<-binmod22reduced.summary$cov.unscaled
se<-sqrt(t(x0)%*%cm%*%x0)

ilogit(c(est-1.96*se,est+1.96*se))

#2.2d
p <- predict(binmod22reduced, type="response")
type2 <- sum(p >= 0.5 & !wbca$Class)/sum(!wbca$Class)
type1 <- sum(p < 0.5 & wbca$Class)/sum(wbca$Class)
type2
type1

#2.2e
type2.90 <- sum(p >= 0.9 & !wbca$Class)/sum(!wbca$Class)
type1.90 <- sum(p < 0.9 & wbca$Class)/sum(wbca$Class)
type2.90
type1.90

#2.2f
#Testing and training split
test <- wbca[seq(1,nrow(wbca),by=3),]
train <- wbca[-seq(1,nrow(wbca),by=3),]

binmodtest <- glm(Class ~ Adhes + BNucl + Chrom + Mitos + NNucl+ Thick + UShap,
                  family=binomial,data=train)
summary(binmodtest)
p2 <- predict(binmodtest, test, type="response")

plot(p2,binmodtest$Class)
head(wbca)
length(wbca$class)

#3a
# missing <- with(pima, missing <- glucose==0 | diastolic==0 | triceps==0 | bmi == 0)
# missing
# pima2 <- pima[-missing,]

pima2 <- pima
pima2$diastolic[pima2$diastolic == 0] <- NA
pima2$glucose[pima2$glucose == 0] <- NA
pima2$triceps[pima2$triceps == 0] <- NA
pima2$insulin[pima2$insulin == 0] <- NA
pima2$bmi[pima2$bmi == 0] <- NA

pima2 <- na.omit(pima2)

#3b
mod3b<- glm(cbind(test, 1-test)~., family=binomial(link=logit), data=pima2)
summary(mod3b)
pchisq(deviance(mod3b),df.residual(mod3b),lower=FALSE)
pchisq(458.6,367,lower=FALSE)
df.residual(mod3b)
#3c
quantile(pima2$bmi,na.rm=FALSE)
c(exp(-8.7*.007-1.96*.00013808), exp(-8.7*(.0859+1.96*.00013808)))
pima2$bmi
exp(-8.7*.007)
#3d
cor(pima2)
summary(mod3b)
#3e
par(mfrow=c(2,2))
plot(mod3b)
#3f
prediction <- predict(mod3b, newdata = list(pregnant=1, glucose=99, diastolic=64, triceps=22, insulin=76, bmi=27, diabetes=.25, age=25),type="link", se.fit=TRUE)
ilogit(c(prediction$fit-2*prediction$se.fit, prediction$fit, prediction$fit+2*prediction$se.fit))
#4a Build a model to predict the occurrence of liver cancer. Compute the ED50 level.
data(aflatoxin)
aflatoxin
mod4a<- glm(cbind(tumor,total-tumor)~dose, family="binomial", data=aflatoxin)
summary(mod4a)
pchisq(deviance(mod4a),df.residual(mod4a),lower=FALSE)
plot(mod4a)

ld50<- -mod4a$coef[1]/mod4a$coef[2]
ld50
#4b Discuss the extrapolation properties of your chosen model for low doses.
#se
mod4a.summary <-summary(mod4a)
dr<-c(-1/mod4a$coef[2],mod4a$coef[1]/mod4a$coef[2]^2)
seLD<-sqrt(dr%*%mod4a.summary$cov.unscaled%*%dr)
c(ld50-seLD*1.96,ld50+seLD*1.96)
x<-seq(-10,120,2)
pl<-ilogit(mod4a$coef[1]+mod4a$coef[2]*x)
obj<-predict(mod4a,newdata=data.frame(dose=x),se=TRUE)
phat<-obj$fit
sehat<-obj$se.fit
CIlow<-ilogit(c(phat-1.96*sehat))
CIup<-ilogit(c(phat+1.96*sehat))
par(mfrow=(c(1,1)))
plot(aflatoxin[,1],aflatoxin[,3]/aflatoxin[,2],ylim=c(0,1),xlim=c(-10,120),ylab="Probability",xlab="Dose",pch=16)
lines(x,pl,lwd=2)[]
lines(x,CIlow,lty=2,lwd=2,col="red")
lines(x,CIup,lty=2,lwd=2,col="red")

#2.5
mod1<- glm(grade~psi+tuce+gpa, family=binomial, data=spector)
summary(mod1)
pchisq(25.779,df=28,lower=FALSE)
par(mfrow=c(2,2))
plot(mod1)

#Removing outlier and assessing model with tuce removed
spector2 <- spector[-32,]
mod2<- glm(grade~psi+tuce+gpa, family=binomial, data=spector2)
summary(mod2)
plot(mod2)

mod3 <- glm(grade~psi+gpa, family=binomial, data=spector)
summary(mod3)
anova(mod3, mod1,test="Chisq")

colMeans(spector)

#We can't have PSI of spector be a decimal, round .4375 to 1
z <- 1.96
pred <- predict(mod3, newdata=data.frame(psi=1,gpa=3.117188), se=TRUE)
se <- ilogit(pred$fit)
ci <- ilogit(pred$fit+c(-1,1)*z*se)
ci

spectorMax <- apply(spector, 2, max)
pred2 <- predict(mod3, newdata=data.frame(psi=spectorMax["psi"],gpa=spectorMax["gpa"]), se=TRUE)
se2 <- ilogit(pred2$fit)
ci2 <- ilogit(pred2$fit+c(-1,1)*z*se2)
ci2

x <- seq(2,4,0.01) #x sequenced such that it covers all GPA values
##Curve when psi=1
pl <- ilogit(mod3$coef[1]+mod3$coef[2]*1+mod3$coef[3]*x)
obj <- predict(mod3,newdata=data.frame(psi=1,gpa=x),se=T)
phat <- obj$fit
sehat <- obj$se.fit
CIlow <- ilogit(c(phat-1.96*sehat))
CIup <- ilogit(c(phat+1.96*sehat))
par(mfrow=(c(1,1)))
plot(x,pl,ylim=c(0,1),type="l",lwd=2,main="psi=1",
     xlim=c(2,4),ylab="Probability",xlab="gpa",pch=16)
lines(x,CIlow,lty=2,lwd=2,col="red")
lines(x,CIup,lty=2,lwd=2,col="red")
legend(2,1.0,legend=c("Estimated Prob","95% Confidence Band"),
       col=c(1,"red"),lty=c(1,2),lwd=c(2,2))

pl2 <- ilogit(mod3$coef[1]+mod3$coef[2]*0+mod3$coef[3]*x)
obj2 <- predict(mod3,newdata=data.frame(psi=0,gpa=x),se=T)
phat <- obj2$fit
sehat <- obj2$se.fit
CIlow <- ilogit(c(phat-1.96*sehat))
CIup <- ilogit(c(phat+1.96*sehat))
plot(x,pl2,ylim=c(0,1),type="l",lwd=2,main="psi=0",
     xlim=c(2,4),ylab="Probability",xlab="gpa",pch=16)
lines(x,CIlow,lty=2,lwd=2,col="red")
lines(x,CIup,lty=2,lwd=2,col="red")
legend(2,1.0,legend=c("Estimated Prob","95% Confidence Band"),
       col=c(1,"red"),lty=c(1,2),lwd=c(2,2))

summary(mod3)

#2.6
#estimating model
data(turtle)
sex<-cbind(turtle$male,turtle$female)
turtle2=turtle + sex
sexmod<- glm(sex~temp, family=binomial, data=turtle)
#diagnostics
summary(sexmod)
pchisq(24.942,13,lower=FALSE) 
par(mfrow=c(2,2))
plot(sexmod) #possible issue of a quadratic trend in the residual plot. look at the bump in the line
#fit a quadratic model
sexmod2 <- glm(sex ~temp+I(temp^2), family=binomial, data=turtle)
summary(sexmod2)
pchisq(20.256,df=12,lower=FALSE)    #.0623956
#LRT test 
pchisq(24.942-20.256,1,lower=FALSE)    #Why is it 24.942-20.256,df=1
#####new diagnostics
par(mfrow=c(2,2))
plot(sexmod2) #keep using sexmod2
#Check for evidence of overdispersion in a binomial model for the sex of the turtle.
phi <- sum(residuals(sexmod2,type="pearson")^2)/(12)
summary(sexmod2,dispersion=phi)
drop1(sexmod2,scale=phi,test="F")
# Warning message: In drop1.glm(sexmod2, scale = phi, test = "F") : F test assumes 'quasibinomial' family
#based on this, the quadratic term does not seem to be needed once we have looked into overdispersion
#fit a binomial beta model
betasex <- betabin(sex~temp, random=~1,link="logit", data=turtle)


#2.7a - model estimation
data(infert)
infertmod<-glm(case~.,family="binomial",data=infert)
#remove stratum because it is an ID, so we also remove pooled stratum
infertmod2<-glm(case~ age + parity + induced + spontaneous + education,family="binomial",data=infert)
anova(infertmod2,infertmod,test="Chisq") #smaller is good
#####(2)model diagnostics (you need to fix any problems you found)
summary(infertmod2)
pchisq(257.8,241,lower=FALSE) 
par(mfrow=c(2,2))
plot(infertmod2) #diagnostics look good, maybe consider removing the influential points 39 and 108
#####(3) model comparison and model selection 
stepwise(infertmod2,direction="backward",criterion="AIC")
#eductation is removed by stepwise
infertmod3<-glm(case~ age + parity + induced + spontaneous ,family="binomial",data=infert)
anova(infertmod3,fartmod2,test="Chisq") #smaller is good
#####(4) make predictions at two points: one interpolation and on extrapolation. 
#interpolation based on average of the data set
mean(infert$age)                        # 31.504
mean(infert$spontaneous)                # .5766
mean(infert$parity)                     # 2.0927
mean(infert$induced)                    # .57258

predict(infertmod3,data.frame(spontaneous=1,induced=1,parity=2,age=32))
a4 <- ilogit(predict(infertmod3,data.frame(spontaneous=1,induced=1,parity=2,age=32)))
ci4 <- ilogit(.5467+c(-1,1)*1.96*a4)

#extrapolation based on min/max of each column
min(infert$age)                        # 21
min(infert$spontaneous)                # 0
min(infert$parity)                     # 1
min(infert$induced)                    # 0
predict(infertmod3,data.frame(spontaneous=0,induced=0,parity=1,age=21))
a3 <- ilogit(predict(infertmod3,data.frame(spontaneous=0,induced=0,parity=1,age=21)))
ci3 <- ilogit(-2.44442+c(-1,1)*1.96*a3)
#####(6) finally the interpretation of the model and any message you can take away from your modeling.
summary(infertmod3)