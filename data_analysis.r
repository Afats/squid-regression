x1=c(1.29,1.59,.96,.99,1.03,1.15,1.15,1.24,.93,1.32,1.36,1.30,1.83,1.51,2.04,1.82,1.71,1.73,1.70,1.76,2.19,1.64)
x2=c(1.05,1.53,.81,.83,.88,.99,.97,1.05,.79,1.11,1.16,1.07,1.44,1.27,1.66,1.58,1.54,1.44,1.59,1.60,1.86,1.58)
x3=c(.43,.55,.32,.34,.35,.46,.44,.42,.32,.44,.49,.46,.58,.48,.71,.67,.60,.64,.73,.69,.75,.58)
x4=c(.74,.92,.55,.54,.63,.65,.55,.75,.52,.76,.80,.75,.99,.91,1.24,1.03,1.06,1.02,.97,1.09,1.24,1.08)
x5=c(.34,.48,.31,.27,.29,.34,.34,.33,.27,.36,.41,.37,.64,.47,.62,.60,.57,.63,.69,.62,.72,.51)
y=c(1.86,3.08,.56,.80,.98,1.54,1.38,1.78,.33,1.99,2.30,1.76,8.43,4.15,8.83,6.26,7.32,6.39,7.74,7.83,10.13,6.41)

squid=data.frame(x1,x2,x3,x4,x5,y)

library(leaps)
# risk <- read.table('risk.txt',header = T)
risk.best.subsets <- regsubsets(y~.,data=squid,nvmax=5)
summary(risk.best.subsets)

squid_mod = lm(y~x1+x2+x3+x4+x5,data=squid)
Cp = leaps(x = squid[,1:5], y=squid$y, names=names(squid)[1:5],method = "Cp")
adjr2 = leaps(x = squid[,1:5], y = squid$y, method = "adjr2")
final = cbind(Cp$which, AdjR2 = adjr2$adjr2, MallowCp = Cp$Cp, p = Cp$size);final
# Taking the ones that we need, based on regsubsets
final[c(1,6,16,26,31),]

PRESS <- function(linear.model) {
  pr <- residuals(linear.model)/(1 - lm.influence(linear.model)$hat)
  sum(pr^2)
}
# The best 5 models from the regsubsets

pressdata =c(PRESS(lm(y~x5,data=squid)), PRESS(lm(y~x4+x5,data=squid)),PRESS(lm(y~x2+x4+x5,data=squid)), PRESS(lm(y~x2+x3+x4+x5,data=squid)),PRESS(lm(y~x1+x2+x3+x4+x5,data=squid)))
final = cbind(Cp$which, AdjR2 = adjr2$adjr2, MallowCp = Cp$Cp, p = Cp$size)
final_data = final[c(1,6,16,26,31),]
finalANS = cbind(final_data, PRESS = pressdata)


x<-c(1,2,3,4,5)

plot(x,pressdata, xlab="Number of predictors", ylab="PRESS", type = "b")
plot(x, final_data[,6],xlab="Number of predictors", ylab="Adjusted R2", type = "b")
plot(x, final_data[,7],xlab="Number of predictors", ylab="Mallow's Cp", type = "b")

# FOR AIC Q1 (b)
# (i) & (ii) part
library(MASS)
null_squid_mod_intercept = lm(y~1,data=squid)
full_squid_mod_intercept = lm(y~.,data=squid)

stepAIC(null_squid_mod_intercept, scope = list(lower = null_squid_mod_intercept , upper = full_squid_mod_intercept), direction = 'forward')
stepAIC(full_squid_mod_intercept, direction='backward')

# (iii) part
x1_null_squid_mod_intercept = lm(y~x1,data=squid)
stepAIC(x1_null_squid_mod_intercept, scope = list(lower = x1_null_squid_mod_intercept , upper = full_squid_mod_intercept), direction = 'both')

x4_model = lm(y~x4,data=squid)
AIC(x4_model)


par(mfrow=c(2,2))
plot(x4x5_model)