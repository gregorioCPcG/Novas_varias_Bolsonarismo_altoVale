# simulacro

var01 <- rnorm(n = 1000, mean = c(48.140, 46.830, 39.150, 38.460, 61.340, 39.850, 39.820, 45.290, 53.850, 42.330,
                         47.970, 43.210, 43.050, 39.720, 47.050, 43.300, 37.280, 43.890,
                         43.270, 36.670, 50.580, 42.300, 29.880, 45.495, 48.833, 43.500, 37.390,
                         42.620), sd = 6.005)
dep <- rnorm(n=1000, mean = c(0.9,1,1.1), sd=0.07)
hist(var01)
hist(dep)
var02 <-rnorm(n=1000, mean=c(25,50,89,89.1),sd=6)
hist(var02)
cor.test(var01,var02)
cor.test(var01, dep)
cor.test(var02, dep)
regressor <-rnorm(n=1000, mean=c(49.8,50.8,51.1), sd=0.4)
hist(regressor)
cor(regressor, dep)
cor.test(regressor, var02)
data <- data.frame(dep, regressor, var01, var02)
model1 <- lm(dep ~ regressor)
summary(model1)
model2 <- lm(dep ~ regressor + var01 + var02, data=data)
summary(model2)
library(huxtable)
huxreg(model1, model2)
library(AICcmodavg)
models <- list(model1,model2)
aictab(cand.set=models)#tem q ter as mesmsas variáveis 


plot(dep~regressor)
plot(dep~var02)
plot(dep~var01)
plot(regressor~var02)


library(BAS)
model<-bas.lm(dep~.,data=data, prior="ZS-null", modelprior = uniform(), method = "MCMC")
plot(model, which=1, add.smooth=F)
#2. cumulative probability
plot(model, which=2)

#3. model dimension plot
plot(model, which=2)
#4.PIP
plot(model, which = 4, ask=FALSE, caption="", sub.caption="")
#model rank
image(model, rotate = F)
summary(model)
summary(data)
predicao<-data.frame(var01=43.96,var02=63.22,regressor=50.56)

pred = predict(model, newdata = predicao, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values
#var01min
predicao<-data.frame(var01=18.12,var02=63.22,regressor=50.56)
pred = predict(model, newdata = predicao, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values

#var01min + var02max
predicao<-data.frame(var01=18.12,var02=110.449,regressor=50.56)
pred = predict(model, newdata = predicao, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values

sd(regressor)
50.56+0.7
#agora mexer no regressor (1 sd para cima)
predicao<-data.frame(var01=18.12,var02=110.449,regressor=51.26)
pred = predict(model, newdata = predicao, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values

#agora mexer no regressor (2 sd para cima)
50.56+1.4
predicao<-data.frame(var01=18.12,var02=110.449,regressor=51.96)
pred = predict(model, newdata = predicao, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values
