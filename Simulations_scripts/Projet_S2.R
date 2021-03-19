#log(X)=mu+gammaZ+sigmaW
#Où Z ~ unif[15,80]
#W=log(-log(1-U)) où U ~ unif[0,1]
#mu=10
#gamma=1
#sigma=0.5
n=1000
mu=0.2
gamma=0.05
sigma=0.5
Z <- runif(n, 15, 80)
U <- runif(n, 0, 1)
W <- log(-log(1-U))
X <- exp(mu+ gamma*Z+ sigma*W)

T=rweibull(n, 1/sigma, exp(mu+gamma*Z))
#View(T)
#View(X)


mu=0.05
C=rexp(n,mu)

time=pmin(X,C)
#indicatrice de l'évènement d'intérêt
#delta vaut 1 si X<c et O sinon
#delta=as.numeric(TT==X)
#ou bien
status=as.numeric(X<C)
#View(Time)
pourcentage_cens=1-sum(status)/n
D<-data.frame(id=1:n, time = time, status=status, Z=Z)
D
View(D)

library(survival)
library(survminer)

strata_Z<-cut(D$Z,breaks=c(15,50,80))

f<- survfit(Surv(time, status) ~ strata_Z, type = "kaplan-meier", conf.type = "plain", D)
ggsurvplot(f, data = D)
ggsurvplot(f, fun = "cloglog", data = D, xlim=c(-50,50))


fit<-coxph(Surv(time,status)~Z, data = D)
summary(fit)

cox.zph(fit)
plot(cox.zph(fit))
abline(h=0, col='red')

# Modèle log-linéaire avec covariable dépendant du temps

n=1000
mu=0.2
gamma=0.05
sigma=0.5
Z <- runif(n, 15, 80)
U <- runif(n, 0, 1)
W <- log(-log(1-U))
X <- exp(mu+ gamma*Z+ sigma*W)

mu=0.05
C=rexp(n,mu)

time=pmin(X,C)
#indicatrice de l'évènement d'intérêt
#delta vaut 1 si X<c et O sinon
#delta=as.numeric(TT==X)
#ou bien
status=as.numeric(X<C)
#View(Time)
pourcentage_cens=1-sum(status)/n
#D<-data.frame(id=1:n, time = time, status=status, Z=Z)
#D
#View(D)

library(survival)
library(survminer)

Z_2<-Z*time
D_2<-data.frame(id=1:n, time = time, status=status, Z=Z, Z_2=Z_2)
strata_Z<-cut(D_2$Z_2,breaks=c(0,200,Inf))


f<- survfit(Surv(time, status) ~ strata_Z, type = "kaplan-meier", conf.type = "plain", D_2)
ggsurvplot(f, data = D_2)
ggsurvplot(f, fun = "cloglog", data = D_2, xlim=c(-50,50), ylim=c(-5,10))

fit_2<-coxph(Surv(time,status)~Z_2, data = D_2)
summary(fit_2)

cox.zph(fit_2)
plot(cox.zph(fit_2))
abline(h=0, col='red')

