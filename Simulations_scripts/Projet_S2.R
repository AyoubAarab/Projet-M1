#log(X)=mu+gammaZ+sigmaW
#Où Z ~ unif[15,80]
#W=log(-log(1-U)) où U ~ unif[0,1]
#mu=10
#gamma=1
#sigma=0.5

mu=0.2
gamma=0.3
sigma=0.5
Z <- as.numeric(runif(50, 15, 80) < 50)
U <- runif(50, 0, 1)
W <- log(-log(1-U))
X <- exp(mu+ gamma*Z+ sigma*W)

T=rweibull(50, 1/sigma, exp(mu+gamma*Z))
#View(T)
#View(X)


mu=0.1
C=rexp(50,mu)

time=pmin(X,C)
#indicatrice de l'évènement d'intérêt
#delta vaut 1 si X<c et O sinon
#delta=as.numeric(TT==X)
#ou bien
status=as.numeric(X<C)
#View(Time)
D<-data.frame(id=1:50, time = time, status=status, Z=Z)
D
View(D)

library(survival)
library(survminer)

f<- survfit(Surv(time, status) ~ Z, type = "kaplan-meier", conf.type = "plain", D)
ggsurvplot(f, data = D)
ggsurvplot(f, fun = "cloglog", data = D, xlim=c(-50,50))

