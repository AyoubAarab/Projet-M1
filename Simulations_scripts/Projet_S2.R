#log(X)=mu+gammaZ+sigmaW
#Où Z ~ unif[15,80]
#W=log(-log(1-U)) où U ~ unif[0,1]
#mu=10
#gamma=1
#sigma=0.5

mu=0.2
gamma=0.3
sigma=0.5
Z <- runif(50, 15, 80)
U <- runif(50, 0, 1)
W <- log(-log(1-U))
X <- (mu+ gamma*Z+ sigma*W)

T=rweibull(50, 1/sigma, exp(mu+gamma*Z))
#View(T)
View(X)


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
