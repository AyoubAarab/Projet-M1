# Modèle exponentiel
# TP1
n=50
lambda=1
#st.seed(7)
X=rexp(n,lambda)
X

# Génération de la suite des censures
mu=0.5
C=rexp(n,mu)

TT=pmin(X,C)
#indicatrice de l'évènement d'intérêt
#delta vaut 1 si X<c et O sinon
delta=as.numeric(TT==X)
#ou bien
delta=as.numeric(X<C)
pourcentage_cens=1-sum(delta)/n
cat("Pourcentage de censure :", pourcentage_cens)
# On observe que le pourcentage de données censurées dans l'échantillon
# fluctue selon le tirage autour d'une valeur environ égale à 0.3 ... à la louche

# Question 2
#EMV de lambda
lambda_chapo<-sum(delta)/sum(TT)
cat("L'estimation de lambda est:", lambda_chapo)
# A taille d'échantillon fixée, ici pour n=50, on observe la fluctuation de l'estimateur
#lambda_chapo : pour chaque échantillon, on a une nouvelle réalisation de la variable 
#aléatoire lambda_chapo

#Question 3 : Illustration de la cvg p.s de lambda_chapo vers lambda

n=5000
X=rexp(n,lambda)
C=rexp(n,mu)
TT=pmin(X,C)#calcule le minimum entre X et C pour chaque composante des vecteurs
delta=as.numeric(X<C)
lambda_chapo_n=rep(NA,n-10+1)
for (i in 10:n) {lambda_chapo_n[i-10+1]=sum(delta[1:i])/sum(TT[1:i])}
index=c(10:5000)
plot(index,lambda_chapo_n,type="l",col="blue",xlab = "taille de l'échantillon",ylab=expression(hat(lambda)))
abline(h=1)
# pour un omega, on observe lambda_chap converge vers lambda lorsque n tend ver +inf

#Question 4

#Intervalle de confiance de ?? pour un niveau de confiance de 95% pour l'échantillon complètement observé
n=5000 # Pour n allant de 10 à 5000
#set.seed(7)
X<- rexp(n,lambda)
lambda_chap_n<- rep(0,n-10+1)
for (i in 10:n){lambda_chap_n[i-10+1]=i/ sum(X[1:i])}

borne_inf <- rep(NA,5000)
borne_sup <- rep(NA,5000) 
for(i in 10:n){
  borne_inf[i-10+1] <- lambda_chap_n[i-10+1]*(1-(1.96)/sqrt(i))
  borne_sup[i-10+1] <- lambda_chap_n[i-10+1]*(1+(1.96)/sqrt(i))
}
# Création de l'intervalle de confiance à 95% avec les bornes inf et sup

#Graphique de l'intervalle de confiance pour l'échantillon complètement observé
plot(index,lambda_chap_n,col='red',xlab = "n",ylab="EMV")
abline(h=1)
points(index,y=borne_inf[10:5000],col='blue',type = 'o')
points(index,y=borne_sup[10:5000],col='grey',type='o')

#Question 5
#Intervalle de confiance de ?? pour un niveau de confiance de 95% pour l'échantillon censuré
# On fait la même chose pour les censures.
n=5000
set.seed(7) # on fixe la graine
X<- rexp(n,lambda)
C<-rexp(n,mu) # Censure
TT=pmin(X,C)
delta=as.numeric(TT==X)
lambda_chap_n<- rep(0,n-10+1)
for (i in 10:n){lambda_chap_n[i-10+1]=sum(delta[1:i])/ sum(TT[1:i])}
borne_inf2 <- rep(NA,5000)
borne_sup2 <- rep(NA,5000)
for(i in 10:n){
  borne_inf2[i-10+1] <- lambda_chap_n[i-10+1]*(1-(1.96)/sqrt(i))
  borne_sup2[i-10+1] <- lambda_chap_n[i-10+1]*(1+(1.96)/sqrt(i))
}
#Graphique de l'intervalle de confiance
plot(index,lambda_chap_n,col='red',xlab = "n",ylab="EMV")
abline(h=1)
points(index,y=borne_inf2[10:5000],col='blue',type = 'o')
points(index,y=borne_sup2[10:5000],col='grey', type = 'o')

#Question 6 
n=50
lambda=1
mu=0.5
X<- rexp(n,lambda)
C<-rexp(n,mu) # Censure
TT=pmin(X,C)
delta=as.numeric(TT==X)
lambda_chapo_n<-sum(delta)/sum(TT)

t<-seq(0,3,by=0.01)
#length(t)
#t<-seq(0,3,length.out = 100)
alpha=0.05
s_chapo<-exp(-lambda_chapo_n*t)
#s_vraie<-exp(-lambda*t)
plot(t,s_chapo,type = "l")

bande_inf<-s_chapo-qnorm(1-alpha/2)/sqrt(sum(delta))
bande_sup<-s_chapo+qnorm(1-alpha/2)/sqrt(sum(delta))
plot(t,s_chapo,type = "l",col="blue")
lines(t,bande_inf,col="cyan")
lines(t,bande_sup,col="cyan")
#lines(t,s_vraie,col="black")
curve(exp(-lambda*x),add = TRUE)

#Question bonus
library(survival)
don.s<-Surv(time=TT, event=delta)
s_km<-survfit(don.s~1,type="kaplan-meier",conf.type="plain")
lines(s_km,col="magenta")
#max(TT)=1.45 donc l'estimateur de K-M ne fournit pas d'estimation
# sur l'observation du maximum

#Question 7
n=100 #taille des echantillons
K=1000 #on va simuler K=1000 echantillons de taille n
X<-matrix(rexp(n*K,lambda),ncol=K)
mu=0.5
C<-matrix(rexp(n*K,mu),ncol=K)
TT<-pmin(X,C) #matrice n*K qui contient le min de X et de C
delta=as.matrix(TT==X) #ou as.matrix(X<C)
# delta est aussi une matrice de taille n*K qui contient
# des 1 et des 0 qui indiquent si la valeur est cennsurée

lambda_chapo<-apply(delta,2,sum)/apply(TT,2,sum)
#A/B pour des matrices A et B de meme taille
#applique la division des coefficients terme à terme
#A*B différent de A%*%B (produit matriciel)

#On obtient dans lambda_chapo K=1000 estimations de lambda
#donc on a un echantillon de taille 1000 de lambda_chapo

p1=lambda/(lambda+mu)
Zn=sqrt(n*p1)*(lambda_chapo-lambda)/lambda
Zn_chapo=sqrt(apply(delta,2,sum))*(lambda_chapo-lambda)/lambda_chapo
#Zn et Zn_chapo sont asymptotiquement Gaussienne
#On compare la fonction de répartition empirique de Zn et Zn_chapo
#avec la fdr de la loi N(0,1)
par(mfrow=c(1,2))#prédéfinir un graphique avec 1 ligne et 2 colonnes
plot(ecdf(Zn))
curve(pnorm(x),add=TRUE,col="magenta")

plot(ecdf(Zn_chapo))
curve(pnorm(x),add=TRUE,col="magenta")

#TP2
#Exercice 1
#1)
n=50
lambda=2
mu=0.5
X=rexp(n,lambda)
C=rexp(n,mu)
TT=pmin(X,C)
delta=as.numeric(X==TT)

D<-data.frame(TT,delta)

#Formule exacte pour l'estimateur du paramètre lambda
lambda_chap=sum(D$delta)/sum(D$TT)

#ecrire une fitexp qui renvoie la valeur du maximum de vraisemblance
#en utilisant une fonction "nlm" (lire help("nlm"))
#qui calcule numériquement le minimum d'une fonction non linéaire
#par une descente de gradient.

#tester nlm sur une fonction connue

f<-function(x){(x-1)^2}
curve(f(x),xlim=c(-2,5))
#admet un minimum en x=1
help("nlm")
nlm(f,p=-5,hessian=TRUE,iterlim = 10^3)
# à explorer avec d'autres fonctions
#Revoir l'algorithme de descente de gradient
#vraisemblance avec données censurées
#log(L)=sum(log(densité(obs))*delta) + sum(log(survie(obs))*(1-delta))
#On va minimiser -log(L)-> LL
fitexp<-function(obs,delta){
LL<-function(param){-sum(dexp(obs[delta==1],param,log=TRUE))-sum(1-pexp(obs[delta==0],param,log=TRUE))}
init<-1/mean(obs)
optimum<-nlm(LL,p=init,hessian = TRUE,iterlim = 10^3)
return(optimum)
}
fitexp(D$TT,D$delta)

#Terminer l'exercice 1
#Question 4 Survreg : modèle log-linéaire sans covariable avec loi exponentielle
#compéter le code pour éviter que le paramètre soit négatif avec un test
#ou un changement de parametrisation : exp(log(param))>0