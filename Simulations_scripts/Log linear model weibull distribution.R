
mu=0.5
gamma = 1
sigma = 2
U<-runif(50,0,1)
W <- log(-log(1-U))
Z <- runif(50, 15, 80)
X = exp(mu+gamma*Z+sigma*W)
#R1 = 1 - exp(-exp((log(50)-mu-gamma*55.4)/sigma))

T = dweibull(1/sigma,exp(mu+gamma*Z))
T1 = pweibull(1/sigma,exp(mu+gamma*Z))

             
             
          

