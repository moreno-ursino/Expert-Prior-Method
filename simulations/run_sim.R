####################################################################
#
# for simulation. Code by MU 24/07/2016
# run before: analyse.R (SZ and MU)
#             regression_mugamma.R (MU)
#
#
####################################################################
library(mvtnorm)
library(rstan)
betanoninf <- stan_model(file="betareg_noninf.stan", model_name='betanoninf',verbose=FALSE)
sm_betamixinteg <- stan_model(file="mixture_prior_integration.stan", model_name='regpaper',verbose=FALSE)



# empirical Bayes - histrograms in endoxb and mfb
empmean <- function(x, low, up) sum(x*(up+low)/2)

hismean1 <- apply(endoxb,2,empmean,low=lowbeta,up=upbeta)
hismean2 <- apply(mmfb,2,empmean,low=lowbeta,up=upbeta)



Met = 1 #or 2
We = 1  # or 2 or 3


### weight ########################################################


y1 <- dataset$CRITPRINC[which(dataset$BRAS == 1)]
y2 <- dataset$CRITPRINC[which(dataset$BRAS == 2)]
data_w <- list(y1=y1, y2=y2, N1 = length(y1), N2 = length(y2))
fit_w <- sampling(betanoninf, data=data_w ,iter=4000, chains=4) # control = list(max_treedepth = 15))
postmeanweight <- get_posterior_mean(fit_w)[,5]
w1supp <- (abs(hismean1-postmeanweight[1]) + abs(hismean2-postmeanweight[2]))/2 
w1 = w1supp^(-1)/sum(w1supp^(-1) )


# expert covariate
w2 = (expilb$nbped/sum(expilb$nbped) + expilb$nbpat/sum(expilb$nbpat) + expilb$methodf/sum(expilb$methodf))/3 
#w2b = (log(expilb$nbped)/sum(log(expilb$nbped))+ log(expilb$nbpat)/sum(log(expilb$nbpat)) +
#        expilb$methodf/sum(expilb$methodf))/3

# equal
w3 = rep(1/17,17)

# cbind(w1,w2,w3)

################################ method 1 or method 2

# method1
logit <- function(x) log(x/(1-x))

param3 <- cbind(logit(muallendox), log(gammaallendox)) 
param4 <- cbind(logit(muallmmf), log(gammaallmmf)) 

# method2
param1 <- NULL
param2 <- NULL
for (k in 1:K){
  param1 <- rbind(param1, c(postmean5[1] + Xmu[k,] %*% postmean5[5:(5+n1-1)], postmean5[3] + Xgamma[k,] %*% postmean5[(5+n1):(5+n1+n2-1)]))
  param2 <- rbind( param2, c(postmean5[2] + Xmu[k,] %*% postmean5[5:(5+n1-1)], postmean5[4] + Xgamma[k,] %*% postmean5[(5+n1):(5+n1+n2-1)])) 
}

Sigma <- matrix( c(postmean5[5+n1+n2+1]^2,prod(postmean5[(5+n1+n2):(5+n1+n2+2)]), 
                   prod(postmean5[(5+n1+n2):(5+n1+n2+2)]), postmean5[5+n1+n2+2]^2) , ncol=2, byrow = T )

NSAMP=500



######################################## analysis
deltasim <- NULL
theta1sim <- NULL
theta2sim <- NULL

set.seed(99)
gen_random <- rmvnorm(NSAMP, sigma=Sigma)
set.seed(100)
# example for 100 trials
for (tr in 1:100){
    
    wn <- We #1,2,3
    methodn <- Met #1,2
    
    eval(parse(text = paste("datarun <- data",tr, sep="")))
    
    y1 <- datarun$y1
    y2 <- datarun$y2
    
    if(wn==1) {
      data_w <- list(y1=y1, y2=y2, N1 = length(y1), N2 = length(y2))
      fit_w <- sampling(betanoninf, data=data_w ,iter=4000, chains=4) # control = list(max_treedepth = 15))
      postmeanweight <- get_posterior_mean(fit_w)[,5]
      w1supp <- (abs(hismean1-postmeanweight[1]) + abs(hismean2-postmeanweight[2]))/2 
      w1 = w1supp^(-1)/sum(w1supp^(-1) )
      w=w1
    }
    if(wn==2) w=w2
    if(wn==3) w=w3
    
    if(methodn==1) {
      param1r <- param3
      param2r <- param4
    } else {
      param1r <- param1
      param2r <- param2
    }
    
    
    
    data_p = list(K=K,  muphi1=param1r, muphi2=param2r, N1=length(y1), N2=length(y2), y1=y1, y2=y2, w=w, epsilons=gen_random, NSAMP=NSAMP)
    fit_reg <- sampling(sm_betamixinteg, data=data_p ,iter=2000, chains=3) 
    deltasim <- rbind(deltasim,extract(fit_reg, pars="Delta")$Delta)
    theta1sim <- rbind(theta1sim,extract(fit_reg, pars="p1")$p1)
    theta2sim <- rbind(theta2sim,extract(fit_reg, pars="p2")$p2)
    #eval(parse(text = paste("fitm",mt,"w",ww," <- fit_reg", sep="")))
}
save.image()





