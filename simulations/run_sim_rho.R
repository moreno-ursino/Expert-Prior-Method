####################################################################
#
# for simulation. Code by MU 24/07/2016
# run before: analyse.R (SZ and MU)
#             regression_mugamma.R (MU)
#             gen_scenario_sensitivity.R (MU)
#
#
####################################################################
library(mvtnorm)
library(rstan)
betanoninf <- stan_model(file="betareg_noninf.stan", model_name='betanoninf',verbose=FALSE)
sm_betamixinteg <- stan_model(file="mixture_prior_integration.stan", model_name='regpaper',verbose=FALSE)





Met = 1
We = 3


### weight ########################################################

# equal
w3 = rep(1/17,17)



################################ method 1 or method 2

# method1
logit <- function(x) log(x/(1-x))

param3 <- cbind(logit(muallendox), log(gammaallendox)) 
param4 <- cbind(logit(muallmmf), log(gammaallmmf)) 

rhos = 0.5

Sigma <- matrix( c(postmean5[5+n1+n2+1]^2, rhos*prod(postmean5[(5+n1+n2+1):(5+n1+n2+2)]),
                   rhos*prod(postmean5[(5+n1+n2+1):(5+n1+n2+2)]), postmean5[5+n1+n2+2]^2) , ncol=2, byrow = T )

NSAMP=500
set.seed(99)
gen_random <- rmvnorm(NSAMP, sigma=Sigma)
set.seed(100)


######################################## analysis
deltasim <- NULL
theta1sim <- NULL
theta2sim <- NULL

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







