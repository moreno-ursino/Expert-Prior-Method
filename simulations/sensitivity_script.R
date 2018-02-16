####################################################################
#
# for sensitivity analysis. Code by MU 24/11/2016
# run before: analyse.R (SZ and MU)
#             regression_mugamma.R (MU)
#
#
####################################################################
library(mvtnorm)
library(rstan)
betanoninf <- stan_model(file="betareg_noninf.stan", model_name='betanoninf',verbose=FALSE)
sm_betamixinteg <- stan_model(file="mixture_prior_integration.stan", model_name='regpaper',verbose=FALSE)




# empmean <- function(x, low, up) sum(x*(up+low)/2)
# 
# hismean1 <- apply(endoxb,2,empmean,low=lowbeta,up=upbeta)
# hismean2 <- apply(mmfb,2,empmean,low=lowbeta,up=upbeta)



Met = 1
We = 3


### weight ########################################################



# data_w <- list(y1=y1, y2=y2, N1 = length(y1), N2 = length(y2))
# fit_w <- sampling(betanoninf, data=data_w ,iter=4000, chains=4) # control = list(max_treedepth = 15))
# postmeanweight <- get_posterior_mean(fit_w)[,5]
# w1supp <- (abs(hismean1-postmeanweight[1]) + abs(hismean2-postmeanweight[2]))/2 
# w1 = w1supp^(-1)/sum(w1supp^(-1) )


### weight ########################################################


# equal
w3 = rep(1/17,17)

# cbind(w1,w2,w3)

################################ method 1 

# method1
logit <- function(x) log(x/(1-x))

param3 <- cbind(logit(muallendox), log(gammaallendox)) 
param4 <- cbind(logit(muallmmf), log(gammaallmmf)) 

Sigma <- matrix( c(postmean5[5+n1+n2+1]^2,prod(postmean5[(5+n1+n2):(5+n1+n2+2)]), 
                   prod(postmean5[(5+n1+n2):(5+n1+n2+2)]), postmean5[5+n1+n2+2]^2) , ncol=2, byrow = T )

NSAMP=500


#simulate dataset
set.seed(401)
y1 = rbinom(33,1,prob=0.4)
y2 = rbinom(35,1,prob=0.4)
# sum(y1)
# sum(y2)


gen_random <- rmvnorm(NSAMP, sigma=Sigma)

######################################## analysis
phi_tot <- c(1,0.5,0)
lambda_tot <- c(1,0.75,0.5,0.25)

param1r <- param3
param2r <- param4

for (i in 1:3){
  for (j in 1:4){
    
    # mean
    param2r[,1] <- (1-phi_tot[i])*param1r[,1] +  phi_tot[i]*param2r[,1]
    # location
    param1r[,2] <- lambda_tot[j]*param1r[,2]
    param2r[,2] <- lambda_tot[j]*param2r[,2]
    
    
    data_p = list(K=K,  muphi1=param1r, muphi2=param2r, N1=length(y1), N2=length(y2), y1=y1, y2=y2, w=w3, epsilons=gen_random, NSAMP=NSAMP)
    fit_reg <- sampling(sm_betamixinteg, data=data_p ,iter=3000, chains=3) 
    eval(parse(text = paste("fit_phi",i,"lambda",j," <- fit_reg", sep="")))
    save.image()
  }
}



