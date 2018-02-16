################################################################################
#
#   Script used to compute the regression parameters for mu and gamma (MU)
#   Run before: analyse.R
#
#
################################################################################






library(rstan)


sm_hyper <- stan_model(file="hyperparam.stan", model_name='hyp',verbose=FALSE)
######################### mu gamma previously computed

muallendox <- c(muendoxexp2,muendoxexp3,muendoxexp4,muendoxexp5,muendoxexp6,muendoxexp7,muendoxexp8,muendoxexp9,muendoxexp10,
                muendoxexp13,muendoxexp14,muendoxexp15,muendoxexp16,muendoxexp17,muendoxexp18,muendoxexp20,muendoxexp21)
gammaallendox <- c(gammaendoxexp2,gammaendoxexp3,gammaendoxexp4,gammaendoxexp5,gammaendoxexp6,gammaendoxexp7,gammaendoxexp8,gammaendoxexp9,gammaendoxexp10,
                   gammaendoxexp13,gammaendoxexp14,gammaendoxexp15,gammaendoxexp16,gammaendoxexp17,gammaendoxexp18,gammaendoxexp20,gammaendoxexp21)
muallmmf <- c(mummfexp2,mummfexp3,mummfexp4,mummfexp5,mummfexp6,mummfexp7,mummfexp8,mummfexp9,mummfexp10,
              mummfexp13,mummfexp14,mummfexp15,mummfexp16,mummfexp17,mummfexp18,mummfexp20,mummfexp21)
gammaallmmf <- c(gammammfexp2,gammammfexp3,gammammfexp4,gammammfexp5,gammammfexp6,gammammfexp7,gammammfexp8,gammammfexp9,gammammfexp10,
                 gammammfexp13,gammammfexp14,gammammfexp15,gammammfexp16,gammammfexp17,gammammfexp18,gammammfexp20,gammammfexp21)

mugammaendox <- cbind(muallendox,gammaallendox)
mugammammf <- cbind(muallmmf,gammaallmmf)


############ formula for covariate
formulamu <- ~ log(nbped) + log(nbpat) + methodf -1 # + log(nbpatmmf) +log(nbptendox+0.5) -1 
Xmu <- model.matrix(formulamu, expilb)
formulagamma <- ~  log(nbped) + log(nbpat) + methodf -1 # + log(nbpatmmf) +log(nbptendox+0.5) -1
Xgamma <- model.matrix(formulagamma, expilb)

n1 <- dim(Xmu)[2]
n2 <- dim(Xgamma)[2]
K <- dim(Xmu)[1]


data_p = list(K=K, mugamma1=mugammaendox, mugamma2=mugammammf, n1=n1, n2=n2, Xmu=Xmu, Xgamma=Xgamma   )
set.seed(1200)
#fit5 <- sampling(sm_hyper5, data=data_p ,iter=5000, chains=2, control = list(adapt_delta = 0.85))

fit5 <- sampling(sm_hyper, data=data_p ,iter=4000, chains=4, control = list(adapt_delta = 0.85))

postmean5 <- get_posterior_mean(fit5)[,5]



