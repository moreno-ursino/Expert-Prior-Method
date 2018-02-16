##################### plotting joint distributions

library(mvtnorm)

inv.logit <- function(x) 1/(1+exp(-x))
#### function for z-axis
zprior <- function(param1, param2, Sigma, w, grid_space){
  nsim=500
  gen_random <- rmvnorm(nsim, sigma=Sigma)
  x <- y <- seq(0,1, length.out = grid_space) 
  x[1] <- y[1] <- 0.005
  x[grid_space] <- y[grid_space] <- 0.995
  z <- matrix(rep(0, length(x)*length(x)), ncol=length(x) )
  
  for(i in 1:length(x)){
    for(j in 1:length(y)){
      for(k in 1:dim(param1)[1]){
        supp1 <- inv.logit(param1[k,1] + gen_random[,1])
        supp2 <- exp(param1[k,2] + gen_random[,2])
        supp3 <- inv.logit(param2[k,1] + gen_random[,1])
        supp4 <- exp(param2[k,2] + gen_random[,2])
        
        prod_k <- sum( dbeta(x[i], supp1*supp2, (1-supp1)*supp2) * dbeta(y[j], supp3*supp4, (1-supp3)*supp4))/ nsim
        z[i,j] <- z[i,j] + w[k]*prod_k
      }
    }
  }
  z
}




#### building of param1 and param2
param1 <- NULL
param2 <- NULL
for (k in 1:K){
  param1 <- rbind(param1, c(postmean5[1] + Xmu[k,] %*% postmean5[5:(5+n1-1)], postmean5[3] + Xgamma[k,] %*% postmean5[(5+n1):(5+n1+n2-1)]))
  param2 <- rbind( param2, c(postmean5[2] + Xmu[k,] %*% postmean5[5:(5+n1-1)], postmean5[4] + Xgamma[k,] %*% postmean5[(5+n1):(5+n1+n2-1)])) 
}

Sigma <- matrix( c(postmean5[5+n1+n2+1]^2, prod(postmean5[(5+n1+n2):(5+n1+n2+2)]), 
                   prod(postmean5[(5+n1+n2):(5+n1+n2+2)]), postmean5[5+n1+n2+2]^2),  ncol=2 )

grid_space=40
z <- zprior(param1, param2, Sigma, w=rep(1/17,17), grid_space=grid_space)
# inf and zero
# z[,1] <- rep(0,dim(z)[1])
# z[,dim(z)[1]] <- rep(0,dim(z)[1])
# z[1,] <- rep(0,dim(z)[1])
# z[dim(z)[1],] <- rep(0,dim(z)[1])

################### plotting 
x <- y <- seq(0,1, length.out = grid_space) 
x <- y <- seq(0,1, length.out = grid_space) 
x[1] <- y[1] <- 0.005
# 3000 x 1000 eps
par(mfrow = c(1,3))
persp(x, y, z, theta= -50, phi=15, col = "gray79", ticktype = "detailed", nticks = 6, xlab=" ", ylab=" ", zlab=" ", cex.axis=4 )
persp(x, y, z, theta= 0, phi=0, col = "gray79", ticktype = "detailed", nticks = 6, xlab=" ", ylab=" ", zlab=" ", cex.axis=4 )
persp(x, y, z, theta= 90, phi=0, col = "gray79", ticktype = "detailed", nticks = 6, xlab=" ", ylab=" ", zlab=" ", cex.axis=4 )
# Transform them in 3D


#######################################################################################################
#####################################################  without regression
#######################################################################################################
#######################################################################################################



logit <- function(x) log(x/(1-x))

param3 <- cbind(logit(muallendox), log(gammaallendox)) 
param4 <- cbind(logit(muallmmf), log(gammaallmmf)) 

Sigma1 <- Sigma #matrix(c(1,0.5,0.5,1), ncol=2 )
#grid_space=40
z2 <- zprior(param3, param4, Sigma1, w=rep(1/17,17), grid_space=grid_space)

# z2[,1] <- rep(0,dim(z2)[1])
# z2[,dim(z2)[1]] <- rep(0,dim(z2)[1])
# z2[1,] <- rep(0,dim(z2)[1])
# z2[dim(z2)[1],] <- rep(0,dim(z2)[1])

################### plotting 
# x <- y <- seq(0,1, length.out = grid_space) 
par(mfrow = c(1,3))
persp(x, y, z2, theta= -50, phi=15, col = "gray79", ticktype = "detailed", nticks = 6, xlab=" ", ylab=" ", zlab=" ", cex.axis=4 )
persp(x, y, z2, theta= 0, phi=0, col = "gray79", ticktype = "detailed", nticks = 6, xlab=" ", ylab=" ", zlab=" ", cex.axis=4 )
persp(x, y, z2, theta= 90, phi=0, col = "gray79", ticktype = "detailed", nticks = 6, xlab=" ", ylab=" ", zlab=" ", cex.axis=4 )


save.image()


################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################


