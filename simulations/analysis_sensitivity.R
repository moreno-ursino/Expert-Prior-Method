library(rstan)

checkprob <- function(x,epsilon){
  sum(x>=-epsilon)/length(x)
}


toprint <- data.frame(matrix(NA, nrow=9, ncol=4))
for(i in 1:3){
  for (j in 1:4){
    
    eval(parse(text = paste("regr <- fit_phi",i,"lambda",j, sep="")))
    deltar <- extract(regr, pars="Delta")$Delta
    pai1 <- checkprob(deltar,0.05)
    pai2 <- checkprob(deltar,-0.15)
    estim <- quantile(deltar, probs=c(0.5,0.975,0.025))
    
    toprint[((i-1)*3+1),j] <- paste("& $/pi_{(1,2,/phi,/delta)}^{E}(.05) = ",round(pai1,2),"$", sep="")
    toprint[((i-1)*3+2),j] <- paste("& $/pi_{(1,2,/phi,/delta)}^{S}(.15) = ",round(pai2,2),"$", sep="")
    toprint[((i-1)*3+3),j] <- paste("& $CI_{95}(/theta_2 - /theta_1~|~ data, /phi, /delta) = (",round(estim[3],2),",",round(estim[2],2),")$", sep="")
    }
}                

write.table(toprint, file="toprint.txt")
library(xtable)
xtable(toprint)

# pi1 <- apply(deltasim,1,checkprob, epsilon=0.05)
# pi2 <- apply(deltasim,1,checkprob, epsilon=0.10)
# 
# q1 = quantile(pi1, probs = c(0.5, 0.25, 0.75))
# q2 = quantile(pi2, probs = c(0.5, 0.25, 0.75))
# 
# est1 <- apply(theta1sim,1,median)
# est2 <- apply(theta2sim,1,median)
# q3 = quantile(est1, probs = c(0.5, 0.25, 0.75))
# q4 = quantile(est2, probs = c(0.5, 0.25, 0.75))
# 
# 
# xt1= paste(round(q1[1],2), " (", round(q1[2],2),", ",round(q1[3],2), ")", sep = "")
# xt2= paste(round(q2[1],2), " (", round(q2[2],2),", ",round(q2[3],2), ")", sep = "")
# xt3= paste(round(q3[1],2), " (", round(q3[2],2),", ",round(q3[3],2), ")", sep = "")
# xt4= paste(round(q4[1],2), " (", round(q4[2],2),", ",round(q4[3],2), ")", sep = "")

library(xtable)
xtable(cbind(xt1,xt2,xt3,xt4))


save.image()
q()
n








