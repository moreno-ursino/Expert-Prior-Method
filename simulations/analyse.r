######################################################################
#
#   Script to obtain the mu and gamma of each expert for each arm
#                   by SZ
#
#
######################################################################




rm(list = ls())
require(stats4)
#require(LearnBayes)
require(MCMCpack)

expil <- read.table(file=".../code/experts_table.txt",sep="\t",header=T)
endox <- read.table(file=".../code/endox.txt",sep="\t",header=T)
mmf <- read.table(file=".../code/mmf.txt",sep="\t",header=T)
delta <- read.table(file="C:/code/experts_elicitation/delta.txt",sep="\t",header=T)


summary(expil)
table(expil$methodf)
table(expil$statf)
table(expil$resul)


#######################################################################################
#####Beta priors
lowbeta <<- seq(0,0.95,0.05)
upbeta <<- seq(0.05,1,0.05)

betaprior<- function(a=1.1,b=1.1)
{
  sum((pbeta(upbeta,a,b)-pbeta(lowbeta,a,b)-pexpert)^2)
}

#########Endoxan###############
endoxb <- endox[1:20,-1]
endoxb$exp2 <- ifelse(is.na(endoxb$exp2)==T,0,endoxb$exp2*0.05)
endoxb$exp3 <- ifelse(is.na(endoxb$exp3)==T,0,endoxb$exp3*0.05)
endoxb$exp4 <- ifelse(is.na(endoxb$exp4)==T,0,endoxb$exp4*0.05)
endoxb$exp5 <- ifelse(is.na(endoxb$exp5)==T,0,endoxb$exp5*0.05)
endoxb$exp6 <- ifelse(is.na(endoxb$exp6)==T,0,endoxb$exp6*0.05)
endoxb$exp7 <- ifelse(is.na(endoxb$exp7)==T,0,endoxb$exp7*0.05)
endoxb$exp8 <- ifelse(is.na(endoxb$exp8)==T,0,endoxb$exp8*0.05)
endoxb$exp9 <- ifelse(is.na(endoxb$exp9)==T,0,endoxb$exp9*0.05)
endoxb$exp10 <- ifelse(is.na(endoxb$exp10)==T,0,endoxb$exp10*0.05)
endoxb$exp13 <- ifelse(is.na(endoxb$exp13)==T,0,endoxb$exp13*0.05)
endoxb$exp14 <- ifelse(is.na(endoxb$exp14)==T,0,endoxb$exp14*0.05)
endoxb$exp15 <- ifelse(is.na(endoxb$exp15)==T,0,endoxb$exp15*0.05)
endoxb$exp16 <- ifelse(is.na(endoxb$exp16)==T,0,endoxb$exp16*0.05)
endoxb$exp17 <- ifelse(is.na(endoxb$exp17)==T,0,endoxb$exp17*0.05)
endoxb$exp18 <- ifelse(is.na(endoxb$exp18)==T,0,endoxb$exp18*0.05)
endoxb$exp19 <- ifelse(is.na(endoxb$exp19)==T,0,endoxb$exp19*0.05)
endoxb$exp20 <- ifelse(is.na(endoxb$exp20)==T,0,endoxb$exp20*0.05)
endoxb$exp21 <- ifelse(is.na(endoxb$exp21)==T,0,endoxb$exp21*0.05)
endoxb <- endoxb[,c(-1,-17)]

pexpert <<- endoxb$exp2
bendoxexp2 <- attr(mle(betaprior,method="Nelder-Mead"),"details")$par
pexpert <<- endoxb$exp3
bendoxexp3 <- attr(mle(betaprior,method="Nelder-Mead"),"details")$par
pexpert <<- endoxb$exp4
bendoxexp4 <- attr(mle(betaprior,method="Nelder-Mead"),"details")$par
pexpert <<- endoxb$exp5
bendoxexp5 <- attr(mle(betaprior,method="Nelder-Mead"),"details")$par
pexpert <<- endoxb$exp6
bendoxexp6 <- attr(mle(betaprior,method="Nelder-Mead"),"details")$par
pexpert <<- endoxb$exp7
bendoxexp7 <- attr(mle(betaprior,method="Nelder-Mead"),"details")$par
pexpert <<- endoxb$exp8
bendoxexp8 <- attr(mle(betaprior,method="Nelder-Mead"),"details")$par
pexpert <<- endoxb$exp9
bendoxexp9 <- attr(mle(betaprior,method="Nelder-Mead"),"details")$par
pexpert <<- endoxb$exp10
bendoxexp10 <- attr(mle(betaprior,method="Nelder-Mead"),"details")$par
pexpert <<- endoxb$exp13
bendoxexp13 <- attr(mle(betaprior,method="Nelder-Mead"),"details")$par
pexpert <<- endoxb$exp14
bendoxexp14 <- attr(mle(betaprior,method="Nelder-Mead"),"details")$par
pexpert <<- endoxb$exp15
bendoxexp15 <- attr(mle(betaprior,method="Nelder-Mead"),"details")$par
pexpert <<- endoxb$exp16
bendoxexp16 <- attr(mle(betaprior,method="Nelder-Mead"),"details")$par
pexpert <<- endoxb$exp17
bendoxexp17 <- attr(mle(betaprior,method="Nelder-Mead"),"details")$par
pexpert <<- endoxb$exp18
bendoxexp18 <- attr(mle(betaprior,method="Nelder-Mead"),"details")$par
pexpert <<- endoxb$exp20
bendoxexp20 <- attr(mle(betaprior,method="Nelder-Mead"),"details")$par
pexpert <<- endoxb$exp21
bendoxexp21 <- attr(mle(betaprior,method="Nelder-Mead"),"details")$par

##################MMF################
mmfb <- mmf[1:20,-1]
mmfb$exp2 <- ifelse(is.na(mmfb$exp2)==T,0,mmfb$exp2*0.05)
mmfb$exp3 <- ifelse(is.na(mmfb$exp3)==T,0,mmfb$exp3*0.05)
mmfb$exp4 <- ifelse(is.na(mmfb$exp4)==T,0,mmfb$exp4*0.05)
mmfb$exp5 <- ifelse(is.na(mmfb$exp5)==T,0,mmfb$exp5*0.05)
mmfb$exp6 <- ifelse(is.na(mmfb$exp6)==T,0,mmfb$exp6*0.05)
mmfb$exp7 <- ifelse(is.na(mmfb$exp7)==T,0,mmfb$exp7*0.05)
mmfb$exp8 <- ifelse(is.na(mmfb$exp8)==T,0,mmfb$exp8*0.05)
mmfb$exp9 <- ifelse(is.na(mmfb$exp9)==T,0,mmfb$exp9*0.05)
mmfb$exp10 <- ifelse(is.na(mmfb$exp10)==T,0,mmfb$exp10*0.05)
mmfb$exp13 <- ifelse(is.na(mmfb$exp13)==T,0,mmfb$exp13*0.05)
mmfb$exp14 <- ifelse(is.na(mmfb$exp14)==T,0,mmfb$exp14*0.05)
mmfb$exp15 <- ifelse(is.na(mmfb$exp15)==T,0,mmfb$exp15*0.05)
mmfb$exp16 <- ifelse(is.na(mmfb$exp16)==T,0,mmfb$exp16*0.05)
mmfb$exp17 <- ifelse(is.na(mmfb$exp17)==T,0,mmfb$exp17*0.05)
mmfb$exp18 <- ifelse(is.na(mmfb$exp18)==T,0,mmfb$exp18*0.05)
mmfb$exp19 <- ifelse(is.na(mmfb$exp19)==T,0,mmfb$exp19*0.05)
mmfb$exp20 <- ifelse(is.na(mmfb$exp20)==T,0,mmfb$exp20*0.05)
mmfb$exp21 <- ifelse(is.na(mmfb$exp21)==T,0,mmfb$exp21*0.05)
mmfb <- mmfb[,c(-1,-17)]

pexpert <<- mmfb$exp2
bmmfexp2 <- attr(mle(betaprior,method="Nelder-Mead"),"details")$par
pexpert <<- mmfb$exp3
bmmfexp3 <- attr(mle(betaprior,method="Nelder-Mead"),"details")$par
pexpert <<- mmfb$exp4
bmmfexp4 <- attr(mle(betaprior,method="Nelder-Mead"),"details")$par
pexpert <<- mmfb$exp5
bmmfexp5 <- attr(mle(betaprior,method="Nelder-Mead"),"details")$par
pexpert <<- mmfb$exp6
bmmfexp6 <- attr(mle(betaprior,method="Nelder-Mead"),"details")$par
pexpert <<- mmfb$exp7
bmmfexp7 <- attr(mle(betaprior,method="Nelder-Mead"),"details")$par
pexpert <<- mmfb$exp8
bmmfexp8 <- attr(mle(betaprior,method="Nelder-Mead"),"details")$par
pexpert <<- mmfb$exp9
bmmfexp9 <- attr(mle(betaprior,method="Nelder-Mead"),"details")$par
pexpert <<- mmfb$exp10
bmmfexp10 <- attr(mle(betaprior,method="Nelder-Mead"),"details")$par
pexpert <<- mmfb$exp13
bmmfexp13 <- attr(mle(betaprior,method="Nelder-Mead"),"details")$par
pexpert <<- mmfb$exp14
bmmfexp14 <- attr(mle(betaprior,method="Nelder-Mead"),"details")$par
pexpert <<- mmfb$exp15
bmmfexp15 <- attr(mle(betaprior,method="Nelder-Mead"),"details")$par
pexpert <<- mmfb$exp16
bmmfexp16 <- attr(mle(betaprior,method="Nelder-Mead"),"details")$par
pexpert <<- mmfb$exp17
bmmfexp17 <- attr(mle(betaprior,method="Nelder-Mead"),"details")$par
pexpert <<- mmfb$exp18
bmmfexp18 <- attr(mle(betaprior,method="Nelder-Mead"),"details")$par
pexpert <<- mmfb$exp20
bmmfexp20 <- attr(mle(betaprior,method="Nelder-Mead"),"details")$par
pexpert <<- mmfb$exp21
bmmfexp21 <- attr(mle(betaprior,method="Nelder-Mead"),"details")$par

mummfexp2 <- bmmfexp2[1] / (bmmfexp2[1] +bmmfexp2[2] )
gammammfexp2 <- bmmfexp2[1] +bmmfexp2[2]
mummfexp3 <- bmmfexp3[1] / (bmmfexp3[1] +bmmfexp3[2] )
gammammfexp3 <- bmmfexp3[1] +bmmfexp3[2]
mummfexp4 <- bmmfexp4[1] / (bmmfexp4[1] +bmmfexp4[2] )
gammammfexp4 <- bmmfexp4[1] +bmmfexp4[2]
mummfexp5 <- bmmfexp5[1] / (bmmfexp5[1] +bmmfexp5[2] )
gammammfexp5 <- bmmfexp5[1] +bmmfexp5[2]
mummfexp6 <- bmmfexp6[1] / (bmmfexp6[1] +bmmfexp6[2] )
gammammfexp6 <- bmmfexp6[1] +bmmfexp6[2]
mummfexp7 <- bmmfexp7[1] / (bmmfexp7[1] +bmmfexp7[2] )
gammammfexp7 <- bmmfexp7[1] +bmmfexp7[2]
mummfexp8 <- bmmfexp8[1] / (bmmfexp8[1] +bmmfexp8[2] )
gammammfexp8 <- bmmfexp8[1] +bmmfexp8[2]
mummfexp9 <- bmmfexp9[1] / (bmmfexp9[1] +bmmfexp9[2] )
gammammfexp9 <- bmmfexp9[1] +bmmfexp9[2]
mummfexp10 <- bmmfexp10[1] / (bmmfexp10[1] +bmmfexp10[2] )
gammammfexp10 <- bmmfexp10[1] +bmmfexp10[2]
mummfexp13 <- bmmfexp13[1] / (bmmfexp13[1] +bmmfexp13[2] )
gammammfexp13 <- bmmfexp13[1] +bmmfexp13[2]
mummfexp14 <- bmmfexp14[1] / (bmmfexp14[1] +bmmfexp14[2] )
gammammfexp14 <- bmmfexp14[1] +bmmfexp14[2]
mummfexp15 <- bmmfexp15[1] / (bmmfexp15[1] +bmmfexp15[2] )
gammammfexp15 <- bmmfexp15[1] +bmmfexp15[2]
mummfexp16 <- bmmfexp16[1] / (bmmfexp16[1] +bmmfexp16[2] )
gammammfexp16 <- bmmfexp16[1] +bmmfexp16[2]
mummfexp17 <- bmmfexp17[1] / (bmmfexp17[1] +bmmfexp17[2] )
gammammfexp17 <- bmmfexp17[1] +bmmfexp17[2]
mummfexp18 <- bmmfexp18[1] / (bmmfexp18[1] +bmmfexp18[2] )
gammammfexp18 <- bmmfexp18[1] +bmmfexp18[2]
mummfexp20 <- bmmfexp20[1] / (bmmfexp20[1] +bmmfexp20[2] )
gammammfexp20 <- bmmfexp20[1] +bmmfexp20[2]
mummfexp21 <- bmmfexp21[1] / (bmmfexp21[1] +bmmfexp21[2] )
gammammfexp21 <- bmmfexp21[1] +bmmfexp21[2]

muendoxexp2 <- bendoxexp2[1] / (bendoxexp2[1] +bendoxexp2[2] )
gammaendoxexp2 <- bendoxexp2[1] +bendoxexp2[2]
muendoxexp3 <- bendoxexp3[1] / (bendoxexp3[1] +bendoxexp3[2] )
gammaendoxexp3 <- bendoxexp3[1] +bendoxexp3[2]
muendoxexp4 <- bendoxexp4[1] / (bendoxexp4[1] +bendoxexp4[2] )
gammaendoxexp4 <- bendoxexp4[1] +bendoxexp4[2]
muendoxexp5 <- bendoxexp5[1] / (bendoxexp5[1] +bendoxexp5[2] )
gammaendoxexp5 <- bendoxexp5[1] +bendoxexp5[2]
muendoxexp6 <- bendoxexp6[1] / (bendoxexp6[1] +bendoxexp6[2] )
gammaendoxexp6 <- bendoxexp6[1] +bendoxexp6[2]
muendoxexp7 <- bendoxexp7[1] / (bendoxexp7[1] +bendoxexp7[2] )
gammaendoxexp7 <- bendoxexp7[1] +bendoxexp7[2]
muendoxexp8 <- bendoxexp8[1] / (bendoxexp8[1] +bendoxexp8[2] )
gammaendoxexp8 <- bendoxexp8[1] +bendoxexp8[2]
muendoxexp9 <- bendoxexp9[1] / (bendoxexp9[1] +bendoxexp9[2] )
gammaendoxexp9 <- bendoxexp9[1] +bendoxexp9[2]
muendoxexp10 <- bendoxexp10[1] / (bendoxexp10[1] +bendoxexp10[2] )
gammaendoxexp10 <- bendoxexp10[1] +bendoxexp10[2]
muendoxexp13 <- bendoxexp13[1] / (bendoxexp13[1] +bendoxexp13[2] )
gammaendoxexp13 <- bendoxexp13[1] +bendoxexp13[2]
muendoxexp14 <- bendoxexp14[1] / (bendoxexp14[1] +bendoxexp14[2] )
gammaendoxexp14 <- bendoxexp14[1] +bendoxexp14[2]
muendoxexp15 <- bendoxexp15[1] / (bendoxexp15[1] +bendoxexp15[2] )
gammaendoxexp15 <- bendoxexp15[1] +bendoxexp15[2]
muendoxexp16 <- bendoxexp16[1] / (bendoxexp16[1] +bendoxexp16[2] )
gammaendoxexp16 <- bendoxexp16[1] +bendoxexp16[2]
muendoxexp17 <- bendoxexp17[1] / (bendoxexp17[1] +bendoxexp17[2] )
gammaendoxexp17 <- bendoxexp17[1] +bendoxexp17[2]
muendoxexp18 <- bendoxexp18[1] / (bendoxexp18[1] +bendoxexp18[2] )
gammaendoxexp18 <- bendoxexp18[1] +bendoxexp18[2]
muendoxexp20 <- bendoxexp20[1] / (bendoxexp20[1] +bendoxexp20[2] )
gammaendoxexp20 <- bendoxexp20[1] +bendoxexp20[2]
muendoxexp21 <- bendoxexp21[1] / (bendoxexp21[1] +bendoxexp21[2] )
gammaendoxexp21 <- bendoxexp21[1] +bendoxexp21[2]


source(file="/Users/sarahzohar/Dropbox/shared_files/InSPiReShared/Peter/experts_elicitation/graphiques_experts.r")

muallendox <- c(muendoxexp2,muendoxexp3,muendoxexp4,muendoxexp5,muendoxexp6,muendoxexp7,muendoxexp8,muendoxexp9,muendoxexp10,
                muendoxexp13,muendoxexp14,muendoxexp15,muendoxexp16,muendoxexp17,muendoxexp18,muendoxexp20,muendoxexp21)
gammaallendox <- c(gammaendoxexp2,gammaendoxexp3,gammaendoxexp4,gammaendoxexp5,gammaendoxexp6,gammaendoxexp7,gammaendoxexp8,gammaendoxexp9,gammaendoxexp10,
                   gammaendoxexp13,gammaendoxexp14,gammaendoxexp15,gammaendoxexp16,gammaendoxexp17,gammaendoxexp18,gammaendoxexp20,gammaendoxexp21)
muallmmf <- c(mummfexp2,mummfexp3,mummfexp4,mummfexp5,mummfexp6,mummfexp7,mummfexp8,mummfexp9,mummfexp10,
              mummfexp13,mummfexp14,mummfexp15,mummfexp16,mummfexp17,mummfexp18,mummfexp20,mummfexp21)
gammaallmmf <- c(gammammfexp2,gammammfexp3,gammammfexp4,gammammfexp5,gammammfexp6,gammammfexp7,gammammfexp8,gammammfexp9,gammammfexp10,
                 gammammfexp13,gammammfexp14,gammammfexp15,gammammfexp16,gammammfexp17,gammammfexp18,gammammfexp20,gammammfexp21)

mubarendox <-  sum(muallendox)/length(muallendox)
mubarmmf <-  sum(muallmmf)/length(muallmmf)
gammabarendox <- sum(gammaallendox)/length(gammaallendox)
gammabarmmf <- sum(gammaallmmf)/length(gammaallmmf)
espdelta <- mubarmmf-mubarendox

