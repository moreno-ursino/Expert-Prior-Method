#########Endoxan##################
histmain <- hist(endox$X,breaks =endox$X,ylim=c(0,10))
xvec <- seq(0,1,0.001)

par(mfrow=c(2,4),mar=c(4, 3.8, 3, 1.5))
histmain$counts <- endox$exp2
histmain$density <- endox$exp2/sum(endox$exp2,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main=bquote("Exp2" ~ mu == .(round(muendoxexp2,3)) ~ gamma== .(round(gammaendoxexp2,3))))
lines(xvec,dbeta(xvec,bendoxexp2[1],bendoxexp2[2]),type="l")

histmain$counts <- endox$exp3
histmain$density <- endox$exp3/sum(endox$exp3,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main=bquote("Exp3" ~ mu == .(round(muendoxexp3,3)) ~ gamma== .(round(gammaendoxexp3,3))))
lines(xvec,dbeta(xvec,bendoxexp3[1],bendoxexp3[2]),type="l")


histmain$counts <- endox$exp4
histmain$density <- endox$exp4/sum(endox$exp4,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main=bquote("Exp4" ~ mu == .(round(muendoxexp4,3)) ~ gamma== .(round(gammaendoxexp4,3))))
lines(xvec,dbeta(xvec,bendoxexp4[1],bendoxexp4[2]),type="l")


histmain$counts <- endox$exp5
histmain$density <- endox$exp5/sum(endox$exp5,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main=bquote("Exp5" ~ mu == .(round(muendoxexp5,3)) ~ gamma== .(round(gammaendoxexp5,3))))
lines(xvec,dbeta(xvec,bendoxexp5[1],bendoxexp5[2]),type="l")


histmain$counts <- endox$exp6
histmain$density <- endox$exp6/sum(endox$exp6,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main=bquote("Exp6" ~ mu == .(round(muendoxexp6,3)) ~ gamma== .(round(gammaendoxexp6,3))))
lines(xvec,dbeta(xvec,bendoxexp6[1],bendoxexp6[2]),type="l")

histmain$counts <- endox$exp7
histmain$density <- endox$exp7/sum(endox$exp7,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main=bquote("Exp7" ~ mu == .(round(muendoxexp7,3)) ~ gamma== .(round(gammaendoxexp7,3))))
lines(xvec,dbeta(xvec,bendoxexp7[1],bendoxexp7[2]),type="l")

histmain$counts <- endox$exp8
histmain$density <- endox$exp8/sum(endox$exp8,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main=bquote("Exp8" ~ mu == .(round(muendoxexp8,3)) ~ gamma== .(round(gammaendoxexp8,3))))
lines(xvec,dbeta(xvec,bendoxexp8[1],bendoxexp8[2]),type="l")

histmain$counts <- endox$exp9
histmain$density <- endox$exp9/sum(endox$exp9,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main=bquote("Exp9" ~ mu == .(round(muendoxexp9,3)) ~ gamma== .(round(gammaendoxexp9,3))))
lines(xvec,dbeta(xvec,bendoxexp9[1],bendoxexp9[2]),type="l")

par(mfrow=c(3,3),mar=c(4, 3.8, 3, 1.5))

histmain$counts <- endox$exp10
histmain$density <- endox$exp10/sum(endox$exp10,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main=bquote("Exp10" ~ mu == .(round(muendoxexp10,3)) ~ gamma== .(round(gammaendoxexp10,3))))
lines(xvec,dbeta(xvec,bendoxexp10[1],bendoxexp10[2]),type="l")

histmain$counts <- endox$exp13
histmain$density <- endox$exp13/sum(endox$exp13,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main=bquote("Exp13" ~ mu == .(round(muendoxexp13,3)) ~ gamma== .(round(gammaendoxexp13,3))))
lines(xvec,dbeta(xvec,bendoxexp13[1],bendoxexp13[2]),type="l")

histmain$counts <- endox$exp14
histmain$density <- endox$exp14/sum(endox$exp14,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main=bquote("Exp14" ~ mu == .(round(muendoxexp14,3)) ~ gamma== .(round(gammaendoxexp14,3))))
lines(xvec,dbeta(xvec,bendoxexp14[1],bendoxexp14[2]),type="l")

histmain$counts <- endox$exp15
histmain$density <- endox$exp15/sum(endox$exp15,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main=bquote("Exp15" ~ mu == .(round(muendoxexp15,3)) ~ gamma== .(round(gammaendoxexp15,3))))
lines(xvec,dbeta(xvec,bendoxexp15[1],bendoxexp15[2]),type="l")

histmain$counts <- endox$exp16
histmain$density <- endox$exp16/sum(endox$exp16,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main=bquote("Exp16" ~ mu == .(round(muendoxexp16,3)) ~ gamma== .(round(gammaendoxexp16,3))))
lines(xvec,dbeta(xvec,bendoxexp16[1],bendoxexp16[2]),type="l")

histmain$counts <- endox$exp17
histmain$density <- endox$exp17/sum(endox$exp17,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main=bquote("Exp17" ~ mu == .(round(muendoxexp17,3)) ~ gamma== .(round(gammaendoxexp17,3))))
lines(xvec,dbeta(xvec,bendoxexp17[1],bendoxexp17[2]),type="l")

histmain$counts <- endox$exp18
histmain$density <- endox$exp18/sum(endox$exp18,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main=bquote("Exp18" ~ mu == .(round(muendoxexp18,3)) ~ gamma== .(round(gammaendoxexp18,3))))
lines(xvec,dbeta(xvec,bendoxexp18[1],bendoxexp18[2]),type="l")

histmain$counts <- endox$exp20
histmain$density <- endox$exp20/sum(endox$exp20,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main=bquote("Exp20" ~ mu == .(round(muendoxexp20,3)) ~ gamma== .(round(gammaendoxexp20,3))))
lines(xvec,dbeta(xvec,bendoxexp20[1],bendoxexp20[2]),type="l")

histmain$counts <- endox$exp21
histmain$density <- endox$exp21/sum(endox$exp21,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main=bquote("Exp21" ~ mu == .(round(muendoxexp21,3)) ~ gamma== .(round(gammaendoxexp21,3))))
lines(xvec,dbeta(xvec,bendoxexp21[1],bendoxexp21[2]),type="l")

############MMF

histmain <- hist(mmf$X,breaks =mmf$X,ylim=c(0,10))
xvec <- seq(0,1,0.001)

par(mfrow=c(2, 4),mar=c(4, 3.8, 3, 1.5))
histmain$counts <- mmf$exp2
histmain$density <- mmf$exp2/sum(mmf$exp2,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main=bquote("Exp2" ~ mu == .(round(muendoxexp2,3)) ~ gamma== .(round(gammaendoxexp2,3))))
lines(xvec,dbeta(xvec,bmmfexp2[1],bmmfexp2[2]),type="l")

histmain$counts <- mmf$exp3
histmain$density <- mmf$exp3/sum(mmf$exp3,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main=bquote("Exp3" ~ mu == .(round(muendoxexp3,3)) ~ gamma== .(round(gammaendoxexp3,3))))
lines(xvec,dbeta(xvec,bmmfexp3[1],bmmfexp3[2]),type="l")


histmain$counts <- mmf$exp4
histmain$density <- mmf$exp4/sum(mmf$exp4,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main=bquote("Exp4" ~ mu == .(round(muendoxexp4,3)) ~ gamma== .(round(gammaendoxexp4,3))))
lines(xvec,dbeta(xvec,bmmfexp4[1],bmmfexp4[2]),type="l")


histmain$counts <- mmf$exp5
histmain$density <- mmf$exp5/sum(mmf$exp5,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main=bquote("Exp5" ~ mu == .(round(muendoxexp5,3)) ~ gamma== .(round(gammaendoxexp5,3))))
lines(xvec,dbeta(xvec,bmmfexp5[1],bmmfexp5[2]),type="l")


histmain$counts <- mmf$exp6
histmain$density <- mmf$exp6/sum(mmf$exp6,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main=bquote("Exp6" ~ mu == .(round(muendoxexp6,3)) ~ gamma== .(round(gammaendoxexp6,3))))
lines(xvec,dbeta(xvec,bmmfexp6[1],bmmfexp6[2]),type="l")

histmain$counts <- mmf$exp7
histmain$density <- mmf$exp7/sum(mmf$exp7,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main=bquote("Exp7" ~ mu == .(round(muendoxexp7,3)) ~ gamma== .(round(gammaendoxexp7,3))))
lines(xvec,dbeta(xvec,bmmfexp7[1],bmmfexp7[2]),type="l")

histmain$counts <- mmf$exp8
histmain$density <- mmf$exp8/sum(mmf$exp8,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main=bquote("Exp8" ~ mu == .(round(muendoxexp8,3)) ~ gamma== .(round(gammaendoxexp8,3))))
lines(xvec,dbeta(xvec,bmmfexp8[1],bmmfexp8[2]),type="l")

histmain$counts <- mmf$exp9
histmain$density <- mmf$exp9/sum(mmf$exp9,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main=bquote("Exp9" ~ mu == .(round(muendoxexp9,3)) ~ gamma== .(round(gammaendoxexp9,3))))
lines(xvec,dbeta(xvec,bmmfexp9[1],bmmfexp9[2]),type="l")

par(mfrow=c(3,3),mar=c(4, 3.8, 3, 1.5))

histmain$counts <- mmf$exp10
histmain$density <- mmf$exp10/sum(mmf$exp10,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main=bquote("Exp10" ~ mu == .(round(muendoxexp10,3)) ~ gamma== .(round(gammaendoxexp10,3))))
lines(xvec,dbeta(xvec,bmmfexp10[1],bmmfexp10[2]),type="l")

histmain$counts <- mmf$exp13
histmain$density <- mmf$exp13/sum(mmf$exp13,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main=bquote("Exp13" ~ mu == .(round(muendoxexp13,3)) ~ gamma== .(round(gammaendoxexp13,3))))
lines(xvec,dbeta(xvec,bmmfexp13[1],bmmfexp13[2]),type="l")

histmain$counts <- mmf$exp14
histmain$density <- mmf$exp14/sum(mmf$exp14,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main=bquote("Exp14" ~ mu == .(round(muendoxexp14,3)) ~ gamma== .(round(gammaendoxexp14,3))))
lines(xvec,dbeta(xvec,bmmfexp14[1],bmmfexp14[2]),type="l")

histmain$counts <- mmf$exp15
histmain$density <- mmf$exp15/sum(mmf$exp15,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main=bquote("Exp15" ~ mu == .(round(muendoxexp15,3)) ~ gamma== .(round(gammaendoxexp15,3))))
lines(xvec,dbeta(xvec,bmmfexp15[1],bmmfexp15[2]),type="l")

histmain$counts <- mmf$exp16
histmain$density <- mmf$exp16/sum(mmf$exp16,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main=bquote("Exp15" ~ mu == .(round(muendoxexp15,3)) ~ gamma== .(round(gammaendoxexp15,3))))
lines(xvec,dbeta(xvec,bmmfexp16[1],bmmfexp16[2]),type="l")

histmain$counts <- mmf$exp17
histmain$density <- mmf$exp17/sum(mmf$exp17,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main=bquote("Exp17" ~ mu == .(round(muendoxexp17,3)) ~ gamma== .(round(gammaendoxexp17,3))))
lines(xvec,dbeta(xvec,bmmfexp17[1],bmmfexp17[2]),type="l")

histmain$counts <- mmf$exp18
histmain$density <- mmf$exp18/sum(mmf$exp18,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main=bquote("Exp18" ~ mu == .(round(muendoxexp18,3)) ~ gamma== .(round(gammaendoxexp18,3))))
lines(xvec,dbeta(xvec,bmmfexp18[1],bmmfexp18[2]),type="l")

histmain$counts <- mmf$exp20
histmain$density <- mmf$exp20/sum(mmf$exp20,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main=bquote("Exp20" ~ mu == .(round(muendoxexp20,3)) ~ gamma== .(round(gammaendoxexp20,3))))
lines(xvec,dbeta(xvec,bmmfexp20[1],bmmfexp20[2]),type="l")

histmain$counts <- mmf$exp21
histmain$density <- mmf$exp21/sum(mmf$exp21,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main=bquote("Exp21" ~ mu == .(round(muendoxexp21,3)) ~ gamma== .(round(gammaendoxexp21,3))))
lines(xvec,dbeta(xvec,bmmfexp21[1],bmmfexp21[2]),type="l")

histmain <- hist(delta$X,breaks =delta$X,ylim=c(0,10))
histmain$mids <-delta$X

par(mfrow=c(4,2),mar=c(4, 3.8, 3, 1.5))
histmain$counts <- delta$exp2
histmain$density <- delta$exp2/sum(delta$exp2,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main="Exp2")

histmain$counts <- delta$exp3
histmain$density <- delta$exp3/sum(delta$exp3,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main="Exp3")

histmain$counts <- delta$exp4
histmain$density <- delta$exp4/sum(delta$exp4,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main="Exp4")

histmain$counts <- delta$exp5
histmain$density <- delta$exp5/sum(delta$exp5,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main="Exp5")

histmain$counts <- delta$exp6
histmain$density <- delta$exp6/sum(delta$exp6,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main="Exp6")

histmain$counts <- delta$exp7
histmain$density <- delta$exp7/sum(delta$exp7,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main="Exp7")

histmain$counts <- delta$exp8
histmain$density <- delta$exp8/sum(delta$exp8,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main="Exp8")

histmain$counts <- delta$exp9
histmain$density <- delta$exp9/sum(delta$exp9,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main="Exp9")

par(mfrow=c(3,3),mar=c(4, 4, 4, 2))

histmain$counts <- delta$exp10
histmain$density <- delta$exp10/sum(delta$exp10,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main="Exp10")

histmain$counts <- delta$exp13
histmain$density <- delta$exp13/sum(delta$exp13,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main="Exp13")

histmain$counts <- delta$exp14
histmain$density <- delta$exp14/sum(delta$exp14,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main="Exp14")

histmain$counts <- delta$exp15
histmain$density <- delta$exp15/sum(delta$exp15,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main="Exp15")

histmain$counts <- delta$exp16
histmain$density <- delta$exp16/sum(delta$exp16,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main="Exp16")

histmain$counts <- delta$exp17
histmain$density <- delta$exp17/sum(delta$exp17,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main="Exp17")

histmain$counts <- delta$exp18
histmain$density <- delta$exp18/sum(delta$exp18,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main="Exp18")

histmain$counts <- delta$exp20
histmain$density <- delta$exp20/sum(delta$exp20,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main="Exp20")

histmain$counts <- delta$exp21
histmain$density <- delta$exp21/sum(delta$exp21,na.rm=T)
plot(histmain,ylim=c(0,10),xlab="Probability",main="Exp21")

xvec <- seq(0,1,0.001)


par(mfrow=c(1,1))
curve((1/17)*dbeta(x,bendoxexp2[1],bendoxexp2[2]) + 
        (1/17)*dbeta(x,bendoxexp3[1],bendoxexp3[2])+
        (1/17)*dbeta(x,bendoxexp4[1],bendoxexp4[2])+
        (1/17)*dbeta(x,bendoxexp5[1],bendoxexp5[2])+
        (1/17)*dbeta(x,bendoxexp6[1],bendoxexp6[2])+
        (1/17)*dbeta(x,bendoxexp7[1],bendoxexp7[2])+
        (1/17)*dbeta(x,bendoxexp8[1],bendoxexp8[2])+
        (1/17)*dbeta(x,bendoxexp9[1],bendoxexp9[2])+
        (1/17)*dbeta(x,bendoxexp10[1],bendoxexp10[2])+
        (1/17)*dbeta(x,bendoxexp13[1],bendoxexp13[2])+
        (1/17)*dbeta(x,bendoxexp14[1],bendoxexp14[2])+
        (1/17)*dbeta(x,bendoxexp15[1],bendoxexp15[2])+
        (1/17)*dbeta(x,bendoxexp16[1],bendoxexp16[2])+
        (1/17)*dbeta(x,bendoxexp17[1],bendoxexp17[2])+
        (1/17)*dbeta(x,bendoxexp18[1],bendoxexp18[2])+
        (1/17)*dbeta(x,bendoxexp20[1],bendoxexp20[2])+
        (1/17)*dbeta(x,bendoxexp21[1],bendoxexp21[2]),
      from=0, to=1, xlab="Probability", ylab="Density")

par(mfrow=c(1,1))
curve((1/17)*dbeta(x,bmmfexp2[1],bmmfexp2[2]) + 
        (1/17)*dbeta(x,bmmfexp3[1],bmmfexp3[2])+
        (1/17)*dbeta(x,bmmfexp4[1],bmmfexp4[2])+
        (1/17)*dbeta(x,bmmfexp5[1],bmmfexp5[2])+
        (1/17)*dbeta(x,bmmfexp6[1],bmmfexp6[2])+
        (1/17)*dbeta(x,bmmfexp7[1],bmmfexp7[2])+
        (1/17)*dbeta(x,bmmfexp8[1],bmmfexp8[2])+
        (1/17)*dbeta(x,bmmfexp9[1],bmmfexp9[2])+
        (1/17)*dbeta(x,bmmfexp10[1],bmmfexp10[2])+
        (1/17)*dbeta(x,bmmfexp13[1],bmmfexp13[2])+
        (1/17)*dbeta(x,bmmfexp14[1],bmmfexp14[2])+
        (1/17)*dbeta(x,bmmfexp15[1],bmmfexp15[2])+
        (1/17)*dbeta(x,bmmfexp16[1],bmmfexp16[2])+
        (1/17)*dbeta(x,bmmfexp17[1],bmmfexp17[2])+
        (1/17)*dbeta(x,bmmfexp18[1],bmmfexp18[2])+
        (1/17)*dbeta(x,bmmfexp20[1],bmmfexp20[2])+
        (1/17)*dbeta(x,bmmfexp21[1],bmmfexp21[2]),
      from=0, to=1, xlab="Probability", ylab="Density")

