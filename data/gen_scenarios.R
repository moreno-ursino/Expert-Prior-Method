########################################################################
#
#   Generating scenarios
#
################ starting simulation

N1= N2 = 35

TR=500
SEDD <- 957

#example scen 1, change p1r and p2r for other scenarios
p1r=0.5
p2r=0.5

for (tr in 1:TR){
  set.seed(SEDD)
  y1=rbinom(N1,1,p1r)
  y2=rbinom(N2,1,p2r)
  
  eval(parse(text = paste("data",tr, " <- list(y1=y1, y2=y2)", sep="")))
  SEDD = SEDD + tr
}

save.image()


