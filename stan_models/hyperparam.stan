data {
  int<lower=1> K; // number of expert
  real mugamma1[K,2]; // matrix of mu and gamma, each row is one expert, first arm
  real mugamma2[K,2]; // matrix of mu and gamma, each row is one expert, second arm
  int<lower=1> n1; // number of covaraite for mu
  int<lower=1> n2; // number of covaraite for phi
  matrix[K,n1] Xmu; // matmu
  matrix[K,n2] Xgamma; // matmu
  //real<lower=0,upper=1> w[K]; // weight 
}
transformed data {
  matrix[K,2] mugamma1transf;
  matrix[K,2] mugamma2transf;
  
  for (k in 1:K){
    mugamma1transf[k,1] <- log(mugamma1[k,1]/(1 - mugamma1[k,1]));
    mugamma2transf[k,1] <- log(mugamma2[k,1]/(1 - mugamma2[k,1]));
    mugamma1transf[k,2] <- log(mugamma1[k,2]);
    mugamma2transf[k,2] <- log(mugamma2[k,2]);
  }
}
parameters {
  real upsilonMU[2]; // one for each arm
  real upsilonGAMMA[2]; // one for each arm
  vector[n1] betaMU; // parameters for mu
  vector[n2] betaGAMMA; // parameters for gamma
  real<lower=-1,upper=1> rho; // correlation
  vector<lower=0.001>[2] tau;
  matrix[K,2] Epsilon; // randome effects for expert
  real<lower=0.001> sigma0;
}
model {
  vector[2] meankj1; 
  vector[2] meankj2; 
  matrix[2,2] Sigma;
  
  Sigma[1,1] <- tau[1]^2;
  Sigma[2,2] <- tau[2]^2;
  Sigma[2,1] <- tau[1]*tau[2]*rho;
  Sigma[1,2] <- tau[1]*tau[2]*rho;
  
  
  for (k in 1:K){
    meankj1[1] <- upsilonMU[1] + row(Xmu,k)*betaMU + Epsilon[k,1];
    meankj1[2] <- upsilonGAMMA[1] + row(Xgamma,k)*betaGAMMA + Epsilon[k,2];
    row(mugamma1transf,k) ~ multi_normal(meankj1,diag_matrix(rep_vector(sigma0,2)));
    
    meankj2[1] <- upsilonMU[2] + row(Xmu,k)*betaMU + Epsilon[k,1];
    meankj2[2] <- upsilonGAMMA[2] + row(Xgamma,k)*betaGAMMA + Epsilon[k,2];
    row(mugamma2transf,k) ~ multi_normal(meankj2,diag_matrix(rep_vector(sigma0,2)));
  }
  
  // priors
  upsilonMU ~ normal(0,100);
  upsilonGAMMA  ~ normal(0,100);
  betaMU  ~ normal(0,10);
  betaGAMMA  ~ normal(0,10);
  rho ~ uniform(-1,1);
  tau ~ inv_gamma(1,1);
  for (k in 1:K) row(Epsilon,k)  ~ multi_normal(rep_vector(0,2), Sigma );
  
}