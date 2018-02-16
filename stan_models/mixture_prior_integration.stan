data {
  int<lower=0> N1; // number of previous patients in first arm
  int<lower=0> N2; // in the second arm
  int y1[N1]; // binary vector for fist arm
  int y2[N2]; // for the second arm
  int K; // number of expert
  real muphi1[K,2]; // matrix of mu and phi to be integrated, each row is one expert, first arm 
  real muphi2[K,2]; // matrix of mu and phi to be integrated, each row is one expert, second arm
  real<lower=0,upper=1> w[K]; // weight 
  //cov_matrix[K] Sigma; # random effect expert
  int<lower=0> NSAMP;
  real epsilons[NSAMP,2];
}
parameters {
  real<lower=0.001,upper=0.999> p1; // prob of success first arm
  real<lower=0.001,upper=0.999> p2; // prob of success second arm
}
transformed parameters {
  real Delta;
  Delta <- p2 - p1;
}
model {
  real mixture[K]; 
  //real epsilons[2];
  //real finalprior;
  real supp[4];
  // likelihood
  y1 ~ bernoulli(p1);
  y2 ~ bernoulli(p2);
  
  // prior
  for (k in 1:K){
    mixture[k] <- 0;
    for (i in 1:NSAMP){
      
    //epsilons <- multi_normal_rng(rep_vector(0,2),Sigma);
    supp[1] <- 1/(1+exp(-(muphi1[k,1] + epsilons[i,1])));
    supp[2] <- exp(muphi1[k,2] + epsilons[i,2] );
    supp[3] <- 1/(1+exp(-(muphi2[k,1] + epsilons[i,1])));
    supp[4] <- exp(muphi2[k,2] + epsilons[i,2] );
    
    mixture[k] <- mixture[k] + exp(beta_log(p1, supp[1]*supp[2], (1-supp[1])*supp[2]))*
                   exp(beta_log(p2, supp[3]*supp[4], (1-supp[3])*supp[4]));
    }
  mixture[k] <- w[k]*(mixture[k]/NSAMP);
  }
  increment_log_prob(log(sum(mixture)));
}