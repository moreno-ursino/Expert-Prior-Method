data {
  int<lower=0> N1; // number of previous patients in first arm
  int<lower=0> N2; // in the second arm
  int y1[N1]; // binary vector for fist arm
  int y2[N2]; // for the second arm
}
parameters {
  real<lower=0,upper=1> p1; // prob of success first arm
  real<lower=0,upper=1> p2; // prob of success second arm
}
transformed parameters {
  real theta;
  theta <- p2 - p1;
}
model {

  // likelihood
  y1 ~ bernoulli(p1);
  y2 ~ bernoulli(p2);
  
  // prior
  p1 ~ beta(0.5,0.5);
  p2 ~ beta(0.5,0.5);
}