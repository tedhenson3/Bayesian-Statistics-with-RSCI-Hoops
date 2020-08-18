 data {
   // Define variables in data
   // Number of observations (an integer)
   int<lower=0> N;
   // Number of parameters
   int<lower=0> p;
   // Variables
   real y[N];
   int<lower=0>  Is_Big[N];
   int<lower=0>  Is_Guard[N];
   real Composite_Rating[N];
   int<lower=0>  Age[N];
 }
 
 parameters {
   // Define parameters to estimate
   real beta[p];
   // standard deviation (a positive real number)
   real<lower=0> sigma;
 }
 
 transformed parameters  {
   // Mean
   real mu[N];
  for (i in 1:N) {
   mu[i] = beta[1] + beta[2]*Is_Big[i] + beta[3]*Is_Guard[i] + beta[4]*Composite_Rating[i] + beta[5]*Age[i]; 
  }
 }
 
 model {
   // Prior part of Bayesian inference (flat if unspecified)
  beta[4] ~ lognormal(0, .25);
  beta[3] ~ normal(0, 1);
  beta[2] ~ normal(0, 1);
  beta[5] ~ normal(2,1);
   // Likelihood part of Bayesian inference
     y ~ normal(mu, sigma);  
 }