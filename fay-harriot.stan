// Stan File 

// Covariates: 
//------------------------------------------------
data {
int K; // number of observations 
int P; // number of variables 
matrix[K, (P+1)] X; // DESIGN MATRIX
vector[K] y; // response 
vector[K] D; // sampling errors
}
//------------------------------------------------

// Parameters: 
parameters {
vector[P+1] beta;
vector[K] theta;
real<lower=0> sigma_v; // this is the standard deviation
}

//------------------------------------------------
model {
  vector[K] mu;
// PRIORS: 
  // Beta & Sigma^2_v
  target += -log(sigma_v^2);
  // Theta
  mu = X*beta;
  for(k in 1:K){ 
    theta[k] ~ normal(mu[k], sigma_v); //includes intercept
  }
  
  // ***********
  // Likelihood 
  for(i in 1:K){
    y[i] ~ normal(theta[i], D[i]); 
  }
} 
