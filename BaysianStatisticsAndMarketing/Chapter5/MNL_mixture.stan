data{
  int<lower=0> N_x;
  int<lower=0> p_y;
  int<lower=0> p_x;
  
  int y[N_x];
  matrix[N_x, p_x] X;
  
  int K;
}
parameters{
  vector[p_x] delta_0;
  vector[1] delta;
  vector[p_x+1] u[N_x];
  vector[p_x+1] mu[K];
  cov_matrix[p_x+1] Sigma[K];
  simplex[K] pi[N_x];
}
model{
  real ps[K];
  vector[p_y] temp;
  real nu;
  matrix[p_x+1, p_x+1] I;
  
  nu = p_x + 3;
  I = diag_matrix(rep_vector(1, p_x+1));
  
  for(i in 1:N_x){
    // coef * var
    temp = to_vector(X[i] * delta[1] + u[i][p_x+1]);
    // intercept
    temp = temp + delta_0 + u[i][1:p_x];
    y[i] ~ categorical(softmax(temp));
    
    pi[i] ~ dirichlet(rep_vector(2, K));
    for(k in 1:K){
      ps[k] = log(pi[i][k]) + multi_normal_lpdf(u[i] | mu[k], Sigma[k]);
    }
    target += log_sum_exp(ps);
  }
  
  for(k in 1:K){
    mu[k] ~ multi_normal(rep_vector(0, p_x+1), 16*Sigma[k]);
    Sigma[k] ~ inv_wishart(nu, nu*I);
  }
  append_row(delta_0, to_vector(delta)) ~ multi_normal(rep_vector(0, p_x+1), 100*I);
}
generated quantities{
  vector[p_x] beta;
  beta[1:(p_x-1)] = delta_0[1:(p_x-1)] - delta_0[p_x];
  beta[p_x] = delta[1];
}