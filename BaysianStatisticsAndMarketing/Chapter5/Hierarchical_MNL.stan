data{
  int<lower=0> N_x;
  int<lower=0> N_z;
  int<lower=0> p_x;
  int<lower=0> p_z;
  
  int y[N_x];
  matrix[N_x, p_x] X;
  matrix[N_z, p_z] Z;
  int<lower=0> hhid[N_x]; 
}

transformed data{
  real nu;
  matrix[p_x, p_x] I;
  
  nu = p_x + 3;
  I = diag_matrix(rep_vector(1, p_x));
}

parameters{
  vector[p_x] beta_ast[N_z];
  matrix[p_z, p_x] Delta;
  cov_matrix[p_x] V_b;
}

transformed parameters{
  vector[p_x] beta[N_z];
  matrix[p_x, p_x] L_b;
  matrix[p_x, p_x] L_d;
  
  L_b = cholesky_decompose(V_b);
  L_d = cholesky_decompose(100*V_b);
  for(i in 1:N_z){
    beta[i] = beta_ast[i] + Delta' * Z[i]';
  }
}

model{
  for(i in 1:N_x){
    y[i] ~ categorical(softmax(beta[hhid[i]] .* to_vector(X[i])));
  }
  for(i in 1:p_z){
    Delta[i] ~ multi_normal_cholesky(rep_vector(0, p_x), L_d);
  }
  beta_ast ~ multi_normal_cholesky(rep_vector(0, p_x), L_b);
  V_b ~ inv_wishart(nu, nu*I);
}
