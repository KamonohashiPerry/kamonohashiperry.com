data{
  int<lower=0> N_x; // 家計の数
  int<lower=0> p_y; // ブランド数
  int<lower=0> p_x; // 説明変数の数
  
  int y[N_x]; // 被説明変数
  matrix[N_x, p_x] X; // 説明変数
  
  int K; // 混合ガウスの平均値などの数
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
    y[i] ~ categorical(softmax(temp)); //カテゴリカル分布にソフトマックス関数を組み合わせて多項ロジスティックモデルにしている。
    
    pi[i] ~ dirichlet(rep_vector(2, K)); // 多項分布の混合確率の共役事前分布
    for(k in 1:K){
      ps[k] = log(pi[i][k]) + multi_normal_lpdf(u[i] | mu[k], Sigma[k]);
    }
    target += log_sum_exp(ps);
  }
  
  for(k in 1:K){
    mu[k] ~ multi_normal(rep_vector(0, p_x+1), 16*Sigma[k]); // 混合ガウスのパラメータの事前分布
    Sigma[k] ~ inv_wishart(nu, nu*I); // 多変量正規分布の共分散の共役事前分布
  }
  append_row(delta_0, to_vector(delta)) ~ multi_normal(rep_vector(0, p_x+1), 100*I);
}

generated quantities{
  vector[p_x] beta;
  beta[1:(p_x-1)] = delta_0[1:(p_x-1)] - delta_0[p_x];
  beta[p_x] = delta[1];
}
